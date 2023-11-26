pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

const MEM_SIZE: usize = 65536 * 2;

const START_ADDR: u16 = 0x0100;

const ZERO_FLAG: u8 = 0b1000_0000;
const SUB_FLAG: u8 = 0b0100_0000;
const HALF_FLAG: u8 = 0b0010_0000;
const CARRY_FLAG: u8 = 0b0001_0000;

const INTERRUPT_ENABLE: usize = 0xFFFF; // Interrupt Enable Register
const INTERRUPT_FLAG: usize = 0xFF0F; // Interrupt Request (Flag) Register

// Each bit in the interrupt registers corresponds to a different interrupt:
const VBLANK: u8 = 1 << 0; // V-Blank  Interrupt Request
const LCD_STAT: u8 = 1 << 1; // LCD STAT Interrupt Request
const TIMER: u8 = 1 << 2; // Timer    Interrupt Request
const SERIAL: u8 = 1 << 3; // Serial   Interrupt Request
const JOYPAD: u8 = 1 << 4; // Joypad   Interrupt Request

const VBLANK_ISR: u16 = 0x0040; // V-Blank interrupt vector address
const LCD_STAT_ISR: u16 = 0x0048; // LCD STAT interrupt vector address
const TIMER_ISR: u16 = 0x0050; // Timer interrupt vector address
const SERIAL_ISR: u16 = 0x0058; // Serial interrupt vector address
const JOYPAD_ISR: u16 = 0x0060; // Joypad interrupt vector address

const DMA_REGISTER: u16 = 0xFF46;
const OAM_START: u16 = 0xFE00;
const OAM_SIZE: usize = 160; // 40 sprites * 4 bytes each

#[derive(Debug)]
enum PpuState {
    GpuModeHblank = 0,
    GpuModeVblank = 1,
    GpuModeOam = 2,
    GpuModeVram = 3,
}

pub struct Emu {
    mem: [u8; MEM_SIZE],
    screen: [(u8, u8, u8); SCREEN_WIDTH * SCREEN_HEIGHT],
    sp: u16,
    pc: u16,
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    is_halted: bool,
    interrupts_enabled: bool,
    gpu_state: PpuState,
    cycles: u16,
    scanline: u8,
}

impl Emu {
    pub fn new() -> Self {
        let new_emu = Self {
            mem: [0; MEM_SIZE],
            screen: [(255, 255, 255); SCREEN_WIDTH * SCREEN_HEIGHT],
            sp: 0xFFFE,
            pc: 0x0100,
            a: 0x01,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            f: 0xB0,
            h: 0x01,
            l: 0x4D,
            is_halted: false,
            interrupts_enabled: false,
            gpu_state: PpuState::GpuModeHblank,
            cycles: 0,
            scanline: 0,
        };

        new_emu
    }

    fn af(&self) -> u16 {
        ((self.a as u16) << 8) | (self.f as u16)
    }

    fn bc(&self) -> u16 {
        ((self.b as u16) << 8) | (self.c as u16)
    }

    fn de(&self) -> u16 {
        ((self.d as u16) << 8) | (self.e as u16)
    }

    fn hl(&self) -> u16 {
        ((self.h as u16) << 8) | (self.l as u16)
    }

    fn set_bc(&mut self, val: u16) {
        let higher: u8 = (val >> 8) as u8;
        let lower: u8 = val as u8;
        self.b = higher;
        self.c = lower;
    }

    fn set_de(&mut self, val: u16) {
        let higher: u8 = (val >> 8) as u8;
        let lower: u8 = val as u8;
        self.d = higher;
        self.e = lower;
    }

    fn set_hl(&mut self, val: u16) {
        let higher: u8 = (val >> 8) as u8;
        let lower: u8 = val as u8;
        self.h = higher;
        self.l = lower;
    }

    fn set_af(&mut self, val: u16) {
        let higher: u8 = (val >> 8) as u8;
        let lower: u8 = val as u8;
        self.a = higher;
        self.f = lower & 0xF0;
    }

    fn set_zero(&mut self, z: bool) {
        match z {
            true => self.f |= ZERO_FLAG,
            false => self.f &= !ZERO_FLAG,
        }
    }

    fn set_sub(&mut self, s: bool) {
        match s {
            true => self.f |= SUB_FLAG,
            false => self.f &= !SUB_FLAG,
        }
    }

    fn set_half(&mut self, h: bool) {
        match h {
            true => self.f |= HALF_FLAG,
            false => self.f &= !HALF_FLAG,
        }
    }

    fn set_carry(&mut self, c: bool) {
        match c {
            true => self.f |= CARRY_FLAG,
            false => self.f &= !CARRY_FLAG,
        }
    }

    fn get_carry(&mut self) -> bool {
        (self.f & 0b0001_0000) != 0
    }

    fn get_sub(&mut self) -> bool {
        (self.f & SUB_FLAG) != 0
    }

    fn get_half(&mut self) -> bool {
        (self.f & HALF_FLAG) != 0
    }

    fn get_zero(&mut self) -> bool {
        (self.f & ZERO_FLAG) != 0
    }

    pub fn reset(&mut self) {
        self.mem = [0; MEM_SIZE];
        self.screen = [(0, 0, 0); SCREEN_WIDTH * SCREEN_HEIGHT];
        self.sp = 0xFFFE;
        self.pc = START_ADDR;
        self.a = 0;
        self.b = 0;
        self.c = 0;
        self.d = 0;
        self.e = 0;
        self.f = 0;
        self.h = 0;
        self.l = 0;
        self.interrupts_enabled = false;
        self.is_halted = false;
        self.gpu_state = PpuState::GpuModeHblank;
        self.cycles = 0;
        self.scanline = 0;
    }

    pub fn tick(&mut self) {
        let op = self.fetch();
        let cycles = self.execute(op);
        self.ppu(cycles);

        // Check if a DMA transfer should start
        let dma_value = self.mem[DMA_REGISTER as usize];
        if dma_value != 0 {
            println!("{:04X}", self.pc);
            // Calculate the source address from the DMA register value
            let src_start = (dma_value as u16) << 8;

            // Perform the DMA transfer, copying byte by byte
            for i in 0..OAM_SIZE {
                let byte = self.mem[src_start as usize + i];
                self.mem[OAM_START as usize + i] = byte;
            }

            // Clear the DMA start condition by writing 0 to the DMA register
            // to indicate that the DMA transfer has completed
            self.mem[DMA_REGISTER as usize] = 0;
        }

        if self.interrupts_enabled {
            self.handle_interrupts();
        }
    }

    fn handle_interrupts(&mut self) {
        let enabled = self.mem[INTERRUPT_ENABLE];
        let requested = self.mem[INTERRUPT_FLAG];
        let fired = enabled & requested;

        if fired != 0 {
            self.interrupts_enabled = false;
            self.push_stack(self.pc);

            if fired & VBLANK != 0 {
                self.mem[INTERRUPT_FLAG] &= !VBLANK;
                self.pc = VBLANK_ISR;
            } else if fired & LCD_STAT != 0 {
                self.mem[INTERRUPT_FLAG] &= !LCD_STAT;
                self.pc = LCD_STAT_ISR;
            } else if fired & TIMER != 0 {
                self.mem[INTERRUPT_FLAG] &= !TIMER;
                self.pc = TIMER_ISR;
            } else if fired & SERIAL != 0 {
                self.mem[INTERRUPT_FLAG] &= !SERIAL;
                self.pc = SERIAL_ISR;
            } else if fired & JOYPAD != 0 {
                self.mem[INTERRUPT_FLAG] &= !JOYPAD;
                self.pc = JOYPAD_ISR;
            }

            // Handling the interrupt takes some cycles, adjust your cycle count accordingly
            // self.cycles -= 5;
        }
    }

    fn push_stack(&mut self, value: u16) {
        // Gameboy stack is full descending, which means it grows down in memory
        self.sp = self.sp.wrapping_sub(2);
        self.mem[(self.sp + 1) as usize] = (value >> 8) as u8; // Push high byte
        self.mem[self.sp as usize] = (value & 0xFF) as u8; // Push low byte
    }

    pub fn tick_timers(&mut self) {
        // TODO
    }

    pub fn print_data(&mut self) -> String {
        format!(
            "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:02X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}\n",
            self.a,
            self.f,
            self.b,
            self.c,
            self.d,
            self.e,
            self.h,
            self.l,
            self.sp,
            self.pc,
            self.mem[self.pc as usize],
            self.mem[self.pc as usize + 1],
            self.mem[self.pc as usize + 2],
            self.mem[self.pc as usize + 3]
        )
    }

    pub fn get_display(&self) -> &[(u8, u8, u8)] {
        &self.screen
    }

    fn geti_vblank(&mut self) -> bool {
        (self.mem[0xFF0F as usize] & 0b0000_0001) != 0
    }

    fn get_lcd_control(&mut self) -> u8 {
        self.mem[0xFF40 as usize]
    }

    fn scrolly(&mut self) -> u8 {
        self.mem[0xFF42 as usize]
    }

    fn scrollx(&mut self) -> u8 {
        self.mem[0xFF43 as usize]
    }

    pub fn ppu(&mut self, cycles: u16) {
        self.cycles += cycles;

        // println!("{} {} {:?}", self.cycles, self.scanline, self.gpu_state);

        match self.gpu_state {
            PpuState::GpuModeHblank => {
                if self.cycles >= 204 {
                    self.cycles -= 204;
                    self.scanline += 1;
                    self.mem[0xFF44] = self.scanline; // Update the scanline counter

                    if self.scanline == 144 {
                        // Transition to VBlank
                        self.mem[0xFF0F] |= 0b0000_0001; // Request VBlank interrupt
                        self.gpu_state = PpuState::GpuModeVblank;
                    } else {
                        self.gpu_state = PpuState::GpuModeOam;
                    }
                }
            }
            PpuState::GpuModeVblank => {
                if self.cycles >= 456 {
                    self.cycles -= 456;
                    self.scanline += 1;
                    if self.scanline > 153 {
                        self.scanline = 0;
                        self.gpu_state = PpuState::GpuModeOam;
                    }
                    self.mem[0xFF44] = self.scanline; // Update the scanline counter
                }
            }
            PpuState::GpuModeOam => {
                if self.cycles >= 80 {
                    self.cycles -= 80;
                    self.gpu_state = PpuState::GpuModeVram;
                }
            }
            PpuState::GpuModeVram => {
                if self.cycles >= 172 {
                    self.cycles -= 172;
                    self.gpu_state = PpuState::GpuModeHblank;
                    self.render_scanline();
                }
            }
        }
    }

    fn render_scanline(&mut self) {
        let bg_map_offset = if self.get_lcd_control() & (1 << 3) != 0 {
            0x9C00
        } else {
            0x9800
        };
        let tile_data_base = if self.get_lcd_control() & (1 << 4) != 0 {
            0x8000
        } else {
            0x8800
        };

        let map_offset = bg_map_offset + ((self.scanline as u16 + self.scrolly() as u16) / 8) * 32;

        let line_offset = self.scrollx() as u16 / 8;

        let y = (self.scanline.wrapping_add(self.scrolly())) & 7;

        let pixel_offset = self.scanline as u16 * 160;

        // background
        for tile_x in 0..20 {
            let tile_index = self.mem[(map_offset + line_offset + tile_x) as usize];

            let tile_addr = if self.get_lcd_control() & (1 << 4) == 0 {
                // Signed tile index (0x8800 - 0x97FF)
                let signed_index = tile_index as i8 as i16;
                tile_data_base + ((signed_index + 128) * 16) as usize
            } else {
                // Unsigned tile index (0x8000 - 0x8FFF)
                tile_data_base + (tile_index as u16 * 16) as usize
            };

            let tile_data_lower = self.mem[tile_addr + 2 * y as usize];
            let tile_data_higher = self.mem[tile_addr + 2 * y as usize + 1];

            for tile_pixel in 0..8 {
                let higher = tile_data_higher & (1 << (7 - tile_pixel)) != 0;
                let lower = tile_data_lower & (1 << (7 - tile_pixel)) != 0;

                let color: (u8, u8, u8) = match (higher, lower) {
                    (false, false) => (255, 255, 255),
                    (false, true) => (148, 148, 148),
                    (true, false) => (96, 96, 96),
                    (true, true) => (0, 0, 0),
                };

                // println!("{} {} {}", color.0, color.1, color.2);

                let x = (tile_x * 8 + tile_pixel) as usize;
                self.screen[pixel_offset as usize + x] = color;
            }
        }

        let base_sprite_offset = 0xFE00;
        let tall_mode = self.get_lcd_control() & (1 << 2) == 0;

        // sprites
        for sprite in 0..40 {
            let sprite_addr = base_sprite_offset + sprite * 4;
            let sprite_y = self.mem[sprite_addr] as i16 - 16;
            let sprite_x = self.mem[sprite_addr + 1] as i16 - 8;
            let sprite_tile_index = self.mem[sprite_addr + 2];
            let sprite_flags = self.mem[sprite_addr + 3];
            let x_flip = sprite_flags & (1 << 5) != 0;
            let y_flip = sprite_flags & (1 << 6) != 0;

            let height = match tall_mode {
                true => 16,
                false => 8,
            };
            for sprite_line in 0..height {
                let tile_line = match y_flip {
                    true => height - 1 - sprite_line,
                    false => sprite_line,
                };
                let tile_index = if tall_mode && sprite_line >= 8 {
                    sprite_tile_index as u16 + 1
                } else {
                    sprite_tile_index as u16
                };
                let sprite_tile_addr = 0x8000 + (tile_index as usize * 16) + (tile_line % 8 * 2);

                let tile_data_lower = self.mem[sprite_tile_addr as usize];
                let tile_data_higher = self.mem[sprite_tile_addr as usize + 1];

                for sprite_pixel in 0..8 {
                    let tile_pixel = match x_flip {
                        true => 7 - sprite_pixel,
                        false => sprite_pixel,
                    };
                    let higher = tile_data_higher & (1 << (7 - tile_pixel)) != 0;
                    let lower = tile_data_lower & (1 << (7 - tile_pixel)) != 0;

                    let color: (u8, u8, u8) = match (higher, lower) {
                        (false, false) => (255, 255, 255),
                        (false, true) => (148, 148, 148),
                        (true, false) => (96, 96, 96),
                        (true, true) => (0, 0, 0),
                    };

                    let screen_x = sprite_x as usize + tile_pixel;
                    let screen_y = sprite_y as usize + tile_line;

                    if screen_x < 160 && screen_y < 144 {
                        if color != (0, 0, 0) {
                            self.screen[pixel_offset as usize
                                + (screen_y * 160) as usize
                                + (screen_x) as usize] = color;
                        }
                    }
                }
            }
        }
    }

    pub fn keypress(&mut self, idx: usize, pressed: bool) {
        if idx == 8 {
            println!("{}", self.print_memory());
        }

        let dir = (self.mem[0xFF00] & 0b0010_0000) == 0; // Select direction keys
        let action = (self.mem[0xFF00] & 0b0001_0000) == 0; // Select action buttons

        if dir {
            match idx {
                0 => self.update_key_state(3, pressed), // Down
                1 => self.update_key_state(2, pressed), // Up
                2 => self.update_key_state(1, pressed), // Left
                3 => self.update_key_state(0, pressed), // Right
                _ => {}
            }
        } else if action {
            match idx {
                4 => self.update_key_state(3, pressed), // Start
                5 => self.update_key_state(2, pressed), // Select
                6 => self.update_key_state(1, pressed), // B
                7 => self.update_key_state(0, pressed), // A
                _ => {}
            }
        }
    }

    fn update_key_state(&mut self, bit: usize, pressed: bool) {
        if pressed {
            self.mem[0xFF00] &= !(1 << bit);
        } else {
            self.mem[0xFF00] |= 1 << bit;
        }
    }

    pub fn load(&mut self, data: &[u8]) {
        self.mem[..data.len()].copy_from_slice(&data);
    }

    pub fn print_memory(&mut self) -> String {
        let mut mem_str = String::new();
        for i in 0x8800..MEM_SIZE {
            mem_str.push_str(&format!("{:02X} ", self.mem[i]));
            if i % 16 == 15 {
                mem_str.push_str("\n");
            }
        }
        mem_str
    }

    fn fetch(&mut self) -> u8 {
        let opcode = self.mem[self.pc as usize];
        self.pc = self.pc.wrapping_add(1);
        opcode
    }

    fn execute(&mut self, op: u8) -> u16 {
        match op {
            0xCB => self.execute_16bit(),
            0x00 => self.nop(),
            0x01 => self.ld_bcnn(),
            0x02 => self.ld_bca(),
            0x03 => self.inc_bc(),
            0x04 => self.inc_b(),
            0x05 => self.dec_b(),
            0x06 => self.ld_bn(),
            0x07 => self.rlca(),
            0x08 => self.ld_nnsp(),
            0x09 => self.add_hlbc(),
            0x0A => self.ld_abc(),
            0x0B => self.dec_bc(),
            0x0C => self.inc_c(),
            0x0D => self.dec_c(),
            0x0E => self.ld_cn(),
            0x0F => self.rrca(),
            0x10 => self.stop(),
            0x11 => self.ld_denn(),
            0x12 => self.ld_dea(),
            0x13 => self.inc_de(),
            0x14 => self.inc_d(),
            0x15 => self.dec_d(),
            0x16 => self.ld_dn(),
            0x17 => self.rla(),
            0x18 => self.jr_dd(),
            0x19 => self.add_hlde(),
            0x1A => self.ld_ade(),
            0x1B => self.dec_de(),
            0x1C => self.inc_e(),
            0x1D => self.dec_e(),
            0x1E => self.ld_en(),
            0x1F => self.rra(),
            0x20 => self.jr_nz(),
            0x21 => self.ld_hlnn(),
            0x22 => self.ldi_hla(),
            0x23 => self.inc_hlr(),
            0x24 => self.inc_h(),
            0x25 => self.dec_h(),
            0x26 => self.ld_hn(),
            0x27 => self.daa(),
            0x28 => self.jr_z(),
            0x29 => self.add_hlhl(),
            0x2A => self.ldi_ahl(),
            0x2B => self.dec_hlr(),
            0x2C => self.inc_l(),
            0x2D => self.dec_l(),
            0x2E => self.ld_ln(),
            0x2F => self.cpl(),
            0x30 => self.jr_nc(),
            0x31 => self.ld_spnn(),
            0x32 => self.ldd_hla(),
            0x33 => self.inc_sp(),
            0x34 => self.inc_hl(),
            0x35 => self.dec_hl(),
            0x36 => self.ld_hln(),
            0x37 => self.scf(),
            0x38 => self.jr_c(),
            0x39 => self.add_hlsp(),
            0x3A => self.ldd_ahl(),
            0x3B => self.dec_sp(),
            0x3C => self.inc_a(),
            0x3D => self.dec_a(),
            0x3E => self.ld_an(),
            0x3F => self.ccf(),
            0x40 => self.ld_b(self.b),
            0x41 => self.ld_b(self.c),
            0x42 => self.ld_b(self.d),
            0x43 => self.ld_b(self.e),
            0x44 => self.ld_b(self.h),
            0x45 => self.ld_b(self.l),
            0x46 => self.ld_b(self.mem[self.hl() as usize]),
            0x47 => self.ld_b(self.a),
            0x48 => self.ld_c(self.b),
            0x49 => self.ld_c(self.c),
            0x4A => self.ld_c(self.d),
            0x4B => self.ld_c(self.e),
            0x4C => self.ld_c(self.h),
            0x4D => self.ld_c(self.l),
            0x4E => self.ld_c(self.mem[self.hl() as usize]),
            0x4F => self.ld_c(self.a),
            0x50 => self.ld_d(self.b),
            0x51 => self.ld_d(self.c),
            0x52 => self.ld_d(self.d),
            0x53 => self.ld_d(self.e),
            0x54 => self.ld_d(self.h),
            0x55 => self.ld_d(self.l),
            0x56 => self.ld_d(self.mem[self.hl() as usize]),
            0x57 => self.ld_d(self.a),
            0x58 => self.ld_e(self.b),
            0x59 => self.ld_e(self.c),
            0x5A => self.ld_e(self.d),
            0x5B => self.ld_e(self.e),
            0x5C => self.ld_e(self.h),
            0x5D => self.ld_e(self.l),
            0x5E => self.ld_e(self.mem[self.hl() as usize]),
            0x5F => self.ld_e(self.a),
            0x60 => self.ld_h(self.b),
            0x61 => self.ld_h(self.c),
            0x62 => self.ld_h(self.d),
            0x63 => self.ld_h(self.e),
            0x64 => self.ld_h(self.h),
            0x65 => self.ld_h(self.l),
            0x66 => self.ld_h(self.mem[self.hl() as usize]),
            0x67 => self.ld_h(self.a),
            0x68 => self.ld_l(self.b),
            0x69 => self.ld_l(self.c),
            0x6A => self.ld_l(self.d),
            0x6B => self.ld_l(self.e),
            0x6C => self.ld_l(self.h),
            0x6D => self.ld_l(self.l),
            0x6E => self.ld_l(self.mem[self.hl() as usize]),
            0x6F => self.ld_l(self.a),
            0x70 => self.ld_hl(self.b),
            0x71 => self.ld_hl(self.c),
            0x72 => self.ld_hl(self.d),
            0x73 => self.ld_hl(self.e),
            0x74 => self.ld_hl(self.h),
            0x75 => self.ld_hl(self.l),
            0x76 => self.halt(),
            0x77 => self.ld_hl(self.a),
            0x78 => self.ld_a(self.b),
            0x79 => self.ld_a(self.c),
            0x7A => self.ld_a(self.d),
            0x7B => self.ld_a(self.e),
            0x7C => self.ld_a(self.h),
            0x7D => self.ld_a(self.l),
            0x7E => self.ld_a(self.mem[self.hl() as usize]),
            0x7F => self.ld_a(self.a),
            0x80 => self.add_ar(self.b),
            0x81 => self.add_ar(self.c),
            0x82 => self.add_ar(self.d),
            0x83 => self.add_ar(self.e),
            0x84 => self.add_ar(self.h),
            0x85 => self.add_ar(self.l),
            0x86 => self.add_ar(self.mem[self.hl() as usize]),
            0x87 => self.add_ar(self.a),
            0x88 => self.adc_ar(self.b),
            0x89 => self.adc_ar(self.c),
            0x8A => self.adc_ar(self.d),
            0x8B => self.adc_ar(self.e),
            0x8C => self.adc_ar(self.h),
            0x8D => self.adc_ar(self.l),
            0x8E => self.adc_ar(self.mem[self.hl() as usize]),
            0x8F => self.adc_ar(self.a),
            0x90 => self.sub_ar(self.b),
            0x91 => self.sub_ar(self.c),
            0x92 => self.sub_ar(self.d),
            0x93 => self.sub_ar(self.e),
            0x94 => self.sub_ar(self.h),
            0x95 => self.sub_ar(self.l),
            0x96 => self.sub_ar(self.mem[self.hl() as usize]),
            0x97 => self.sub_ar(self.a),
            0x98 => self.sbc_ar(self.b),
            0x99 => self.sbc_ar(self.c),
            0x9A => self.sbc_ar(self.d),
            0x9B => self.sbc_ar(self.e),
            0x9C => self.sbc_ar(self.h),
            0x9D => self.sbc_ar(self.l),
            0x9E => self.sbc_ar(self.mem[self.hl() as usize]),
            0x9F => self.sbc_ar(self.a),
            0xA0 => self.and_ar(self.b),
            0xA1 => self.and_ar(self.c),
            0xA2 => self.and_ar(self.d),
            0xA3 => self.and_ar(self.e),
            0xA4 => self.and_ar(self.h),
            0xA5 => self.and_ar(self.l),
            0xA6 => self.and_ar(self.mem[self.hl() as usize]),
            0xA7 => self.and_ar(self.a),
            0xA8 => self.xor_ar(self.b),
            0xA9 => self.xor_ar(self.c),
            0xAA => self.xor_ar(self.d),
            0xAB => self.xor_ar(self.e),
            0xAC => self.xor_ar(self.h),
            0xAD => self.xor_ar(self.l),
            0xAE => self.xor_ar(self.mem[self.hl() as usize]),
            0xAF => self.xor_ar(self.a),
            0xB0 => self.or_ar(self.b),
            0xB1 => self.or_ar(self.c),
            0xB2 => self.or_ar(self.d),
            0xB3 => self.or_ar(self.e),
            0xB4 => self.or_ar(self.h),
            0xB5 => self.or_ar(self.l),
            0xB6 => self.or_ar(self.mem[self.hl() as usize]),
            0xB7 => self.or_ar(self.a),
            0xB8 => self.cp_ar(self.b),
            0xB9 => self.cp_ar(self.c),
            0xBA => self.cp_ar(self.d),
            0xBB => self.cp_ar(self.e),
            0xBC => self.cp_ar(self.h),
            0xBD => self.cp_ar(self.l),
            0xBE => self.cp_ar(self.mem[self.hl() as usize]),
            0xBF => self.cp_ar(self.a),
            0xC0 => self.ret_nz(),
            0xC1 => self.pop_bc(),
            0xC2 => self.jp_nz(),
            0xC3 => self.jp_nn(),
            0xC4 => self.call_nz(),
            0xC5 => self.push(self.bc()),
            0xC6 => self.add_an(),
            0xC7 => self.rst(0x00),
            0xC8 => self.ret_z(),
            0xC9 => self.ret(),
            0xCA => self.jp_z(),
            0xCC => self.call_z(),
            0xCD => self.call_nn(),
            0xCE => self.adc_an(),
            0xCF => self.rst(0x08),
            0xD0 => self.ret_nc(),
            0xD1 => self.pop_de(),
            0xD2 => self.jp_nc(),
            0xD4 => self.call_nc(),
            0xD5 => self.push(self.de()),
            0xD6 => self.sub_an(),
            0xD7 => self.rst(0x10),
            0xD8 => self.ret_c(),
            0xD9 => self.reti(),
            0xDA => self.jp_c(),
            0xDC => self.call_c(),
            0xDE => self.sbc_an(),
            0xDF => self.rst(0x18),
            0xE0 => self.ld_ff00na(),
            0xE1 => self.pop_hl(),
            0xE2 => self.ld_ff00ca(),
            0xE5 => self.push(self.hl()),
            0xE6 => self.and_an(),
            0xE7 => self.rst(0x20),
            0xE8 => self.add_spdd(),
            0xE9 => self.jp_hl(),
            0xEA => self.ld_nna(),
            0xEE => self.xor_an(),
            0xEF => self.rst(0x28),
            0xF0 => self.ld_aff00n(),
            0xF1 => self.pop_af(),
            0xF2 => self.ld_ff00ca(),
            0xF3 => self.di(),
            0xF5 => self.push(self.af()),
            0xF6 => self.or_an(),
            0xF7 => self.rst(0x30),
            0xF8 => self.ld_hlspdd(),
            0xF9 => self.ld_sphl(),
            0xFA => self.ld_ann(),
            0xFB => self.ei(),
            0xFE => self.cp_an(),
            0xFF => self.rst(0x38),
            _ => panic!("Unknown opcode: {:X}, PC: {:04X}", op, self.pc),
        }
    }

    fn execute_16bit(&mut self) -> u16 {
        let op = self.fetch();
        match op {
            0x00 => self.rlcb(),
            0x01 => self.rlcc(),
            0x02 => self.rlcd(),
            0x03 => self.rlce(),
            0x04 => self.rlch(),
            0x05 => self.rlcl(),
            0x06 => self.rlchl(),
            0x07 => self.rlca(),
            0x08 => self.rrcb(),
            0x09 => self.rrcc(),
            0x0A => self.rrcd(),
            0x0B => self.rrce(),
            0x0C => self.rrch(),
            0x0D => self.rrcl(),
            0x0E => self.rrchl(),
            0x0F => self.rrca(),
            0x10 => self.rlb(),
            0x11 => self.rlc(),
            0x12 => self.rld(),
            0x13 => self.rle(),
            0x14 => self.rlh(),
            0x15 => self.rll(),
            0x16 => self.rlhl(),
            0x17 => self.rla(),
            0x18 => self.rrb(),
            0x19 => self.rrc(),
            0x1A => self.rrd(),
            0x1B => self.rre(),
            0x1C => self.rrh(),
            0x1D => self.rrl(),
            0x1E => self.rrhl(),
            0x1F => self.rra(),
            0x20 => self.slb(),
            0x21 => self.slc(),
            0x22 => self.sld(),
            0x23 => self.sle(),
            0x24 => self.slh(),
            0x25 => self.sll(),
            0x26 => self.slhl(),
            0x27 => self.sla(),
            0x28 => self.srab(),
            0x29 => self.srac(),
            0x2A => self.srad(),
            0x2B => self.srae(),
            0x2C => self.srah(),
            0x2D => self.sral(),
            0x2E => self.srahl(),
            0x2F => self.sraa(),
            0x30 => self.swap_b(),
            0x31 => self.swap_c(),
            0x32 => self.swap_d(),
            0x33 => self.swap_e(),
            0x34 => self.swap_h(),
            0x35 => self.swap_l(),
            0x36 => self.swap_hl(),
            0x37 => self.swap_a(),
            0x38 => self.srlb(),
            0x39 => self.srlc(),
            0x3A => self.srld(),
            0x3B => self.srle(),
            0x3C => self.srlh(),
            0x3D => self.srll(),
            0x3E => self.srlhl(),
            0x3F => self.srla(),
            0x40 => self.bit(self.b, 0, 8),
            0x41 => self.bit(self.c, 0, 8),
            0x42 => self.bit(self.d, 0, 8),
            0x43 => self.bit(self.e, 0, 8),
            0x44 => self.bit(self.h, 0, 8),
            0x45 => self.bit(self.l, 0, 8),
            0x46 => self.bit(self.mem[self.hl() as usize], 0, 12),
            0x47 => self.bit(self.a, 0, 8),
            0x48 => self.bit(self.b, 1, 8),
            0x49 => self.bit(self.c, 1, 8),
            0x4A => self.bit(self.d, 1, 8),
            0x4B => self.bit(self.e, 1, 8),
            0x4C => self.bit(self.h, 1, 8),
            0x4D => self.bit(self.l, 1, 8),
            0x4E => self.bit(self.mem[self.hl() as usize], 1, 12),
            0x4F => self.bit(self.a, 1, 8),
            0x50 => self.bit(self.b, 2, 8),
            0x51 => self.bit(self.c, 2, 8),
            0x52 => self.bit(self.d, 2, 8),
            0x53 => self.bit(self.e, 2, 8),
            0x54 => self.bit(self.h, 2, 8),
            0x55 => self.bit(self.l, 2, 8),
            0x56 => self.bit(self.mem[self.hl() as usize], 2, 12),
            0x57 => self.bit(self.a, 2, 8),
            0x58 => self.bit(self.b, 3, 8),
            0x59 => self.bit(self.c, 3, 8),
            0x5A => self.bit(self.d, 3, 8),
            0x5B => self.bit(self.e, 3, 8),
            0x5C => self.bit(self.h, 3, 8),
            0x5D => self.bit(self.l, 3, 8),
            0x5E => self.bit(self.mem[self.hl() as usize], 3, 12),
            0x5F => self.bit(self.a, 3, 8),
            0x60 => self.bit(self.b, 4, 8),
            0x61 => self.bit(self.c, 4, 8),
            0x62 => self.bit(self.d, 4, 8),
            0x63 => self.bit(self.e, 4, 8),
            0x64 => self.bit(self.h, 4, 8),
            0x65 => self.bit(self.l, 4, 8),
            0x66 => self.bit(self.mem[self.hl() as usize], 4, 12),
            0x67 => self.bit(self.a, 4, 8),
            0x68 => self.bit(self.b, 5, 8),
            0x69 => self.bit(self.c, 5, 8),
            0x6A => self.bit(self.d, 5, 8),
            0x6B => self.bit(self.e, 5, 8),
            0x6C => self.bit(self.h, 5, 8),
            0x6D => self.bit(self.l, 5, 8),
            0x6E => self.bit(self.mem[self.hl() as usize], 5, 12),
            0x6F => self.bit(self.a, 5, 8),
            0x70 => self.bit(self.b, 6, 8),
            0x71 => self.bit(self.c, 6, 8),
            0x72 => self.bit(self.d, 6, 8),
            0x73 => self.bit(self.e, 6, 8),
            0x74 => self.bit(self.h, 6, 8),
            0x75 => self.bit(self.l, 6, 8),
            0x76 => self.bit(self.mem[self.hl() as usize], 6, 12),
            0x77 => self.bit(self.a, 6, 8),
            0x78 => self.bit(self.b, 7, 8),
            0x79 => self.bit(self.c, 7, 8),
            0x7A => self.bit(self.d, 7, 8),
            0x7B => self.bit(self.e, 7, 8),
            0x7C => self.bit(self.h, 7, 8),
            0x7D => self.bit(self.l, 7, 8),
            0x7E => self.bit(self.mem[self.hl() as usize], 7, 12),
            0x7F => self.bit(self.a, 7, 8),
            0x80 => self.res_b(0),
            0x81 => self.res_c(0),
            0x82 => self.res_d(0),
            0x83 => self.res_e(0),
            0x84 => self.res_h(0),
            0x85 => self.res_l(0),
            0x86 => self.res_hl(0),
            0x87 => self.res_a(0),
            0x88 => self.res_b(1),
            0x89 => self.res_c(1),
            0x8A => self.res_d(1),
            0x8B => self.res_e(1),
            0x8C => self.res_h(1),
            0x8D => self.res_l(1),
            0x8E => self.res_hl(1),
            0x8F => self.res_a(1),
            0x90 => self.res_b(2),
            0x91 => self.res_c(2),
            0x92 => self.res_d(2),
            0x93 => self.res_e(2),
            0x94 => self.res_h(2),
            0x95 => self.res_l(2),
            0x96 => self.res_hl(2),
            0x97 => self.res_a(2),
            0x98 => self.res_b(3),
            0x99 => self.res_c(3),
            0x9A => self.res_d(3),
            0x9B => self.res_e(3),
            0x9C => self.res_h(3),
            0x9D => self.res_l(3),
            0x9E => self.res_hl(3),
            0x9F => self.res_a(3),
            0xA0 => self.res_b(4),
            0xA1 => self.res_c(4),
            0xA2 => self.res_d(4),
            0xA3 => self.res_e(4),
            0xA4 => self.res_h(4),
            0xA5 => self.res_l(4),
            0xA6 => self.res_hl(4),
            0xA7 => self.res_a(4),
            0xA8 => self.res_b(5),
            0xA9 => self.res_c(5),
            0xAA => self.res_d(5),
            0xAB => self.res_e(5),
            0xAC => self.res_h(5),
            0xAD => self.res_l(5),
            0xAE => self.res_hl(5),
            0xAF => self.res_a(5),
            0xB0 => self.res_b(6),
            0xB1 => self.res_c(6),
            0xB2 => self.res_d(6),
            0xB3 => self.res_e(6),
            0xB4 => self.res_h(6),
            0xB5 => self.res_l(6),
            0xB6 => self.res_hl(6),
            0xB7 => self.res_a(6),
            0xB8 => self.res_b(7),
            0xB9 => self.res_c(7),
            0xBA => self.res_d(7),
            0xBB => self.res_e(7),
            0xBC => self.res_h(7),
            0xBD => self.res_l(7),
            0xBE => self.res_hl(7),
            0xBF => self.res_a(7),
            0xC0 => self.set_b(0),
            0xC1 => self.set_c(0),
            0xC2 => self.set_d(0),
            0xC3 => self.set_e(0),
            0xC4 => self.set_h(0),
            0xC5 => self.set_l(0),
            0xC6 => self.set_bit_hl(0),
            0xC7 => self.set_a(0),
            0xC8 => self.set_b(1),
            0xC9 => self.set_c(1),
            0xCA => self.set_d(1),
            0xCB => self.set_e(1),
            0xCC => self.set_h(1),
            0xCD => self.set_l(1),
            0xCE => self.set_bit_hl(1),
            0xCF => self.set_a(1),
            0xD0 => self.set_b(2),
            0xD1 => self.set_c(2),
            0xD2 => self.set_d(2),
            0xD3 => self.set_e(2),
            0xD4 => self.set_h(2),
            0xD5 => self.set_l(2),
            0xD6 => self.set_bit_hl(2),
            0xD7 => self.set_a(2),
            0xD8 => self.set_b(3),
            0xD9 => self.set_c(3),
            0xDA => self.set_d(3),
            0xDB => self.set_e(3),
            0xDC => self.set_h(3),
            0xDD => self.set_l(3),
            0xDE => self.set_bit_hl(3),
            0xDF => self.set_a(3),
            0xE0 => self.set_b(4),
            0xE1 => self.set_c(4),
            0xE2 => self.set_d(4),
            0xE3 => self.set_e(4),
            0xE4 => self.set_h(4),
            0xE5 => self.set_l(4),
            0xE6 => self.set_bit_hl(4),
            0xE7 => self.set_a(4),
            0xE8 => self.set_b(5),
            0xE9 => self.set_c(5),
            0xEA => self.set_d(5),
            0xEB => self.set_e(5),
            0xEC => self.set_h(5),
            0xED => self.set_l(5),
            0xEE => self.set_bit_hl(5),
            0xEF => self.set_a(5),
            0xF0 => self.set_b(6),
            0xF1 => self.set_c(6),
            0xF2 => self.set_d(6),
            0xF3 => self.set_e(6),
            0xF4 => self.set_h(6),
            0xF5 => self.set_l(6),
            0xF6 => self.set_bit_hl(6),
            0xF7 => self.set_a(6),
            0xF8 => self.set_b(7),
            0xF9 => self.set_c(7),
            0xFA => self.set_d(7),
            0xFB => self.set_e(7),
            0xFC => self.set_h(7),
            0xFD => self.set_l(7),
            0xFE => self.set_bit_hl(7),
            0xFF => self.set_a(7),
        }
    }

    // 8 BIT LOADS

    fn ld_a(&mut self, r: u8) -> u16 {
        self.a = r;
        4
    }

    fn ld_b(&mut self, r: u8) -> u16 {
        self.b = r;
        4
    }

    fn ld_c(&mut self, r: u8) -> u16 {
        self.c = r;
        4
    }

    fn ld_d(&mut self, r: u8) -> u16 {
        self.d = r;
        4
    }

    fn ld_e(&mut self, r: u8) -> u16 {
        self.e = r;
        4
    }

    fn ld_h(&mut self, r: u8) -> u16 {
        self.h = r;
        4
    }

    fn ld_l(&mut self, r: u8) -> u16 {
        self.l = r;
        4
    }

    fn ld_hl(&mut self, r: u8) -> u16 {
        self.mem[self.hl() as usize] = r;
        8
    }

    fn ld_an(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.a = val;
        8
    }

    fn ld_bn(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.b = val;
        8
    }

    fn ld_cn(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.c = val;
        8
    }

    fn ld_dn(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.d = val;
        8
    }

    fn ld_en(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.e = val;
        8
    }

    fn ld_hn(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.h = val;
        8
    }

    fn ld_ln(&mut self) -> u16 {
        let val: u8 = self.fetch();
        self.l = val;
        8
    }

    fn ld_hln(&mut self) -> u16 {
        self.mem[self.hl() as usize] = self.fetch();
        12
    }

    fn ld_abc(&mut self) -> u16 {
        self.a = self.mem[self.bc() as usize];
        8
    }

    fn ld_ade(&mut self) -> u16 {
        self.a = self.mem[self.de() as usize];
        8
    }

    fn ld_ann(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let addr = ((higher as u16) << 8) | lower as u16;
        self.a = self.mem[addr as usize];
        16
    }

    fn ld_bca(&mut self) -> u16 {
        self.mem[self.bc() as usize] = self.a;
        8
    }

    fn ld_dea(&mut self) -> u16 {
        self.mem[self.de() as usize] = self.a;
        8
    }

    fn ld_nna(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let addr = ((higher as u16) << 8) | lower as u16;
        self.mem[addr as usize] = self.a;
        16
    }

    fn ld_aff00n(&mut self) -> u16 {
        let n = self.fetch();
        self.a = self.mem[0xFF00 + n as usize];
        12
    }

    fn ld_ff00na(&mut self) -> u16 {
        let n = self.fetch();
        self.mem[0xFF00 + n as usize] = self.a;
        12
    }

    fn ld_ff00ca(&mut self) -> u16 {
        self.mem[0xFF00 + self.c as usize] = self.a;
        8
    }

    fn ldi_ahl(&mut self) -> u16 {
        self.a = self.mem[self.hl() as usize];
        self.set_hl(self.hl().wrapping_add(1));
        8
    }

    fn ldi_hla(&mut self) -> u16 {
        self.mem[self.hl() as usize] = self.a;
        self.set_hl(self.hl().wrapping_add(1));
        8
    }

    fn ldd_ahl(&mut self) -> u16 {
        self.a = self.mem[self.hl() as usize];
        self.dec_hl();
        8
    }

    fn ldd_hla(&mut self) -> u16 {
        self.mem[self.hl() as usize] = self.a;
        self.set_hl(self.hl().wrapping_sub(1));
        8
    }

    // 16 BIT LOADS
    fn ld_bcnn(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let val: u16 = (higher as u16) << 8 | lower as u16;
        self.set_bc(val);
        12
    }

    fn ld_denn(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let val: u16 = (higher as u16) << 8 | lower as u16;
        self.set_de(val);
        12
    }

    fn ld_hlnn(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let val: u16 = (higher as u16) << 8 | lower as u16;
        self.set_hl(val);
        12
    }

    fn ld_spnn(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let val: u16 = (higher as u16) << 8 | lower as u16;
        self.sp = val;
        12
    }

    fn ld_nnsp(&mut self) -> u16 {
        let lower: u8 = self.fetch();
        let higher: u8 = self.fetch();
        let addr = ((higher as u16) << 8) | lower as u16;
        self.mem[addr as usize] = (self.sp & 0xFF) as u8;
        self.mem[(addr + 1) as usize] = (self.sp >> 8) as u8;
        20
    }

    fn ld_sphl(&mut self) -> u16 {
        self.sp = self.hl();
        8
    }

    fn push(&mut self, value: u16) -> u16 {
        self.sp = self.sp.wrapping_sub(2);
        self.mem[(self.sp + 1) as usize] = (value >> 8) as u8; // Push high byte
        self.mem[self.sp as usize] = (value & 0xFF) as u8; // Push low byte
        16
    }

    fn pop_bc(&mut self) -> u16 {
        let lower: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let higher: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let val: u16 = ((higher as u16) << 8) | lower as u16;
        self.set_bc(val);
        12
    }

    fn pop_de(&mut self) -> u16 {
        let lower: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let higher: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let val: u16 = ((higher as u16) << 8) | lower as u16;
        self.set_de(val);
        12
    }

    fn pop_hl(&mut self) -> u16 {
        let lower: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let higher: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let val: u16 = ((higher as u16) << 8) | lower as u16;
        self.set_hl(val);
        12
    }

    fn pop_af(&mut self) -> u16 {
        let lower: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let higher: u8 = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let val: u16 = ((higher as u16) << 8) | lower as u16;
        self.set_af(val);
        12
    }

    // 8 BIT ARITHMETIC

    fn add_ar(&mut self, r: u8) -> u16 {
        let h: bool = ((self.a & 0xF) + (r & 0xF)) > 0xF;

        let sum: u16 = self.a as u16 + r as u16;
        self.a = sum as u8;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(h);
        self.set_carry(sum > 0xFF);
        4
    }

    fn add_an(&mut self) -> u16 {
        let n = self.fetch();
        let h: bool = ((self.a & 0xF) + (n & 0xF)) > 0xF;

        let sum: u16 = self.a as u16 + n as u16;
        self.a = sum as u8;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(h);
        self.set_carry(sum > 0xFF);
        8
    }

    fn adc_ar(&mut self, r: u8) -> u16 {
        let h: bool = ((self.a & 0xF) + (r & 0xF) + self.get_carry() as u8) > 0xF;

        let sum = self.a as u16 + r as u16 + self.get_carry() as u16;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(h);
        self.set_carry(sum > 0xFF);
        4
    }

    fn adc_an(&mut self) -> u16 {
        let n: u8 = self.fetch();
        let h: bool = ((self.a & 0xF) + (n & 0xF) + self.get_carry() as u8) > 0xF;

        let sum: u16 = self.a as u16 + n as u16 + self.get_carry() as u16;
        self.a = sum as u8;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(h);
        self.set_carry(sum > 0xFF);
        8
    }

    fn sub_ar(&mut self, r: u8) -> u16 {
        let h: bool = (r & 0xF) > (self.a & 0xF);
        let c: bool = r > self.a;

        self.a = self.a.wrapping_sub(r);

        self.set_zero(self.a == 0);
        self.set_sub(true);
        self.set_half(h);
        self.set_carry(c);
        4
    }

    fn sub_an(&mut self) -> u16 {
        let n = self.fetch();
        let h: bool = (n & 0xF) > (self.a & 0xF);
        let c: bool = n > self.a;

        self.a = self.a.wrapping_sub(n);

        self.set_zero(self.a == 0);
        self.set_sub(true);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn sbc_ar(&mut self, r: u8) -> u16 {
        let carry = self.get_carry() as u8;
        let sub = r.wrapping_add(carry);

        let h = (self.a & 0xF) < (sub & 0xF);
        let c = (self.a as u16) < (sub as u16);

        self.a = self.a.wrapping_sub(sub);

        self.set_zero(self.a == 0);
        self.set_sub(true);
        self.set_half(h);
        self.set_carry(c);
        4
    }

    fn sbc_an(&mut self) -> u16 {
        let n = self.fetch();
        let carry = self.get_carry() as u8;
        let sub = n.wrapping_add(carry);

        let h = (self.a & 0xF) < (sub & 0xF);
        let c = (self.a as u16) < (sub as u16);

        self.a = self.a.wrapping_sub(sub);

        self.set_zero(self.a == 0);
        self.set_sub(true);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn and_ar(&mut self, r: u8) -> u16 {
        self.a &= r;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(true);
        self.set_carry(false);
        4
    }

    fn and_an(&mut self) -> u16 {
        let n = self.fetch();
        self.a &= n;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(true);
        self.set_carry(false);
        8
    }

    fn xor_ar(&mut self, r: u8) -> u16 {
        self.a ^= r;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        4
    }

    fn xor_an(&mut self) -> u16 {
        let n = self.fetch();
        self.a ^= n;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn or_ar(&mut self, r: u8) -> u16 {
        self.a |= r;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        4
    }

    fn or_an(&mut self) -> u16 {
        let n = self.fetch();
        self.a |= n;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn cp_ar(&mut self, r: u8) -> u16 {
        let h: bool = (r & 0xF) > (self.a & 0xF);
        let c: bool = r > self.a;

        self.set_zero(self.a == r);
        self.set_sub(true);
        self.set_half(h);
        self.set_carry(c);
        4
    }

    fn cp_an(&mut self) -> u16 {
        let n = self.fetch();
        let h: bool = (n & 0xF) > (self.a & 0xF);
        let c: bool = n > self.a;

        self.set_zero(self.a == n);
        self.set_sub(true);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn inc_a(&mut self) -> u16 {
        let h = (1 + (self.a & 0xF)) > 0xF;

        self.a = self.a.wrapping_add(1);

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_b(&mut self) -> u16 {
        let h = (1 + (self.b & 0xF)) > 0xF;

        self.b = self.b.wrapping_add(1);

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_c(&mut self) -> u16 {
        let h = (1 + (self.c & 0xF)) > 0xF;

        self.c = self.c.wrapping_add(1);

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_d(&mut self) -> u16 {
        let h = (1 + (self.d & 0xF)) > 0xF;

        self.d = self.d.wrapping_add(1);

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_e(&mut self) -> u16 {
        let h = (1 + (self.e & 0xF)) > 0xF;

        self.e = self.e.wrapping_add(1);

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_h(&mut self) -> u16 {
        let h = (1 + (self.h & 0xF)) > 0xF;

        self.h = self.h.wrapping_add(1);

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_l(&mut self) -> u16 {
        let h = (1 + (self.l & 0xF)) > 0xF;

        self.l = self.l.wrapping_add(1);

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(h);
        4
    }

    fn inc_hl(&mut self) -> u16 {
        let val = self.mem[self.hl() as usize];
        let h = (1 + (val & 0xF)) > 0xF;

        self.mem[self.hl() as usize] = val.wrapping_add(1);

        self.set_zero(val.wrapping_add(1) == 0);
        self.set_sub(false);
        self.set_half(h);
        12
    }

    fn dec_a(&mut self) -> u16 {
        let h = 1 > (self.a & 0xF);

        self.a = self.a.wrapping_sub(1);

        self.set_zero(self.a == 0);
        self.set_sub(true);
        self.set_half(h);
        4
    }

    fn dec_b(&mut self) -> u16 {
        let h = 1 > (self.b & 0xF);

        self.b = self.b.wrapping_sub(1);

        self.set_zero(self.b == 0);
        self.set_sub(true);
        self.set_half(h);
        8
    }

    fn dec_c(&mut self) -> u16 {
        let h = 1 > (self.c & 0xF);

        self.c = self.c.wrapping_sub(1);

        self.set_zero(self.c == 0);
        self.set_sub(true);
        self.set_half(h);
        8
    }

    fn dec_d(&mut self) -> u16 {
        let h = 1 > (self.d & 0xF);

        self.d = self.d.wrapping_sub(1);

        self.set_zero(self.d == 0);
        self.set_sub(true);
        self.set_half(h);
        8
    }

    fn dec_e(&mut self) -> u16 {
        let h = 1 > (self.e & 0xF);

        self.e = self.e.wrapping_sub(1);

        self.set_zero(self.e == 0);
        self.set_sub(true);
        self.set_half(h);
        8
    }

    fn dec_h(&mut self) -> u16 {
        let h = 1 > (self.h & 0xF);

        self.h = self.h.wrapping_sub(1);

        self.set_zero(self.h == 0);
        self.set_sub(true);
        self.set_half(h);
        8
    }

    fn dec_l(&mut self) -> u16 {
        let h = 1 > (self.l & 0xF);

        self.l = self.l.wrapping_sub(1);

        self.set_zero(self.l == 0);
        self.set_sub(true);
        self.set_half(h);
        8
    }

    fn dec_hl(&mut self) -> u16 {
        let val = self.mem[self.hl() as usize];
        let h = 1 > (val & 0xF);

        self.mem[self.hl() as usize] = val.wrapping_sub(1);

        self.set_zero(val.wrapping_sub(1) == 0);
        self.set_sub(true);
        self.set_half(h);
        12
    }

    fn daa(&mut self) -> u16 {
        let mut correction = 0u8;

        if !self.get_sub() {
            if self.get_carry() || self.a > 0x99 {
                correction += 0x60;
                self.set_carry(true);
            }
            if self.get_half() || (self.a & 0xF) > 0x09 {
                correction += 0x06;
            }
        } else {
            if self.get_carry() {
                correction += 0x60;
            }
            if self.get_half() {
                correction += 0x06;
            }
        }

        if !self.get_sub() {
            self.a = self.a.wrapping_add(correction);
        } else {
            self.a = self.a.wrapping_sub(correction);
        }

        self.set_zero(self.a == 0);
        self.set_half(false);
        4
    }

    fn cpl(&mut self) -> u16 {
        self.a = self.a ^ 0xFF;
        self.set_sub(true);
        self.set_half(true);
        4
    }

    // 16 BIT ARITHMETIC

    fn add_hlbc(&mut self) -> u16 {
        let val = self.bc();
        let h = ((self.hl() & 0xFFF) + (val & 0xFFF)) > 0xFFF;

        let (new_hl, c) = val.overflowing_add(self.hl());
        self.set_hl(new_hl);

        self.set_sub(false);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn add_hlde(&mut self) -> u16 {
        let val = self.de();
        let h = ((self.hl() & 0xFFF) + (val & 0xFFF)) > 0xFFF;

        let (new_hl, c) = val.overflowing_add(self.hl());
        self.set_hl(new_hl);

        self.set_sub(false);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn add_hlhl(&mut self) -> u16 {
        let val = self.hl();
        let h = ((self.hl() & 0xFFF) + (val & 0xFFF)) > 0xFFF;

        let (new_hl, c) = val.overflowing_add(self.hl());
        self.set_hl(new_hl);

        self.set_sub(false);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn add_hlsp(&mut self) -> u16 {
        let val = self.sp;
        let h = ((self.hl() & 0xFFF) + (val & 0xFFF)) > 0xFFF;

        let (new_hl, c) = val.overflowing_add(self.hl());
        self.set_hl(new_hl);

        self.set_sub(false);
        self.set_half(h);
        self.set_carry(c);
        8
    }

    fn inc_bc(&mut self) -> u16 {
        let val = self.bc();
        let (new_val, _c) = val.overflowing_add(1);
        self.set_bc(new_val);
        8
    }

    fn inc_de(&mut self) -> u16 {
        let val = self.de();
        let (new_val, _c) = val.overflowing_add(1);
        self.set_de(new_val);
        8
    }

    fn inc_hlr(&mut self) -> u16 {
        let val = self.hl();
        let (new_val, _c) = val.overflowing_add(1);
        self.set_hl(new_val);
        8
    }

    fn inc_sp(&mut self) -> u16 {
        let val = self.sp;
        let (new_val, _c) = val.overflowing_add(1);
        self.sp = new_val;
        8
    }

    fn dec_bc(&mut self) -> u16 {
        let val = self.bc();
        let (new_val, _c) = val.overflowing_sub(1);
        self.set_bc(new_val);
        8
    }

    fn dec_de(&mut self) -> u16 {
        let val = self.de();
        let (new_val, _c) = val.overflowing_sub(1);
        self.set_de(new_val);
        8
    }

    fn dec_hlr(&mut self) -> u16 {
        let val = self.hl();
        let (new_val, _c) = val.overflowing_sub(1);
        self.set_hl(new_val);
        8
    }

    fn dec_sp(&mut self) -> u16 {
        let val = self.sp;
        let (new_val, _c) = val.overflowing_sub(1);
        self.sp = new_val;
        8
    }

    fn add_spdd(&mut self) -> u16 {
        let val = self.fetch() as i8 as u16;

        let h = ((self.sp & 0xF) + (val & 0xF)) > 0xF;
        let c = ((self.sp & 0xFF) + (val & 0xFF)) > 0xFF;

        let new_sp = self.sp.wrapping_add(val);
        self.sp = new_sp;

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(h);
        self.set_carry(c);
        16
    }

    fn ld_hlspdd(&mut self) -> u16 {
        let val = self.fetch() as i8 as u16;

        let h = ((self.sp & 0xF) + (val & 0xF)) > 0xF;
        let c = ((self.sp & 0xFF) + (val & 0xFF)) > 0xFF;

        let new_hl = self.sp.wrapping_add(val);
        self.set_hl(new_hl);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(h);
        self.set_carry(c);
        12
    }

    // ROTATES AND SHIFTS
    fn rlca(&mut self) -> u16 {
        let c = (self.a & 0b1000_0000) != 0;
        self.a = self.a.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn rlcb(&mut self) -> u16 {
        let c = (self.b & 0b1000_0000) != 0;
        self.b = self.b.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlcc(&mut self) -> u16 {
        let c = (self.c & 0b1000_0000) != 0;
        self.c = self.c.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlcd(&mut self) -> u16 {
        let c = (self.d & 0b1000_0000) != 0;
        self.d = self.d.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlce(&mut self) -> u16 {
        let c = (self.e & 0b1000_0000) != 0;
        self.e = self.e.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlch(&mut self) -> u16 {
        let c = (self.h & 0b1000_0000) != 0;
        self.h = self.h.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlcl(&mut self) -> u16 {
        let c = (self.l & 0b1000_0000) != 0;
        self.l = self.l.rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlchl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0b1000_0000) != 0;
        self.mem[self.hl() as usize] = self.mem[self.hl() as usize].rotate_left(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    fn rla(&mut self) -> u16 {
        let c = (self.a & 0b1000_0000) != 0;
        self.a <<= 1;
        self.a |= self.get_carry() as u8;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn rlb(&mut self) -> u16 {
        let c = (self.b & 0b1000_0000) != 0;
        self.b <<= 1;
        self.b |= self.get_carry() as u8;

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlc(&mut self) -> u16 {
        let c = (self.c & 0b1000_0000) != 0;
        self.c <<= 1;
        self.c |= self.get_carry() as u8;

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rld(&mut self) -> u16 {
        let c = (self.d & 0b1000_0000) != 0;
        self.d <<= 1;
        self.d |= self.get_carry() as u8;

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rle(&mut self) -> u16 {
        let c = (self.e & 0b1000_0000) != 0;
        self.e <<= 1;
        self.e |= self.get_carry() as u8;

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlh(&mut self) -> u16 {
        let c = (self.h & 0b1000_0000) != 0;
        self.h <<= 1;
        self.h |= self.get_carry() as u8;

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rll(&mut self) -> u16 {
        let c = (self.l & 0b1000_0000) != 0;
        self.l <<= 1;
        self.l |= self.get_carry() as u8;

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rlhl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0b1000_0000) != 0;
        self.mem[self.hl() as usize] <<= 1;
        self.mem[self.hl() as usize] |= self.get_carry() as u8;

        self.set_zero(self.mem[self.hl() as usize] == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    fn rrca(&mut self) -> u16 {
        let c = (self.a & 0x1) != 0;
        self.a = self.a.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn rrcb(&mut self) -> u16 {
        let c = (self.b & 0x1) != 0;
        self.b = self.b.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrcc(&mut self) -> u16 {
        let c = (self.c & 0x1) != 0;
        self.c = self.c.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrcd(&mut self) -> u16 {
        let c = (self.d & 0x1) != 0;
        self.d = self.d.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrce(&mut self) -> u16 {
        let c = (self.e & 0x1) != 0;
        self.e = self.e.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrch(&mut self) -> u16 {
        let c = (self.h & 0x1) != 0;
        self.h = self.h.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrcl(&mut self) -> u16 {
        let c = (self.l & 0x1) != 0;
        self.l = self.l.rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrchl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0x1) != 0;
        self.mem[self.hl() as usize] = self.mem[self.hl() as usize].rotate_right(1);

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    fn rra(&mut self) -> u16 {
        let c = (self.a & 0x1) != 0;
        self.a >>= 1;
        self.a |= (self.get_carry() as u8) << 7;

        self.set_zero(false);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn rrb(&mut self) -> u16 {
        let c = (self.b & 0x1) != 0;
        self.b >>= 1;
        self.b |= (self.get_carry() as u8) << 7;

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrc(&mut self) -> u16 {
        let c = (self.c & 0x1) != 0;
        self.c >>= 1;
        self.c |= (self.get_carry() as u8) << 7;

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrd(&mut self) -> u16 {
        let c = (self.d & 0x1) != 0;
        self.d >>= 1;
        self.d |= (self.get_carry() as u8) << 7;

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rre(&mut self) -> u16 {
        let c = (self.e & 0x1) != 0;
        self.e >>= 1;
        self.e |= (self.get_carry() as u8) << 7;

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrh(&mut self) -> u16 {
        let c = (self.h & 0x1) != 0;
        self.h >>= 1;
        self.h |= (self.get_carry() as u8) << 7;

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrl(&mut self) -> u16 {
        let c = (self.l & 0x1) != 0;
        self.l >>= 1;
        self.l |= (self.get_carry() as u8) << 7;

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn rrhl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0x1) != 0;
        self.mem[self.hl() as usize] >>= 1;
        self.mem[self.hl() as usize] |= (self.get_carry() as u8) << 7;

        self.set_zero(self.mem[self.hl() as usize] == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    fn sla(&mut self) -> u16 {
        let c = (self.a & 0b1000_0000) != 0;
        self.a <<= 1;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn slb(&mut self) -> u16 {
        let c = (self.b & 0b1000_0000) != 0;
        self.b <<= 1;

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn slc(&mut self) -> u16 {
        let c = (self.c & 0b1000_0000) != 0;
        self.c <<= 1;

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn sld(&mut self) -> u16 {
        let c = (self.d & 0b1000_0000) != 0;
        self.d <<= 1;

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn sle(&mut self) -> u16 {
        let c = (self.e & 0b1000_0000) != 0;
        self.e <<= 1;

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn slh(&mut self) -> u16 {
        let c = (self.h & 0b1000_0000) != 0;
        self.h <<= 1;

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn sll(&mut self) -> u16 {
        let c = (self.l & 0b1000_0000) != 0;
        self.l <<= 1;

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn slhl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0b1000_0000) != 0;
        self.mem[self.hl() as usize] <<= 1;

        self.set_zero(self.mem[self.hl() as usize] == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    fn swap_a(&mut self) -> u16 {
        self.a = (self.a << 4) | (self.a >> 4);

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        4
    }

    fn swap_b(&mut self) -> u16 {
        self.b = (self.b << 4) | (self.b >> 4);

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn swap_c(&mut self) -> u16 {
        self.c = (self.c << 4) | (self.c >> 4);

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn swap_d(&mut self) -> u16 {
        self.d = (self.d << 4) | (self.d >> 4);

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn swap_e(&mut self) -> u16 {
        self.e = (self.e << 4) | (self.e >> 4);

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn swap_h(&mut self) -> u16 {
        self.h = (self.h << 4) | (self.h >> 4);

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn swap_l(&mut self) -> u16 {
        self.l = (self.l << 4) | (self.l >> 4);

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        8
    }

    fn swap_hl(&mut self) -> u16 {
        let val = self.mem[self.hl() as usize];
        self.mem[self.hl() as usize] = (val << 4) | (val >> 4);

        self.set_zero(self.mem[self.hl() as usize] == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(false);
        16
    }

    fn sraa(&mut self) -> u16 {
        let c = (self.a & 0x1) != 0;
        let b7: u8 = self.a & 0b1000_0000;
        self.a = (self.a >> 1) | b7;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn srab(&mut self) -> u16 {
        let c = (self.b & 0x1) != 0;
        let b7: u8 = self.b & 0b1000_0000;
        self.b = (self.b >> 1) | b7;

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srac(&mut self) -> u16 {
        let c = (self.c & 0x1) != 0;
        let b7: u8 = self.c & 0b1000_0000;
        self.c = (self.c >> 1) | b7;

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srad(&mut self) -> u16 {
        let c = (self.d & 0x1) != 0;
        let b7: u8 = self.d & 0b1000_0000;
        self.d = (self.d >> 1) | b7;

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srae(&mut self) -> u16 {
        let c = (self.e & 0x1) != 0;
        let b7: u8 = self.e & 0b1000_0000;
        self.e = (self.e >> 1) | b7;

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srah(&mut self) -> u16 {
        let c = (self.h & 0x1) != 0;
        let b7: u8 = self.h & 0b1000_0000;
        self.h = (self.h >> 1) | b7;

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn sral(&mut self) -> u16 {
        let c = (self.l & 0x1) != 0;
        self.l >>= 1;

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srahl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0x1) != 0;
        let b7: u8 = self.mem[self.hl() as usize] & 0b1000_0000;
        self.mem[self.hl() as usize] = (self.mem[self.hl() as usize] >> 1) | b7;

        self.set_zero(self.mem[self.hl() as usize] == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    fn srla(&mut self) -> u16 {
        let c = (self.a & 0x1) != 0;
        self.a >>= 1;

        self.set_zero(self.a == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        4
    }

    fn srlb(&mut self) -> u16 {
        let c = (self.b & 0x1) != 0;
        self.b >>= 1;

        self.set_zero(self.b == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srlc(&mut self) -> u16 {
        let c = (self.c & 0x1) != 0;
        self.c >>= 1;

        self.set_zero(self.c == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srld(&mut self) -> u16 {
        let c = (self.d & 0x1) != 0;
        self.d >>= 1;

        self.set_zero(self.d == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srle(&mut self) -> u16 {
        let c = (self.e & 0x1) != 0;
        self.e >>= 1;

        self.set_zero(self.e == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srlh(&mut self) -> u16 {
        let c = (self.h & 0x1) != 0;
        self.h >>= 1;

        self.set_zero(self.h == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srll(&mut self) -> u16 {
        let c = (self.l & 0x1) != 0;
        self.l >>= 1;

        self.set_zero(self.l == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        8
    }

    fn srlhl(&mut self) -> u16 {
        let c = (self.mem[self.hl() as usize] & 0x1) != 0;
        self.mem[self.hl() as usize] >>= 1;

        self.set_zero(self.mem[self.hl() as usize] == 0);
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(c);
        16
    }

    // SINGLE BIT OPERATIONS
    fn bit(&mut self, r: u8, n: u8, cycles: u16) -> u16 {
        self.set_zero((r & (1 << n)) == 0);
        self.set_sub(false);
        self.set_half(true);
        cycles
    }

    fn set_b(&mut self, n: u8) -> u16 {
        self.b |= 1 << n;
        8
    }

    fn set_c(&mut self, n: u8) -> u16 {
        self.c |= 1 << n;
        8
    }

    fn set_d(&mut self, n: u8) -> u16 {
        self.d |= 1 << n;
        8
    }

    fn set_e(&mut self, n: u8) -> u16 {
        self.e |= 1 << n;
        8
    }

    fn set_h(&mut self, n: u8) -> u16 {
        self.h |= 1 << n;
        8
    }

    fn set_l(&mut self, n: u8) -> u16 {
        self.l |= 1 << n;
        8
    }

    fn set_bit_hl(&mut self, n: u8) -> u16 {
        self.mem[self.hl() as usize] |= 1 << n;
        16
    }

    fn set_a(&mut self, n: u8) -> u16 {
        self.a |= 1 << n;
        8
    }

    fn res_a(&mut self, n: u8) -> u16 {
        self.a &= !(1 << n);
        8
    }

    fn res_b(&mut self, n: u8) -> u16 {
        self.b &= !(1 << n);
        8
    }

    fn res_c(&mut self, n: u8) -> u16 {
        self.c &= !(1 << n);
        8
    }

    fn res_d(&mut self, n: u8) -> u16 {
        self.d &= !(1 << n);
        8
    }

    fn res_e(&mut self, n: u8) -> u16 {
        self.e &= !(1 << n);
        8
    }

    fn res_h(&mut self, n: u8) -> u16 {
        self.h &= !(1 << n);
        8
    }

    fn res_l(&mut self, n: u8) -> u16 {
        self.l &= !(1 << n);
        8
    }

    fn res_hl(&mut self, n: u8) -> u16 {
        self.mem[self.hl() as usize] &= !(1 << n);
        16
    }

    // CONTROL
    fn ccf(&mut self) -> u16 {
        let carry = self.get_carry();
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(carry ^ true);
        4
    }

    fn scf(&mut self) -> u16 {
        self.set_sub(false);
        self.set_half(false);
        self.set_carry(true);
        4
    }

    fn nop(&mut self) -> u16 {
        4
    }

    fn halt(&mut self) -> u16 {
        self.is_halted = true;
        4
    }

    fn stop(&mut self) -> u16 {
        // STOP
        4
    }

    fn di(&mut self) -> u16 {
        self.interrupts_enabled = false;
        4
    }

    fn ei(&mut self) -> u16 {
        self.interrupts_enabled = true;
        4
    }

    // JUMP INSTRUCTIONS
    fn jp_nn(&mut self) -> u16 {
        let lower = self.fetch();
        let higher = self.fetch();
        let addr = (higher as u16) << 8 | lower as u16;
        self.pc = addr;
        16
    }

    fn jp_hl(&mut self) -> u16 {
        self.pc = self.hl();
        4
    }

    fn jp_nz(&mut self) -> u16 {
        let cycles = if !self.get_zero() {
            let higher = self.fetch();
            let lower = self.fetch();
            let addr = (higher as u16) << 8 | lower as u16;
            self.pc = addr;
            16
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // If the jump is not taken, the cycle count is typically different.
        };
        cycles
    }

    fn jp_z(&mut self) -> u16 {
        let cycles = if self.get_zero() {
            let higher = self.fetch();
            let lower = self.fetch();
            let addr = (higher as u16) << 8 | lower as u16;
            self.pc = addr;
            16
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // If the jump is not taken, the cycle count is typically different.
        };
        cycles
    }

    fn jp_nc(&mut self) -> u16 {
        let cycles = if !self.get_carry() {
            let higher = self.fetch();
            let lower = self.fetch();
            let addr = (higher as u16) << 8 | lower as u16;
            self.pc = addr;
            16
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // If the jump is not taken, the cycle count is typically different.
        };
        cycles
    }

    fn jp_c(&mut self) -> u16 {
        let cycles = if self.get_carry() {
            let higher = self.fetch();
            let lower = self.fetch();
            let addr = (higher as u16) << 8 | lower as u16;
            self.pc = addr;
            16
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // If the jump is not taken, the cycle count is typically different.
        };
        cycles
    }

    fn jr_dd(&mut self) -> u16 {
        let n: i8 = self.fetch() as i8;
        self.pc = ((n as i16).wrapping_add(self.pc as i16)) as u16;
        12
    }

    fn jr_nz(&mut self) -> u16 {
        let cycles = if !self.get_zero() {
            let n: i8 = self.fetch() as i8; // Fetch the next byte and treat it as a signed value.

            // Since `fetch` increments `pc`, we need to subtract 1 so the relative jump is correct.
            self.pc = ((self.pc as i16).wrapping_add(n as i16)) as u16;

            12 // If the jump is taken, the cycle count is typically different.
        } else {
            self.pc = self.pc.wrapping_add(1);
            8 // If the jump is not taken, the cycle count is typically different.
        };
        cycles // Return the cycle count.
    }

    fn jr_z(&mut self) -> u16 {
        let cycles = if self.get_zero() {
            let n: i8 = self.fetch() as i8; // Fetch the next byte and treat it as a signed value.

            // Since `fetch` increments `pc`, we need to subtract 1 so the relative jump is correct.
            self.pc = ((self.pc as i16).wrapping_add(n as i16)) as u16;

            12 // If the jump is taken, the cycle count is typically different.
        } else {
            self.pc = self.pc.wrapping_add(1);
            8 // If the jump is not taken, the cycle count is typically different.
        };
        cycles // Return the cycle count.
    }

    fn jr_nc(&mut self) -> u16 {
        let cycles = if !self.get_carry() {
            let n: i8 = self.fetch() as i8; // Fetch the next byte and treat it as a signed value.

            // Since `fetch` increments `pc`, we need to subtract 1 so the relative jump is correct.
            self.pc = ((self.pc as i16).wrapping_add(n as i16)) as u16;

            12 // If the jump is taken, the cycle count is typically different.
        } else {
            self.pc = self.pc.wrapping_add(1);
            8 // If the jump is not taken, the cycle count is typically different.
        };
        cycles // Return the cycle count.
    }

    fn jr_c(&mut self) -> u16 {
        let cycles = if self.get_carry() {
            let n: i8 = self.fetch() as i8; // Fetch the next byte and treat it as a signed value.

            // Since `fetch` increments `pc`, we need to subtract 1 so the relative jump is correct.
            self.pc = ((self.pc as i16).wrapping_add(n as i16)) as u16;

            12 // If the jump is taken, the cycle count is typically different.
        } else {
            self.pc = self.pc.wrapping_add(1);
            8 // If the jump is not taken, the cycle count is typically different.
        };
        cycles // Return the cycle count.
    }

    fn call_nn(&mut self) -> u16 {
        let lower = self.fetch();
        let higher = self.fetch();
        let addr = ((higher as u16) << 8) | lower as u16;
        self.sp -= 1;
        self.mem[self.sp as usize] = (self.pc >> 8) as u8;
        self.sp -= 1;
        self.mem[self.sp as usize] = (self.pc) as u8;
        self.pc = addr;
        24
    }

    fn call_nz(&mut self) -> u16 {
        let cycles = if !self.get_zero() {
            let lower = self.fetch();
            let higher = self.fetch();
            let addr = ((higher as u16) << 8) | lower as u16;

            // Push the address of the next instruction onto the stack before the jump.
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = ((self.pc) >> 8) as u8; // Higher byte of PC
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = (self.pc) as u8; // Lower byte of PC

            self.pc = addr; // Jump to the address

            24 // Cycles if the call is taken
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // Cycles if the call is not taken
        };

        cycles // Return the cycle count
    }

    fn call_z(&mut self) -> u16 {
        let cycles = if self.get_zero() {
            let lower = self.fetch();
            let higher = self.fetch();
            let addr = ((higher as u16) << 8) | lower as u16;

            // Push the address of the next instruction onto the stack before the jump.
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = ((self.pc) >> 8) as u8; // Higher byte of PC
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = (self.pc) as u8; // Lower byte of PC

            self.pc = addr; // Jump to the address

            24 // Cycles if the call is taken
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // Cycles if the call is not taken
        };

        cycles // Return the cycle count
    }

    fn call_nc(&mut self) -> u16 {
        let cycles = if !self.get_carry() {
            let lower = self.fetch();
            let higher = self.fetch();
            let addr = ((higher as u16) << 8) | lower as u16;

            // Push the address of the next instruction onto the stack before the jump.
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = ((self.pc) >> 8) as u8; // Higher byte of PC
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = (self.pc) as u8; // Lower byte of PC

            self.pc = addr; // Jump to the address

            24 // Cycles if the call is taken
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // Cycles if the call is not taken
        };

        cycles // Return the cycle count
    }

    fn call_c(&mut self) -> u16 {
        let cycles = if self.get_carry() {
            let lower = self.fetch();
            let higher = self.fetch();
            let addr = ((higher as u16) << 8) | lower as u16;

            // Push the address of the next instruction onto the stack before the jump.
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = ((self.pc) >> 8) as u8; // Higher byte of PC
            self.sp = self.sp.wrapping_sub(1);
            self.mem[self.sp as usize] = (self.pc) as u8; // Lower byte of PC

            self.pc = addr; // Jump to the address

            24 // Cycles if the call is taken
        } else {
            self.pc = self.pc.wrapping_add(2);
            12 // Cycles if the call is not taken
        };

        cycles // Return the cycle count
    }

    fn ret(&mut self) -> u16 {
        let lower = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let higher = self.mem[self.sp as usize];
        self.sp = self.sp.wrapping_add(1);
        let addr = ((higher as u16) << 8) | lower as u16;
        self.pc = addr;
        16
    }

    fn ret_nz(&mut self) -> u16 {
        if !self.get_zero() {
            let lower = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let higher = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let addr = ((higher as u16) << 8) | lower as u16;
            self.pc = addr;
        }
        20 / 8
    }

    fn ret_z(&mut self) -> u16 {
        if self.get_zero() {
            let lower = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let higher = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let addr = ((higher as u16) << 8) | lower as u16;
            self.pc = addr;
        }
        20 / 8
    }

    fn ret_nc(&mut self) -> u16 {
        if !self.get_carry() {
            let lower = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let higher = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let addr = ((higher as u16) << 8) | lower as u16;
            self.pc = addr;
        }
        20 / 8
    }

    fn ret_c(&mut self) -> u16 {
        if self.get_carry() {
            let lower = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let higher = self.mem[self.sp as usize];
            self.sp = self.sp.wrapping_add(1);
            let addr = ((higher as u16) << 8) | lower as u16;
            self.pc = addr;
        }
        20 / 8
    }

    fn reti(&mut self) -> u16 {
        self.ret();
        self.ei();
        16
    }

    fn rst(&mut self, n: u16) -> u16 {
        self.sp -= 1;
        self.mem[self.sp as usize] = (self.pc >> 8) as u8;
        self.sp -= 1;
        self.mem[self.sp as usize] = (self.pc) as u8;
        self.pc = n;
        16
    }
}
