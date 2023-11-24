use rand::random;

pub const SCREEN_WIDTH: usize = 160;
pub const SCREEN_HEIGHT: usize = 144;

const MEM_SIZE: usize = 65536;
const NUM_KEYS: usize = 16;

const START_ADDR: u16 = 0x0100;

const ZERO_FLAG: u8 = 0b1000_0000;
const SUB_FLAG: u8 = 0b0100_0000;
const HALF_FLAG: u8 = 0b0010_0000;
const CARRY_FLAG: u8 = 0b0001_0000;

pub struct Emu {
    mem: [u8; MEM_SIZE],
    screen: [u8; SCREEN_WIDTH * SCREEN_HEIGHT],
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
}

impl Emu {
    pub fn new() -> Self {
        let mut new_emu = Self {
            mem: [0; MEM_SIZE],
            screen: [0; SCREEN_WIDTH * SCREEN_HEIGHT],
            sp: 0xFFFE,
            pc: START_ADDR,
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: 0,
            h: 0,
            l: 0
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

    pub fn set_flags(&mut self, zero: bool, sub: bool, half: bool, carry: bool) {
        if zero {
            self.f |= ZERO_FLAG;
        }
        if sub {
            self.f |= SUB_FLAG;
        }
        if half {
            self.f |= HALF_FLAG;
        }
        if carry {
            self.f |= CARRY_FLAG;
        }
    }

    pub fn reset(&mut self) {
        self.mem = [0; MEM_SIZE];
        self.screen = [0; SCREEN_WIDTH * SCREEN_HEIGHT];
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
    }

    pub fn tick(&mut self) {
        let op = self.fetch();
        self.execute(op);
    }

    pub fn tick_timers(&mut self) {
        // TODO
    }

    pub fn get_display(&self) -> &[u8] {
        &self.screen
    }

    pub fn keypress(&mut self, idx: usize, pressed: bool) {
        // TODO
    }

    pub fn load(&mut self, data: &[u8]) {
        let start = START_ADDR as usize;
        let end = (START_ADDR as usize) + data.len();
        self.mem[start..end].copy_from_slice(data);
    }

    fn fetch(&mut self) -> u8 {
        let opcode = self.mem[self.pc as usize];
        self.pc += 1;
        opcode
    }

    fn execute(&mut self, op: u8) {
        match op {
            0xCB => self.execute_16bit(),
            0x00 => self.op_00(),
            0x01 => self.op_01(),
            0x02 => self.op_02(),
            0x03 => self.op_03(),
            0x04 => self.op_04(),
            0x05 => self.op_05(),
            0x06 => self.op_06(),
            0x07 => self.op_07(),
            0x08 => self.op_08(),
            0x09 => self.op_09(),
            0x0A => self.op_0A(),
            0x0B => self.op_0B(),
            0x0C => self.op_0C(),
            0x0D => self.op_0D(),
            0x0E => self.op_0E(),
            0x0F => self.op_0F(),
            0x10 => self.op_10(),
            0x11 => self.op_11(),
            0x12 => self.op_12(),
            0x13 => self.op_13(),
            0x14 => self.op_14(),
            0x15 => self.op_15(),
            0x16 => self.op_16(),
            0x17 => self.op_17(),
            0x18 => self.op_18(),
            0x19 => self.op_19(),
            0x1A => self.op_1A(),
            0x1B => self.op_1B(),
            0x1C => self.op_1C(),
            0x1D => self.op_1D(),
            0x1E => self.op_1E(),
            0x1F => self.op_1F(),
            0x20 => self.op_20(),
            0x21 => self.op_21(),
            0x22 => self.op_22(),
            0x23 => self.op_23(),
            0x24 => self.op_24(),
            0x25 => self.op_25(),
            0x26 => self.op_26(),
            0x27 => self.op_27(),
            0x28 => self.op_28(),
            0x29 => self.op_29(),
            0x2A => self.op_2A(),
            0x2B => self.op_2B(),
            0x2C => self.op_2C(),
            0x2D => self.op_2D(),
            0x2E => self.op_2E(),
            0x2F => self.op_2F(),
            
            _ => panic!("Unknown opcode: {:X}", op)
        }
    }

    fn execute_16bit(&mut self) {
        let op = self.fetch();
        match op {
            _ => panic!("Unknown opcode: {:X}", op)
        }
    }

    // NOP
    fn op_00(&mut self) {
        
    }

    // LD BC, d16
    fn op_01(&mut self) {
        let lower: u8 = self.fetch();
        let upper: u8 = self.fetch();
        self.b = upper;
        self.c = lower;
    }

    // LD (BC), A
    fn op_02(&mut self) {
        let addr = self.bc();
        self.mem[addr as usize] = self.a;
    }

    // INC BC
    fn op_03(&mut self) {
        let bc = self.bc();
        self.set_bc(bc.wrapping_add(1));
    }
    
    // INC B
    fn op_04(&mut self) {
        let h: bool = (self.b & 0xF) == 0xF;
        self.b = self.b.wrapping_add(1);
        self.set_flags(self.b == 0, false, h, false);
    }

    // DEC B
    fn op_05(&mut self) {
        let h: bool = (self.b & 0xF) == 0;
        self.b = self.b.wrapping_sub(1);
        self.set_flags(self.b == 0, true, h, false);
    }

    // LD B, d8
    fn op_06(&mut self) {
        let addr: u8 = self.fetch();
        self.b = addr;
    }

    // RLCA
    fn op_07(&mut self) {
        let carry: bool = (self.a & 0x80) != 0;
        self.a.rotate_left(1);
        self.set_flags(false, false, false, carry);
    }

    // LD (a16), SP
    fn op_08(&mut self) {
        let lower: u8 = self.sp as u8;
        let higher: u8 = (self.sp >> 8) as u8;
        let addr: u16 = self.fetch() as u16;
        self.mem[addr as usize] = lower;
        self.mem[(addr + 1) as usize] = higher;
    }

    // ADD HL, HC
    fn op_09(&mut self) {
        let h = (self.hl() & 0xFFF) + (self.bc() & 0xFFF) > 0xFFF;
        let carry = self.hl() + self.bc() > 0xFFFF;
        self.set_flags(false, false, h, carry);
        self.set_bc(self.hl().wrapping_add(self.bc()));
    }

    // LD A, (BC)
    fn op_0A(&mut self) {
        let addr = self.bc();
        self.a = self.mem[addr as usize];
    }

    // DEC BC
    fn op_0B(&mut self) {
        let bc: u16 = self.bc();
        self.set_bc(bc - 1);
    }
    
    // INC C
    fn op_0C(&mut self) {
        let h: bool = (self.c & 0xF) == 0xF;
        self.c = self.c.wrapping_add(1);
        self.set_flags(self.c == 0, false, h, false);
    }

    // DEC C
    fn op_0D(&mut self) {
        let h: bool = (self.c & 0xF) == 0;
        self.c = self.c.wrapping_sub(1);
        self.set_flags(self.c == 0, true, h, false)
    }

    // LD C, d8
    fn op_0E(&mut self) {
        let val: u8 = self.fetch();
        self.c = val;
    }

    // RRCA
    fn op_0F(&mut self) {
        let carry: bool = (self.a & 0x01) != 0;
        self.a.rotate_right(1);
        self.set_flags(false, false, false, carry);
    }

    // STOP
    fn op_10(&mut self) {
        // TODO
    }

    fn op_11(&mut self) {
        let lower = self.fetch();
        let higher = self.fetch();
        
    }

}
