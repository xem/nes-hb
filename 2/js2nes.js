// JS2NES
// ======

// Globals
// -------
bin = [];
asm = [];
error = 0;
addr = 0;
imp = 0;
A = 0;
imm = 1;
abs = 2;
absx = 3;
rel = 0;
pc = 0;

// Helpers
// -------

// Complete the binary opcode with the right argument according to the addressing mode
arg_bin = (mode, arg) => {
  
  console.log(mode, arg);

  // immediate
  if(mode == 1){
    return [arg];
  }
  // absolute, absolute+X
  else if(mode == 2 || mode == 3){
    return [arg & 0xFF, (arg >> 8) & 0xFF];
  }
  return []
}

// Complete the assembly opcode with the right argument according to the addressing mode
arg_asm = (mode, arg) => {
  // immediate
  if(mode == 1){
    return "#" + arg.toString(16).padStart(2,0) + "  ";
  }
  // absolute, absolute+X
  else if(mode == 2){
    return "$" + arg.toString(16).padStart(4,0);
  }
  else if(mode == 3){
    return "$" + arg.toString(16).padStart(4,0) + "+X";
  }
  return []
}

// Add an opcode with implicit / immediate / absolute / absolute+X addressing modes
opc = (name, opcodes) => {
  return (mode = 0, arg, comment, tmp, tmp2=[]) => {
    tmp = 0x8000 + bin.length;
    if(!error) bin.push(opcodes[mode], ...(tmp2 = arg_bin(mode, arg)));
    if(!error) asm.push(`${tmp.toString(16).padStart(4,0)}: ${name} ${arg_asm(mode, arg)}${((name == "ASL" || name == "LSR" || name == "ROL" || name == "ROR" || name == "SBC") && mode == 0) ? "A" : ""} ${comment ? ("; " + comment) : ""}`);
    pc = tmp + 1 + tmp2.length;
    return tmp;
  }
}

// Add an opcode with relative addressing mode
opc_rel = (name, opcode) => {
  return (mode, arg=mode, comment, tmp, Z) => {
    tmp = 0x8000 + bin.length; // PC
    // arg = (PC+1)+Z-256*(Z>>7);
    if(arg >= tmp && arg <= tmp + 128) {
      Z = arg - tmp - 1;
    }
    else if(arg < tmp && arg > tmp - 128){
      Z = arg - tmp - 1 + (1 << 8);
    }
    else {
      console.error(`${tmp.toString(16).padStart(4,0)} : ${name} ${arg.toString(16).padStart(4,0)} out of range. Target must be between PC-127 ($${(tmp-127).toString(16).padStart(4,0)}) and PC+128 ($${(tmp+128).toString(16).padStart(4,0)})`);
      error = 1;
      asm.push(`${tmp.toString(16).padStart(4,0)}: error (see console)`);
    }
    if(!error) bin.push(opcode, Z);
    if(!error) asm.push(`${tmp.toString(16).padStart(4,0)}: ${name} $${arg.toString(16).padStart(4,0)} ${comment ? ("; " + comment) : ""}`);
    return tmp;
  }
}

// Store bytes / ASCII in the ROM
db = (...p) => {
  if(error) return;
  var tmp = 0x8000 + bin.length; // PC
  for(var i in p){
    if(typeof p[i] == "string"){
      [...p[i]].map(x=>bin.push(x.charCodeAt()&0xFF));
      tmp += p[i].length;
      asm.push(`${tmp.toString(16).padStart(4,0)}: db ${[...p[i]].map(x=>(x.charCodeAt()&0xFF).toString(16).padStart(2,0)).join(" ")}; "${p[i]}"`);
    }
    else {
      bin.push(p[i]);
      tmp ++;
      asm.push(`${tmp.toString(16).padStart(4,0)}: db ${p[i].toString(16).padStart(2,0)}`);
    }
  }
  return tmp;
}

// Level 0: 6502 opcodes
// ---------------------

// ADC (add to accumulator with carry)
// A = A + a byte in memory + Carry. Flags: N, Z, C, V
// Flag C is set if there's a carry
// Flag V is set if the sum of two positive numbers is incorrectly considered negative
ADC = opc("ADC A,", [,0x69, 0x6D, 0x7D]); // name, [imp, imm, abs, absX]

// AND (AND memory and accumulator)
// A = A AND a byte in memory. Flags: N, Z
AND = opc("AND A,", [,0x29, 0x2D, 0x3D]); // name, [imp, imm, abs, absX]

// ASL (shift left)
// - ASL A: accumulator is left shifted. Flags: N, Z, C
// - ASL: A byte in memory is left shifted. Flags: N, Z, C
// The shifted-out bit 7 is saved in C
ASL = opc("ASL", [0x0A, , 0x0E, 0x1E]); // name, [imp, imm, abs, absX]

// BCC (branch on carry clear)
// PC = address if C is 0
BCC = opc_rel("BCC", 0x90); // name, rel

// BCS (branch on carry set)
// PC = address if C is 1
BCS = opc_rel("BCS", 0xB0);

// BEQ (branch if equal)
// PC = address if Z is 0
BEQ = opc_rel("BEQ", 0xF0);

// BIT (test bits in memory)
// N and V = bits 7 and 6 of operand. Z is set if operand AND A is not zero. Flags: N, Z, V
BIT = opc("BIT", [, , 0x2C, ]);

// BMI (branch on minus)
// PC = address if N is 1
BMI = opc_rel("BMI", 0x30);

// BNE (branch if not equal)
// PC = address if Z is 1
BNE = opc_rel("BNE", 0xD0);

// BPL (branch on plus)
// PC = address if N is 0
BPL = opc_rel("BPL", 0x10);

// BRK (force break)
// Interrupt, push PC+2 (PC+1 is a padding byte), push P with B flag set to 1, set I to 1
BRK = opc("BRK", [0x00, , , ]);

// BVC (branch on overflow clear)
// PC = address if V is 0
BVC = opc_rel("BVC", 0x50);

// BVS (branch on overflow set)
// PC = address if V is 1
BVS = opc_rel("BVS", 0x70);

// CLC (clear carry flag)
// C is set to 0
CLC = opc("CLC", [0x18, , , ]);

// CLD (clear decimal flag)
// D is set to 0
CLD = opc("CLD", [0xD8, , , ]);

// CLI (clear interrupt disable flag)
// I is set to 0
CLI = opc("CLI", [0x58, , , ]);

// CLV (clear overflow flag)
// V is set to 0
CLV = opc("CLV", [0xB8, , , ]);

// CMP (compare memory and accumulator)
// N, Z and C are set with the result of A minus a byte in memory
// Flag C is set if there's no borrow
CMP = opc("CMP A,", [, 0xC9, 0xCD, 0xDD]); // name, [imp, imm, abs, absX]

// CPX (compare memory and X)
// N, Z and C are set with the result of X minus a byte in memory
// Flag C is set if there's no borrow
CPX = opc("CPX", [, 0xE0, 0xEC, ]); // name, [imp, imm, abs, absX]

// CPY (compare memory and Y)
// N, Z and C are set with the result of Y minus a byte in memory
// Flag C is set if there's no borrow
CPY = opc("CPY", [, 0xC0, 0xCC, ]);

// DEC (decrement memory)
// A byte in memory is decremented. Flags: N, Z
DEC = opc("DEC", [, , 0xCE, 0xDE]);

// DEX (decrement X)
// X is decremented. Flags: N, Z
DEX = opc("DEX", [0xCA, , , ]);

// DEY (decrement Y)
// Y is decremented. Flags: N, Z
DEY = opc("DEY", [0x88, , , ]);

// EOR (exclusive-or memory and accumulator)
// A = A XOR a byte in memory. Flags: N, Z
EOR = opc("EOR A,", [, 0x49, 0x4D, 0x5D]);

// INC (increment memory)
// A byte in memory is incremented. Flags: N, Z
INC = opc("INC", [, , 0xEE, 0xFE]);

// INX (increment X)
// X is incremented. Flags: N, Z
INX = opc("INX", [0xE8, , ,]);

// INY (increment Y)
// Y is incremented. Flags: N, Z
INY = opc("INY", [0xAC, , , ]);

// JMP (jump to new location)
// Set a new value to PC
JMP = opc("JMP", [, , 0x4C, ]);

// JSR (jump to subroutine)
// Push PC + 2, PC = absolute address
JSR = opc("JSR", [, , 0x20, ]);

// LDA (load accumulator with memory)
// A = a byte from memory. Flags: N, Z
LDA = opc("LDA", [, 0xA9, 0xAD, 0xBD]);

// LDX (load X with memory)
// X = a byte from memory. Flags: N, Z
LDX = opc("LDX", [, 0xA2, 0xAE, ]);

// LDY (load Y with memory)
// Y = a byte from memory. Flags: N, Z
LDY = opc("LDY", [, 0xA0, 0xAC, 0xBC]);

// LSR (shift right)
// - LSR A: A is shifted right. Flags: N, Z, C
// - LSR: A byte in memory is shifted right. Flags: N, Z, C
LSR = opc("LSR", [0x4A, , 0x4E, 0x5E]);

// NOP (no operation)
NOP = opc("NOP", [0xEA, , ,]);

// ORA (OR memory and accumulator)
// A = A OR a byte in memory. Flags: N, Z. 
ORA = opc("ORA", [, 0x09, 0x0D, 0x1D]);

// PHA (push accumulator)
// Push A
PHA = opc("PHA", [0x48, , , ]);

// PHP (push processor status)
// Push P with B flag set to 1
PHP = opc("PHP", [0x08, , , ]);

// PLA (pull accumulator)
// Pull A. Flags: N, Z.
PLA = opc("PLA", [0x68, , , ]);

// PLP (pull processor status)
// Pull P and set all flags
// (According to nestest, the B flag stays at 0) 
PLP = opc("PLP", [0x28, , , ]);

// ROL (rotate left)
// - ROL A (rotate left accumulator): Same as left shift A but C flag is put into bit 0. Flags: N, Z, C
// - ROL (rotate left a byte in memory): Same as left shift but C flag is put into bit 0. Flags: N, Z, C
// The shifted-out bit 7 is saved in C
ROL = opc("ROL", [0x2A, , 0x2E, 0x3E]);

// ROR (rotate right)
// - ROR A (rotate right accumulator): Same as right shift A but C flag is put into bit 0. Flags: N, Z, C
// - ROR (rotate right a byte in memory): Same as right shift but C flag is put into bit 0. Flags: N, Z, C
// The shifted-out bit 0 is saved in C
ROR = opc("ROR", [0x6A, , 0xEE, 0xFE]);

// RTI (return from interrupt)
// Pull P, set all flags, pull PC
RTI = opc("RTI", [0x40, , , ]);

// RTS (return from subroutine)
// Pull and increment PC
RTS = opc("RTS", [0x60, , , ]);

// SBC (subtract from accumulator with carry)
// A = A - a byte from memory - (1 - Carry). Flags: N, Z, C, V
// Flag C is set if there's no borrow
// Flag V is set if the subtraction is incorrectly considered positive
SBC = opc("SBC", [, 0xE9, 0xED, 0xFD]);

// SEC (set carry flag)
// C is set to 1
SEC = opc("SEC", [0x38, , , ]);

// SED (set decomal flag)
// D is set to 1
SED = opc("SED", [0xF8, , , ]);

// SEI (set interrupt disable flag)
// I is set to 1
SEI = opc("SEI", [0x78, , , ]);

// STA (store accumulator)
// A is copied in memory
STA = opc("STA", [, , 0x8D, 0x9D]);

// STX (store X)
// X is copied in memory
STX = opc("STX", [, , 0x8E, ]);

// STY (store Y)
// Y is copied in memory
STY = opc("STY", [, , 0x8C, ]);

// TAX (transfer accumulator to X)
// X = A. Flags: N, Z
TAX = opc("TAX", [0xAA, , , ]);

// TAY (transfer accumulator to Y)
// Y = A. Flags: N, Z
TAY = opc("TAY", [0xA8, , , ]);

// TSX (transfer stack pointer to X)
// X = S. Flags: N, Z
TSX = opc("TSX", [0xBA, , , ]);

// TXA (transfer X to accumulator)
// A = X. Flags: N, Z
TXA = opc("TXA", [0x8A, , , ]);

// TXS (transfer X to stack pointer)
// Stack pointer = X
TXS = opc("TXS", [0x9A, , , ]);

// TYA (transfer Y to accumulator)
// A = Y. Flags: N, Z
TYA = opc("TYA", [0x98, , , ]);

// Level 1: registers
// ------------------

// Write PPUCTRL register
// NN: screen where scroll takes place (0-3)
// I: address increment after vram read/write (0:1, 1:32)
// S: sprite pattern table (0-1)
// B: background pattern table (0-1)
// H: sprites height (0:8px, 1:16px)
// V: trigger NMI on VBlank (0-1)
set_ppuctrl = (NN, I, S, B, H, V, val) => {
  val = (V << 7) + (H << 5) + (B << 4) + (S << 3) + (I << 2) + NN; 
  LDX(imm, val, "set PPUCTRL = " + val.toString(16).padStart(2,0)); // Put value in X
  STX(abs, 0x2000); // Put X at $2000
}

// Write PPUMASK register
// G: greyscale (0-1)
// m: show background on leftmost 8px column (0-1)
// M: show sprites on leftmost 8px column (0-1)
// b: show background (0-1)
// s: show sprites (0-1)
// eb, eg, er: blue, green, red emphasis (on NTSC consoles) (0-1)
set_ppumask = (G, m, M, b, s, er, eg, eb) => {
  val = (eb << 7) + (eg << 6) + (er << 5) + (s << 4) + (b << 3) + (M << 2) + (m << 1) + G;
  LDX(imm, val, "set PPUMASK = " + val.toString(16).padStart(2,0)); // Put value in X
  STX(abs, 0x2000); // Put X at $2000
}

// Read PPUSTATUS register, store it in A
get_ppustatus = () => {
  LDA(abs, 0x2002, "A = PPUSTATUS");
}

// Wait for next VBlank
wait_vblank = (loop) => {
  loop = BIT(abs, 0x2002, "wait for VBlank"); // test bits of PPUSTATUS
  BNE(rel,loop,"on VBlank:"); // return to test while bit 7 (Z) is not 1
}

// Wait for sprite 0 hit
wait_sprite_0_hit = (loop) => {
  loop = BIT(abs, 0x2002, "wait for Sprite 0 hit"); // test bits of PPUSTATUS
  BVC(rel,loop,"on Sprite 0 hit:"); // return to test while bit 6 (V) is not 1
}

// Set OAMADDR register (0-255)
set_oamaddr = (addr) => {
  LDX(imm, addr, "set OAMADDR = " + addr.toString(16).padStart(2,0)); // Put value in X
  STX(abs, 0x2003); // Put X at $2003
}

// Write bytes in OAM memory then increment OAMADDR
set_oamdata = (...val) => {
  for(var i in val){
    LDX(imm, val[i], "set OAMDATA = " + val[i].toString(16).padStart(2,0)); // Put value in X
    STX(abs, 0x2004); // Put X at $2004
  }
}

// Read a byte in OAM memory at current OAM address and save it in A
get_oamdata = () => {
  LDA(abs, 0x2004, "A = OAMDATA");
}

// Set PPUSCROLL register
// First write: set horizontal scroll (XXXXXxxx)
// Second write: set vertical scroll (YYYYYyyy)
set_ppuscroll = (XXXXXxxx = 0, YYYYYyyy = 0) => {
  LDX(imm, XXXXXxxx, `set PPUSCROLL (x = ${XXXXXxxx.toString(16).padStart(2,0)})`);
  STX(abs, 0x2005);
  
  LDX(imm, YYYYYyyy, `set PPUSCROLL (y = ${YYYYYyyy.toString(16).padStart(2,0)})`);
  STX(abs, 0x2005);
}

// Set PPUADDR register
// First write: high byte of PPUADDR + scroll (00yyNNYY)
// (yy: bits 0-1 of yyy. yyy bit 2 = 0. YY: bits 3-4 of YYYYY)
// Second write: low byte of PPUADDR + scroll (YYYXXXXX)
// (YYY: bits 0-2 of YYY. XXXXX bits 0-4 of XXXXX)
// It's recommended to properly set scroll later
set_ppuaddr = (addr, tmp) => {
  tmp = addr & 0xFF;
  LDX(imm, tmp, `set PPUADDR = ${addr.toString(16).padStart(4,0)}`);
  STX(abs, 0x2006);
  
  val = (addr >> 8) & 0xFF;
  LDX(imm, tmp);
  STX(abs, 0x2006); 
}

// Set PPUDATA register
// Write a byte in PPU memory then PPU address += 1 or 32
set_ppudata = (val) => {
  LDX(imm, val);
  STX(abs, 0x2007); 
}

// Read PPUDATA register, store the value in A
// Read a byte at current PPU address, // Then PPUaddress += 1 or 32
// Warning: reads between $0000 and $3EFF are buffered
// (read 1: 0, read 2: value of read 1, etc...)
// A quick solution is to always read twice. (implemented here)
get_ppudata = () => {
  LDA(abs, 0x2007);
  LDA(abs, 0x2007);
} 

// OAM DMA: write 256 bytes from CPU memory in OAM memory at once
// Reads from CPU address = page * 0x100
// Writes in OAM memory at address OAMADDR (wrap at 0xFF)
oamdma = (page) => {
  LDX(imm, page);
  STX(abs, 0x4014); 
}

 