

pub mod luaval {

use core::hashmap::linear;
use core::ops::*;
use core::to_bytes::*;


#[deriving(Eq)]
pub enum Instr {
 IAdd(int, int, int),
 ISub(int, int, int),
 IMul(int, int, int),
 IDiv(int, int, int),
 IConcat(int, int, int),
 IJmp(int),
 ILt(int, int),
 IMove(int, int),
 ILoadK(int, int),
 IReturn(int, int),
 ICall(int, int, int), 
 ITailCall(int, int, int),
 ILoadNil(uint, uint),
 IGetTable(int, int, int),
 ISetTable(int, int, int),
 IForPrep(int, int),
 IForLoop(int, int),
 INot(int, int),
 IUnm(int, int),
}



#[deriving(Eq)]
pub struct Program(~[Instr]);


#[deriving(Eq)]
pub struct Execution {
  state: @mut bool,
  constants: ~[LuaVal],
  prog: Program,
}


//#[deriving(Eq)]
enum LuaVal {
 LString(@~str),
 LNum(float),
 LBool(bool),
 LTable(@mut linear::LinearMap<LuaVal, LuaVal>, @[LuaVal]),
 LFunc(@Execution),
 LRustFunc(extern "Rust" fn(reg: &mut ~[LuaVal]) -> ~[LuaVal] ),
 LNil,
}

fn cmp_fn(a: extern "Rust" fn(&mut ~[LuaVal]) -> ~[LuaVal], b: extern "Rust" fn(&mut ~[LuaVal]) -> ~[LuaVal]) -> bool {
    unsafe {
        let a_: *() = cast::transmute(a), b_: *() = cast::transmute(b);
        a_ == b_
    }
}

impl Eq for LuaVal {
  fn eq(&self, other: &LuaVal) -> bool {
    match (self, other) {
      (&LNum(x), &LNum(y)) => x == y,
      (&LString(x), &LString(y)) => x == y,
      (&LBool(x), &LBool(y)) => x == y,
      (&LTable(x, x1), &LTable(y, y1)) => (x == y) && (x1 == y1),
      (&LNil, &LNil) => true,
      (&LRustFunc(x), &LRustFunc(y)) => { cmp_fn(x, y) }
      (_, _) => false
    }
  }

  fn ne(&self, other: &LuaVal) -> bool {
    return !(self == other);
  }
}


impl IterBytes for LuaVal {
  fn iter_bytes(&self, lsb0: bool, f: Cb) {
    match *self {
      LString(x) => x.iter_bytes(lsb0, f),
      LNum(x) => (x as uint).iter_bytes(lsb0, f),
      LBool(x) => x.iter_bytes(lsb0, f),
      LNil => (true, false).iter_bytes(lsb0, f),
      _ => fail!(~"Tried to hash a function!"),
     }
  }
}


impl Add<LuaVal, LuaVal> for LuaVal {
 fn add(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x+y),
   (&LString(x), &LString(y)) => LString(@(x.to_owned() + y.to_owned()) ),
   _ => LNil,
  }

 }
}

impl Sub<LuaVal, LuaVal> for LuaVal {
 fn sub(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x-y),
   _ => LNil,
  }
 }
}

impl Mul<LuaVal, LuaVal> for LuaVal {
 fn mul(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x*y),
   _ => LNil,
  }
 }
}

impl Div<LuaVal, LuaVal> for LuaVal {
 fn div(&self, other: &LuaVal) -> LuaVal {
  match (self, other) {
   (&LNum(x), &LNum(y)) => LNum(x/y),
   _ => fail!(~"Attempt to divide non-number values!"),
  }
 }
}



impl Ord for LuaVal {
 fn lt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x < y,
   (&LString(x), &LString(y)) => x < y,
   _ => false,
  }
 }

 fn le(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x <= y,
   (&LString(x), &LString(y)) => x.to_owned() <= y.to_owned(),
   _ => false,
  }
 }


 fn ge(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x >= y,
   (&LString(x), &LString(y)) => x.to_owned() >= y.to_owned(),
   _ => false,
  }

 }

 fn gt(&self, other: &LuaVal) -> bool {
  match (self, other) {
   (&LNum(x), &LNum(y)) => x > y,
   (&LString(x), &LString(y)) => x.to_owned() > y.to_owned(),
   _ => false,
  }
 }
}


impl ToStr for LuaVal {
 fn to_str(&self) -> ~str { 
  match *self {
   LNum(x) => x.to_str(),
   LString(s) => s.to_owned(),
   LNil => ~"nil",
   LRustFunc(_) => ~"Rust function",
   LFunc(_) => ~"Lua function",
   _ => ~"something else",
  }
 }
}


}