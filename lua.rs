
extern mod luaval;

use core::hashmap::linear;
use core::ops::*;
use core::vec::*;

use luaval::luaval::*;


fn run( execution: &Execution, regs: &mut ~[LuaVal] ) -> ~[LuaVal] {
 let mut pc = 0;
 let mut &execution_prime = &(*execution);
 let mut &regs_prime = regs;

 loop {
   match execution_prime.prog[pc] {
    IReturn(src, extent) => return from_fn( (extent-1) as uint, |i| { regs_prime[src+i as int] }),
    ITailCall(func, extent, _) => match regs_prime[func] {
      LFunc(f_execution) => {
        execution_prime = copy(*f_execution);
    	regs_prime = from_fn( (extent-1) as uint, |i| { regs_prime[func+1+i as int] }); 	
	pc = 0;
      }
      _ => fail!(~"Cannot tail call to non-Lua function!")
    },
    _ => step(execution_prime.prog[pc], &mut pc, &mut regs_prime, &execution_prime.constants)
  }
 }
}

fn step( instr: Instr, pc: &mut int, reg: &mut ~[LuaVal], constants: &~[LuaVal] ) {

 let jump = |n|  *pc+=n;
 let bump = || jump(1);;
 let reg_k = |r: int| if r<0 { constants[-r - 1] } else { reg[r] };
 let num_to_bool = |n:int| match n { 0 => false, _ => true };
 let lval_to_bool = |lval:LuaVal| match lval {
       LBool(x) => x,
       LNil => false,
       _ => true,
    };

 bump();
 match instr {
    IEq(check, t1, t2) => if (reg_k(t1) == reg_k(t2)) != num_to_bool(check)  { bump(); },
    ILt(check, t1, t2) => if (reg_k(t1) < reg_k(t2)) != num_to_bool(check)  { bump(); },
    ILe(check, t1, t2) => if (reg_k(t1) <= reg_k(t2)) != num_to_bool(check)  { bump(); },

    ITest(r, c) => if !(lval_to_bool(reg[r]) != num_to_bool(c)) { bump(); },
    ITestSet(dst, r, c) => if (lval_to_bool(reg[r]) != num_to_bool(c)) { reg[dst] = reg[r]; } else { bump(); },

    IAdd(dst, r1, r2) => reg[dst] = reg_k(r1) + reg_k(r2),
    ISub(dst, r1, r2) => reg[dst] = reg_k(r1) - reg_k(r2),
    IMul(dst, r1, r2) => reg[dst] = reg_k(r1) * reg_k(r2),
    IDiv(dst, r1, r2) => reg[dst] = reg_k(r1) / reg_k(r2),

    // TODO: this should operate over all the strings between r1 and r2
    IConcat(dst, r1, r2) => reg[dst] = LString(@(reg_k(r1).to_str() + reg_k(r2).to_str())),

    INot(dst, src) =>  reg[dst] = LBool(!lval_to_bool(reg[src])),

    IUnm(dst, src) => reg[src] = match reg[dst] {
        LNum(x) => LNum(-x),
	_ => fail!(~"Attempt to perform arithmetic on a non-number value!")
      },

    ILoadK(dst, src) => reg[dst] = constants[src],
    ILoadNil(start, end) => { grow(reg, end, &LNil); for uint::range(start, end) |i| { reg[i] = LNil; }; }
    IMove(r1, r2) => reg[r1] = reg_k(r2),
    ILoadBool(dst, b, c) => { reg[dst] = LBool(num_to_bool(b)); if num_to_bool(c) { bump(); } },


    IGetTable(dst, tbl, index) => match reg[tbl] {
         LTable(table, _) => reg[dst] = *table.get(&reg_k(index)),
	 _ => fail!(~"Tried to index a non-table"),
       },

    ISetTable(tbl, index, value) => match reg[tbl] {
        LTable(table, _) => { table.insert(reg_k(index), reg_k(value)); },
	// Note: need to handle meta-tables here
	_ => fail!(~"Tried to insert a value into a non-table!")
      },

    ISelf(dst, tbl, method) => {
      reg[dst+1] = reg[tbl];
      match reg[tbl] {
        LTable(table, _) => reg[dst] = *table.get(&reg_k(method)),
	_ => fail!(~"Expecting a table for method call!")
      }
    }

    IJmp(offset) => jump(offset - 1),

    IForPrep(index, offset) => { reg[index] = reg[index] - reg[index+2]; jump(offset); },
    IForLoop(index, offset) => { reg[index] = reg[index] + reg[index+2];
    		    	         let action = ||  { jump(offset); reg[index+3] = reg[index]; };
    		    	         if reg[index+2] > LNum(0f) { 
				   if reg[index] <= reg[index+1] { action(); }
				 }
				 else {
				   if reg[index] >= reg[index+1] { action(); }
				 }
			       }
    

    ICall(func, call_extent, ret_extent) =>  {
    	let mut reg_prime = from_fn( (call_extent-1) as uint, |i| { reg[func+1+i as int] }); 
	let ret_regs = match reg[func] {
          LFunc(subexec) => run( subexec, &mut reg_prime),
	  LRustFunc(f) =>  f(&mut reg_prime),
          _ => fail!(~"Tried to call a non-function!"),
	};
	match ret_extent as uint {
	    0 => return, // TODO: take all return values
	    1 => return,
	    _ => for int::range(0, ret_extent-2+1) |i| { reg[func+i] = ret_regs[i]; },
	}
    },
    IReturn(_, _) => { /* can't get here */ },
    ITailCall(_, _, _) => { /* can't get here */ },

  }
}

fn c(reg: &mut ~[LuaVal]) -> ~[LuaVal] { 
    io::println(fmt!("somthing cool: %s", reg[0].to_str()));
    return ~[LNum(44f)];
}



fn main() {

   // let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];


 let mut hmap: linear::LinearMap<LuaVal, LuaVal> = linear::LinearMap::new();

 hmap.insert( LString(@~"a"), LNum(56f) );


 let subprog = Execution { state: @mut true, constants: ~[LNum(70f), LTable(@mut hmap, @[]), LString(@~"a")], prog: Program(~[
  ILoadNil(0, 55),
  ILoadK(1, 1),
  IGetTable(2, 1, -3),
  IReturn(2, 2)
 ]) };

 let s = ~Execution { state: @mut true, constants: ~[LNum(500f), LNum(300f), LRustFunc(c), LFunc(@subprog)], prog: Program(~[
     ILoadK(1, 2),
     ILoadK(2, 0),
     ICall(1, 2, 2),
//     ILoadK(1, 0),
     IAdd(3,1,-2), 
     ISub(3,3,2),
     IReturn(3, 2),
    ]) };

 let fancy = ~Execution { state: @mut true, constants: ~[LNum(0f), LNum(1f), LNum(100f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   ILoadK(3, 1),
   IForPrep(1, 1),
   IAdd(0, 4, 0),
   IForLoop(1, -2),
   IReturn(0, 2),
  ]) };

  let concat = ~Execution { state: @mut true, constants: ~[LNum(55f), LNum(33f), LNum(22f), LNum(66f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   IConcat(3, 0, 1),
   IConcat(3, 3, 1),
   IConcat(3, 3, -4),
   IReturn(3, 2),
  ]) };


 let mut regs = ~[LNum(5050f), LNum(0f),LNum(111f),LNum(0f), LNum(0f),];
 let out = run(s, &mut regs );
 io::println( out.to_str() );

}



fn main_test() {
 let v1 = LNum(1.0f);
 let v2 = LNum(3.0f);
 let v3 = v1 + v2;

 let s1 = LString(@~"hello");
 let s2 = LString(@~" world");
 let s3 = s1 + s2;

 io::println( (v2 < v1).to_str() );
 io::println( s3.to_str() );
 match v3 {
  LNum(x) => { io::println(fmt!("%f", x)); },
  LString(s) => { io::println(*s); },
  _ => { ; },
 }

}