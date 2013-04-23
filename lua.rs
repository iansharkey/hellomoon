
extern mod luaval;

use core::hashmap::linear;
use core::ops::*;
use core::vec::*;

//use core::str;
//use core::task::try;

use luaval::luaval::*;



/*
fn lpcall(reg: &mut ~[LuaVal]) -> ~[LuaVal] {
   let luafunc = reg.remove(0);

    match luafunc {
          LFunc(subexec) => {
              let owned_subexec = ~(copy *subexec);
	    do try {

	      let mut params = ~[];
	       run(owned_subexec, &mut params);
	    }
	  }


          _ => fail!(~"Tried to call a non-function!"),
     };
  return ~[];
}
*/

fn ltype(reg: &mut ~[LuaVal]) -> ~[LuaVal] {
  return ~[ LString(match reg[0] {
    LString(_) => ~"string",
    LNum(_) => ~"number",
    LBool(_) => ~"boolean",
    LTable(_, _) => ~"table",
    LFunc(_) => ~"function",
    LRustFunc(_) => ~"function",
    LNil => ~"nil",
  })
  ]
}


fn lprint(reg: &mut ~[LuaVal]) -> ~[LuaVal] { 
   let strs = map(*reg, |v| { v.to_str() });
   let line = str::connect(strs, ~"\t");
   io::println(line);
   return ~[];
}




fn list_get(reg: &mut ~[LuaVal]) -> ~[LuaVal] {
  let table = match reg[0] {
    LTable(ref table, _) => copy table,
    _ => fail!(~"ipairs: Expecting a table in arg 1!"),
  };

  let index = match &reg[1] {
    &LNum(v) => v,
    _ => fail!(~"ipairs: Expecting a table arg 2!"),
  };

  let mut lua_index = LNum(index+1f);
  if table.contains_key(&lua_index) {  
    return ~[copy lua_index, copy *table.get(&lua_index ) ];
  }

  return ~[LNil, LNil];
}


fn ipairs(reg: &mut ~[LuaVal]) -> ~[LuaVal] {
   let table = copy reg[0];
   return ~[LRustFunc(list_get), table, LNum(0f) ];
}




fn run( execution: &Execution, regs: &mut ~[LuaVal] ) -> ~[LuaVal] {
 let mut pc = 0;
 let mut &execution_prime = &(*execution);
 let mut regs_prime = regs;
// let mut empty_regs: &~[LuaVal] = &mut ~[];

 loop {
   match  execution_prime.prog[pc] {
    IReturn(src, extent) => return from_fn( (extent-1) as uint, |i| { copy regs_prime[src+i as int] }),
    ITailCall(func, extent, _) => match copy regs_prime[func] {
      LFunc(ref f_execution) => {
        execution_prime = copy *f_execution;
 	
	//grow_fn( regs_prime, (extent-1) as uint, |i: uint| { regs_prime[func+1+(i as int) ] }); 	
	for uint::range(func as uint, 0) |i| {
	  regs_prime.remove(i);
	}
	pc = 0;
      }
      _ => fail!(~"Cannot tail call to non-Lua function!")
    }, 
    _ => step(execution_prime.prog[pc], &mut pc, regs_prime, &execution_prime.constants, execution_prime.globals)
  }
 }
}



fn step( instr: Instr, pc: &mut int, reg: &mut ~[LuaVal], constants: &~[LuaVal], globals: &mut linear::LinearMap<LuaVal, LuaVal> ) {

 let jump = |n|  *pc+=n;
 let bump = || jump(1);;
 let reg_k = |r: int| if r<0 { copy constants[-r - 1] } else { copy reg[r] };
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

    ITest(r, c) => if !(lval_to_bool(copy reg[r]) != num_to_bool(c)) { bump(); },
    ITestSet(dst, r, c) => if (lval_to_bool(copy reg[r]) != num_to_bool(c)) { reg[dst] = copy reg[r]; } else { bump(); },

    IAdd(dst, r1, r2) => reg[dst] = reg_k(r1) + reg_k(r2),
    ISub(dst, r1, r2) => reg[dst] = reg_k(r1) - reg_k(r2),
    IMul(dst, r1, r2) => reg[dst] = reg_k(r1) * reg_k(r2),
    IDiv(dst, r1, r2) => reg[dst] = reg_k(r1) / reg_k(r2),

    // TODO: this should operate over all the strings between r1 and r2
    IConcat(dst, r1, r2) => reg[dst] = LString((reg_k(r1).to_str() + reg_k(r2).to_str())),

    INot(dst, src) =>  reg[dst] = LBool(!lval_to_bool(copy reg[src])),

    IUnm(dst, src) => reg[src] = match reg[dst] {
        LNum(x) => LNum(-x),
	_ => fail!(~"Attempt to perform arithmetic on a non-number value!")
      },

    ILen(dst, src) => reg[dst] = match &reg[src] {
      &LString(ref x) => LNum(x.len() as float),
      // TODO: need to handle tables and metatables
      _ => fail!(~"Not a string!"),
    },

    ILoadK(dst, src) => reg[dst] = copy constants[src],
    ILoadNil(start, end) => { grow(reg, end, &LNil); for uint::range(start, end) |i| { reg[i] = LNil; }; }
    IMove(r1, r2) => reg[r1] = reg_k(r2),
    ILoadBool(dst, b, c) => { reg[dst] = LBool(num_to_bool(b)); if num_to_bool(c) { bump(); } },


    IGetGlobal(dst, k) => reg[dst] = match globals.find(&constants[k]) {
      Some(v) => copy *v,
      None => LNil,
    },
    ISetGlobal(src, k) => {globals.insert(copy constants[k], copy reg[src]); },


    INewTable(dst, _, _) => reg[dst] = LTable(linear::LinearMap::new(), ~[]),
    IGetTable(dst, tbl, index) => match copy reg[tbl] {
         LTable(ref table, _) => reg[dst] = match table.find(&reg_k(index)) {
	    Some(v) => copy *v,
	    None => LNil,
	   },
	 _ => fail!(~"Tried to index a non-table"),
       },

    ISetTable(tbl, index, value) => match reg[tbl] {
        LTable(ref mut table, _) => { table.insert(reg_k(index), reg_k(value)); },
	// Note: need to handle meta-tables here
	_ => fail!(~"Tried to insert a value into a non-table!")
      },


    ISelf(dst, tbl, method) => {
      reg[dst+1] = copy reg[tbl];
      match copy reg[tbl] {
        LTable(ref table, _) => reg[dst] = copy *table.get(&reg_k(method)),
	_ => fail!(~"Expecting a table for method call!")
      }
    }


    ISetList(tbl, num, block) => {
      match reg[tbl] {
        LTable(ref mut hmap, _) =>  for int::range(0, num) |i| {
	          hmap.insert(LNum(((block-1)*50+i) as float), copy reg[tbl+i]);
      		},
	_ => fail!(~"Expecting a table to set values!"), 
      }
    },

    IJmp(offset) => jump(offset),

    IForPrep(index, offset) => { reg[index] = reg[index] - reg[index+2]; jump(offset); },

    IForLoop(index, offset) => { reg[index] = reg[index] + reg[index+2];
    		    	         let action = ||  { jump(offset); reg[index+3] = copy reg[index]; };
    		    	         if reg[index+2] > LNum(0f) { 
				   if reg[index] <= reg[index+1] { action(); }
				 }
				 else {
				   if reg[index] >= reg[index+1] { action(); }
				 }
			       }


    ITForLoop(func, c) => {
      //io::println("gothere in tforloop");
      let mut args = ~[ copy reg[func+1], copy reg[func+2] ];
      let ret_regs = match copy reg[func] {
          LFunc(subexec) => run( &subexec, &mut args),
	  LRustFunc(f) =>  f(&mut args),
          _ => fail!(~"Tried to call a non-function!"),
	};
      for int::range(0, c) |i| { reg[func+3+i] = copy ret_regs[i]; };
      if reg[func+3] != LNil {
        reg[func+2] = copy reg[func+3];
      }
      else {
        bump();
      }
    }
    

    ICall(func, call_extent, ret_extent) =>  {
    	let mut reg_prime = from_fn( (call_extent-1) as uint, |i| { copy reg[func+1+i as int] }); 

	//io::println(fmt!("calling %s", reg[func].to_str()));
	let ret_regs = match copy reg[func] {
          LFunc(subexec) => run( &subexec, &mut reg_prime),
	  LRustFunc(f) =>  f(&mut reg_prime),
          _ => fail!(~"Tried to call a non-function!"),
	};

	//io::println(fmt!("got %d items, expecting %d, first: %s", ret_regs.len() as int, ret_extent-2+1, ret_regs[0].to_str()));	
	match ret_extent as uint {
	    0 => return, // TODO: take all return values
	    1 => return,
	    _ => for int::range(0, ret_extent-2+1) |i| { reg[func+i] = copy ret_regs[i]; },
	}
    },

    IReturn(_, _) => { /* can't get here */ },
    ITailCall(_, _, _) => { /* can't get here */ },
  }
}




fn main() {

   // let registers = @mut [LNum(0.0f), LNum(3.0f), LNum(1.0f), LNum(2.0f)];


 let mut globals = ~linear::LinearMap::new();
 
 let mut hmap: linear::LinearMap<LuaVal, LuaVal> = linear::LinearMap::new();

 hmap.insert( LNum(1f), LNum(56f) );
 hmap.insert( LNum(2f), LNum(57f) );
 hmap.insert( LNum(3f), LNum(58f) );

/*
 let subprog = Execution { globals: globals, state: @mut true, constants: ~[LNum(70f), LTable(@mut hmap, @[]), LString(@~"a")], prog: Program(~[
  ILoadNil(0, 55),
  ILoadK(1, 1),
  IGetTable(2, 1, -3),
  IReturn(2, 2)
 ]) };
*/

 globals.insert(LString(~"print"), LRustFunc(lprint));
 globals.insert(LString(~"ipairs"), LRustFunc(ipairs));
 globals.insert(LString(~"type"), LRustFunc(ltype));


  let mut s = ~Execution { globals: globals, state: true, constants: ~[
      	LTable(hmap, ~[]), LString(~"print"), LString(~"ipairs"), LString(~"type"), LNil ], prog: Program(~[

    INewTable(0, 0, 0),
    ILoadK(1, 1),
    ILoadK(2, 2),
    ILoadK(3, 3),
    ILoadK(4, 4),
    ISetList(0, 4, 1),
    


    IGetGlobal(0, 3),
    IGetGlobal(1, 4),
    ICall(0, 2, 2),

    IMove(1, 0),

    IGetGlobal(0, 1),
    ICall(0, 2, 1),

    IGetGlobal(0, 2),
    ILoadK(1, 0),
    ICall(0, 2, 4),
    IJmp(4),
    IGetGlobal(5, 1),
    IMove(6, 3),
    IMove(7, 4),
    ICall(5, 3, 1),
    ITForLoop(0, 2),
    IJmp(-6),
    IReturn(0, 1),
  ]) };

/*
 let s = ~Execution { globals: globals, state: @mut true, constants: ~[LNum(500f), LNum(300f), LString(@~"print")], prog: Program(~[
     IGetGlobal(1, 2),
     ILoadK(2, 0),
     ILoadK(3, 2),
     ILen(3, 3),
     ICall(1, 3, 1),
     ILoadK(1, 0),
     IAdd(3,1,-1), 
     ISub(3,3,2),
     IReturn(3, 2),
    ]) };

 let fancy = ~Execution { globals: globals, state: @mut true, constants: ~[LNum(0f), LNum(1f), LNum(100f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   ILoadK(3, 1),
   IForPrep(1, 1),
   IAdd(0, 4, 0),
   IForLoop(1, -2),
   IReturn(0, 2),
  ]) };

  let concat = ~Execution { globals: globals, state: @mut true, constants: ~[LNum(55f), LNum(33f), LNum(22f), LNum(66f)], prog: Program(~[
   ILoadK(0, 0),
   ILoadK(1, 1),
   ILoadK(2, 2),
   IConcat(3, 0, 1),
   IConcat(3, 3, 1),
   IConcat(3, 3, -4),
   IReturn(3, 2),
  ]) };
*/



 let mut regs = ~[LNum(5050f), LNum(0f),LNum(111f),LNum(0f), LNum(0f),LNum(0f), LNum(0f), LNum(0f),];
 let out = run(s, &mut regs );
 io::println( out.to_str() );

}



fn main_test() {
 let v1 = LNum(1.0f);
 let v2 = LNum(3.0f);
 let v3 = v1 + v2;

 let s1 = LString(~"hello");
 let s2 = LString(~" world");
 let s3 = s1 + s2;

 io::println( (v2 < v1).to_str() );
 io::println( s3.to_str() );
 match v3 {
  LNum(x) => { io::println(fmt!("%f", x)); },
  LString(s) => { io::println(s); },
  _ => { ; },
 }

}