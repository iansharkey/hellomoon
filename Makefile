
lua: lua.rs luaval.rs
	rustc --lib luaval.rs
	rustc -L. lua.rs

