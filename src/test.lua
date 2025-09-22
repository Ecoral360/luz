-- Test for the 'and' operator
-- local a = {x=10, y=20}
-- local b = {x=30, y=40}
-- local c = a and b
-- assert(c == b, c)
-- assert(c.x == 30 and c.y == 40)
-- assert((a and b).x == 30 and (a and b).y == 40)
-- assert((nil and b) == nil)

-- local d = nil and b
-- assert(d == nil)
-- local e = false and b
-- assert(e == false)
-- assert((a and b) == b)

-- assert(type(1<2) == 'boolean')
-- assert(type(true) == 'boolean' and type(false) == 'boolean')
assert(type(nil) == 'nil'
   and type(-3) == 'number'
   and type'x' == 'string'
   and type{} == 'table'
   and type(type) == 'function')
--
-- assert(type(assert) == type(print))
-- local function f (x) return a:x (x) end
-- assert(type(f) == 'function')
-- assert(not pcall(type))
--
