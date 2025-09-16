-- $Id: testes/calls.lua $
-- See Copyright Notice in file all.lua

-- print("testing functions and calls")
--
-- -- get the opportunity to test 'type' too ;)
--
-- assert(type(1<2) == 'boolean')
-- assert(type(true) == 'boolean' and type(false) == 'boolean')
-- assert(type(nil) == 'nil'
--    and type(-3) == 'number'
--    and type'x' == 'string'
--    and type{} == 'table'
--    and type(type) == 'function')
--
-- assert(type(assert) == type(print))
-- local function f (x) return a:x (x) end
-- assert(type(f) == 'function')

-- testing local-function recursion
fact = false
do
  local res = 1
  local function fact (n)
    if n==0 then return res
    else return n*fact(n-1)
    end
  end
  assert(fact(5) == 120)
end
assert(fact == false)
fact = nil

-- testing declarations
-- local a = {i = 10}
-- local self = 20
-- function a:x (x) return x+self.i end
-- function a.y (x) return x+self end
--
-- assert(a:x(1)+10 == a.y(1))
--
-- a.t = {i=-100}
-- a["t"].x = function (self, a,b) return self.i+a+b end
--
-- assert(a.t:x(2,3) == -95)
--
-- do
--   local a = {x=0}
--   function a:add (x) self.x, a.y = self.x+x, 20; return self end
--   assert(a:add(10):add(20):add(30).x == 60 and a.y == 20)
-- end
--
-- local a = {b={c={}}}
--
-- function a.b.c.f1 (x) return x+1 end
-- function a.b.c:f2 (x,y) self[x] = y end
-- assert(a.b.c.f1(4) == 5)
-- a.b.c:f2('k', 12); assert(a.b.c.k == 12)
--
-- print('+')
--
-- t = nil   -- 'declare' t
-- function f(a,b,c) local d = 'a'; t={a,b,c,d} end
--
-- f(      -- this line change must be valid
--   1,2)
-- assert(t[1] == 1 and t[2] == 2 and t[3] == nil and t[4] == 'a')
-- f(1,2,   -- this one too
--       3,4)
-- assert(t[1] == 1 and t[2] == 2 and t[3] == 3 and t[4] == 'a')
--
-- t = nil   -- delete 't'
--
-- function fat(x)
--   if x <= 1 then return 1
--   else return x*load("return fat(" .. x-1 .. ")", "")()
--   end
-- end
--
-- assert(load "load 'assert(fat(6)==720)' () ")()
-- a = load('return fat(5), 3')
-- local a,b = a()
-- assert(a == 120 and b == 3)
-- fat = nil
-- print('+')
--
-- -- local function err_on_n (n)
-- --   if n==0 then error(); exit(1);
-- --   else err_on_n (n-1); exit(1);
-- --   end
-- -- end
-- --
-- -- do
-- --   local function dummy (n)
-- --     if n > 0 then
-- --       assert(not pcall(err_on_n, n))
-- --       dummy(n-1)
-- --     end
-- --   end
-- --
-- --   dummy(10)
-- -- end
-- --
-- -- _G.deep = nil   -- "declaration"  (used by 'all.lua')
-- --
-- -- function deep (n)
-- --   if n>0 then deep(n-1) end
-- -- end
-- -- deep(10)
-- -- deep(180)
