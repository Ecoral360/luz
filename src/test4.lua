local function test(_ENV)
  print(a)
  a = 333
  print(a)
end


a = 12
local print = print
test(setmetatable({}, {__index = _ENV}))
print(a)
test(_ENV)
print(a)
