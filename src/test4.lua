local function test(_ENV)
  print(a)
end


a = 12
local print = print
test(_ENV)
