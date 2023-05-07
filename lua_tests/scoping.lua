-- testing naming
local a = {b={c={}}}

function a.b.c.f1 (x) return x+1 end
print(a.b.c.f1)
function a.b.c:f2 (x,y) self[x] = y end
assert(a.b.c.f1(4) == 5)
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
