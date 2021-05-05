-- this test is added by me, I add stuff I run across


print("testing multiple returns")
function f()
  return 1, 2
end

print(f() + 1)
assert((f() + 1) == 2)
x = f()
assert(x==1)
x, y = f()
assert(x==1 and y==2)
print("TYPE")
print(math.type(-3e23))

a,b = math.modf(-3e23)
assert(a == -3e23)
assert(b == 0.0)
