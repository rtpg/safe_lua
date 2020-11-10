-- this is a test file for me to try running the commands
print "Hi there";
print "OK";

local function foo(n)
   if i <= 2 then return 1;
   else return foo(n-1) + foo(n-1);
   end;
end;

print "Attempting to calculate fib(4), should get 16";
print foo(4);