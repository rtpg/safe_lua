-- this is a test file for me to try running the commands
print "Hi there";
print "OK";

local function fib(n)
   if n <= 2 then return 1;
   else return fib(n-1) + fib(n-2);
   end;
end;

local function pow(n)
   if n <= 0 then return 1;
   else return 2 * pow(n-1);
   end;
end;
print "Attempting to calculate pow(4), should get 16";
print(pow(4));

print "Attempting to calculate fib(7), should get 13";
print(fib(7));


