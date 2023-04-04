function f()
  return 1 + "a"
end

failed, result = pcall(f)
assert(failed == false)
assert(type(result) == "string")
