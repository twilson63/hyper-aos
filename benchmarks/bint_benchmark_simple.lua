-- Simple benchmark for original bint library
-- Run with: hype run benchmarks/bint_benchmark_simple.lua

-- Load the original bint library
local bint_module = dofile("src/bint.lua")
local bint = bint_module(256)  -- 256-bit integers

-- Benchmark utilities
local function benchmark(name, func, iterations)
    iterations = iterations or 10000
    local start_time = os.clock()
    
    for i = 1, iterations do
        func()
    end
    
    local end_time = os.clock()
    local total_time = end_time - start_time
    local avg_time = (total_time / iterations) * 1000000  -- Convert to microseconds
    
    print(string.format("%-40s: %8.2f µs/op (%d ops in %.3fs)", 
                        name, avg_time, iterations, total_time))
end

print("=" .. string.rep("=", 70))
print("BINT BENCHMARK (Original Implementation - Limited Test)")
print("=" .. string.rep("=", 70))

-- Test data
local small_a = bint.new(123456789)
local small_b = bint.new(987654321)

local medium_a = bint.new("123456789012345678901234567890")
local medium_b = bint.new("987654321098765432109876543210")

local large_a = bint.new("1" .. string.rep("0", 50))
local large_b = bint.new("9" .. string.rep("9", 50))

print("\n-- Creation Operations --")
benchmark("Create from small integer", function()
    local n = bint.new(42)
end, 10000)

benchmark("Create from string (30 digits)", function()
    local n = bint.new("123456789012345678901234567890")
end, 5000)

print("\n-- Addition Operations --")
benchmark("Add small numbers", function()
    local c = small_a + small_b
end, 10000)

benchmark("Add medium numbers (30 digits)", function()
    local c = medium_a + medium_b
end, 5000)

benchmark("Add large numbers (50 digits)", function()
    local c = large_a + large_b
end, 2000)

print("\n-- Subtraction Operations --")
benchmark("Subtract small numbers", function()
    local c = small_b - small_a
end, 10000)

benchmark("Subtract medium numbers (30 digits)", function()
    local c = medium_b - medium_a
end, 5000)

print("\n-- Multiplication Operations --")
benchmark("Multiply small numbers", function()
    local c = small_a * small_b
end, 5000)

benchmark("Multiply medium numbers (30 digits)", function()
    local c = medium_a * medium_b
end, 1000)

print("\n-- String Conversion --")
benchmark("Convert small to string", function()
    local s = tostring(small_a)
end, 10000)

benchmark("Convert medium to string", function()
    local s = tostring(medium_a)
end, 5000)

print("\n-- Mixed Operations (Factorial) --")
benchmark("Factorial(20)", function()
    local result = bint.one()
    for i = 2, 20 do
        result = result * bint.new(i)
    end
end, 1000)

print("\n" .. string.rep("=", 71))
print("Note: Limited test due to Lua environment constraints")
print("Full benchmark requires Lua 5.3 with bitwise operator support")