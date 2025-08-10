-- Benchmark for original bint library using Lua 5.3
-- Run with: hype run benchmarks/bint_benchmark.lua

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
print("BINT BENCHMARK (Original Lua 5.3 Implementation)")
print("=" .. string.rep("=", 70))

-- Test data
local small_a = bint.new(123456789)
local small_b = bint.new(987654321)

local medium_a = bint.new("123456789012345678901234567890")
local medium_b = bint.new("987654321098765432109876543210")

local large_a = bint.new("1" .. string.rep("0", 50))
local large_b = bint.new("9" .. string.rep("9", 50))

local huge_a = bint.new("1" .. string.rep("0", 100))
local huge_b = bint.new("9" .. string.rep("9", 100))

print("\n-- Creation Operations --")
benchmark("Create from small integer", function()
    local n = bint.new(42)
end, 50000)

benchmark("Create from string (30 digits)", function()
    local n = bint.new("123456789012345678901234567890")
end, 10000)

benchmark("Create from string (100 digits)", function()
    local n = bint.new("1" .. string.rep("2", 99))
end, 5000)

print("\n-- Addition Operations --")
benchmark("Add small numbers", function()
    local c = small_a + small_b
end, 50000)

benchmark("Add medium numbers (30 digits)", function()
    local c = medium_a + medium_b
end, 20000)

benchmark("Add large numbers (50 digits)", function()
    local c = large_a + large_b
end, 10000)

benchmark("Add huge numbers (100 digits)", function()
    local c = huge_a + huge_b
end, 5000)

print("\n-- Subtraction Operations --")
benchmark("Subtract small numbers", function()
    local c = small_b - small_a
end, 50000)

benchmark("Subtract medium numbers (30 digits)", function()
    local c = medium_b - medium_a
end, 20000)

benchmark("Subtract large numbers (50 digits)", function()
    local c = large_b - large_a
end, 10000)

print("\n-- Multiplication Operations --")
benchmark("Multiply small numbers", function()
    local c = small_a * small_b
end, 30000)

benchmark("Multiply medium numbers (30 digits)", function()
    local c = medium_a * medium_b
end, 5000)

benchmark("Multiply large numbers (50 digits)", function()
    local c = large_a * large_b
end, 1000)

benchmark("Multiply huge numbers (100 digits)", function()
    local c = huge_a * huge_b
end, 500)

print("\n-- Division Operations --")
benchmark("Divide small numbers", function()
    local c = bint.tdiv(small_b, small_a)  -- Use truncate division
end, 30000)

benchmark("Divide medium numbers (30 digits)", function()
    local c = bint.tdiv(medium_b, medium_a)
end, 5000)

benchmark("Divide large numbers (50 digits)", function()
    local c = bint.tdiv(large_b, large_a)
end, 1000)

print("\n-- Comparison Operations --")
benchmark("Compare small numbers", function()
    local result = small_a < small_b
end, 100000)

benchmark("Compare medium numbers (30 digits)", function()
    local result = medium_a < medium_b
end, 50000)

benchmark("Compare large numbers (50 digits)", function()
    local result = large_a < large_b
end, 30000)

print("\n-- Bitwise Operations --")
benchmark("Bitwise AND small numbers", function()
    local c = bint.__band(small_a, small_b)
end, 50000)

benchmark("Bitwise OR medium numbers", function()
    local c = bint.__bor(medium_a, medium_b)
end, 20000)

benchmark("Bitwise XOR large numbers", function()
    local c = bint.__bxor(large_a, large_b)
end, 10000)

benchmark("Left shift small number", function()
    local c = bint.__shl(small_a, 10)
end, 50000)

benchmark("Right shift medium number", function()
    local c = bint.__shr(medium_a, 10)
end, 20000)

print("\n-- Power Operations --")
benchmark("Power small (2^100)", function()
    local c = bint.ipow(bint.new(2), bint.new(100))
end, 5000)

benchmark("Power medium (10^50)", function()
    local c = bint.ipow(bint.new(10), bint.new(50))
end, 2000)

print("\n-- String Conversion --")
benchmark("Convert small to string", function()
    local s = tostring(small_a)
end, 50000)

benchmark("Convert medium to string", function()
    local s = tostring(medium_a)
end, 20000)

benchmark("Convert large to string", function()
    local s = tostring(large_a)
end, 10000)

benchmark("Convert to hex (medium number)", function()
    local s = medium_a:tobase(16)
end, 20000)

print("\n-- Mixed Operations (Factorial) --")
benchmark("Factorial(20)", function()
    local result = bint.one()
    for i = 2, 20 do
        result = result * bint.new(i)
    end
end, 5000)

benchmark("Factorial(50)", function()
    local result = bint.one()
    for i = 2, 50 do
        result = result * bint.new(i)
    end
end, 1000)

print("\n" .. string.rep("=", 71))
print("Benchmark completed successfully!")