local chance = { _version = "1.0.0" }

local N = 624
local M = 397
local MATRIX_A = 0x9908b0df
local UPPER_MASK = 0x80000000
local LOWER_MASK = 0x7fffffff

local function init_genrand(o, s)
    o.mt[0] = s & 0xffffffff
    for i = 1, N - 1 do
        o.mt[i] = (1812433253 * (o.mt[i - 1] ~ (o.mt[i - 1] >> 30))) + i
        o.mt[i] = o.mt[i] & 0xffffffff
    end
    o.mti = N
end

local function genrand_int32(o)
    local y
    local mag01 = {}
    mag01[0] = 0x0
    mag01[1] = MATRIX_A
    
    if o.mti >= N then
        if o.mti == N + 1 then
            init_genrand(o, 5489)
        end
        
        for kk = 0, N - M - 1 do
            y = (o.mt[kk] & UPPER_MASK) | (o.mt[kk + 1] & LOWER_MASK)
            o.mt[kk] = o.mt[kk + M] ~ (y >> 1) ~ mag01[y & 0x1]
        end
        
        for kk = N - M, N - 2 do
            y = (o.mt[kk] & UPPER_MASK) | (o.mt[kk + 1] & LOWER_MASK)
            o.mt[kk] = o.mt[kk + (M - N)] ~ (y >> 1) ~ mag01[y & 0x1]
        end
        
        y = (o.mt[N - 1] & UPPER_MASK) | (o.mt[0] & LOWER_MASK)
        o.mt[N - 1] = o.mt[M - 1] ~ (y >> 1) ~ mag01[y & 0x1]

        o.mti = 0
    end

    y = o.mt[o.mti]
    o.mti = o.mti + 1

    y = y ~ (y >> 11)
    y = y ~ ((y << 7) & 0x9d2c5680)
    y = y ~ ((y << 15) & 0xefc60000)
    y = y ~ (y >> 18)

    return y
end

local MersenneTwister = {}
MersenneTwister.mt = {}
MersenneTwister.mti = N + 1

function chance.seed(seed)
    init_genrand(MersenneTwister, seed)
end

function chance.random()
    return genrand_int32(MersenneTwister) * (1.0 / 4294967296.0)
end

function chance.integer(min, max)
    if max < min then
        error("max must be greater than or equal to min", 2)
    end
    return math.floor(chance.random() * (max - min + 1) + min)
end

function chance.bool(likelihood)
    likelihood = likelihood or 0.5
    return chance.random() < likelihood
end

function chance.character(options)
    options = options or {}
    local pool = ""
    
    if options.alpha or not (options.numeric or options.symbols) then
        pool = pool .. "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end
    
    if options.numeric then
        pool = pool .. "0123456789"
    end
    
    if options.symbols then
        pool = pool .. "!@#$%^&*()"
    end
    
    if options.casing == "lower" then
        pool = string.lower(pool)
    elseif options.casing == "upper" then
        pool = string.upper(pool)
    end
    
    if #pool == 0 then
        pool = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    end
    
    local index = chance.integer(1, #pool)
    return string.sub(pool, index, index)
end

function chance.string(options)
    options = options or {}
    local length = options.length or 8
    local result = {}
    
    for i = 1, length do
        result[i] = chance.character(options)
    end
    
    return table.concat(result)
end

function chance.pick(array)
    if type(array) ~= "table" or #array == 0 then
        error("pick requires a non-empty table", 2)
    end
    
    local index = chance.integer(1, #array)
    return array[index]
end

function chance.shuffle(array)
    if type(array) ~= "table" then
        error("shuffle requires a table", 2)
    end
    
    local result = {}
    for i, v in ipairs(array) do
        result[i] = v
    end
    
    for i = #result, 2, -1 do
        local j = chance.integer(1, i)
        result[i], result[j] = result[j], result[i]
    end
    
    return result
end

function chance.weighted(choices, weights)
    if type(choices) ~= "table" or type(weights) ~= "table" then
        error("weighted requires two tables", 2)
    end
    
    if #choices ~= #weights then
        error("choices and weights must have the same length", 2)
    end
    
    local total = 0
    for i = 1, #weights do
        total = total + weights[i]
    end
    
    if total <= 0 then
        error("total weight must be positive", 2)
    end
    
    local random = chance.random() * total
    local cumulative = 0
    
    for i = 1, #weights do
        cumulative = cumulative + weights[i]
        if random <= cumulative then
            return choices[i]
        end
    end
    
    return choices[#choices]
end

function chance.normal(options)
    options = options or {}
    local mean = options.mean or 0
    local dev = options.dev or 1
    
    local u = 0
    local v = 0
    while u == 0 do
        u = chance.random()
    end
    while v == 0 do
        v = chance.random()
    end
    
    local z = math.sqrt(-2.0 * math.log(u)) * math.cos(2.0 * math.pi * v)
    return z * dev + mean
end

return chance