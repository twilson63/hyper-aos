#!/usr/bin/env lua
-- Sandbox Environment Validation Tool
-- Validates that the sandbox environment meets all requirements for HyperAOS modules

local validation_results = {
    passed = 0,
    failed = 0,
    warnings = 0,
    tests = {}
}

local function add_result(category, name, status, message)
    table.insert(validation_results.tests, {
        category = category,
        name = name,
        status = status,
        message = message
    })
    
    if status == "PASS" then
        validation_results.passed = validation_results.passed + 1
    elseif status == "FAIL" then
        validation_results.failed = validation_results.failed + 1
    elseif status == "WARN" then
        validation_results.warnings = validation_results.warnings + 1
    end
end

-- Load the sandbox environment from test.lua
local function create_sandbox_env()
    local env = {
        _G = {},
        print = function() end,  -- Silent print
        io = { open = io.open, close = io.close },
        os = { date = os.date, time = os.time, clock = os.clock, difftime = os.difftime },
        
        -- Standard libraries
        table = table,
        string = string,
        math = math,
        coroutine = coroutine,
        
        -- Core functions
        pairs = pairs,
        ipairs = ipairs,
        next = next,
        select = select,
        unpack = unpack or table.unpack,
        
        -- Type system
        type = type,
        tostring = tostring,
        tonumber = tonumber,
        
        -- Metatables
        setmetatable = setmetatable,
        getmetatable = getmetatable,
        rawget = rawget,
        rawset = rawset,
        rawequal = rawequal,
        rawlen = rawlen or function(t) return #t end,
        
        -- Error handling
        pcall = pcall,
        xpcall = xpcall,
        error = error,
        assert = assert,
        
        -- Module system
        require = function() end,  -- Stub for safety
        package = { 
            loaded = {},
            path = "",
            cpath = "",
            preload = {}
        },
        
        -- Loading
        load = load,
        loadstring = loadstring or load,
    }
    env._G = env
    return env
end

-- Validation functions
local function validate_core_globals(env)
    local category = "Core Globals"
    
    -- Check _G self-reference
    if env._G == env then
        add_result(category, "_G self-reference", "PASS", "_G correctly references itself")
    else
        add_result(category, "_G self-reference", "FAIL", "_G does not reference itself")
    end
    
    -- Check essential globals
    local essential = {
        "type", "tostring", "tonumber", "pairs", "ipairs", "next",
        "select", "pcall", "xpcall", "error", "assert"
    }
    
    for _, name in ipairs(essential) do
        if type(env[name]) == "function" then
            add_result(category, name, "PASS", name .. " function available")
        else
            add_result(category, name, "FAIL", name .. " function missing or wrong type")
        end
    end
end

local function validate_table_library(env)
    local category = "Table Library"
    
    if not env.table then
        add_result(category, "table", "FAIL", "table library missing entirely")
        return
    end
    
    local required_functions = {
        "insert", "remove", "concat", "sort", "unpack"
    }
    
    for _, func in ipairs(required_functions) do
        if type(env.table[func]) == "function" then
            add_result(category, "table." .. func, "PASS", "table." .. func .. " available")
        else
            add_result(category, "table." .. func, "FAIL", "table." .. func .. " missing")
        end
    end
    
    -- Check for table.unpack vs unpack compatibility
    if env.unpack or env.table.unpack then
        add_result(category, "unpack compatibility", "PASS", "unpack function available")
    else
        add_result(category, "unpack compatibility", "FAIL", "No unpack function found")
    end
end

local function validate_string_library(env)
    local category = "String Library"
    
    if not env.string then
        add_result(category, "string", "FAIL", "string library missing entirely")
        return
    end
    
    local required_functions = {
        "match", "gmatch", "gsub", "find", "sub", "format",
        "byte", "char", "len", "lower", "upper", "rep", "reverse"
    }
    
    for _, func in ipairs(required_functions) do
        if type(env.string[func]) == "function" then
            add_result(category, "string." .. func, "PASS", "string." .. func .. " available")
        else
            add_result(category, "string." .. func, "FAIL", "string." .. func .. " missing")
        end
    end
end

local function validate_math_library(env)
    local category = "Math Library"
    
    if not env.math then
        add_result(category, "math", "WARN", "math library missing (may be optional)")
        return
    end
    
    local common_functions = {
        "abs", "ceil", "floor", "max", "min", "random", "sqrt"
    }
    
    for _, func in ipairs(common_functions) do
        if type(env.math[func]) == "function" then
            add_result(category, "math." .. func, "PASS", "math." .. func .. " available")
        else
            add_result(category, "math." .. func, "WARN", "math." .. func .. " missing")
        end
    end
end

local function validate_metatables(env)
    local category = "Metatables"
    
    local metatable_functions = {
        "setmetatable", "getmetatable", "rawget", "rawset", "rawequal"
    }
    
    for _, func in ipairs(metatable_functions) do
        if type(env[func]) == "function" then
            add_result(category, func, "PASS", func .. " available")
        else
            add_result(category, func, "FAIL", func .. " missing")
        end
    end
    
    -- Check rawlen (Lua 5.2+) with fallback
    if type(env.rawlen) == "function" then
        add_result(category, "rawlen", "PASS", "rawlen available")
    elseif env.rawlen then
        add_result(category, "rawlen", "PASS", "rawlen fallback available")
    else
        add_result(category, "rawlen", "WARN", "rawlen not available (Lua 5.1?)")
    end
end

local function validate_module_system(env)
    local category = "Module System"
    
    if not env.package then
        add_result(category, "package", "FAIL", "package table missing")
        return
    end
    
    if type(env.package.loaded) == "table" then
        add_result(category, "package.loaded", "PASS", "package.loaded table available")
    else
        add_result(category, "package.loaded", "FAIL", "package.loaded missing or wrong type")
    end
    
    -- Check require stub
    if env.require then
        if type(env.require) == "function" then
            add_result(category, "require", "PASS", "require function stubbed")
        else
            add_result(category, "require", "WARN", "require exists but wrong type")
        end
    else
        add_result(category, "require", "WARN", "require not stubbed")
    end
end

local function validate_security(env)
    local category = "Security"
    
    -- Check that dangerous functions are NOT present
    local dangerous = {
        {"os.execute", env.os and env.os.execute},
        {"os.exit", env.os and env.os.exit},
        {"os.remove", env.os and env.os.remove},
        {"os.rename", env.os and env.os.rename},
        {"io.popen", env.io and env.io.popen},
        {"io.write", env.io and env.io.write},
        {"io.output", env.io and env.io.output},
        {"loadfile", env.loadfile},
        {"dofile", env.dofile},
        {"debug", env.debug}
    }
    
    for _, check in ipairs(dangerous) do
        local name, func = check[1], check[2]
        if func == nil then
            add_result(category, name, "PASS", name .. " correctly blocked")
        else
            add_result(category, name, "FAIL", name .. " is exposed (SECURITY RISK)")
        end
    end
    
    -- Check safe OS functions
    if env.os then
        local safe_os = {"date", "time", "clock", "difftime"}
        for _, func in ipairs(safe_os) do
            if type(env.os[func]) == "function" then
                add_result(category, "os." .. func, "PASS", "os." .. func .. " safely available")
            else
                add_result(category, "os." .. func, "WARN", "os." .. func .. " missing")
            end
        end
    end
end

local function validate_loading_functions(env)
    local category = "Code Loading"
    
    if type(env.load) == "function" then
        add_result(category, "load", "PASS", "load function available")
    else
        add_result(category, "load", "FAIL", "load function missing")
    end
    
    -- Check loadstring (Lua 5.1 compatibility)
    if type(env.loadstring) == "function" or type(env.load) == "function" then
        add_result(category, "loadstring", "PASS", "loadstring or fallback available")
    else
        add_result(category, "loadstring", "FAIL", "No code loading function available")
    end
end

local function validate_optional_features(env)
    local category = "Optional Features"
    
    -- Coroutines
    if env.coroutine then
        add_result(category, "coroutine", "PASS", "coroutine library available")
    else
        add_result(category, "coroutine", "WARN", "coroutine library missing (optional)")
    end
    
    -- UTF-8 support (Lua 5.3+)
    if env.utf8 then
        add_result(category, "utf8", "PASS", "utf8 library available")
    else
        add_result(category, "utf8", "WARN", "utf8 library missing (Lua < 5.3?)")
    end
end

-- Run validation
print("🔍 HyperAOS Sandbox Environment Validator")
print("=" .. string.rep("=", 50))
print("")

local env = create_sandbox_env()

validate_core_globals(env)
validate_table_library(env)
validate_string_library(env)
validate_math_library(env)
validate_metatables(env)
validate_module_system(env)
validate_security(env)
validate_loading_functions(env)
validate_optional_features(env)

-- Print results
print("")
print("📊 Validation Results")
print("=" .. string.rep("=", 50))

local categories = {}
for _, test in ipairs(validation_results.tests) do
    if not categories[test.category] then
        categories[test.category] = {}
    end
    table.insert(categories[test.category], test)
end

for category, tests in pairs(categories) do
    print("")
    print("🏷️  " .. category .. ":")
    for _, test in ipairs(tests) do
        local icon = test.status == "PASS" and "✅" or 
                     test.status == "FAIL" and "❌" or "⚠️"
        print(string.format("  %s %s: %s", icon, test.name, test.message))
    end
end

print("")
print("=" .. string.rep("=", 50))
print(string.format("📈 Summary: %d passed, %d failed, %d warnings",
    validation_results.passed, validation_results.failed, validation_results.warnings))

if validation_results.failed > 0 then
    print("❌ VALIDATION FAILED - Sandbox environment incomplete")
    os.exit(1)
elseif validation_results.warnings > 5 then
    print("⚠️  VALIDATION PASSED with warnings - Review optional features")
    os.exit(0)
else
    print("✅ VALIDATION PASSED - Sandbox environment ready")
    os.exit(0)
end