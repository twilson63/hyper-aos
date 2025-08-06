-- Test Script for HyperAOS Build System
-- Validates the concatenated Lua module for LUERL compatibility

local test_results = {
    passed = 0,
    failed = 0,
    warnings = 0,
    tests = {}
}

local function add_result(name, status, message)
    table.insert(test_results.tests, {
        name = name,
        status = status,
        message = message
    })
    
    if status == "PASS" then
        test_results.passed = test_results.passed + 1
    elseif status == "FAIL" then
        test_results.failed = test_results.failed + 1
    elseif status == "WARN" then
        test_results.warnings = test_results.warnings + 1
    end
end

local function file_exists(path)
    local file = io.open(path, "r")
    if file then
        file:close()
        return true
    end
    return false
end

local function read_file(path)
    local file = io.open(path, "r")
    if not file then
        return nil, "Cannot open file: " .. path
    end
    local content = file:read("*all")
    file:close()
    return content, nil
end

local function test_file_exists()
    local name = "Output file exists"
    if file_exists("dist/aos.lua") then
        add_result(name, "PASS", "dist/aos.lua found")
    else
        add_result(name, "FAIL", "dist/aos.lua not found - run 'make build' first")
        return false
    end
    return true
end

local function test_file_size()
    local name = "File size validation"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    local size = #content
    if size < 1000 then
        add_result(name, "FAIL", string.format("File too small: %d bytes", size))
        return false
    elseif size > 1000000 then
        add_result(name, "WARN", string.format("File very large: %.2f KB", size/1024))
    else
        add_result(name, "PASS", string.format("File size OK: %.2f KB", size/1024))
    end
    return true
end

local function test_module_registration()
    local name = "Module registration"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    if content:find("_G%.package%.loaded") then
        add_result(name, "PASS", "Package.loaded registration found")
    else
        add_result(name, "FAIL", "Missing _G.package.loaded registration")
        return false
    end
    return true
end

local function test_required_modules()
    local name = "Required modules"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    local utils_found = content:find('package%.loaded%["utils"%]')
    local aos_found = content:find('package%.loaded%["aos"%]')
    
    if utils_found and aos_found then
        add_result(name, "PASS", "Both utils and aos modules registered")
    elseif utils_found then
        add_result(name, "FAIL", "Missing aos module registration")
        return false
    elseif aos_found then
        add_result(name, "FAIL", "Missing utils module registration")
        return false
    else
        add_result(name, "FAIL", "Missing both module registrations")
        return false
    end
    return true
end

local function test_module_order()
    local name = "Module load order"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    local utils_pos = content:find('Module: utils')
    local aos_pos = content:find('Module: aos')
    
    if utils_pos and aos_pos then
        if utils_pos < aos_pos then
            add_result(name, "PASS", "Utils loaded before aos (correct order)")
        else
            add_result(name, "FAIL", "Incorrect module order - utils must load before aos")
            return false
        end
    else
        add_result(name, "FAIL", "Cannot determine module order")
        return false
    end
    return true
end

local function test_lua_syntax()
    local name = "Lua syntax validation"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    local func, err = load(content, "aos-bundle")
    if func then
        add_result(name, "PASS", "Valid Lua syntax")
    else
        add_result(name, "FAIL", "Syntax error: " .. (err or "unknown"))
        return false
    end
    return true
end

local function test_luerl_compatibility()
    local name = "LUERL compatibility"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    -- Check for LUERL-specific patterns
    local checks = {
        {pattern = "_G%.", desc = "_G namespace usage"},
        {pattern = "package%.loaded", desc = "package.loaded pattern"},
        {pattern = "pcall%(load_", desc = "Protected loading"},
    }
    
    local all_found = true
    local missing = {}
    
    for _, check in ipairs(checks) do
        if not content:find(check.pattern) then
            all_found = false
            table.insert(missing, check.desc)
        end
    end
    
    if all_found then
        add_result(name, "PASS", "All LUERL patterns found")
    else
        add_result(name, "WARN", "Missing patterns: " .. table.concat(missing, ", "))
    end
    return true
end

local function test_metadata()
    local name = "Build metadata"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    local has_header = content:find("HyperAOS Concatenated Module")
    local has_date = content:find("Generated: %d%d%d%d%-%d%d%-%d%d")
    local has_modules = content:find("Modules: ")
    
    if has_header and has_date and has_modules then
        add_result(name, "PASS", "Complete build metadata present")
    elseif has_header then
        add_result(name, "WARN", "Partial metadata - missing date or module list")
    else
        add_result(name, "FAIL", "Missing build metadata")
        return false
    end
    return true
end

local function test_runtime_execution()
    local name = "Runtime execution test"
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    -- Create a sandboxed environment
    local env = {
        _G = {},
        print = function() end,  -- Silent print
        io = { open = io.open, close = io.close },
        os = { date = os.date },
        pairs = pairs,
        ipairs = ipairs,
        pcall = pcall,
        load = load,
        tostring = tostring,
        type = type,
        setmetatable = setmetatable,
        getmetatable = getmetatable
    }
    env._G = env
    
    local func, err = load(content, "aos-bundle", "t", env)
    if not func then
        add_result(name, "FAIL", "Cannot load module: " .. (err or "unknown"))
        return false
    end
    
    local success, result = pcall(func)
    if success then
        add_result(name, "PASS", "Module executes without errors")
    else
        add_result(name, "FAIL", "Runtime error: " .. tostring(result))
        return false
    end
    return true
end

-- Run all tests
print("🧪 HyperAOS Build System Test Suite")
print("========================================")
print("")

local tests = {
    test_file_exists,
    test_file_size,
    test_module_registration,
    test_required_modules,
    test_module_order,
    test_lua_syntax,
    test_luerl_compatibility,
    test_metadata,
    test_runtime_execution
}

-- Execute tests
for i, test in ipairs(tests) do
    local success = test()
    if not success and test == test_file_exists then
        -- Stop if the file doesn't exist
        break
    end
end

-- Print results
print("")
print("📊 Test Results")
print("========================================")

for _, test in ipairs(test_results.tests) do
    local icon = test.status == "PASS" and "✅" or 
                 test.status == "FAIL" and "❌" or "⚠️"
    print(string.format("%s %s: %s", icon, test.name, test.message))
end

print("")
print("========================================")
print(string.format("📈 Summary: %d passed, %d failed, %d warnings",
    test_results.passed, test_results.failed, test_results.warnings))

if test_results.failed > 0 then
    print("❌ TESTS FAILED")
    os.exit(1)
else
    print("✅ ALL TESTS PASSED")
    if test_results.warnings > 0 then
        print("⚠️  Some warnings were detected - review above")
    end
end
print("========================================")