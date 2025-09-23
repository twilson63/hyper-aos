-- Test Script for HyperAOS Build System
-- Validates the concatenated Lua module for LUERL compatibility
--
-- Usage:
--   lua scripts/test.lua                    # Standard test run
--   VERBOSE=1 lua scripts/test.lua          # Verbose logging enabled
--   DIAGNOSTIC=1 lua scripts/test.lua       # Full diagnostic mode with tracking
--   VERBOSE=1 DIAGNOSTIC=1 lua scripts/test.lua  # Both modes enabled
--
-- Diagnostic mode provides:
--   - Global access tracking and reporting
--   - Standard library usage analysis  
--   - Dependency issue detection
--   - Environment completeness validation
--   - Enhanced error context reporting

-- Configuration
local VERBOSE_MODE = os.getenv("VERBOSE") == "1" or false
local DIAGNOSTIC_MODE = os.getenv("DIAGNOSTIC") == "1" or false

local test_results = {
    passed = 0,
    failed = 0,
    warnings = 0,
    tests = {}
}

-- Utility functions for enhanced diagnostics
local function log_verbose(message)
    if VERBOSE_MODE then
        print("🔍 VERBOSE: " .. message)
    end
end

local function log_diagnostic(message)
    if DIAGNOSTIC_MODE then
        print("🔧 DIAGNOSTIC: " .. message)
    end
end

local function format_table(tbl, max_depth)
    max_depth = max_depth or 2
    if max_depth <= 0 then return "{...}" end
    
    local result = "{"
    local first = true
    for k, v in pairs(tbl) do
        if not first then result = result .. ", " end
        first = false
        
        local key_str = type(k) == "string" and k or tostring(k)
        local val_str
        if type(v) == "table" then
            val_str = format_table(v, max_depth - 1)
        else
            val_str = tostring(v)
            if #val_str > 50 then
                val_str = val_str:sub(1, 47) .. "..."
            end
        end
        result = result .. key_str .. "=" .. val_str
    end
    return result .. "}"
end

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

-- Enhanced environment validation
local function validate_runtime_environment()
    log_verbose("Validating runtime environment...")
    
    local required_globals = {
        "_G", "print", "table", "string", "math", "pairs", "ipairs", 
        "type", "tostring", "setmetatable", "getmetatable", "pcall", "load"
    }
    
    local missing = {}
    for _, global in ipairs(required_globals) do
        if _G[global] == nil then
            table.insert(missing, global)
        end
    end
    
    if #missing > 0 then
        log_diagnostic("Missing required globals: " .. table.concat(missing, ", "))
        return false, "Missing required globals: " .. table.concat(missing, ", ")
    end
    
    log_verbose("Runtime environment validation passed")
    return true, nil
end

local function test_runtime_execution()
    local name = "Runtime execution test"
    
    log_verbose("Starting enhanced runtime execution test...")
    
    -- Environment validation
    local env_valid, env_error = validate_runtime_environment()
    if not env_valid then
        add_result(name, "FAIL", "Environment validation failed: " .. env_error)
        return false
    end
    
    local content, err = read_file("dist/aos.lua")
    if not content then
        add_result(name, "FAIL", err)
        return false
    end
    
    log_verbose("Module content loaded, size: " .. #content .. " bytes")
    
    -- Global access tracker for diagnostics
    local accessed_globals = {}
    local standard_libraries_used = {}
    local dependency_errors = {}
    
    -- Create enhanced sandboxed environment with tracking
    local function track_global_access(t, k)
        accessed_globals[k] = (accessed_globals[k] or 0) + 1
        
        -- Track standard library usage
        local std_libs = {
            table = "table", string = "string", math = "math", 
            io = "io", os = "os", coroutine = "coroutine"
        }
        
        if std_libs[k] then
            standard_libraries_used[k] = true
            log_diagnostic("Standard library accessed: " .. k)
        end
        
        -- Return the actual global or nil
        return rawget(t, k)
    end
    
    local env = {
        _G = {},
        
        -- Enhanced print function for diagnostic mode
        print = DIAGNOSTIC_MODE and function(...)
            local args = {...}
            local output = ""
            for i, v in ipairs(args) do
                output = output .. tostring(v)
                if i < #args then output = output .. "\t" end
            end
            log_diagnostic("PRINT: " .. output)
        end or function() end,  -- Silent print in normal mode
        
        -- Safe I/O and OS operations with tracking
        io = { 
            open = function(...) 
                log_diagnostic("IO operation: io.open called")
                return io.open(...) 
            end,
            close = io.close 
        },
        os = { 
            date = function(...)
                log_diagnostic("OS operation: os.date called")
                return os.date(...)
            end
        },
        
        -- Essential standard libraries
        table = table,           -- Required for table operations
        string = string,         -- Required for string manipulation  
        math = math,            -- May be required for calculations
        
        -- Core language functions
        pairs = pairs,
        ipairs = ipairs,
        next = next,            -- Add for table iteration
        select = select,        -- Add for varargs handling
        unpack = unpack or table.unpack,  -- Lua 5.1/5.2 compatibility
        
        -- Type checking and conversion
        type = type,
        tostring = tostring,
        tonumber = tonumber,    -- Add for conversions
        
        -- Metatables and raw access
        setmetatable = setmetatable,
        getmetatable = getmetatable,
        rawget = rawget,        -- Add for raw table access
        rawset = rawset,        -- Add for raw table access
        rawequal = rawequal,    -- Add for raw comparison
        rawlen = rawlen,        -- Add for Lua 5.2+ compatibility
        
        -- Error handling with enhanced reporting
        pcall = pcall,
        xpcall = xpcall,        -- Add for better error handling
        error = function(msg, level)
            log_diagnostic("ERROR called: " .. tostring(msg))
            return error(msg, level)
        end,
        assert = function(v, msg)
            if not v and DIAGNOSTIC_MODE then
                log_diagnostic("ASSERT failed: " .. tostring(msg or "assertion failed"))
            end
            return assert(v, msg)
        end,
        
        -- Loading and compilation
        load = load,
        loadstring = loadstring or load,  -- Lua 5.1 compatibility
        
        -- Enhanced module system with dependency tracking
        require = function(modname) 
            log_diagnostic("REQUIRE called for: " .. tostring(modname))
            table.insert(dependency_errors, "require('" .. tostring(modname) .. "') called but stubbed")
            return nil
        end,
        package = { 
            loaded = {},
            path = "",
            cpath = ""
        },
        
        -- Coroutines (if needed)
        coroutine = coroutine
    }
    
    -- Set up global access tracking if in diagnostic mode
    if DIAGNOSTIC_MODE then
        local env_mt = {
            __index = track_global_access,
            __newindex = function(t, k, v)
                log_diagnostic("Global assignment: " .. tostring(k) .. " = " .. tostring(v):sub(1, 50))
                rawset(t, k, v)
            end
        }
        setmetatable(env, env_mt)
    end
    
    env._G = env  -- Self-reference
    
    log_verbose("Environment prepared with " .. 
        (DIAGNOSTIC_MODE and "diagnostic tracking enabled" or "standard sandbox"))
    
    -- Load the module
    local func, load_err = load(content, "aos-bundle", "t", env)
    if not func then
        local error_msg = "Cannot load module: " .. (load_err or "unknown")
        log_diagnostic("Load error details: " .. error_msg)
        add_result(name, "FAIL", error_msg)
        return false
    end
    
    log_verbose("Module loaded successfully, executing...")
    
    -- Execute with enhanced error reporting
    local success, result = pcall(func)
    
    -- Generate diagnostic report
    if DIAGNOSTIC_MODE then
        print("\n🔧 DIAGNOSTIC REPORT")
        print("==================")
        
        -- Global access report
        if next(accessed_globals) then
            print("Globals accessed:")
            for global, count in pairs(accessed_globals) do
                print("  - " .. global .. " (accessed " .. count .. " times)")
            end
        else
            print("No globals accessed (may indicate sandboxing issues)")
        end
        
        -- Standard library usage
        if next(standard_libraries_used) then
            print("\nStandard libraries used:")
            for lib in pairs(standard_libraries_used) do
                print("  - " .. lib)
            end
        else
            print("\nNo standard libraries explicitly accessed")
        end
        
        -- Dependency issues
        if #dependency_errors > 0 then
            print("\nDependency issues detected:")
            for _, err in ipairs(dependency_errors) do
                print("  - " .. err)
            end
        else
            print("\nNo dependency issues detected")
        end
        
        -- Environment completeness check
        local env_keys = {}
        for k in pairs(env) do
            if k ~= "_G" then  -- Exclude self-reference
                table.insert(env_keys, k)
            end
        end
        print("\nEnvironment provided " .. #env_keys .. " globals:")
        table.sort(env_keys)
        local line = "  "
        for i, k in ipairs(env_keys) do
            if #line + #k > 70 then
                print(line)
                line = "  " .. k
            else
                line = line .. k .. " "
            end
        end
        if #line > 2 then print(line) end
        
        print("==================\n")
    end
    
    if success then
        local message = "Module executes without errors"
        if VERBOSE_MODE then
            message = message .. " (verbose diagnostics available)"
        end
        if #dependency_errors > 0 then
            message = message .. " - Warning: " .. #dependency_errors .. " dependency stubs used"
        end
        add_result(name, "PASS", message)
        log_verbose("Runtime execution completed successfully")
    else
        local error_context = tostring(result)
        
        -- Enhanced error reporting
        if DIAGNOSTIC_MODE then
            error_context = error_context .. "\n  Context: " .. #accessed_globals .. 
                          " globals accessed, " .. #env_keys .. " globals provided"
            if #dependency_errors > 0 then
                error_context = error_context .. "\n  Dependencies: " .. #dependency_errors .. " missing"
            end
        end
        
        add_result(name, "FAIL", "Runtime error: " .. error_context)
        log_diagnostic("Runtime execution failed with error: " .. tostring(result))
        return false
    end
    return true
end

-- Run all tests
print("🧪 HyperAOS Build System Test Suite")
print("========================================")

-- Show active modes
local modes = {}
if VERBOSE_MODE then table.insert(modes, "VERBOSE") end
if DIAGNOSTIC_MODE then table.insert(modes, "DIAGNOSTIC") end
if #modes > 0 then
    print("Active modes: " .. table.concat(modes, ", "))
else
    print("Standard mode (set VERBOSE=1 or DIAGNOSTIC=1 for enhanced output)")
end
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