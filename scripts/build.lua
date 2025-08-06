-- Lua Script Concatenation Build System
-- Concatenates multiple Lua modules into a single distributable file for LUERL/HyperBEAM

local function read_file(path)
    local file = io.open(path, "r")
    if not file then
        error("❌ Cannot open file: " .. path)
    end
    local content = file:read("*all")
    file:close()
    return content
end

local function write_file(path, content)
    local file = io.open(path, "w")
    if not file then
        error("❌ Cannot write to file: " .. path)
    end
    file:write(content)
    file:close()
end

local function file_exists(path)
    local file = io.open(path, "r")
    if file then
        file:close()
        return true
    end
    return false
end

local function validate_lua_syntax(content, module_name)
    local func, err = load(content, module_name)
    if not func then
        print("⚠️  Warning: Syntax validation failed for " .. module_name)
        print("   Error: " .. (err or "unknown"))
        return false
    end
    return true
end

-- Module loading order (dependencies first)
local modules = {
    {name = "utils", path = "src/utils.lua", optional = false},
    {name = "aos", path = "src/aos.lua", optional = false}
}

-- Additional optional modules to check for
local optional_modules = {
    {name = "crypto", path = "src/crypto.lua"},
    {name = "json", path = "src/json.lua"},
    {name = "base64", path = "src/base64.lua"}
}

-- Check for additional modules
for _, opt_module in ipairs(optional_modules) do
    if file_exists(opt_module.path) then
        table.insert(modules, #modules, {
            name = opt_module.name,
            path = opt_module.path,
            optional = true
        })
        print("📦 Found optional module: " .. opt_module.name)
    end
end

print("🔨 Building concatenated Lua module...")
print("📋 Module load order:")
for i, mod in ipairs(modules) do
    local status = mod.optional and " (optional)" or " (required)"
    print("   " .. i .. ". " .. mod.name .. status .. " - " .. mod.path)
end
print("")

-- Create dist directory
os.execute("mkdir -p dist")

-- Build concatenated file
local output = {}

-- Add header with metadata
table.insert(output, "-- HyperAOS Concatenated Module Bundle")
table.insert(output, "-- Generated: " .. os.date("%Y-%m-%d %H:%M:%S"))
table.insert(output, "-- Platform: LUERL/HyperBEAM")
table.insert(output, "-- Modules: " .. table.concat(
    (function()
        local names = {}
        for _, m in ipairs(modules) do
            if file_exists(m.path) then
                table.insert(names, m.name)
            end
        end
        return names
    end)(), ", "
))
table.insert(output, "")
table.insert(output, "-- Initialize package system for LUERL compatibility")
table.insert(output, "_G.package = _G.package or {}")
table.insert(output, "_G.package.loaded = _G.package.loaded or {}")
table.insert(output, "")

-- Process each module
local loaded_modules = {}
for _, module in ipairs(modules) do
    if not file_exists(module.path) then
        if module.optional then
            print("⏭️  Skipping optional module: " .. module.name)
        else
            error("❌ Required module not found: " .. module.path)
        end
    else
        print("📄 Processing: " .. module.name .. " (" .. module.path .. ")")
        local content = read_file(module.path)
        
        -- Validate syntax
        if validate_lua_syntax(content, module.name) then
            print("   ✅ Syntax validation passed")
        end
        
        -- Calculate module size
        local size = #content
        print("   📊 Size: " .. string.format("%.2f KB", size / 1024))
        
        -- Process require statements for LUERL
        if module.name == "aos" then
            -- Replace require calls with direct package.loaded access
            content = content:gsub('require%s*%([\"\']utils[\"\']%)', '_G.package.loaded["utils"]')
            content = content:gsub('require%s*%([\"\']%.process[\"\']%)', '_G.process')
            print("   🔄 Processed require() statements for LUERL")
        end
        
        -- Wrap module in package.loaded registration
        table.insert(output, "-- ============================================")
        table.insert(output, "-- Module: " .. module.name)
        table.insert(output, "-- Source: " .. module.path)
        table.insert(output, "-- ============================================")
        table.insert(output, "")
        
        -- Use protected block for module loading
        table.insert(output, "do")
        table.insert(output, "  local function load_" .. module.name .. "()")
        
        -- Add module content (but check if it already has a return statement)
        local lines = {}
        local has_return = content:match("return%s+_G%.utils") or content:match("return%s+_G%." .. module.name)
        
        for line in content:gmatch("[^\n]*") do
            table.insert(lines, "    " .. line)
        end
        table.insert(output, table.concat(lines, "\n"))
        
        -- Only add return if the module doesn't already have one
        if not has_return then
            table.insert(output, "")
            if module.name == "utils" then
                table.insert(output, "    -- Return utils module")
                table.insert(output, "    return _G.utils")
            elseif module.name == "aos" then
                table.insert(output, "    -- AOS module doesn't return a value, it sets up _G")
            else
                table.insert(output, "    -- Return module exports if any")
                table.insert(output, "    return _G." .. module.name .. " or {}")
            end
        end
        
        table.insert(output, "  end")
        table.insert(output, "")
        table.insert(output, "  -- Register module in package.loaded")
        table.insert(output, "  local status, result = pcall(load_" .. module.name .. ")")
        table.insert(output, "  if status then")
        table.insert(output, "    _G.package.loaded[\"" .. module.name .. "\"] = result")
        table.insert(output, "    print(\"✅ Loaded module: " .. module.name .. "\")")
        table.insert(output, "  else")
        table.insert(output, "    print(\"❌ Failed to load module " .. module.name .. ": \" .. tostring(result))")
        table.insert(output, "  end")
        table.insert(output, "end")
        table.insert(output, "")
        
        table.insert(loaded_modules, module.name)
        print("   ✅ Module processed successfully")
    end
end

-- Add footer
table.insert(output, "-- ============================================")
table.insert(output, "-- Bundle initialization complete")
table.insert(output, "-- ============================================")
table.insert(output, "")
table.insert(output, "-- Verify module loading")
table.insert(output, "local loaded_count = 0")
table.insert(output, "for name, _ in pairs(_G.package.loaded) do")
table.insert(output, "  loaded_count = loaded_count + 1")
table.insert(output, "end")
table.insert(output, "")
table.insert(output, "if loaded_count > 0 then")
table.insert(output, "  print(\"📦 Bundle loaded successfully with \" .. loaded_count .. \" modules\")")
table.insert(output, "else")
table.insert(output, "  print(\"⚠️  Warning: No modules were loaded\")")
table.insert(output, "end")
table.insert(output, "")
table.insert(output, "-- Return main AOS module for direct usage")
table.insert(output, "return _G.package.loaded[\"aos\"]")

-- Write output file
local final_content = table.concat(output, "\n")
local output_path = "dist/aos.lua"
write_file(output_path, final_content)

-- Report build summary
print("")
print("========================================")
print("✅ BUILD SUCCESSFUL!")
print("========================================")
print("📦 Output: " .. output_path)
print("📊 Size: " .. string.format("%.2f KB", #final_content / 1024))
print("📚 Modules bundled: " .. #loaded_modules)
print("   • " .. table.concat(loaded_modules, "\n   • "))
print("")
print("🚀 Next steps:")
print("   1. Test: make test")
print("   2. Deploy: make deploy")
print("========================================")