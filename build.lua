#!/usr/bin/env lua

--- Comprehensive Lua Module Concatenation Build Script for Hyper-AOS
-- 
-- This production-ready build script concatenates multiple Lua modules into a single 
-- deployable file optimized for the LUERL VM environment in Hyper-AOS.
-- 
-- Key Features:
-- • Module dependency management with proper ordering (utils → aos)
-- • Optional module detection and inclusion (crypto, json, base64)
-- • LUERL-compatible module wrapping with _G.package.loaded registration
-- • Comprehensive error handling and validation at each step
-- • Build metadata with timestamps and module inventories
-- • Require statement processing for internal module references
-- • Production-grade file I/O with directory creation
-- • Progress reporting with emojis and detailed status updates
-- 
-- Usage:
--   lua build.lua                    # Build with default configuration
--   local build = require('build')   # Use as module
--   build.run()                      # Programmatic execution
--
-- @author Hyper-AOS Team  
-- @version 1.0.0
-- @license MIT

local build = {}

--- Configuration
local config = {
  src_dir = "src",
  output_file = "dist/aos-bundle.lua",
  required_modules = {"utils", "aos"},
  optional_modules = {"crypto", "json", "base64"},
  module_order = {"utils", "aos"}, -- Dependency order
}

--- Utility Functions

--- Check if a file exists and is readable
-- @param path string File path to check
-- @return boolean True if file exists and is readable
local function file_exists(path)
  local file = io.open(path, "r")
  if file then
    file:close()
    return true
  end
  return false
end

--- Read entire file content
-- @param path string File path to read
-- @return string|nil File content or nil if error
-- @return string|nil Error message if failed
local function read_file(path)
  local file, err = io.open(path, "r")
  if not file then
    return nil, "Failed to open file: " .. (err or "unknown error")
  end
  
  local content = file:read("*all")
  file:close()
  
  if not content then
    return nil, "Failed to read file content"
  end
  
  return content
end

--- Write content to file, creating directories if needed
-- @param path string File path to write
-- @param content string Content to write
-- @return boolean True if successful
-- @return string|nil Error message if failed
local function write_file(path, content)
  -- Create directory structure if needed
  local dir = path:match("(.+)/[^/]+$")
  if dir then
    os.execute("mkdir -p " .. dir)
  end
  
  local file, err = io.open(path, "w")
  if not file then
    return false, "Failed to create output file: " .. (err or "unknown error")
  end
  
  local success, write_err = file:write(content)
  file:close()
  
  if not success then
    return false, "Failed to write file content: " .. (write_err or "unknown error")
  end
  
  return true
end

--- Get current timestamp in ISO format
-- @return string Current timestamp
local function get_timestamp()
  return os.date("%Y-%m-%dT%H:%M:%S")
end

--- Validate module content for basic syntax
-- @param content string Module content to validate
-- @param module_name string Name of the module for error reporting
-- @return boolean True if valid
-- @return string|nil Error message if invalid
local function validate_module_content(content, module_name)
  if not content or content == "" then
    return false, "Module content is empty"
  end
  
  -- Check for basic Lua syntax by attempting to load as string
  local chunk, err = load(content, module_name)
  if not chunk then
    return false, "Syntax error in module: " .. (err or "unknown error")
  end
  
  return true
end

--- Module Processing Functions

--- Wrap module content in IIFE pattern for _G.package.loaded
-- @param module_name string Name of the module
-- @param content string Module content
-- @return string Wrapped module content
local function wrap_module_iife(module_name, content)
  local template = [[
-- Module: %s
-- Wrapped for LUERL compatibility
_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- Load module in protected environment
do
  local _ENV = _G
%s
end

-- Store module result
%s
]]
  
  -- Generate the appropriate storage logic
  local storage_logic
  if module_name == "utils" then
    storage_logic = string.format("_G.package.loaded[\"%s\"] = _G.utils", module_name)
  else
    storage_logic = string.format("_G.package.loaded[\"%s\"] = _G.package.loaded[\"%s\"] or true", module_name, module_name)
  end
  
  return string.format(template, module_name, content, storage_logic)
end

--- Process require statements in module content
-- @param content string Module content
-- @param available_modules table List of available module names
-- @return string Processed content with proper require handling
local function process_require_statements(content, available_modules)
  -- Convert available_modules array to lookup table
  local module_lookup = {}
  for _, name in ipairs(available_modules) do
    module_lookup[name] = true
  end
  
  -- Split content into lines for line-by-line processing
  local lines = {}
  for line in content:gmatch("[^\n]*") do
    table.insert(lines, line)
  end
  
  -- Process each line, ignoring comments
  for i, line in ipairs(lines) do
    -- Skip commented lines (simple comment detection)
    local trimmed = line:match("^%s*(.-)%s*$")
    if not trimmed:match("^%-%-") then
      -- Pattern to match require statements
      local function replace_require(match)
        local module_name = match:match([["([^"]+)"]])
        if not module_name then
          module_name = match:match([['([^']+)']])
        end
        
        if module_name then
          if module_lookup[module_name] then
            return string.format("(_G.package.loaded[\"%s\"] or error(\"Module '%s' not found\"))", 
                                module_name, module_name)
          elseif module_name == '.process' then
            -- Special case for .process module reference
            return "_G.package.loaded['.process']"
          end
        end
        
        -- Return original require for external modules
        return match
      end
      
      -- Replace require("module") and require('module') patterns in non-comment lines
      lines[i] = line:gsub('require%s*%(%s*["\'][^"\']+["\']%s*%)', replace_require)
    end
  end
  
  return table.concat(lines, "\n")
end

--- Build Functions

--- Load and validate a module
-- @param module_name string Name of the module
-- @param is_optional boolean Whether the module is optional
-- @return string|nil Module content or nil if not found/optional
-- @return string|nil Error message if required module failed
local function load_module(module_name, is_optional)
  local file_path = config.src_dir .. "/" .. module_name .. ".lua"
  
  if not file_exists(file_path) then
    if is_optional then
      print(string.format("  ⚠ Optional module '%s' not found, skipping", module_name))
      return nil
    else
      return nil, string.format("Required module '%s' not found at %s", module_name, file_path)
    end
  end
  
  local content, err = read_file(file_path)
  if not content then
    if is_optional then
      print(string.format("  ⚠ Optional module '%s' failed to load: %s", module_name, err))
      return nil
    else
      return nil, string.format("Failed to load required module '%s': %s", module_name, err)
    end
  end
  
  local valid, validation_err = validate_module_content(content, module_name)
  if not valid then
    if is_optional then
      print(string.format("  ⚠ Optional module '%s' validation failed: %s", module_name, validation_err))
      return nil
    else
      return nil, string.format("Module '%s' validation failed: %s", module_name, validation_err)
    end
  end
  
  print(string.format("  ✓ Loaded module '%s' (%d bytes)", module_name, #content))
  return content
end

--- Generate build metadata header
-- @param modules table List of included module names
-- @return string Build metadata header
local function generate_build_header(modules)
  local template = [=[
--[[ 
===============================================================================
AOS MODULE BUNDLE
Generated by Hyper-AOS Build Script v1.0.0
Build Timestamp: %s
Modules Included: %s
LUERL Compatible: Yes
===============================================================================

This file contains multiple Lua modules concatenated and wrapped for deployment
in the Hyper-AOS environment. Each module is wrapped in an IIFE pattern and
registered in _G.package.loaded for proper module resolution.

Modules are loaded in dependency order to ensure proper initialization.
--]]

]=]
  
  return string.format(template, get_timestamp(), table.concat(modules, ", "))
end

--- Generate module initialization code
-- @return string Initialization code
local function generate_initialization()
  return [[

-- Initialize package system for LUERL compatibility
_G.package = _G.package or {}
_G.package.loaded = _G.package.loaded or {}

-- Define global require function if not present
if not _G.require then
  _G.require = function(name)
    local module = _G.package.loaded[name]
    if module == nil then
      error("Module '" .. tostring(name) .. "' not found", 2)
    end
    return module
  end
end

]]
end

--- Generate module return statement
-- @return string Return statement for main module
local function generate_return_statement()
  return [[

-- Return the main aos module for direct usage
return _G.package.loaded["aos"] or _G.package.loaded[".process"]
]]
end

--- Main build function
-- @return boolean True if build successful
-- @return string|nil Error message if failed
function build.run()
  print("🔨 Starting AOS Module Build Process")
  print("=====================================")
  
  -- Check source directory exists
  if not file_exists(config.src_dir) then
    return false, string.format("Source directory '%s' not found", config.src_dir)
  end
  
  print(string.format("📂 Source directory: %s", config.src_dir))
  print(string.format("📝 Output file: %s", config.output_file))
  print()
  
  local loaded_modules = {}
  local module_contents = {}
  local all_modules = {}
  
  -- Build list of all possible modules in dependency order
  for _, name in ipairs(config.module_order) do
    table.insert(all_modules, name)
  end
  
  -- Add any optional modules not in dependency order
  for _, name in ipairs(config.optional_modules) do
    local found = false
    for _, ordered_name in ipairs(config.module_order) do
      if name == ordered_name then
        found = true
        break
      end
    end
    if not found then
      table.insert(all_modules, name)
    end
  end
  
  -- Load required modules in dependency order
  print("📦 Loading required modules...")
  for _, module_name in ipairs(config.required_modules) do
    local content, err = load_module(module_name, false)
    if not content then
      return false, err
    end
    
    module_contents[module_name] = content
    table.insert(loaded_modules, module_name)
  end
  
  -- Load optional modules
  print("\n📦 Loading optional modules...")
  for _, module_name in ipairs(config.optional_modules) do
    -- Skip if already loaded as required
    local already_loaded = false
    for _, loaded_name in ipairs(loaded_modules) do
      if loaded_name == module_name then
        already_loaded = true
        break
      end
    end
    
    if not already_loaded then
      local content = load_module(module_name, true)
      if content then
        module_contents[module_name] = content
        table.insert(loaded_modules, module_name)
      end
    end
  end
  
  if #loaded_modules == 0 then
    return false, "No modules were successfully loaded"
  end
  
  -- Process and concatenate modules
  print(string.format("\n🔧 Processing %d modules...", #loaded_modules))
  
  local bundle_parts = {}
  
  -- Add build header
  table.insert(bundle_parts, generate_build_header(loaded_modules))
  
  -- Add initialization code
  table.insert(bundle_parts, generate_initialization())
  
  -- Process modules in dependency order
  for _, module_name in ipairs(all_modules) do
    local content = module_contents[module_name]
    if content then
      print(string.format("  🔄 Processing module '%s'...", module_name))
      
      -- Process require statements
      content = process_require_statements(content, loaded_modules)
      
      -- Wrap in IIFE pattern
      local wrapped_content = wrap_module_iife(module_name, content)
      
      table.insert(bundle_parts, wrapped_content)
    end
  end
  
  -- Add return statement
  table.insert(bundle_parts, generate_return_statement())
  
  -- Concatenate all parts
  local final_bundle = table.concat(bundle_parts, "\n")
  
  -- Validate final bundle
  print("\n🧪 Validating final bundle...")
  local valid, validation_err = validate_module_content(final_bundle, "bundle")
  if not valid then
    return false, "Final bundle validation failed: " .. validation_err
  end
  
  -- Write output file
  print(string.format("💾 Writing output to '%s'...", config.output_file))
  local success, write_err = write_file(config.output_file, final_bundle)
  if not success then
    return false, write_err
  end
  
  -- Report success
  print("\n✅ Build completed successfully!")
  print(string.format("📊 Bundle size: %d bytes", #final_bundle))
  print(string.format("📋 Modules included: %s", table.concat(loaded_modules, ", ")))
  print(string.format("📍 Output: %s", config.output_file))
  
  return true
end

--- CLI Interface

-- Check if running as script
if arg and arg[0] and arg[0]:match("build%.lua$") then
  local success, err = build.run()
  if not success then
    io.stderr:write("❌ Build failed: " .. (err or "unknown error") .. "\n")
    os.exit(1)
  end
  os.exit(0)
end

-- Return module for require() usage
return build