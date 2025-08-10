-- Build script for hyper-aos
-- Concatenates multiple Lua source files into a single distributable file

local version = "0.1.0" -- Update version as needed

-- Ordered list of source files to concatenate (aos.lua will be added last automatically)
local source_files = {
  -- Add your source files here in the order they should be loaded
  "src/utils.lua",
  "src/handlers-utils.lua",
  "src/handlers.lua",
  "src/bint_luerl.lua",  -- Will be loaded as .bint
  "src/json.lua",  -- Will be loaded as json (not .json)
  -- "src/eval.lua",
  -- aos.lua is automatically loaded last with special handling
}

-- Template for wrapping each module
local function wrap_module(source, filename)
  -- Extract just the filename without path and extension for module name
  local module_name = filename:match("([^/]+)%.lua$") or filename
  
  -- Special handling for bint_luerl - load it as .bint
  if module_name == "bint_luerl" then
    return string.format([[
do
  local module = function()
%s
  end
  -- Load bint_luerl as .bint module (returns the constructor function)
  _G.package.loaded['.bint'] = module()
end
]], source)
  -- Special handling for json - load it as 'json' (not '.json')
  elseif module_name == "json" then
    return string.format([[
do
  local module = function()
%s
  end
  -- Load json module without the dot prefix
  _G.package.loaded['json'] = module()
end
]], source)
  else
    return string.format([[
do
  local module = function()
%s
  end
  _G.package.loaded['.%s'] = module()
end
]], source, module_name)
  end
end

-- Read file contents
local function read_file(filepath)
  local file = io.open(filepath, "r")
  if not file then
    error("Could not open file: " .. filepath)
  end
  local content = file:read("*all")
  file:close()
  return content
end

-- Write output file
local function write_file(filepath, content)
  -- Create dist directory if it doesn't exist
  os.execute("mkdir -p dist")
  
  local file = io.open(filepath, "w")
  if not file then
    error("Could not write file: " .. filepath)
  end
  file:write(content)
  file:close()
end

-- Main build process
local function build()
  print("Building hyper-aos v" .. version .. "...")
  
  -- Start with ASCII art header
  local output = "--[[\n" ..
    "    ██╗  ██╗██╗   ██╗██████╗ ███████╗██████╗      █████╗  ██████╗ ███████╗\n" ..
    "    ██║  ██║╚██╗ ██╔╝██╔══██╗██╔════╝██╔══██╗    ██╔══██╗██╔═══██╗██╔════╝\n" ..
    "    ███████║ ╚████╔╝ ██████╔╝█████╗  ██████╔╝    ███████║██║   ██║███████╗\n" ..
    "    ██╔══██║  ╚██╔╝  ██╔═══╝ ██╔══╝  ██╔══██╗    ██╔══██║██║   ██║╚════██║\n" ..
    "    ██║  ██║   ██║   ██║     ███████╗██║  ██║    ██║  ██║╚██████╔╝███████║\n" ..
    "    ╚═╝  ╚═╝   ╚═╝   ╚═╝     ╚══════╝╚═╝  ╚═╝    ╚═╝  ╚═╝ ╚═════╝ ╚══════╝\n" ..
    "    \n" ..
    "    Hyper-AOS v" .. version .. "\n" ..
    "    Built: " .. os.date("%Y-%m-%d %H:%M:%S") .. "\n" ..
    "--]]\n\n"
  
  local module_count = 0
  
  -- Process each source file
  for i, filepath in ipairs(source_files) do
    print("  Processing: " .. filepath)
    
    local source = read_file(filepath)
    local wrapped = wrap_module(source, filepath)
    
    output = output .. wrapped
    
    -- Add separator comment between modules
    output = output .. "\n-- next file\n\n"
    
    module_count = module_count + 1
  end
  
  -- Check if aos.lua exists and add it last with special handling
  local aos_file = io.open("aos.lua", "r")
  if aos_file then
    print("  Processing: aos.lua (main module)")
    local aos_source = aos_file:read("*all")
    aos_file:close()
    
    -- Wrap aos.lua with simple do...end block
    output = output .. "do\n" .. aos_source .. "\nend\n"
    module_count = module_count + 1
  end
  
  -- Output filename with version
  local output_file = string.format("dist/hyper-aos-%s.lua", version)
  
  -- Write the concatenated result
  write_file(output_file, output)
  
  print(string.format("✓ Built %d modules -> %s", module_count, output_file))
  print(string.format("  Output size: %d bytes", #output))
end

-- Run the build
local status, err = pcall(build)
if not status then
  print("Build failed: " .. err)
  os.exit(1)
end
