-- Arweave Deployment Script for HyperAOS
-- Deploys concatenated Lua module to Arweave using ANS-104

local function read_file(path)
    local file = io.open(path, "r")
    if not file then
        error("❌ Cannot open file: " .. path)
    end
    local content = file:read("*all")
    file:close()
    return content
end

local function file_exists(path)
    local file = io.open(path, "r")
    if file then
        file:close()
        return true
    end
    return false
end

local function find_wallet()
    -- Check environment variable first
    local wallet_path = os.getenv("WALLET_PATH")
    if wallet_path and file_exists(wallet_path) then
        return wallet_path, "environment"
    end
    
    -- Check common wallet locations
    local wallet_locations = {
        "wallet.json",
        "demo.json",
        ".secrets/wallet.json",
        "../wallet.json",
        os.getenv("HOME") .. "/.aos/wallet.json"
    }
    
    for _, path in ipairs(wallet_locations) do
        if file_exists(path) then
            return path, "found"
        end
    end
    
    return nil, "not_found"
end

local function command_exists(cmd)
    local handle = io.popen("command -v " .. cmd .. " 2>/dev/null")
    local result = handle:read("*a")
    handle:close()
    return result ~= ""
end

print("🚀 Arweave Deployment Script")
print("========================================")

-- Check for built file
local built_file = "dist/aos.lua"
if not file_exists(built_file) then
    print("❌ Error: No built file found at " .. built_file)
    print("   Run 'make build' first to create the bundle")
    os.exit(1)
end

-- Find wallet
local wallet_path, wallet_status = find_wallet()
if not wallet_path then
    print("❌ Error: No wallet found")
    print("   Please provide a wallet in one of these ways:")
    print("   1. Set WALLET_PATH environment variable")
    print("   2. Create wallet.json in current directory")
    print("   3. Create demo.json in current directory")
    print("   4. Place wallet in ~/.aos/wallet.json")
    os.exit(1)
end

print("✅ Wallet found: " .. wallet_path)
if wallet_status == "environment" then
    print("   (from WALLET_PATH environment variable)")
end

-- Read and analyze the built module
local content = read_file(built_file)
local size = #content
local lines = 0
for _ in content:gmatch("\n") do
    lines = lines + 1
end

print("📦 Module details:")
print("   • File: " .. built_file)
print("   • Size: " .. string.format("%.2f KB", size / 1024))
print("   • Lines: " .. lines)

-- Extract module list from header
local modules = content:match("-- Modules: ([^\n]+)")
if modules then
    print("   • Modules: " .. modules)
end

-- Determine deployment tool
local deploy_tool = nil
local deploy_cmd = nil

if command_exists("arx") then
    deploy_tool = "arx"
    deploy_cmd = string.format(
        'arx upload %s -w %s -t arweave ' ..
        '--content-type application/lua ' ..
        '--tags "Data-Protocol" "ao" ' ..
        '--tags "Module-Format" "concatenated" ' ..
        '--tags "App-Name" "HyperAOS" ' ..
        '--tags "Module-Type" "LUERL" ' ..
        '--tags "Build-Date" "%s"',
        built_file,
        wallet_path,
        os.date("%Y-%m-%d")
    )
elseif command_exists("arkb") then
    deploy_tool = "arkb"
    deploy_cmd = string.format(
        'arkb deploy %s ' ..
        '--wallet %s ' ..
        '--tag "Data-Protocol:ao" ' ..
        '--tag "Content-Type:application/lua" ' ..
        '--tag "Module-Format:concatenated" ' ..
        '--tag "App-Name:HyperAOS" ' ..
        '--tag "Module-Type:LUERL"',
        built_file,
        wallet_path
    )
elseif command_exists("hype") then
    deploy_tool = "hype"
    -- For Hype, we need to use the ANS-104 plugin
    print("⚠️  Using Hype framework (requires ANS-104 plugin)")
    deploy_cmd = string.format(
        'hype upload %s --wallet %s',
        built_file,
        wallet_path
    )
else
    print("❌ Error: No deployment tool found")
    print("   Please install one of the following:")
    print("   • arx: npm install -g @permaweb/arx")
    print("   • arkb: npm install -g arkb")
    print("   • hype: curl -sSL https://raw.githubusercontent.com/twilson63/hype/main/install.sh | bash")
    os.exit(1)
end

print("")
print("🔧 Deployment tool: " .. deploy_tool)
print("📝 Deployment command:")
print("   " .. deploy_cmd)
print("")

-- Confirm deployment
print("⚠️  Ready to deploy to Arweave mainnet")
print("   This will consume AR tokens from your wallet")
print("")
io.write("❓ Proceed with deployment? (y/N): ")
io.flush()
local response = io.read()

if response ~= "y" and response ~= "Y" then
    print("❌ Deployment cancelled")
    os.exit(0)
end

print("")
print("🚀 Deploying to Arweave...")
print("========================================")

-- Execute deployment
local start_time = os.time()
local result = os.execute(deploy_cmd)
local end_time = os.time()
local duration = end_time - start_time

print("")
if result == 0 or result == true then
    print("========================================")
    print("✅ DEPLOYMENT SUCCESSFUL!")
    print("========================================")
    print("⏱️  Duration: " .. duration .. " seconds")
    print("")
    print("📍 Transaction Details:")
    print("   • Status: Pending confirmation")
    print("   • Network: Arweave mainnet")
    print("   • Module type: LUERL/HyperBEAM")
    print("")
    print("🔍 View transaction:")
    print("   • ViewBlock: https://viewblock.io/arweave")
    print("   • ArweaveApp: https://arweave.app")
    print("")
    print("📝 Next steps:")
    print("   1. Wait for transaction confirmation (~2-3 minutes)")
    print("   2. Note the transaction ID when confirmed")
    print("   3. Launch AOS with: aos console --module <TX_ID>")
    print("========================================")
    
    -- Try to save deployment info
    local deploy_info = string.format(
        "Deployment: %s\nWallet: %s\nSize: %.2f KB\nModules: %s\nTool: %s\n",
        os.date("%Y-%m-%d %H:%M:%S"),
        wallet_path,
        size / 1024,
        modules or "unknown",
        deploy_tool
    )
    
    local info_file = io.open("dist/last-deployment.txt", "w")
    if info_file then
        info_file:write(deploy_info)
        info_file:close()
        print("📄 Deployment info saved to: dist/last-deployment.txt")
    end
else
    print("========================================")
    print("❌ DEPLOYMENT FAILED")
    print("========================================")
    print("   Error code: " .. tostring(result))
    print("")
    print("🔍 Troubleshooting:")
    print("   1. Check your wallet has sufficient AR tokens")
    print("   2. Verify network connectivity")
    print("   3. Ensure wallet file is valid JSON")
    print("   4. Check deployment tool is properly installed")
    print("")
    print("💡 Try running the command manually:")
    print("   " .. deploy_cmd)
    print("========================================")
    os.exit(1)
end