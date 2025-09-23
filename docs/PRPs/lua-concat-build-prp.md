# Project Request Protocol: Lua Script Concatenation Build System

## Project Overview

### Project Name
Lua Script Concatenation Build System for HyperBEAM/LUERL

### Purpose
Create a build system that concatenates multiple Lua scripts into a single distributable file for loading into LUERL and running on HyperBEAM. The system will use the Hype framework to build and deploy the aggregated Lua module to Arweave.

### Problem Statement
The current HyperAOS system requires multiple Lua modules (aos.lua, utils.lua, etc.) to be bundled into a single file for deployment. Manual concatenation is error-prone and doesn't properly handle module loading and dependencies.

### Solution Overview
Implement an automated build system using the Hype framework that:
- Concatenates multiple Lua files into a single distributable
- Properly registers modules in `_G.package.loaded`
- Provides automated deployment to Arweave
- Integrates with existing Makefile build process

## Technical Requirements

### Core Requirements

1. **Module Concatenation**
   - Combine multiple Lua files into single `dist/aos.lua`
   - Preserve module structure and dependencies
   - Register each module in `_G.package.loaded["modulename"]`
   - Handle nested directory structures

2. **Build System Integration**
   - Use Hype framework for build scripts
   - Makefile targets for build and deploy
   - Auto-install Hype if not present
   - Clean build artifacts

3. **Module Registration Format**
   ```lua
   _G.package.loaded["utils"] = (function()
     -- utils.lua content
   end)()
   
   _G.package.loaded["aos"] = (function()
     -- aos.lua content with require("utils") working
   end)()
   ```

4. **Deployment System**
   - Use ANS-104 plugin for Hype
   - Deploy to Arweave mainnet
   - Set appropriate content-type and tags
   - Return transaction ID for reference

### Dependencies

1. **Hype Framework**
   - Repository: https://github.com/twilson63/hype
   - Version: Latest stable
   - Installation via npm or direct download

2. **ANS-104 Plugin**
   - Respository: https://github.com/twilson63/hype-plugin-ans104 
   - For Arweave deployment
   - Bundling support

3. **Existing Files**
   - `aos.lua` - Main AOS module
   - `utils.lua` - Utility functions
   - Any additional Lua modules in project

### File Structure
```
hyper-aos-demo/
├── Makefile
├── scripts/
│   ├── build.lua       # Concatenation script
│   └── deploy.lua      # Arweave deployment
├── src/
│   ├── aos.lua
│   └── utils.lua
├── dist/
│   └── aos.lua         # Generated aggregate file
└── .hype/
    └── config.json     # Hype configuration
```

## Implementation Steps

### Phase 1: Setup Infrastructure

1. **Create Makefile targets**
   ```makefile
   # Check and install Hype
   install-hype:
       @command -v hype >/dev/null 2>&1 || curl -sSL https://raw.githubusercontent.com/twilson63/hype/main/install-mac.sh | bash 
   
   # Build concatenated file
   build: install-hype
       hype run scripts/build.lua
   
   # Deploy to Arweave
   deploy: build
       hype run scripts/deploy.lua
   
   # Clean build artifacts
   clean:
       rm -rf dist/
   ```

2. **Initialize Hype configuration**
   - Create `.hype/config.json`
   - Configure ANS-104 plugin
   - Set up wallet configuration - you can use hbwallet https://github.com/twilson63/hbwallet

### Phase 2: Build Script Implementation

1. **Create `scripts/build.lua`**
   ```lua
   -- Module loading order configuration
   local modules = {
       "utils",  -- Load first (no dependencies)
       "aos"     -- Load second (depends on utils)
   }
   
   -- Read and wrap each module
   -- Generate _G.package.loaded entries
   -- Write to dist/aos.lua
   ```

2. **Module Wrapping Logic**
   - Read source file
   - Wrap in IIFE with package.loaded assignment
   - Handle require() statements
   - Preserve original functionality

3. **Output Generation**
   - Create dist/ directory if not exists
   - Write concatenated content
   - Add header comments with build metadata

### Phase 3: Deploy Script Implementation

1. **Create `scripts/deploy.lua`**
   - Load dist/aos.lua
   - Configure ANS-104 transaction
   - Set tags: `Data-Protocol: ao`, `Content-Type: application/lua`
   - Submit to Arweave
   - Output transaction ID

2. **Error Handling**
   - Validate wallet exists
   - Check network connectivity
   - Verify transaction success
   - Provide rollback instructions

### Phase 4: Testing & Validation

1. **Build Validation**
   - Verify concatenated file syntax
   - Test in LUERL environment
   - Validate module loading
   - Check require() functionality

2. **Deployment Testing**
   - Test deployment to testnet first
   - Verify transaction retrieval
   - Validate module execution on HyperBEAM

### Phase 5: Documentation & Integration

1. **Update README**
   - Document build process
   - Add deployment instructions
   - Include troubleshooting guide

2. **CI/CD Integration**
   - Add GitHub Actions workflow
   - Automate builds on merge
   - Tag releases with transaction IDs

## Success Criteria

### Functional Requirements
- [ ] Successfully concatenates multiple Lua files into single distributable
- [ ] Modules properly registered in `_G.package.loaded`
- [ ] `require()` statements work correctly in concatenated file
- [ ] Makefile targets execute without errors
- [ ] Hype auto-installs if not present

### Build System
- [ ] `make build` creates `dist/aos.lua`
- [ ] Generated file maintains module boundaries
- [ ] Build process is idempotent
- [ ] Clean target removes all artifacts

### Deployment
- [ ] `make deploy` publishes to Arweave
- [ ] Transaction ID returned and logged
- [ ] Proper tags applied to transaction
- [ ] Module retrievable and executable

### Testing
- [ ] Concatenated file runs in LUERL
- [ ] All existing tests pass with bundled module
- [ ] Module loading order preserved
- [ ] No runtime errors from concatenation

### Documentation
- [ ] Build process documented in README
- [ ] Deployment instructions clear
- [ ] Troubleshooting guide complete
- [ ] Example usage provided

## Risk Mitigation

### Technical Risks
1. **Module Order Dependencies**
   - Solution: Explicit module order configuration
   - Fallback: Manual ordering specification

2. **LUERL Compatibility**
   - Solution: Test in LUERL before deployment
   - Fallback: Adjust wrapping format as needed

3. **Large File Size**
   - Solution: Minification option
   - Fallback: Module splitting strategy

### Operational Risks
1. **Hype Framework Changes**
   - Solution: Pin to specific version
   - Fallback: Vendor dependencies

2. **Arweave Network Issues**
   - Solution: Retry logic with backoff
   - Fallback: Manual deployment option

## Timeline Estimate

- Phase 1 (Setup): 2 hours
- Phase 2 (Build Script): 4 hours  
- Phase 3 (Deploy Script): 3 hours
- Phase 4 (Testing): 2 hours
- Phase 5 (Documentation): 1 hour

**Total Estimate: 12 hours**

## Additional Considerations

### Future Enhancements
- Module dependency graph resolution
- Automatic minification
- Source map generation
- Watch mode for development
- Module hot-reloading support

### Integration Points
- Existing test suite compatibility
- CI/CD pipeline integration
- Version tagging system
- Rollback mechanism

### Performance Metrics
- Build time < 5 seconds
- Deployment time < 30 seconds
- Zero runtime overhead from concatenation
- Maintains original execution speed
