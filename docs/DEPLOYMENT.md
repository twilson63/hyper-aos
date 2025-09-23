# Hyper-AOS Deployment Guide

## Production Deployment Overview

This guide covers deploying Hyper-AOS to production environments, including publishing to Arweave, launching process instances, configuration management, and operational maintenance.

## Prerequisites and Requirements

### System Requirements

**Hardware Requirements:**
- Minimum: 2 CPU cores, 4GB RAM, 20GB storage
- Recommended: 4+ CPU cores, 8GB+ RAM, 100GB+ storage
- Network: Stable internet connection with low latency to Arweave nodes

**Software Dependencies:**
- Erlang/OTP 24+ (production builds recommend OTP 26+)
- Node.js 18+ (for ARX toolkit)
- Git (for source management)
- Valid Arweave wallet with sufficient AR for transactions

### Development Environment

**Required Tools:**
```bash
# Install Node.js and ARX
npm install -g @permaweb/arx

# Verify installations
node --version  # Should be 18+
arx --version   # Should show latest version
erl -version    # Should show OTP 24+
```

**Arweave Wallet Setup:**
```bash
# Set wallet path environment variable
export WALLET_PATH=/path/to/production-wallet.json
export AR_WALLET=$WALLET_PATH  # Alternative variable name

# Verify wallet has sufficient balance (check on arweave.app)
arx balance -w $WALLET_PATH
```

### Network Access Requirements

**Arweave Network:**
- Port 443 (HTTPS) for Arweave gateway access
- Access to arweave.net and gateway nodes
- DNS resolution for Arweave infrastructure

**Hyperbeam Requirements:**
- Access to Hyperbeam server endpoints
- WebSocket support for real-time communication
- Port ranges as specified by Hyperbeam configuration

## Publishing to Arweave

### Building for Production

1. **Clean and Build:**
```bash
# Clean previous builds
make clean

# Build all components
make all

# Run comprehensive tests
cd aos_test_suite
make eunit

# Verify build artifacts
ls -la dist/
```

2. **Pre-deployment Validation:**
```bash
# Test Lua syntax and loading
lua -l src/aos.lua -e "print('aos.lua loads successfully')"
lua -l src/utils.lua -e "print('utils.lua loads successfully')"

# Run security tests specifically
cd aos_test_suite
rebar3 as authorities_test eunit -m aos_security_test
```

### Deployment Process

#### 1. Primary Module Deployment

**Deploy aos.lua:**
```bash
# Upload main AOS module to Arweave
arx upload src/aos.lua \
    -w $WALLET_PATH \
    -t arweave \
    --content-type application/lua \
    --tags \
      "Data-Protocol:ao" \
      "Module-Format:1.0" \
      "Input-Encoding:JSON-1" \
      "Output-Encoding:JSON-1" \
      "Version:$(cat VERSION)" \
      "Environment:production"

# Save the transaction ID - you'll need this for process instantiation
echo "TX_ID_AOS=$(arx upload result)" >> deployment.env
```

**Deploy utils.lua (if standalone):**
```bash
# Upload utils module (optional - usually bundled with aos.lua)
arx upload src/utils.lua \
    -w $WALLET_PATH \
    -t arweave \
    --content-type application/lua \
    --tags \
      "Data-Protocol:ao" \
      "Module-Format:1.0" \
      "Type:utility" \
      "Version:$(cat VERSION)"

echo "TX_ID_UTILS=$(arx upload result)" >> deployment.env
```

#### 2. Configuration Data

**Process Configuration:**
```bash
# Create and upload process configuration
cat > process-config.json << EOF
{
  "version": "$(cat VERSION)",
  "authorities": [
    "authority_address_1_43_chars_long_base64url",
    "authority_address_2_43_chars_long_base64url", 
    "authority_address_3_43_chars_long_base64url"
  ],
  "settings": {
    "max_inbox_size": 10000,
    "security_level": "high",
    "color_output": true
  },
  "metadata": {
    "name": "Production Hyper-AOS Process",
    "description": "High-performance AO process with enhanced security",
    "environment": "production"
  }
}
EOF

arx upload process-config.json \
    -w $WALLET_PATH \
    -t arweave \
    --content-type application/json \
    --tags \
      "Data-Protocol:ao" \
      "Type:configuration" \
      "Environment:production"

echo "TX_ID_CONFIG=$(arx upload result)" >> deployment.env
```

#### 3. Verification and Validation

**Verify Deployment:**
```bash
# Source deployment environment
source deployment.env

# Verify transactions are confirmed
arx status $TX_ID_AOS
arx status $TX_ID_CONFIG

# Download and verify uploaded files
arx download $TX_ID_AOS -o aos-deployed.lua
diff src/aos.lua aos-deployed.lua

# Test deployed module loading
lua -l aos-deployed.lua -e "print('Deployed module loads successfully')"
```

## Launching Hyper-AOS Instances

### Production Process Launch

#### 1. Basic Process Launch

```bash
# Launch AOS console with deployed module
aos console \
    --module $TX_ID_AOS \
    --mainnet $HYPERBEAM_SERVER \
    --wallet $WALLET_PATH \
    production-process-name

# Alternative with specific authorities
aos console \
    --module $TX_ID_AOS \
    --mainnet $HYPERBEAM_SERVER \
    --wallet $WALLET_PATH \
    --authority "addr1,addr2,addr3" \
    production-process-name
```

#### 2. Advanced Configuration

**With Custom Settings:**
```bash
# Create launch configuration
cat > launch-config.json << EOF
{
  "module": "$TX_ID_AOS",
  "scheduler": "mainnet", 
  "hyperbeam_server": "$HYPERBEAM_SERVER",
  "wallet": "$WALLET_PATH",
  "authorities": [
    "authority_address_1",
    "authority_address_2",
    "authority_address_3"
  ],
  "settings": {
    "memory_limit": "512MB",
    "execution_timeout": "30s",
    "max_message_size": "1MB"
  }
}
EOF

# Launch with configuration file
aos console --config launch-config.json production-process
```

#### 3. Multi-Instance Deployment

**Load Balanced Setup:**
```bash
#!/bin/bash
# deploy-multiple-instances.sh

INSTANCES=("prod-instance-1" "prod-instance-2" "prod-instance-3")
HYPERBEAM_SERVERS=("server1.hyperbeam.io" "server2.hyperbeam.io" "server3.hyperbeam.io")

for i in "${!INSTANCES[@]}"; do
    INSTANCE=${INSTANCES[$i]}
    SERVER=${HYPERBEAM_SERVERS[$i]}
    
    echo "Launching instance: $INSTANCE on $SERVER"
    
    aos console \
        --module $TX_ID_AOS \
        --mainnet $SERVER \
        --wallet $WALLET_PATH \
        --authority "$PRODUCTION_AUTHORITIES" \
        --detach \
        $INSTANCE &
        
    echo "Instance $INSTANCE started with PID $!"
    sleep 5  # Stagger launches
done

wait  # Wait for all instances to initialize
echo "All instances launched successfully"
```

## Configuration Options

### Environment Variables

**Core Configuration:**
```bash
# Arweave Configuration
export AR_WALLET="/path/to/production/wallet.json"
export AR_GATEWAY="https://arweave.net"

# Hyperbeam Configuration  
export HYPERBEAM_SERVER="https://production.hyperbeam.io"
export HYPERBEAM_API_KEY="your_api_key_here"

# AOS Configuration
export AOS_MODULE_TX_ID="your_deployed_module_tx_id"
export AOS_AUTHORITIES="addr1,addr2,addr3"
export AOS_MAX_INBOX_SIZE="10000"

# Security Configuration
export AOS_SECURITY_LEVEL="high"
export AOS_REQUIRE_AUTH="true"
export AOS_VALIDATE_COMMITMENTS="true"
```

### Process Configuration Files

**production.config.json:**
```json
{
  "process": {
    "name": "production-hyper-aos",
    "version": "1.0.0",
    "module_id": "aos_module_tx_id_here",
    "environment": "production"
  },
  "security": {
    "owner_validation": true,
    "authority_validation": true,
    "commitment_validation": true,
    "trusted_authorities": [
      "authority_address_1_43_chars",
      "authority_address_2_43_chars",
      "authority_address_3_43_chars"
    ]
  },
  "performance": {
    "max_inbox_size": 10000,
    "message_timeout_ms": 30000,
    "max_execution_time_ms": 5000,
    "memory_limit_mb": 512
  },
  "logging": {
    "level": "info",
    "colorized_output": false,
    "log_to_file": true,
    "log_file_path": "/var/log/hyper-aos/process.log"
  },
  "monitoring": {
    "health_check_interval_ms": 60000,
    "metrics_collection": true,
    "performance_tracking": true
  }
}
```

### Hyperbeam Server Configuration

**server.config.toml:**
```toml
[server]
host = "0.0.0.0"
port = 8080
workers = 4

[arweave]
gateway = "https://arweave.net"
timeout = "30s"

[lua_vm]
memory_limit = "512MB"
execution_timeout = "30s"
sandbox_enabled = true

[security]
validate_signatures = true
require_commitments = true
authority_checking = true

[logging]
level = "info"
format = "json"
output = "/var/log/hyperbeam/server.log"
```

## Monitoring and Maintenance

### Health Monitoring

#### 1. Process Health Checks

**Basic Health Check Script:**
```bash
#!/bin/bash
# health-check.sh

PROCESS_NAME="production-hyper-aos"
HEALTH_ENDPOINT="http://localhost:8080/health"

# Check if process is running
if pgrep -f "$PROCESS_NAME" > /dev/null; then
    echo "✅ Process $PROCESS_NAME is running"
else
    echo "❌ Process $PROCESS_NAME is not running"
    exit 1
fi

# Check HTTP endpoint health
if curl -s -f $HEALTH_ENDPOINT > /dev/null; then
    echo "✅ Health endpoint is responding"
else
    echo "❌ Health endpoint is not responding"
    exit 1
fi

# Check message processing capability
TEST_RESULT=$(curl -s -X POST $HEALTH_ENDPOINT/test \
    -H "Content-Type: application/json" \
    -d '{"action":"eval","data":"return 1+1"}')

if echo "$TEST_RESULT" | grep -q "ok"; then
    echo "✅ Message processing is working"
else
    echo "❌ Message processing is failing"
    exit 1
fi

echo "✅ All health checks passed"
```

#### 2. Performance Monitoring

**Performance Metrics Collection:**
```bash
#!/bin/bash
# collect-metrics.sh

PROCESS_NAME="production-hyper-aos"
METRICS_FILE="/var/log/hyper-aos/metrics.json"

# Collect system metrics
CPU_USAGE=$(top -p $(pgrep -f "$PROCESS_NAME") -bn1 | grep "$PROCESS_NAME" | awk '{print $9}')
MEMORY_USAGE=$(ps -p $(pgrep -f "$PROCESS_NAME") -o rss= | awk '{print $1*1024}')

# Collect application metrics
MESSAGE_COUNT=$(curl -s http://localhost:8080/metrics | jq '.messages_processed')
INBOX_SIZE=$(curl -s http://localhost:8080/metrics | jq '.inbox_size')
UPTIME=$(curl -s http://localhost:8080/metrics | jq '.uptime_seconds')

# Create metrics record
cat > $METRICS_FILE << EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "process": "$PROCESS_NAME",
  "system": {
    "cpu_usage_percent": $CPU_USAGE,
    "memory_usage_bytes": $MEMORY_USAGE
  },
  "application": {
    "messages_processed": $MESSAGE_COUNT,
    "inbox_size": $INBOX_SIZE,
    "uptime_seconds": $UPTIME
  }
}
EOF

echo "Metrics collected: $METRICS_FILE"
```

### Log Management

#### 1. Log Rotation Configuration

**/etc/logrotate.d/hyper-aos:**
```
/var/log/hyper-aos/*.log {
    daily
    missingok
    rotate 30
    compress
    delaycompress
    notifempty
    sharedscripts
    postrotate
        systemctl reload hyper-aos || true
    endscript
}
```

#### 2. Structured Logging

**Log Analysis Script:**
```bash
#!/bin/bash
# analyze-logs.sh

LOG_FILE="/var/log/hyper-aos/process.log"
TIMEFRAME="${1:-1h}"  # Default to last hour

echo "=== Hyper-AOS Log Analysis (last $TIMEFRAME) ==="

# Error count
ERROR_COUNT=$(journalctl -u hyper-aos --since="$TIMEFRAME ago" | grep -c "ERROR")
echo "Errors: $ERROR_COUNT"

# Message processing stats
MSG_PROCESSED=$(journalctl -u hyper-aos --since="$TIMEFRAME ago" | grep -c "message processed")
echo "Messages processed: $MSG_PROCESSED"

# Security events
SECURITY_EVENTS=$(journalctl -u hyper-aos --since="$TIMEFRAME ago" | grep -c "security")
echo "Security events: $SECURITY_EVENTS"

# Performance warnings
PERF_WARNINGS=$(journalctl -u hyper-aos --since="$TIMEFRAME ago" | grep -c "performance")
echo "Performance warnings: $PERF_WARNINGS"

# Top error types
echo -e "\n=== Top Error Types ==="
journalctl -u hyper-aos --since="$TIMEFRAME ago" | grep "ERROR" | \
    sed 's/.*ERROR: //' | sort | uniq -c | sort -nr | head -10
```

### Backup and Recovery

#### 1. State Backup

**Automated Backup Script:**
```bash
#!/bin/bash
# backup-state.sh

PROCESS_NAME="production-hyper-aos"
BACKUP_DIR="/backup/hyper-aos"
DATE=$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p "$BACKUP_DIR/$DATE"

# Backup process state
curl -s http://localhost:8080/admin/export-state > "$BACKUP_DIR/$DATE/state.json"

# Backup configuration
cp /etc/hyper-aos/production.config.json "$BACKUP_DIR/$DATE/"

# Backup logs (last 24h)
journalctl -u hyper-aos --since="24 hours ago" > "$BACKUP_DIR/$DATE/logs.txt"

# Create archive
cd "$BACKUP_DIR"
tar -czf "hyper-aos-backup-$DATE.tar.gz" "$DATE"
rm -rf "$DATE"

# Cleanup old backups (keep 30 days)
find "$BACKUP_DIR" -name "hyper-aos-backup-*.tar.gz" -mtime +30 -delete

echo "Backup completed: hyper-aos-backup-$DATE.tar.gz"
```

#### 2. Disaster Recovery

**Recovery Procedure:**
```bash
#!/bin/bash
# disaster-recovery.sh

BACKUP_FILE="$1"
RECOVERY_DIR="/tmp/hyper-aos-recovery"

if [[ -z "$BACKUP_FILE" ]]; then
    echo "Usage: $0 <backup-file.tar.gz>"
    exit 1
fi

echo "Starting disaster recovery from: $BACKUP_FILE"

# Stop current process
systemctl stop hyper-aos

# Extract backup
mkdir -p "$RECOVERY_DIR"
cd "$RECOVERY_DIR"
tar -xzf "$BACKUP_FILE"

# Restore configuration
cp production.config.json /etc/hyper-aos/

# Restore state
curl -X POST http://localhost:8080/admin/import-state \
    -H "Content-Type: application/json" \
    -d @state.json

# Restart process
systemctl start hyper-aos

# Verify recovery
sleep 10
if systemctl is-active --quiet hyper-aos; then
    echo "✅ Recovery successful"
else
    echo "❌ Recovery failed - check logs"
    exit 1
fi

# Cleanup
rm -rf "$RECOVERY_DIR"
```

## Troubleshooting Common Issues

### Deployment Issues

#### Issue: Module upload fails with insufficient funds
**Solution:**
```bash
# Check wallet balance
arx balance -w $WALLET_PATH

# If balance is low, transfer AR to wallet
# Calculate required AR (approximate)
FILE_SIZE=$(stat -c%s src/aos.lua)
REQUIRED_AR=$(echo "scale=4; $FILE_SIZE * 0.000001" | bc)  # Rough estimate

echo "File size: $FILE_SIZE bytes"
echo "Estimated required AR: $REQUIRED_AR"
```

#### Issue: Transaction not confirming
**Solution:**
```bash
# Check transaction status
TX_ID="your_transaction_id"
arx status $TX_ID

# If pending too long, check network status
curl -s https://arweave.net/info | jq '.blocks'

# Retry with higher fee if needed
arx upload src/aos.lua -w $WALLET_PATH --reward-multiplier 2.0
```

### Runtime Issues

#### Issue: Process not starting
**Diagnostic Steps:**
```bash
# Check system resources
free -h
df -h /

# Check port availability
netstat -tlnp | grep :8080

# Check firewall rules
ufw status

# Check process logs
journalctl -u hyper-aos -f

# Test module loading manually
aos console --module $TX_ID_AOS --local test-process
```

#### Issue: High memory usage
**Solution:**
```bash
# Monitor memory usage
watch -n 1 'ps aux | grep hyper-aos | head -20'

# Check for memory leaks
valgrind --leak-check=full aos console --module $TX_ID_AOS test-process

# Adjust memory limits in configuration
sed -i 's/"memory_limit_mb": 512/"memory_limit_mb": 256/' /etc/hyper-aos/production.config.json

# Restart process
systemctl restart hyper-aos
```

### Security Issues

#### Issue: Unauthorized access attempts
**Mitigation:**
```bash
# Enable detailed security logging
sed -i 's/"level": "info"/"level": "debug"/' /etc/hyper-aos/production.config.json

# Monitor security events
tail -f /var/log/hyper-aos/process.log | grep -i "unauthorized\|security\|auth"

# Update authority list if compromised
curl -X POST http://localhost:8080/admin/update-authorities \
    -H "Content-Type: application/json" \
    -d '{"authorities": ["new_auth_1", "new_auth_2"]}'

# Rotate process keys if necessary
```

### Performance Issues

#### Issue: Slow message processing
**Optimization:**
```bash
# Check system load
uptime
iostat 1 5

# Analyze message processing times
grep "message processed" /var/log/hyper-aos/process.log | \
    awk '{print $NF}' | sort -n | tail -20

# Optimize configuration
# Increase worker count
sed -i 's/workers = 4/workers = 8/' /etc/hyperbeam/server.config.toml

# Adjust timeout values
sed -i 's/"message_timeout_ms": 30000/"message_timeout_ms": 15000/' \
    /etc/hyper-aos/production.config.json
```

This deployment guide provides comprehensive instructions for successfully deploying and maintaining Hyper-AOS in production environments with proper monitoring, security, and operational procedures.