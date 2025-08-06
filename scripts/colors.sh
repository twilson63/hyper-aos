#!/bin/bash
# Color configuration for Ghostty terminal

# Check if terminal supports colors
if [ -t 1 ] && command -v tput > /dev/null 2>&1; then
    # Use tput for maximum compatibility
    export RED=$(tput setaf 1)
    export GREEN=$(tput setaf 2)
    export YELLOW=$(tput setaf 3)
    export BLUE=$(tput setaf 4)
    export MAGENTA=$(tput setaf 5)
    export CYAN=$(tput setaf 6)
    export WHITE=$(tput setaf 7)
    export BOLD=$(tput bold)
    export NC=$(tput sgr0) # No Color/Reset
else
    # Fallback to ANSI codes
    export RED='\033[0;31m'
    export GREEN='\033[0;32m'
    export YELLOW='\033[0;33m'
    export BLUE='\033[0;34m'
    export MAGENTA='\033[0;35m'
    export CYAN='\033[0;36m'
    export WHITE='\033[0;37m'
    export BOLD='\033[1m'
    export NC='\033[0m'
fi

# Function to print colored text
print_color() {
    local color=$1
    shift
    echo -e "${color}$@${NC}"
}

# Export function for use in Make
export -f print_color