#!/bin/bash
# Wrapper script for make with proper color support in Ghostty

# Set proper terminal for color support
export TERM=xterm-256color

# Run make with all arguments
exec make "$@"