#!/bin/bash

echo "Building AOS Test Suite..."
rebar3 compile

if [ $? -ne 0 ]; then
    echo "Build failed!"
    exit 1
fi

echo ""
echo "Running tests..."
rebar3 shell --eval "aos_sandbox_test:run_tests(), init:stop()."