#!/bin/bash

# SML build script - runs CM.make "sources.cm"

# Check if sml command exists
if ! command -v sml &> /dev/null; then
    echo "Error: SML not found. Please install SML/NJ or ensure it's in your PATH."
    exit 1
fi

# Check if sources.cm exists
if [ ! -f "sources.cm" ]; then
    echo "Error: sources.cm file not found in current directory."
    exit 1
fi

echo "Building SML project using sources.cm..."

# Run SML with the CM.make command
sml << 'EOF'
CM.make "sources.cm";
EOF

echo "Build completed."