#!/bin/bash
# Auto-detect macOS architecture and run clj with appropriate natives

set -e

# Detect architecture
ARCH=$(uname -m)
if [ "$ARCH" = "arm64" ]; then
    NATIVES_ALIAS=":natives-arm"
    echo "Detected Apple Silicon (ARM64)"
else
    NATIVES_ALIAS=":natives-intel"
    echo "Detected Intel Mac (x86_64)"
fi

# Build the aliases string
# First add the natives alias, then any user-provided aliases
ALIASES="$NATIVES_ALIAS"

for arg in "$@"; do
    # Remove leading colon if present
    arg="${arg#:}"
    ALIASES="$ALIASES:$arg"
done

echo "Running: clj -M$ALIASES"
clj -M"$ALIASES"
