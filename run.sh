#!/bin/bash
# Auto-detect OS and architecture, then run clj with appropriate natives

set -e

# Detect OS and architecture
OS=$(uname -s)
ARCH=$(uname -m)

if [ "$OS" = "Linux" ]; then
    if [ "$ARCH" = "x86_64" ]; then
        NATIVES_ALIAS=":natives-linux"
        echo "Detected Linux x86_64"
    else
        echo "Unsupported Linux architecture: $ARCH"
        exit 1
    fi
elif [ "$OS" = "Darwin" ]; then
    if [ "$ARCH" = "arm64" ]; then
        NATIVES_ALIAS=":natives-arm"
        echo "Detected Apple Silicon (ARM64)"
    else
        NATIVES_ALIAS=":natives-intel"
        echo "Detected Intel Mac (x86_64)"
    fi
else
    echo "Unsupported operating system: $OS"
    exit 1
fi

# 如果参数为空，直接运行 natives alias
if [ $# -eq 0 ]; then
    echo "Running: clj -M$NATIVES_ALIAS"
    clj -M"$NATIVES_ALIAS"
    exit 0
fi

# Linux 特殊处理：避免使用包含 -XstartOnFirstThread 的 alias
if [ "$OS" = "Linux" ]; then
    MAIN_ALIAS="$1"
    shift
    EXTRA_ARGS=("$@")
    
    # 使用 Clojure 脚本提取 alias 信息
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    OUTPUT=$(clj -M "$SCRIPT_DIR/scripts/extract-alias-info.clj" "$MAIN_ALIAS" "$NATIVES_ALIAS" 2>&1)
    
    # 检查输出
    if echo "$OUTPUT" | grep -q "^FALLBACK$"; then
        # 回退到原始 clj 命令
        ALIASES="$NATIVES_ALIAS:$MAIN_ALIAS"
        for arg in "${EXTRA_ARGS[@]}"; do
            arg="${arg#:}"
            ALIASES="$ALIASES:$arg"
        done
        echo "Running: clj -M$ALIASES"
        clj -M"$ALIASES"
    elif echo "$OUTPUT" | grep -q "^FAILED:"; then
        echo "Error getting classpath"
        echo "$OUTPUT"
        exit 1
    else
        # 解析输出 (取最后一行)
        LINE=$(echo "$OUTPUT" | tail -1)
        MAIN_CLASS=$(echo "$LINE" | cut -d'|' -f1)
        CLASSPATH=$(echo "$LINE" | cut -d'|' -f2)
        JVM_OPTS_STR=$(echo "$LINE" | cut -d'|' -f3)
        
        # 构建命令
        CMD=("java")
        if [ -n "$JVM_OPTS_STR" ] && [ "$JVM_OPTS_STR" != "--enable-native-access=ALL-UNNAMED" ]; then
            IFS=',' read -ra JVM_OPTS_ARRAY <<< "$JVM_OPTS_STR"
            for opt in "${JVM_OPTS_ARRAY[@]}"; do
                [ -n "$opt" ] && CMD+=("$opt")
            done
        elif [ "$JVM_OPTS_STR" = "--enable-native-access=ALL-UNNAMED" ]; then
            CMD+=("--enable-native-access=ALL-UNNAMED")
        fi
        CMD+=("-cp" "$CLASSPATH" "clojure.main" "-m" "$MAIN_CLASS" "${EXTRA_ARGS[@]}")
        
        echo "Running: java ... -m $MAIN_CLASS"
        exec "${CMD[@]}"
    fi
    
else
    # macOS：使用原始的 alias 方式
    ALIASES="$NATIVES_ALIAS"
    for arg in "$@"; do
        arg="${arg#:}"
        ALIASES="$ALIASES:$arg"
    done
    
    echo "Running: clj -M$ALIASES"
    clj -M"$ALIASES"
fi
