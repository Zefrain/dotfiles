#!/bin/bash

# Define source directories (indexed array)
sources=(
    ""               # source path (adjust as needed)
)

# Define target directories (indexed array, same order as sources)
targets=(
    ""               # macOS/Linux/OpenWrt target
)

# rsync options
RSYNC_OPTS="-avh --progress"

# Polling interval for OpenWrt (in seconds)
POLL_INTERVAL=10

# Function to check and install dependencies
install_dependencies() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if [ -f /etc/openwrt_release ]; then
            # OpenWrt-specific installation
            echo "No event-based tool available. Falling back to polling for OpenWrt."
        else
            # Generic Linux
            echo "Installing dependencies on Linux..."
            sudo apt update && sudo apt install rsync fswatch -y || { echo "Error installing dependencies on Linux"; exit 1; }
        fi
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        echo "Checking for fswatch on macOS..."
        if ! command -v fswatch &> /dev/null; then
            echo "Installing fswatch on macOS..."
            brew install fswatch || { echo "Error installing fswatch on macOS"; exit 1; }
        fi
    fi
}

# Function to validate paths
validate_paths() {
    local source="$1"
    local target="$2"

    # Check source path
    if [ ! -d "$source" ]; then
        echo "Error: Source directory $source does not exist. Skipping..."
        return 1
    fi

    # Check target path
    if [[ "$target" == *:* ]]; then
        # Remote target (SSH)
        ssh_host=$(echo "$target" | cut -d':' -f1)
        ssh_path=$(echo "$target" | cut -d':' -f2)
        ssh "$ssh_host" "[ -d $ssh_path ]" &> /dev/null
        if [ $? -ne 0 ]; then
            echo "Error: Target directory $target is not accessible. Skipping..."
            return 1
        fi
    else
        # Local target
        if [ ! -d "$target" ]; then
            echo "Error: Target directory $target does not exist. Skipping..."
            return 1
        fi
    fi

    return 0
}

# Function to monitor and sync files
monitor_pair() {
    local source="$1"
    local target="$2"

    # Validate paths
    validate_paths "$source" "$target" || return 1

    # Start monitoring
    if [[ "$OSTYPE" == "linux-gnu"* && -f /etc/openwrt_release ]]; then
        # Use polling on OpenWrt
        echo "Using polling to monitor $source on OpenWrt..."
        while true; do
            rsync $RSYNC_OPTS "$source/" "$target/" || { echo "Error during rsync from $source to $target"; }
            sleep "$POLL_INTERVAL"
        done
    elif [[ "$OSTYPE" == "linux-gnu"* || "$OSTYPE" == "darwin"* ]]; then
        # Use fswatch on Linux or macOS
        echo "Using fswatch to monitor $source..."
        fswatch -0 "$source" | while read -d "" event; do
            echo "Detected change in $event. Syncing..."
            rsync $RSYNC_OPTS "$source/" "$target/" || { echo "Error during rsync from $source to $target"; return 1; }
        done
    else
        echo "Unsupported platform: $OSTYPE"
        return 1
    fi
}

# Install dependencies
install_dependencies || { echo "Failed to install dependencies"; exit 1; }

# Loop through each source-target pair
for i in "${!sources[@]}"; do
    source="${sources[$i]}"
    target="${targets[$i]}"
    monitor_pair "$source" "$target" &
    if [ $? -ne 0 ]; then
        echo "Error setting up monitoring for $source to $target"
    fi
done

# Wait for all background processes
wait
