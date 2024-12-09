#!/bin/bash

# Arrays to hold source-to-target mappings
sources=()
targets=()

# rsync options
RSYNC_OPTS="-avh --progress"

# Polling interval for OpenWrt (in seconds)
POLL_INTERVAL=10

# Function to install dependencies
install_dependencies() {
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        if [ -f /etc/openwrt_release ]; then
            echo "No event-based tool available. Falling back to polling for OpenWrt."
        else
            echo "Installing dependencies on Linux..."
            sudo apt update && sudo apt install -y rsync fswatch || { echo "Error installing dependencies on Linux"; exit 1; }
        fi
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        echo "Checking for fswatch on macOS..."
        if ! command -v fswatch &> /dev/null; then
            echo "Installing fswatch on macOS..."
            brew install fswatch || { echo "Error installing fswatch on macOS"; exit 1; }
        fi
    fi
}

# Function to prompt for source-target mappings
prompt_source_target_mappings() {
    echo "Enter source-to-target mappings in the format:"
    echo "  source_dir => target_dir1, target_dir2"
    echo "Press Enter without input to finish."

    while true; do
        read -r mapping
        [[ -z "$mapping" ]] && break

        # Split the mapping into source and targets
        IFS="=>" read -r source targets_input <<< "$mapping"
        source=$(echo "$source" | xargs)  # Trim whitespace
        targets_input=$(echo "$targets_input" | xargs)  # Trim whitespace

        # Remove leading ">" from targets_input (if present)
        targets_input=$(echo "$targets_input" | sed 's/^> //')

        if [[ -z "$source" || -z "$targets_input" ]]; then
            echo "Invalid input. Ensure it follows the format 'source_dir => target_dir1, target_dir2'."
            continue
        fi

        # Add the source and targets to arrays
        sources+=("$source")
        targets+=("$targets_input")
    done
}

# Function to validate paths
validate_paths() {
    local source="$1"
    local targets="$2"

    # Check source
    if [ ! -d "$source" ]; then
        echo "Error: Source directory '$source' does not exist."
        return 1
    fi

    # Check targets
    IFS=',' read -r -a target_list <<< "$targets"
    for target in "${target_list[@]}"; do
        target=$(echo "$target" | xargs)  # Trim whitespace
        if [[ "$target" == *:* ]]; then
            ssh_host=$(echo "$target" | cut -d':' -f1)
            ssh_path=$(echo "$target" | cut -d':' -f2)
            ssh "$ssh_host" "[ -d $ssh_path ]" &> /dev/null
            if [ $? -ne 0 ]; then
                echo "Error: Target directory '$target' is not accessible."
                return 1
            fi
        else
            if [ ! -d "$target" ]; then
                echo "Error: Target directory '$target' does not exist."
                return 1
            fi
        fi
    done

    return 0
}

# Function to monitor and sync files
monitor_pair() {
    local source="$1"
    local targets="$2"

    validate_paths "$source" "$targets" || return 1

    IFS=',' read -r -a target_list <<< "$targets"
    for target in "${target_list[@]}"; do
        target=$(echo "$target" | xargs)  # Trim whitespace

        if [[ "$OSTYPE" == "linux-gnu"* && -f /etc/openwrt_release ]]; then
            echo "Using polling to monitor '$source' to '$target' on OpenWrt..."
            while true; do
                rsync $RSYNC_OPTS "$source/" "$target/" || echo "Error during rsync from '$source' to '$target'"
                sleep "$POLL_INTERVAL"
            done &
        elif [[ "$OSTYPE" == "linux-gnu"* || "$OSTYPE" == "darwin"* ]]; then
            echo "Using fswatch to monitor '$source' to '$target'..."
            fswatch -0 "$source" | while read -d "" event; do
                echo "Detected change in '$event'. Syncing to '$target'..."
                rsync $RSYNC_OPTS "$source/" "$target/" || echo "Error during rsync from '$source' to '$target'"
            done &
        else
            echo "Unsupported platform: $OSTYPE"
            return 1
        fi
    done
}

# Install dependencies
install_dependencies || { echo "Failed to install dependencies"; exit 1; }

# Prompt for source-target mappings
prompt_source_target_mappings

# Loop through each source and its targets
for i in "${!sources[@]}"; do
    source="${sources[$i]}"
    targets="${targets[$i]}"
    monitor_pair "$source" "$targets"
done

# Wait for all background processes
wait
