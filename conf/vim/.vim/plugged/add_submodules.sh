#!/bin/bash

# Get the current directory
current_dir=$(pwd)

# Initialize a variable to track if there are any changes
changes_made=false

# Loop through all directories in the current directory
for repo in */; do
    if [ -d "$repo/.git" ]; then
        # Extract the repository URL and the current branch
        cd "$repo" || continue
        
        repo_url=$(git config --get remote.origin.url)
        branch_name=$(git symbolic-ref --short HEAD)
        
        # Return to the root directory
        cd "$current_dir" || exit
        
        # Check if the submodule path already exists in the index
        submodule_path="$repo"
        
        if git ls-files --error-unmatch "$submodule_path" > /dev/null 2>&1; then
            echo "Warning: '$submodule_path' already exists in the index. Preparing to clean up..."

            # Remove the existing submodule entry from the index
            git rm --cached "$submodule_path"
            changes_made=true
        fi

        # Add the repo as a submodule with URL and branch name
        echo "Preparing to add '$submodule_path' as a submodule..."
        git submodule add -b "$branch_name" "$repo_url" "$submodule_path"
        changes_made=true
    fi
done

# Commit all changes in a single commit if there were any changes
if [ "$changes_made" = true ]; then
    git add .
    git commit -m "Add submodules (multiple) in one commit"
    echo "Submodules added and committed."
else
    echo "No submodule changes made."
fi

echo "Submodule setup complete."
