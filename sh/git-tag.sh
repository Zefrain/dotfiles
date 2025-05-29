#!/usr/bin/env bash

readonly REMAIN_VERSION=3 # 保留最新的版本数

declare OSTYPE="Unknown"
declare RELEASE_CAN_DELETE=1 # 是否可以删除旧版本的 tag 和 release

get_os_type() {
  uNames=$(uname -s)
  osName=${uNames:0:4}
  if [ "$osName" = "Darw" ]; then # Darwin
    OSTYPE="Darwin"
  elif [ "$osName" = "Linu" ]; then # Linux
    OSTYPE="Linux"
  elif [ "$osName" = "MING" ]; then # MINGW, windows, git-bash
    OSTYPE="Windows"
  else
    OSTYPE="Unknown"
  fi
}

install_dependencies() {

  packages_to_install=(gh)

  if [ "$OSTYPE" == "Darwin" ]; then
    if ! command -v brew &>/dev/null; then
      echo "Homebrew is not installed. Please install it from https://brew.sh/."
      exit 1
    fi
    brew install gh || true
  elif [ "$OSTYPE" == "Linux" ]; then

    source /etc/os-release

    if [[ "$ID" == "ubuntu" || "$ID" == "debian" ]]; then
      sudo apt-get update
      sudo apt-get install -y "${packages_to_install[@]}" || {
        echo "Failed to install gh using apt. Trying snap..."
        if ! command -v snap &>/dev/null; then
          echo "Snap is not installed. Please install it first."
          return 1
        fi
        sudo snap install gh
      }
      if command -v dnf &>/dev/null; then
        sudo dnf install -y "${packages_to_install[@]}"
      elif command -v yum &>/dev/null; then
        sudo yum install -y "${packages_to_install[@]}"
      else
        echo "Neither dnf nor yum found. Cannot install gh."
        return 1
      fi
    elif [[ "$ID" == "arch" ]]; then
      sudo pacman -S --noconfirm "${packages_to_install[@]}" || {
        echo "Failed to install gh using pacman. Please install it manually."
        return 1
      }
    else
      echo "Unsupported Linux distribution: $ID"
      return 1
    fi

    if ! command -v gh &>/dev/null; then
      echo "Packages installed failed. Please install it manually."
      return 1
    fi
  elif [ "$OSTYPE" == "Windows" ]; then
    echo "Please install GitHub CLI from https://cli.github.com/."
    return 1
  else
    echo "Unsupported OS type: $OSTYPE"
    return 1
  fi

  if command -v nvm &>/dev/null; then
    npm install -g semver || {
      echo "Failed to install semver using nvm. Please install it manually."
      return 1
    }
  fi
}

setup_github_cli() {
  if ! command -v gh &>/dev/null; then
    echo "gh CLI is not installed. Please install it from https://cli.github.com/."
    install_dependencies || echo "Failed to install dependencies. Please install gh CLI manually." && RELEASE_CAN_DELETE=0
  fi

  # Check if user is authenticated with GitHub CLI
  if ! gh auth status &>/dev/null; then
    gh auth login
  fi
  # Ensure the repository is set up correctly
  if ! gh repo view &>/dev/null; then
    echo "This script must be run in a valid GitHub repository."
    RELEASE_CAN_DELETE=0
  fi
}

bump_version() {

  # Get latest semantic version tag
  LAST_TAG=$(git describe --tags --match "v*" --abbrev=0 2>/dev/null)

  # Set initial version if no tags exist
  if [ -z "$LAST_TAG" ]; then
    CURRENT_VERSION="v0.0.0"
  else
    CURRENT_VERSION="$LAST_TAG"
  fi

  # Analyze commit messages since last tag
  COMMITS_SINCE_TAG=$(git log --pretty=format:"%s" "$CURRENT_VERSION"..HEAD)

  # Determine version bump type
  BUMP_TYPE="patch"
  while IFS= read -r commit_msg; do
    if [[ "$commit_msg" =~ ^BREAKING\ CHANGE: ]]; then
      BUMP_TYPE="major"
      break
    elif [[ "$commit_msg" =~ ^feat: ]]; then
      BUMP_TYPE="minor"
    elif [[ "$commit_msg" =~ ^(fix|docs|style|refactor|perf|test|chore): ]] && [ "$BUMP_TYPE" != "minor" ]; then
      BUMP_TYPE="patch"
    fi
  done <<<"$COMMITS_SINCE_TAG"

  # Bump version using semver-tool if available
  if command -v semver &>/dev/null; then
    NEXT_VERSION="v$(semver bump "$BUMP_TYPE" "${CURRENT_VERSION#v}")"
  else
    # Fallback version bump logic
    IFS='.' read -ra PARTS <<<"${CURRENT_VERSION#v}"
    MAJOR=${PARTS[0]}
    MINOR=${PARTS[1]}
    PATCH=${PARTS[2]}

    case "$BUMP_TYPE" in
    "major")
      MAJOR=$((MAJOR + 1))
      MINOR=0
      PATCH=0
      ;;
    "minor")
      MINOR=$((MINOR + 1))
      PATCH=0
      ;;
    "patch")
      PATCH=$((PATCH + 1))
      ;;
    esac
    NEXT_VERSION="v$MAJOR.$MINOR.$PATCH"
  fi

  # Update version in package.json (example)
  # sed -i "s/\"version\": \".*\"/\"version\": \"${NEXT_VERSION#v}\"/" package.json

  echo "$NEXT_VERSION"
}

remove_old_version() {
  if [ $RELEASE_CAN_DELETE -eq 0 ]; then
    echo "Skipping old version deletion due to previous errors."
    return
  fi

  TAGS_TO_DELETE=$(git tag --sort=-v:refname | tail -n +$((REMAIN_VERSION + 1)) | tr '\n' ' ')
  echo "Tags to delete: ${TAGS_TO_DELETE// /, }"

  for TAG in $TAGS_TO_DELETE; do
    echo "Deleting tag: $TAG"

    # 删除本地 tag
    git tag -d "$TAG"

    # 删除远程 tag
    git push origin ":refs/tags/$TAG"

    if [[ $RELEASE_CAN_DELETE -eq 0 ]]; then
      echo "Skipping release deletion for tag: $TAG"
      continue
    else
      # 删除 GitHub release（需要 gh CLI）
      gh release delete "$TAG" --yes || echo "Release $TAG not found or already deleted"
    fi

  done
  # 删除远程 tag
  git push origin ":refs/tags/latest"

  echo "Old versions deleted successfully."
}

git_handle_version() {
  # Bump version and get new version number
  NEXT_VERSION=$(bump_version)

  remove_old_version || echo "Failed to remove old versions. Exiting."

  # Create new git tag only if current commit is not already tagged
  if ! git tag --points-at HEAD | grep -q "^$NEXT_VERSION$"; then
    git tag -a "$NEXT_VERSION" -m "Version $NEXT_VERSION"
  fi

  git tag -f latest "$NEXT_VERSION" || {
    echo "Failed to update latest tag. Exiting."
    return 1
  }

  if ! git push || ! git push --tags; then
    echo "Failed to push tags. Exiting."
    return 1
  fi
}

main() {
  get_os_type
  setup_github_cli || { echo "Failed to set up GitHub CLI. Exiting."; }
  git_handle_version || { echo "Failed to handle versioning. Exiting."; }
}

main
