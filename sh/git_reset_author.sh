#!/bin/sh

echo 'Enter OLD_EMAIL: '
read -r OLD_EMAIL
echo 'Enter CORRECT_NAME: '
read -r CORRECT_NAME
echo 'Enter CORRECT_EMAIL: '
read -r CORRECT_EMAIL
echo 'Enter START_COMMIT (inclusive): '
read -r START_COMMIT
echo 'Enter END_COMMIT (inclusive): '
read -r END_COMMIT

# Set default values if START_COMMIT or END_COMMIT are empty
if [ -z "$START_COMMIT" ]; then
  START_COMMIT=$(git rev-list --max-parents=0 HEAD)
fi

if [ -z "$END_COMMIT" ]; then
  END_COMMIT=HEAD
fi

export OLD_EMAIL
export CORRECT_NAME
export CORRECT_EMAIL

git filter-branch -f --env-filter '
if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat "$START_COMMIT".."$END_COMMIT"
