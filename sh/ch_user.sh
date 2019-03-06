#!/bin/sh

# read -p 'OLD_EMAIL:' OLD_EMAIL
read -p 'OLD_NAME:' OLD_NAME
read -p 'CORRECT_NAME:' CORRECT_NAME
read -p 'CORRECT_EMAIL:' CORRECT_EMAIL

git filter-branch --force --env-filter '

OLD_EMAIL=$OLD_EMAIL
CORRECT_NAME=$CORRECT_NAME
CORRECT_EMAIL=$CORRECT_EMAIL

# OLD_EMAIL="whiothes81@gmail.com"
# CORRECT_NAME="周尚"
# CORRECT_EMAIL="zhou_s@xingdata.com"

# OLD_EMAIL="zhou_s@xingdata.com"
# CORRECT_NAME="whiothes"
# CORRECT_EMAIL="whiothes81@gmail.com"

# if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
if [ "GIT_COMMITER_NAME" = "$OLD_NAME" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_NAME" = "$OLD_NAME" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags
