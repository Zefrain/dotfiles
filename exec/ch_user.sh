#!/bin/sh

git filter-branch --force --env-filter '
# OLD_EMAIL="whiothes81@gmail.com"
# CORRECT_NAME="周尚"
# CORRECT_EMAIL="zhou_s@xingdata.com"

OLD_EMAIL="zhou_s@xingdata.com"
CORRECT_NAME="whiothes"
CORRECT_EMAIL="whiothes81@gmail.com"
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
' --tag-name-filter cat -- --branches --tags
