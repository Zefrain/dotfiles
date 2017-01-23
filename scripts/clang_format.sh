#!/ usr/bin/sh
# flist=$(find . -name '*.h' -o -name '*.c' -o -name '*.cpp' -o -name '*.sh')
OLD=$IFS
IFS=$'\n'

flist=$(find ! -name '.*' | egrep -v '\.\w+\/' | egrep '\.(c|cpp|h)$')

for fname in $flist;
do
    clang-format -i -style=google $fname
done;

IFS=$OLD
