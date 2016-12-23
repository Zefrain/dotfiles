
OLD=$IFS
IFS=$'\n'

filelist=$(find . -iname '*.rar' -o -iname '*.zip')

for file in $filelist
do
        echo $file
        fpath=$(dirname "$file")
        unar -e cp936 "$file" -o "$fpath" -s
        rm "$file"
done

IFS=$OLD
