tag-overlaps-find () { find "$1"/* | while read f; do tag-find "$f"; done }
tag-overlaps () { tag-overlaps-find "$1" | cut -d "/" -f 3 | sort | uniq -c | sort -rn ; }
tag-root-merge () { echo "functions.sh"; } # ls "$1/tags" | while read tag; do cp "$1/tags/$tag/*" "$2/tags/$tag"; done; }
tag-find () { find . -samefile "$1" 2> /dev/null; } # https://unix.stackexchange.com/questions/201920/how-to-find-all-hard-links-to-a-given-file
tag-stats () {
    ls tags/ | while read t
    do echo -n "$t "
       ls "tags/$t" | wc -l
    done | sort -nr -k 2
}
tag-remove () {
    h=$(pwd)
    n=$(basename $1)
    rm -v $h/tags/*/"$n";
}
