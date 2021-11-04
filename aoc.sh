DIR=~/AdventOfCode

aoc_git_push() {
    OUT=`cd $DIR && git status --porcelain | head -1`
    YEAR=`echo $OUT | cut -d '_' -f 2`
    DAY=`echo $OUT | cut -d '_' -f 3`

    if (( YEAR >= 2015 && DAY >= 1 && DAY <= 25 )); then
        pushd $DIR
        git add $YEAR
        git commit -a -m "$YEAR day $DAY"
        git show
        if read -q "choice?Push commit to remote? [y/n]"; then
            git push origin master
        fi
        popd
    else
        echo "Unable to parse git output:"
        echo $OUT
    fi
}
