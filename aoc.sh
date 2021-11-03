aoc_git_push() {
    OUT=`cd ~/AdventOfCode && git status --porcelain | head -1`
    YEAR=`echo $OUT | cut -d '_' -f 2`
    DAY=`echo $OUT | cut -d '_' -f 3`

    if (( YEAR >= 2015 && DAY >= 1 && DAY <= 25 )); then
        git add $YEAR
        git commit -a -m "$YEAR day $DAY"
        git show
        if read -q "choice?Push commit to remote? [y/n]"; then
            git push origin master
        fi
    else
        echo "Unable to parse git output:"
        echo $OUT
    fi
}
