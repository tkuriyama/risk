while true
do
    fswatch -r --one-event -e modify elm/
    ./tcr.sh
done

