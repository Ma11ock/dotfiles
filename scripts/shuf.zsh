#!/usr/bin/env zsh

# This scripts shuffles the inputs and runs them in as music files.

# Echo to stderr
echoerr() {
    echo "$@" 1>&2
}

# Alter these values as necessary
# Takes the file extension of the file and returns the music player associated with it
getplayer() {
    case $1 in
        "mp3")
            echo "cvlc"
            ;;
        "m4a")
            echo "cvlc"
            ;;
        "modPlayer")
            echo "cvlc"
            ;;
        "webm")
            echo "cvlc"
            ;;
        *)
            echoerr "The music file type $1 is not supported."
            return -1
            ;;
    esac
}

songs=( $(echo "$@" | sed -r 's/(.[^;]*;)/ \1 /g' | tr " " "\n" | shuf | tr -d " " ) )

for song in $songs; do
    filename=`basename -- "$song"`
    extension="${filename##*.}"

    player=$(getplayer "$extension")

    # If getting the type did not failed then play the song
    [[ $? == 0 ]] && echo "Now playing $song" && $player "$song"
done



