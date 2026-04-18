#!/usr/bin/env bash

FILE=flsproject.fnl
SCRIPT=build-flsproject
COMMAND=FlsProject

if [ -f "$FILE" ]; then
    read -r -n 1 -p "Do you want to erase the existing file named $FILE? [y|N] " REPLY;
    if [ "$REPLY" = "${REPLY#[yY]}" ]; then
        printf "\n\033[32m %s \n\033[0m" "The current version of $FILE has been kept."
        exit 0
    fi
fi

echo
nvim --headless -u "$SCRIPT.lua" +$COMMAND +qa
