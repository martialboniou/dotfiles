#!/usr/bin/zsh
if [ -z "$CD_BOOKMARKS" ]; then
 local CD_BOOKMARKS=$ZDOTDIR/.cdbookmarks
fi

function cdb_edit() {
 $EDITOR $CD_BOOKMARKS
}

function cdb() {
 NewDir=`egrep "$1\t" $CD_BOOKMARKS \
        | sed 's/^.*\t//'`;
 echo cd $NewDir
 cd $NewDir
}

function _cdb() {
 reply=(`cat $CD_BOOKMARKS | sed 's/\t.*$//'`);
}

compctl -K _cdb cdb
