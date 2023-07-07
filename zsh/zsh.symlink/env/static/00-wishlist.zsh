# wishlist
# load a wishlist.table (see the pattern in this file)

# \() {
#  for i in `cat wishlist.table`
#}

DIR=${0%/*}
zmodload zsh/mapfile

setopt shwordsplit

trim () {
  echo "$1" | sed -e 's/^ *//g' -e 's/ *$//g'
}

() {
  FILENAME="$DIR/wishlist.table"
  # [[ -r "$FILENAME" ]] || continue
  typeset -a lines
  typeset -a columns
  lines=( "${(f)mapfile[$FILENAME]}" )
  for (( i = ${#lines[@]}; i >= 1; i-- )); do
    line="${lines[$i]}"
    if [[ ! "$line" =~ ^\! ]]; then
      local ADMIN=0
      local FULLPATH=0
      local COM=""
      local saveIFS="$IFS"
      IFS=':'
      columns=( ${line} )
      IFS="$saveIFS"
      action=$(trim "$columns[1]")
      folder=$(trim "$columns[2]")
      os=$(trim "$columns[3]")
      variable=$(trim "$columns[4]")
      if [[ $ADMIN_ACTION -gt 0 && "$action" =~ ^\\* ]]; then
        ADMIN=1 # only admin can have `sbin'
      elif [[ "$action" =~ ^\= ]]; then
        FULLPATH=1 # mutually exclusive with ADMIN
      else
        # permute if no action column
        variable=$os; os=$folder; folder=$action
      fi
      [[ "$os" != "" && "$SYSTEM" != "$os" ]] && continue
      folder=$(eval "echo \"$folder\"" 2> /dev/null) || continue # for variable or command
      [[ "$folder" == "" ]] && continue
      [[ $ADMIN_ACTION -gt 0 && $ADMIN -gt 0 ]] && add_path "${folder}/sbin"
      [[ $FULLPATH -eq 0 ]] && folder+="/bin"
      add_path $folder
      if [[ "$variable" != "" ]]; then
        eval "export ${variable}=\"$folder\"" 2>/dev/null || echo "wishlist: error during ${variable} setting"
      fi
    fi
  done
}

unset DIR

# Last Modified: Fri 19 Feb 2021 15:40:15 CET
