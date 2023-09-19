file=$(type -P $1); shift
c=/tmp/$(basename "$file")
cp "$file" "$c"
codesign --remove-signature "$c"
sudo dtruss "$c" "$@"
