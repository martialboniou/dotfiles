#!/bin/sh

# preamble
echo "Generate vendor directory in tests subdir"
mkdir -p tests/vendor/pack1/third-party
touch tests/vendor/pack1/file1.el
touch tests/vendor/pack1/third-party/file2.el
touch tests/vendor/pack1/third-party/what.txt
echo "This file is a dummy file for testing *defs.el* functions.\nIt should be destroyed after tests." > tests/vendor/README
mkdir tests/vendor/pack2
touch tests/vendor/pack2/file3.el
touch tests/vendor/pack2/file4.el
touch tests/vendor/pack2/and_now_something.txt
touch tests/vendor/pack2/different.txt
touch tests/vendor/pack2/file5.el
mkdir tests/vendor/pack3
touch tests/vendor/file0.el

# tests
emacs -batch -L tests -l test-defs -l test-packs -f ert-run-tests-batch-and-exit

# tidy up
rm tests/vendor/pack1/third-party/*.*
rmdir tests/vendor/pack1/third-party
rm tests/vendor/pack1/*.*
rmdir tests/vendor/pack1
rm tests/vendor/pack2/*.*
rmdir tests/vendor/pack2
rmdir tests/vendor/pack3
rm tests/vendor/*.*
rm tests/vendor/README
rmdir tests/vendor
