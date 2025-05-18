{:filetypes [:c :cpp :objc :objcpp :cuda :proto :h]
 :cmp [:clangd :--background-index]
 :root_markers [".clangd"
                ".clang-format"
                ".clang-tidy :compile_commands.json :compile_flags.txt :configure.ac :.git"]
 :capabilities {:textDocument {:completion {:editsNearCursor true}}
                :offsetEncoding [:utf-8 :utf-16]}
 :flags {:debounce_text_changes 20}}
