{:filetypes [:lua]
 :cmd [:lua-language-server]
 :root_markers [:.luarc.json
                :.luarc.jsonc
                :.luacheckrc
                :.stylua.toml
                :stylua.toml
                :selene.toml
                :selene.yml
                :.git]
 :settings {:Lua {:runtime {:version "LuaJIT"}
                  :diagnostics {:unusedLocalExclude ["_*"]
                                :disable [:unused-vararg :deprecated]
                                :globals [:vim :love]}}}}
