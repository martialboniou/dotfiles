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
                  :diagnostics {:globals [:vim :love]}}}}
