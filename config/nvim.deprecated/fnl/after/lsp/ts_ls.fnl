{:filetypes [:typescript
             :typescriptreact
             :typescript.tsx
             :javascript
             :javascriptreact
             :javascrip.jsx]
 :cmd [:typescript-language-server :--stdio]
 :root_markers [:tsconfig.json :jsconfig.json :package.json :.git]
 :single_file_support true
 :settings {:completions {:completeFunctionCalls true}}}
