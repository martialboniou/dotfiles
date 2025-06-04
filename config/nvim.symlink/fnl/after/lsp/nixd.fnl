;; legacy non-NixOS install: `nix-env -iA nixpkgs.nixd`
;; NOTE: no need to set the formatter for `nixd`
;; `nixfmt` will be called by `conform.nvim`; see hondana-dev.plugins.formatters`
{:filetypes [:nix] :cmd [:nixd] :root_markers [:flake.nix :.git]}
