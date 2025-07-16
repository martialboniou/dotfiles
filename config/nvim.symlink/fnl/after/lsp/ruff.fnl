;; (alpha) an extremely fast Python linter and code formatter, written in Rust
;; (easily installed with `pip` or `brew`)
{:filetypes [:python]
 :cmd [:ruff :server]
 :root_markers [:pyproject.toml :ruff.toml :.ruff.toml :.git]
 :settings {}}
