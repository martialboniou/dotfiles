;; break if no 'MARS/LOCAL-ROOT-DIR

(unless (boundp 'mars/local-root-dir)
  (error "BAD ACCESS"))

(provide 'noaccess)
