(cond ((not (featurep 'booting)) (require 'kernel)))

(unless (locate-library "cl-lib") (error "preamble: cl-lib is required now!")) ; normally installed (using EL-GET in PACKS-EL-GET via WALKER required by KERNEL)

(provide 'preamble)
