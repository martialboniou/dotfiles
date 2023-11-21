(λ direct-call [m f]
  `(.. "lua require('" ,(tostring m) "')['" ,(tostring f) "']()"))

{:cal! (λ [m f & args]
         "call a function from its module; these modules are cached; useful for callbacks in keybindings"
         (if args
             `((. (require ,m) ,f) ,(unpack args))
             (direct-call m f)))
 :**! (λ [str times]
        "repeat a literal string"
        (assert-compile (< 0 times))
        (assert-compile (= :string (type str)))
        (let [out []]
          (for [_ 1 times] (table.insert out str))
          `(.. ,(unpack out))))}
