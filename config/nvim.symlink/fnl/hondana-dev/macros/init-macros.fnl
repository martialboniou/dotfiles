{:cal! (λ [m f & args]
         "call a function from its module; these modules are cached; useful for callbacks in keybindings"
         `((. (require ,m) ,f) (when args ,(unpack args))))
 :**! (λ [str times]
        "repeat a literal string"
        (assert-compile (< 0 times))
        (assert-compile (= :string (type str)))
        (let [out []]
          (for [_ 1 times] (table.insert out str))
          `(.. ,(unpack out))))}
