(fn noremap! [modes lhs rhs c_or_f opts]
  (let [opts (or opts {})]
    (tset opts :remap nil)
    `(vim.keymap.set ,modes ,lhs ,rhs ,c_or_f)))

(fn remap! [modes lhs rhs c_or_f]
  `(vim.keymap.set ,modes ,lhs ,rhs ,c_or_f {:remap true}))

(fn nnoremap! [lhs rhs c_or_f]
  `,(noremap! :n lhs rhs c_or_f))

(fn nmap! [lhs rhs c_or_f]
  `,(remap! :n lhs rhs c_or_f))

(fn inoremap! [lhs rhs c_or_f]
  `,(noremap! :i lhs rhs c_or_f))

(fn tnoremap! [lhs rhs c_or_f]
  `,(noremap! :t lhs rhs c_or_f))

(fn cnoremap! [lhs rhs c_or_f]
  `,(noremap! :c lhs rhs c_or_f))

(fn chunks [n t]
  (let [out []]
    (for [i 1 (length t) n]
      (let [chunk []]
        (for [j i (+ i (- n 1)) 1]
          (table.insert chunk (. t j)))
        (table.insert out chunk)))
    out))

(fn table-set [t ...]
  (let [opts [...]
        opt-pairs (chunks 2 opts)]
    (assert-compile (= 0 (% (length opts) 2)) "")
    `(do
       ,(unpack (icollect [_ [setting value] (ipairs opt-pairs)]
                  `(tset ,t ,(tostring setting) ,value))))))

(fn set! [...]
  `,(table-set `vim.o ...))

(fn let-g! [...]
  `,(table-set `vim.g ...))

{: noremap!
 : remap!
 : nnoremap!
 : nmap!
 : inoremap!
 : tnoremap!
 : cnoremap!
 : set!
 : let-g! }

