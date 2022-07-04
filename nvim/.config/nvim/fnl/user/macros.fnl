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

{: noremap! : remap! : nnoremap! : nmap! : inoremap! : tnoremap! : cnoremap!}

