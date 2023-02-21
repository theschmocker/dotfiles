(fn noremap! [modes lhs rhs opts]
  (let [opts (or opts {})]
    (tset opts :remap nil)
    `(vim.keymap.set ,modes ,lhs ,rhs ,opts)))

(fn remap! [modes lhs rhs opts]
  (let [opts (or opts {})]
    (tset opts :remap true)
    `(vim.keymap.set ,modes ,lhs ,rhs ,opts)))

(fn nnoremap! [lhs rhs opts]
  `,(noremap! :n lhs rhs opts))

(fn nmap! [lhs rhs opts]
  `,(remap! :n lhs rhs opts))

(fn inoremap! [lhs rhs opts]
  `,(noremap! :i lhs rhs opts))

(fn tnoremap! [lhs rhs opts]
  `,(noremap! :t lhs rhs opts))

(fn cnoremap! [lhs rhs opts]
  `,(noremap! :c lhs rhs opts))

(fn chunks [n t]
  "Splits a sequential table `t` into a new table of `n`-length chunks"
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

(fn leader-map! [[name prefix global-opts mode] ...]
  `(let [global-opts# (or ,global-opts {})]
     (do
       ((. (require :which-key) :register)
        {,prefix {:name ,name}}
        {:prefix :<leader>
         :mode ,(or mode :n)
         :buffer (or global-opts#.buffer nil)})
       ,(unpack
          (icollect [_ [lhs rhs desc opts] (ipairs [...])]
            (let [opts `((. (require :aniseed.core) :merge) ,{: desc} global-opts# ,(or opts {}))
                  lhs (.. "<leader>" (tostring prefix) lhs)]
              `(noremap! ,(or mode :n) ,lhs ,rhs ,opts)))))))

(fn augroup! [[name opts] ...]
  `(let [group# (vim.api.nvim_create_augroup ,name ,opts)]
     (do
       ,(unpack (icollect [_ form (ipairs [...])]
                  (do
                    (when (= "au!" (tostring (. form 1)))
                      (tset form 3 `((. (require :aniseed.core) :merge) ,(. form 3) {:group group#})))
                    form))))
     group#))

(fn au! [name opts ...]
  (let [body [...]
        mod-opts (if (< 0 (length body))
                     `((. (require :aniseed.core) :merge) ,opts {:callback (fn []
                                                                             ,(unpack body))})
                     opts)]
    `(vim.api.nvim_create_autocmd ,name ,mod-opts)))

{: noremap!
 : remap!
 : nnoremap!
 : nmap!
 : inoremap!
 : tnoremap!
 : cnoremap!
 : set!
 : let-g!
 : leader-map!
 : augroup!
 : au!}

