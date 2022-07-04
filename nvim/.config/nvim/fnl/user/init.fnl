(module user.init
    {autoload {wk user.which-key}
     require [user.settings
              user.keymap
              user.plugins
              user.lsp.init
              user.completion
              user.snippets]})

(wk.register-global-mappings)
