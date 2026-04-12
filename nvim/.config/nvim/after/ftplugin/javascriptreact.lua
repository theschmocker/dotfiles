local toggle_attr_wrapping = require('config.editing-utils').toggle_jsx_attributes_wrap
vim.keymap.set('n', '<localleader>at', toggle_attr_wrapping, { desc = "Toggle JSX attribute wrapping", buffer = 0 })
