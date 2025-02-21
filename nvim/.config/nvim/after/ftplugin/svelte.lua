local toggle_attr_wrapping = require('config.editing-utils').toggle_vue_svelte_attribute_wrap
vim.keymap.set('n', '<localleader>at', toggle_attr_wrapping, { desc = "Toggle attribute wrapping", buffer = 0 })
