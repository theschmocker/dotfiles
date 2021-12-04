local wk = require'which-key'

wk.register({
  ['<leader>'] = { "<cmd>Telescope find_files<cr>", "Find File" },
  w = {
    name = '+windows',
    w = { '<C-W>w' , 'other-window' },
    d = { '<C-W>c' , 'delete-window' },
    ['-'] = { '<C-W>s' , 'split-window-below' },
    h = { '<C-W>h' , 'window-left' },
    j = { '<C-W>j' , 'window-below' },
    l = { '<C-W>l' , 'window-right' } ,
    k = { '<C-W>k' , 'window-up' },
    H = { '<C-W>5<' , 'expand-window-left' },
    J = { ':resize +5' , 'expand-window-below' },
    L = { '<C-W>5>' , 'expand-window-right' },
    K = { ':resize -5' , 'expand-window-up' },
    ['='] = { '<C-W>=' , 'balance-window' },
    s = { '<C-W>s' , 'split-window-below' },
    v = { '<C-W>v' , 'split-window-vertical' },
  },

  f = {
    name = "+find/file",
    f = { "<cmd>Telescope find_files<cr>", "Find File" },
    r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
    g = { "<cmd>Telescope live_grep<cr>", "Grep" },
    n = { "<cmd>enew<cr>", "New File" },
    p = { "<cmd>lua require'telescope.builtin'.find_files({ cwd = '~/dotfiles' })<cr>", 'Config files' },
    c = { "<cmd>Telescope commands<cr>", "Commands" },
    b = { "<cmd>Telescope buffers<cr>", "Buffers" },
    [':'] = { "<cmd>Telescope command_history<cr>", "Command history" },
  },

  t = {
    name = '+tabs',
    n = { '<cmd>tabn<cr>', 'next tab' },
    p = { '<cmd>tabp<cr>', 'previous tab' },
    c = { '<cmd>tabclose<cr>', 'close tab' },
    t = { '<cmd>tabe | term<cr>', 'open a terminal in a new tab' },
  },

  b = {
    name = '+buffers',
    d = { '<cmd>bd<cr>', 'delete buffer' },
    n = { '<cmd>bn<cr>', 'next buffer' },
    p = { '<cmd>bp<cr>', 'previous buffer' },
  },

  c = {
    name = '+code-actions',
    a = { ':CocAction<cr>', 'lsp code actions' },
    f = { '<Plug>(coc-references)', 'find references' },
    r = { '<Plug>(coc-rename)', 'rename current symbol' },
    R = { '<Plug>(coc-refactor)', 'refactor current symbol' },
    d = { '<Plug>(coc-definition)', 'jump to definition' },
    D = { '<Plug>(coc-declaration)', 'jump to declaration' },
    i = { '<Plug>(coc-implementation)', 'jump to implmentation' },
  },
}, { prefix = "<leader>", mode = "n" })

