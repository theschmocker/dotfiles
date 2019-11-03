" required for Vundle
set nocompatible
filetype off

" Install vim-plug if it isn't already
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" vim-plug setup
call plug#begin()
" Colorschemes
Plug 'flazz/vim-colorschemes'
Plug 'haishanh/night-owl.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'dylanaraps/wal'

" Syntax/Language
Plug 'pangloss/vim-javascript'
Plug 'mxw/vim-jsx'
Plug 'jparise/vim-graphql'
Plug 'hail2u/vim-css3-syntax'
Plug 'posva/vim-vue'
Plug 'StanAngeloff/php.vim'
Plug 'nelsyeung/twig.vim'
Plug 'jwalton512/vim-blade'
Plug 'burner/vim-svelte'
Plug 'styled-components/vim-styled-components'
Plug 'fatih/vim-go'

" Completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'phpactor/phpactor', {'for': 'php', 'do': 'composer install'}

" Utilities
Plug 'Townk/vim-autoclose'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'mattn/emmet-vim'
Plug 'liuchengxu/vim-which-key'
Plug 'scrooloose/nerdtree'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

call plug#end()
filetype plugin indent on

let g:vue_disable_pre_processors = 1


" non-Vundle config

if (has("termguicolors"))
  set termguicolors
endif

let $NVIM_TUI_ENABLE_TRUE_COLOR=1

syntax on

colorscheme night-owl

" JSX in .js files
let g:jsx_ext_required = 0

set number
set relativenumber
set hidden
set tabstop=2
set shiftwidth=2
set expandtab
set ruler
set linebreak
set wildmenu
set wildmode=list:longest,full
set timeoutlen=200
"set showbreak=> " visually indicate a soft wrap"

" Set leader key
let g:mapleader = "\<Space>"
"let g:maplocalleader = ','

call which_key#register('<Space>', "g:which_key_map")

nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
"nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

" Custom mappings
" Normal mode
nnoremap H ^
nnoremap L $
" Movements in wrapped lines
nnoremap <leader>j gj
nnoremap <leader>k gk
" Insert mode
inoremap jk <esc>
" Command mode
cnoremap w!! w !sudo tee > /dev/null %

tnoremap jk <c-\><c-n>
tnoremap <esc> <c-\><c-n>
tnoremap <M-[> <Esc>

" NerdTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" vim-which-key config
let g:which_key_map={}
let g:which_key_map['w'] = {
      \ 'name' : '+windows' ,
      \ 'w' : ['<C-W>w'     , 'other-window']          ,
      \ 'd' : ['<C-W>c'     , 'delete-window']         ,
      \ '-' : ['<C-W>s'     , 'split-window-below']    ,
      \ '|' : ['<C-W>v'     , 'split-window-right']    ,
      \ '2' : ['<C-W>v'     , 'layout-double-columns'] ,
      \ 'h' : ['<C-W>h'     , 'window-left']           ,
      \ 'j' : ['<C-W>j'     , 'window-below']          ,
      \ 'l' : ['<C-W>l'     , 'window-right']          ,
      \ 'k' : ['<C-W>k'     , 'window-up']             ,
      \ 'H' : ['<C-W>5<'    , 'expand-window-left']    ,
      \ 'J' : ['resize +5'  , 'expand-window-below']   ,
      \ 'L' : ['<C-W>5>'    , 'expand-window-right']   ,
      \ 'K' : ['resize -5'  , 'expand-window-up']      ,
      \ '=' : ['<C-W>='     , 'balance-window']        ,
      \ 's' : ['<C-W>s'     , 'split-window-below']    ,
      \ 'v' : ['<C-W>v'     , 'split-window-below']    ,
      \ '?' : ['Windows'    , 'fzf-window']            ,
      \ }
let g:which_key_map['n'] = {
      \ 'name' : '+NERDTree',
      \ 'o' : ['NERDTree', 'open nerdtree'],
      \ 'f' : ['NERDTreeFocus', 'focus nerdtree'],
      \ 'c' : ['NERDTreeClose', 'close nerdtree'],
      \ }

let g:which_key_map['v'] = {
      \ 'name' : '+vimrc',
      \ 'e' : [':split ~/.vimrc', 'edit .vimrc'],
      \ 's' : [':source $MYVIMRC', 'source .vimrc/init.vim'],
      \ 'n' : [':split $MYVIMRC', 'edit init.vim if neovim, otherwise .vimrc'],
      \ }

let g:which_key_map['f'] = {
      \ 'name' : '+fzf',
      \ 'f' : [':Files', 'fuzzy search files'],
      \ 'b' : [':Buffers', 'fuzzy search buffers'],
      \ 't' : [':Tags', 'fuzzy search tags'],
      \ }

" Emmet Config
" let g:user_emmet_leader_key='<C-Z>'

