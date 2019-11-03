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


" non-Vundle config
set t_Co=256
syntax on

let g:nord_italic = 1
let g:nord_italic_comments = 1

colorscheme nord

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
"set showbreak=> " visually indicate a soft wrap"

" Set leader key
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

" Custom mappings
    " Normal mode
    nnoremap - ddp
    nnoremap _ dd2kp
    nnoremap <leader>d dd
    nnoremap <leader>w <c-w><c-w>
    nnoremap H ^
    nnoremap L $
        " Shebang
        nnoremap <leader>sb i#!
        nnoremap <leader>sbb i#!/bin/bash<cr><cr>
        nnoremap <leader>sbp i#!/usr/bin/python3<cr><cr>
        " Movements in wrapped lines
        nnoremap <leader>j gj
        nnoremap <leader>k gk
    " Insert mode
    inoremap jk <esc>
    inoremap <c-u> <esc>viwU<esc>ea
    " Command mode
    cnoremap w!! w !sudo tee > /dev/null %
" Quick .vimrc access
nnoremap <leader>ev :split $MYVIMRC<cr>
" Source .vimrc
nnoremap <leader>sv :source $MYVIMRC<cr>

" Goyo settings
function! s:goyo_enter()
    Limelight
    set number
    set relativenumber
endfunction

function! s:goyo_leave()
    Limelight!
endfunction
autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" CtrlP
let g:ctrlp_map = '<leader>p'

" NerdTree
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()
