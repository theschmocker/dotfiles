" required for Vundle
set nocompatible
filetype off
" Vundle setup
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" plugins go here
Plugin 'VundleVim/Vundle.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'Townk/vim-autoclose'
Plugin 'mxw/vim-jsx'
Plugin 'dylanaraps/wal'
Plugin 'tpope/vim-surround'
Plugin 'jparise/vim-graphql'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'styled-components/vim-styled-components'
Plugin 'mattn/emmet-vim'
Plugin 'prettier/vim-prettier', { 'do': 'yarn install' }
Plugin 'haishanh/night-owl.vim'
"Plugin 'flazz/vim-colorschemes'
Plugin 'arcticicestudio/nord-vim'
Plugin 'posva/vim-vue'
Plugin 'StanAngeloff/php.vim'
Plugin 'nelsyeung/twig.vim'
Plugin 'jwalton512/vim-blade'

let g:vue_disable_pre_processors = 1

if has('nvim')
    Plugin 'Shougo/deoplete.nvim'
    Plugin 'carlitux/deoplete-ternjs'
    Plugin 'othree/yajs.vim'

    let g:deoplete#enable_at_startup = 1
    let g:deoplete#sources#ternjs#tern_bin = '/home/schmo/.config/yarn/global/node_modules/tern/bin/tern'
endif


call vundle#end()
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
nnoremap <SPACE> <Nop>
let mapleader = " "

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
    inoremap <ESC> <Nop>
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

