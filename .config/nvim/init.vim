" let g:python_host_prog = '~/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '~/.pyenv/versions/neovim3/bin/python'

let mapleader = ' '
if &compatible
  set nocompatible
endif

" vim-plug {{{
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
call plug#begin('~/.config/nvim/plugged')
if filereadable(expand("~/.config/nvim/bundles.vim"))
  source ~/.config/nvim/bundles.vim
endif
call plug#end()
" }}}

" sane defaults {{{
filetype plugin indent on
set autoindent
set autoread                                          " reload files when changed on disk
set autowrite                                         " auto write on make
set backspace=2                                       " Fix broken backspace in some setups
set backupcopy=yes                                    " see :help crontab
set clipboard=unnamed                                 " yank and paste with the system clipboard
set directory-=.                                      " don't store swapfiles in the current directory
set encoding=utf-8
set expandtab                                         " expand tabs to spaces
set ignorecase                                        " case-insensitive search
set incsearch                                         " search as you type
set hlsearch                                          " hightlight search
set laststatus=2                                      " always show statusline
set list                                              " show trailing whitespace
set listchars=tab:▸\ ,trail:▫
set number                                            " show line numbers
set ruler                                             " show where you are
set scrolloff=3                                       " show context above/below cursorline
set shiftwidth=4                                      " normal mode indentation commands use 2 spaces
set showcmd
set smartcase                                         " case-sensitive search if any caps
set softtabstop=4                                     " insert mode tab and backspace use 2 spaces
set tabstop=8                                         " actual tabs occupy 8 characters
set wildignore=log,node_modules,target,tmp,dist,*.rbc
set wildmenu                                          " show a navigable menu for tab completion
set wildmode=longest,list,full
set hidden                                            " hidden buffers are the shit!
set modeline                                          " love me some modelines
set modelines=5
set updatetime=250
set inccommand=split                                  " preview inc command results
syntax enable
" }}}

" movement: emacs {{{
imap <C-b> <Left>
imap <C-f> <Right>
imap <A-b> <S-Left>
imap <A-f> <S-Right>
" imap <C-a> <C-o>:call <SID>Home()<CR>
" imap <C-e> <End>
imap <C-d> <Del>
imap <C-h> <BS>

cmap <C-p> <Up>
cmap <C-n> <Down>
cmap <C-b> <Left>
cmap <C-f> <Right>
cmap <M-b> <S-Left>
cmap <M-f> <S-Right>
cmap <C-a> <Home>
cmap <C-e> <End>
cmap <C-p> <Up>
cmap <C-n> <Down>
cnoremap <C-d> <Del>
cnoremap <C-h> <BS>
" }}}
" movement: window {{{
tnoremap <C-w><C-w> <C-\><C-n><C-w>w
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" }}}

" {{{ mappings: quick
nmap vv :vsplit<cr>
" }}}
" {{{ mappings: leader
nmap <nowait> <leader>w :w<cr>
nmap <nowait> <leader>W :wa<cr>
" }}}

" colorscheme {{{
if exists('+termguicolors')
  " let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  " let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
set background=dark
colorscheme gruvbox
" }}}

" augroup: * {{{
augroup default
    au BufReadPre * setlocal foldmethod=marker
augroup END
" }}}
" augroup: markdown {{{
" augroup markdown
    " au BufRead,BufNewFile *.md set filetype=markdown
    " au FileType markdown setlocal spell
" augroup END
" }}}
" augroup: ruby {{{
" augroup ruby
    " au FileType  ruby setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
    " au FileType eruby setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
    " au FileType  yaml setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
" augroup END
" }}}

" functions {{{
" trim trailing whitespace {{{
function! s:TrimWhiteSpace()
    %s/\s\+$//e
endfunction
augroup whitespace
    au BufWritePre *.rb call s:TrimWhiteSpace()
augroup END
" }}}
" }}}
