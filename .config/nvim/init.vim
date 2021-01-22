let g:python_host_prog = '~/.pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = '~/.pyenv/versions/neovim3/bin/python'

if &compatible | set nocompatible | endif
let mapleader = ' '

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
if filereadable(expand("~/.config/nvim/init.local.vim"))
  source ~/.config/nvim/init.local.vim
endif
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
set smarttab
set wildignore=log,node_modules,target,tmp,dist,*.rbc
set wildmenu                                          " show a navigable menu for tab completion
set wildmode=longest:full,full
set hidden                                            " hidden buffers are the shit!
set modeline                                          " love me some modelines
set modelines=5
set updatetime=250
set inccommand=nosplit                                " preview inc command results
set tags=./.git/tags;,tags
set shortmess-=F
set shortmess+=c
set signcolumn=yes
set viewoptions-=options
set viewoptions-=curdir
set gdefault
set cmdheight=2
if has("patch-8.1.1564")
  set signcolumn=number
else
  set signcolumn=yes
endif

syntax enable
" }}}

" prefer rg if available
if executable("rg") | set grepprg=rg\ --color\ never | endif

" colorscheme {{{
if exists('+termguicolors')
  set termguicolors
endif
if filereadable(expand("~/.config/nvim/background.vim"))
  source ~/.config/nvim/background.vim
else
  set background=dark
endif
colorscheme solarized8
" }}}

" movement: emacs {{{
imap <C-b> <Left>
" mapped to complete when pumvisible in bundles
" inoremap <C-f> <Right>
imap <A-b> <S-Left>
imap <A-f> <S-Right>
" imap <C-a> <C-o>:call <SID>Home()<CR>
" imap <C-e> <End>
imap <C-d> <Del>
imap <C-h> <BS>

cmap <C-k> <Up>
cmap <C-j> <Down>
cnoremap <C-h> <Left>
cmap <C-l> <Right>
cmap <M-h> <S-Left>
cmap <M-l> <S-Right>
cmap <C-a> <Home>
cmap <C-e> <End>
cnoremap <C-d> <Del>
" cnoremap <C-h> <BS>
" }}}
" movement: window {{{
tnoremap <C-w><C-w> <C-\><C-n><C-w>w
tnoremap <C-j> <C-\><C-n><C-w>w
tnoremap <C-k> <C-\><C-n><C-w>W
" tnoremap <C-h> <C-\><C-n><C-w>h
" tnoremap <C-j> <C-\><C-n><C-w>j
" tnoremap <C-k> <C-\><C-n><C-w>k
" tnoremap <C-l> <C-\><C-n><C-w>l
" nnoremap <C-h> <C-w>h
" nnoremap <C-j> <C-w>j
" nnoremap <C-k> <C-w>k
" nnoremap <C-l> <C-w>l
" }}}

" {{{ netrw
let g:netrw_liststyle = 3
let g:netrw_banner = 0
" }}}

" {{{ mappings: leader
map <leader><bs> <c-^>
nmap <nowait> <leader>w :w<cr>
nmap <nowait> <leader>W :wa<cr>
" }}}
" {{{ mappings: term
tnoremap <C-g> <C-\><C-n>
" }}}

" augroup: * {{{
augroup default
  au!
  " au BufReadPre * setlocal foldmethod=marker
  " au BufWinLeave ?* if &buftype !~ 'nofile' | mkview | endif
  let btToIgnore = ['terminal', 'nofile']
  au BufWinLeave ?* if index(btToIgnore, &buftype) < 0 | mkview | endif
  au BufWinEnter ?* if &buftype !~ 'nofile' | silent! loadview | endif
augroup END
" }}}
" augroup: markdown {{{
" augroup markdown
"   au!
"   au BufRead,BufNewFile *.md set filetype=markdown
"   au FileType markdown setlocal spell
" augroup END
" }}}
" augroup: ruby {{{
augroup ruby
  au!
  au FileType  ruby setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
  au FileType eruby setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
  au FileType  yaml setlocal shiftwidth=2 softtabstop=2 tabstop=2 expandtab
augroup END
" }}}
" augroup: json {{{
augroup json
  autocmd FileType json syntax match Comment +\/\/.\+$+
augroup END
" }}}
" augroup: javascript {{{
augroup javascript
  autocmd FileType javascript let b:coc_root_patterns = ['.flowconfig', 'package.json', '.git', '.hg']
  autocmd FileType typescript let b:coc_root_patterns = ['.flowconfig', 'package.json', '.git', '.hg']
augroup END
" }}}
" augroup: typescript {{{
augroup typescript
  autocmd FileType typescript let b:coc_root_patterns = ['.flowconfig', 'package.json', '.git', '.hg']
augroup END
" }}}
" augroup: term {{{
augroup Term
  au!
  au TermOpen * setlocal nonumber norelativenumber
  autocmd BufWinEnter,WinEnter term://*/bin/zsh startinsert
  autocmd BufLeave term://* stopinsert
augroup END
" }}}

" functions {{{
" trim trailing whitespace {{{
function! s:trim_white_space()
  %s/\s\+$//e
endfunction
command! TrimWhiteSpace call s:trim_white_space()
augroup whitespace
  au!
  au BufWritePre *.rb call s:trim_white_space()
augroup END
" }}}
" }}}
