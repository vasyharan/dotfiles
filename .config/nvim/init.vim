if &compatible | set nocompatible | endif
let mapleader = ' '

filetype plugin indent on
syntax on

set encoding=utf-8
set clipboard=unnamed                                 " yank and paste with the system clipboard
set number                                            " show line numbers
set linebreak                                         " break lines at word (requires Wrap lines)
set showbreak=+++                                     " wrap-broken line prefix
" set textwidth=100                                   " line wrap (number of cols)
set showmatch                                         " Highlight matching brace
set visualbell                                        " Use visual bell (no beeping)
set directory-=.                                      " don't store swapfiles in the current directory
set hlsearch                                          " hightlight search
set smartcase                                         " case-sensitive search if any caps
set ignorecase                                        " case-insensitive search
set incsearch                                         " search as you type
" set laststatus=2                                    " always show statusline
set scrolloff=3                                       " show context above/below cursorline
set expandtab                                         " expand tabs to spaces
set shiftwidth=2                                      " normal mode indentation commands use 2 spaces
set smartindent                                       " Enable smart-indent
set autoindent                                        " auto-indent new lines
set smarttab                                          " Enable smart-tabs
set softtabstop=2                                     " insert mode tab and backspace use 2 spaces
set tabstop=2                                         " actual tabs occupy 2 characters

"# Advanced
set ruler                                             " show where you are
set list                                              " show trailing whitespace
set listchars=tab:▸\ ,trail:▫

" set undolevels=1000   " Number of undo levels
set backspace=indent,eol,start  " Backspace behaviour

set autoread                                          " reload files when changed on disk
set autowrite                                         " auto write on make
set wildmode=longest:full
set hidden                                            " hidden buffers are the shit!
set inccommand=nosplit                                " preview inc command results
set signcolumn=number
set viewoptions-=options
" set tags=./.git/tags;,tags
" set shortmess-=F
" set shortmess+=c
" set signcolumn=yes
" set backspace=2                                       " Fix broken backspace in some setups
" set backupcopy=yes                                    " see :help crontab
" set showcmd
" set wildignore=log,node_modules,target,tmp,dist,*.rbc
" set wildmenu                                          " show a navigable menu for tab completion
" set modeline                                          " love me some modelines
" set modelines=5
" set updatetime=250
" set viewoptions-=curdir
" set gdefault
" set cmdheight=2


lua require("packages")
lua require("mappings")
lua require("trim").setup({})

" colorscheme {{{
if exists('+termguicolors')
  set termguicolors
endif

if exists("&termguicolors") && exists("&winblend")
  syntax enable
  set termguicolors
  set winblend=0
  set wildoptions=pum
  set pumblend=5
endif

if filereadable(expand("~/.config/nvim/background.vim"))
  source ~/.config/nvim/background.vim
else
  set background=dark
endif

colorscheme gruvbox
" }}}

augroup View
  au!
  let btToIgnore = ['terminal', 'nofile']
  au BufWinLeave ?* if index(btToIgnore, &buftype) < 0 | mkview | endif
  au BufWinEnter ?* if &buftype !~ 'nofile' | silent! loadview | endif
augroup END

nnoremap <silent> <expr> ,t g:NERDTree.IsOpen() ? "\:NERDTreeClose<CR>" : bufexists(expand('%')) ? "\:NERDTreeFind<CR>" : "\:NERDTree<CR>"

augroup NERDTree
  " If another buffer tries to replace NERDTree, put it in the other window, and bring back NERDTree.
  autocmd BufEnter * if bufname('#') =~ 'NERD_tree_\d\+' && bufname('%') !~ 'NERD_tree_\d\+' && winnr('$') > 1 |
        \ let buf=bufnr() | buffer# | execute "normal! \<C-W>w" | execute 'buffer'.buf | endif
  " Exit Vim if NERDTree is the only window left.
  autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
        \ quit | endif
augroup END
