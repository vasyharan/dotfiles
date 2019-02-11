" colors {{{
Plug 'morhetz/gruvbox'
let g:gruvbox_italic=1
let g:gruvbox_contrast_dark='hard'
" }}}
" easy align {{{
Plug 'junegunn/vim-easy-align'
xmap <Leader>= <Plug>(EasyAlign)
nmap <Leader>= <Plug>(EasyAlign)
" }}}
" nerd commenter {{{
Plug 'scrooloose/nerdcommenter'
let g:NERDSpaceDelims = 1
let g:NERDDefaultMappings = 0
nn <Leader>; :call NERDComment('n', 'Toggle')<CR>
xn <Silent> <Leader>; :call NERDComment('x', 'Toggle')<CR>
" }}}
" git plugins {{{
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
" }}}
" other plugins {{{
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-unimpaired'
Plug 'kien/rainbow_parentheses.vim'
Plug 'Yggdroot/indentLine'

" Plug 'justinmk/vim-dirvish'
" }}}
" linter {{{
Plug 'w0rp/ale'
let g:ale_open_list = 1
let g:ale_linters = {'ruby': ['rubocop']}
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
" }}}
" airline {{{
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
" let g:airline_left_sep=''
" let g:airline_right_sep=''
" }}}
" vim-tmux-navigator {{{
Plug 'christoomey/vim-tmux-navigator'
" }}}
" buffer bye {{{
Plug 'moll/vim-bbye'
nmap <Leader>Q :bufdo :Bdelete<CR>
nmap <Leader>q :Bdelete<CR>
cabbrev Bdelete bd
" }}}
" ctrl-p {{{
Plug 'kien/ctrlp.vim'
Plug 'nixprime/cpsm', { 'do': 'PY3=ON ./install.sh' }
let g:ctrlp_match_func = {'match': 'cpsm#CtrlPMatch'}
" }}}
" YouCompleteMe {{{
Plug 'Valloric/YouCompleteMe'
" }}}
" Languages {{{
Plug 'leafgarland/typescript-vim'
Plug 'prettier/vim-prettier', {
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
" }}}
