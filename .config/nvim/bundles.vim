" ctrl-p {{{
Plug 'ctrlpvim/ctrlp.vim'
Plug 'nixprime/cpsm', {'do': 'PY3=ON ./install.sh' }
if executable("rg")
  let g:ctrlp_user_command = 'rg --files --color never --'
endif
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_match_func = {'match': 'cpsm#CtrlPMatch'}
" let g:ctrlp_abbrev = {
      " \ 'gmode': 'i',
      " \ 'abbrevs': [
      " \ {
      " \ 'pattern': ' ',
      " \ 'expanded': '',
      " \ 'mode': 'pfz',
      " \ },
      " \ ]
      " \ }
" }}}
" fzf {{{
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" map <c-p> :<c-u>Files<cr>
" }}}
" theme: gruvbox {{{
Plug 'morhetz/gruvbox'
let g:gruvbox_underline = 1
let g:gruvbox_undercurl = 1
" let g:gruvbox_italic = 1
let g:gruvbox_bold = 1
let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_improved_strings = 0
let g:gruvbox_improved_warnings = 1
" }}}
" tmux {{{
Plug 'christoomey/vim-tmux-navigator'
Plug 'benmills/vimux'
nmap <leader>vt :call VimuxRunCommand('pay test ' . bufname('%'))<cr>
nmap <leader>vl :call VimuxRunCommand('pay test ' . bufname('%') . ' -l ' . line('.'))<cr> 
" }}}
" tpope magic {{{
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
" }}}
" git plugins {{{
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
" }}}
" linter {{{
Plug 'w0rp/ale'
let g:ale_open_list = 1
let g:ale_linters = {'ruby': ['rubocop']}
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
" }}}
" formatter {{{
Plug 'sbdchd/neoformat'
" }}}
" completion {{{
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp' " dependency
" IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-gtags'
" }}}
" easy align {{{
Plug 'junegunn/vim-easy-align'
xmap <leader>= <Plug>(LiveEasyAlign)
nmap <leader>= <Plug>(LiveEasyAlign)
" }}}
" buffer bye {{{
Plug 'moll/vim-bbye'
nmap <Leader>Q :bufdo :Bdelete<CR>
nmap <Leader>q :Bdelete<CR>
cabbrev Bdelete bd
" }}}
" {{{ smartsplit
Plug '~/Workspace/vim-smart-split'
nmap vv <Plug>(SmartSplit)
" }}}
" lightline {{{
Plug 'itchyny/lightline.vim'
set noshowmode
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ 'component_function': {
      \   'readonly': 'LightlineReadonly',
      \   'fugitive': 'LightlineFugitive'
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ }
      " \ 'subseparator': { 'left': '', 'right': '' }
function! LightlineReadonly()
  return &readonly ? '' : ''
endfunction
function! LightlineFugitive()
  if exists('*fugitive#head')
    let branch = fugitive#head()
    return branch !=# '' ? ''.branch : ''
  endif
  return ''
endfunction
" }}}
" parens {{{
Plug 'tpope/vim-unimpaired'
Plug 'kien/rainbow_parentheses.vim'
" }}}
" indent guides {{{
Plug 'Yggdroot/indentLine'
" }}}
" undotree {{{
Plug 'sjl/gundo.vim'
" }}}
" search {{{
Plug 'wincent/ferret'
" }}}

Plug 'bronson/vim-visual-star-search'

" Plug '~/Workspace/vim-commands'

" [java/type]script {{{
Plug 'leafgarland/typescript-vim'
" }}}
