" fzf {{{
" let g:fzf_layout = { 'window': 'call OpenFloatingWin()' }
" function! OpenFloatingWin()
"   let height = &lines - 3
"   let width = float2nr(&columns - (&columns * 2 / 10))
"   let col = float2nr((&columns - width) / 2)

"   let opts = {
"         \ 'relative': 'editor',
"         \ 'row': height * 0.2,
"         \ 'col': col + 30,
"         \ 'width': width * 2 / 3,
"         \ 'height': height / 2
"         \ }

"   let buf = nvim_create_buf(v:false, v:true)
"   let win = nvim_open_win(buf, v:true, opts)

"   call setwinvar(win, '&winhl', 'Normal:Pmenu')

"   setlocal
"         \ buftype=nofile
"         \ nobuflisted
"         \ bufhidden=hide
"         \ nonumber
"         \ norelativenumber
"         \ signcolumn=no
" endfunction

let $FZF_DEFAULT_OPTS = '--layout=reverse'
let g:fzf_action = { 'ctrl-s': 'split', }
let g:fzf_buffers_jump = 1

Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
command! -bang -nargs=? -complete=dir DirFiles
      \ call fzf#vim#files(fnamemodify(expand("%"), ':p:h'), <bang>0)
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

function! s:list_buffers()
  redir => list
  silent ls
  redir END
  return split(list, "\n")
endfunction

function! s:delete_buffers(lines)
  execute 'bwipeout' join(map(a:lines, {_, line -> split(line)[0]}))
endfunction

command! BWipeout call fzf#run(fzf#wrap({
  \ 'source': s:list_buffers(),
  \ 'sink*': { lines -> s:delete_buffers(lines) },
  \ 'options': '--multi --reverse --bind ctrl-a:select-all+accept'
\ }))
cabbrev Bwipeout BWipeout

map <c-p>f :<c-u>Files<cr>
map <c-p>d :<c-u>DirFiles<cr>
map <c-p>b :<c-u>Buffers<cr>
" map <c-p>g :<c-u>Rg<cr>
map <c-p>s :<c-u>BLines<cr>
map <c-p><c-s> :<c-u>Lines<cr>

nnoremap <leader>* :Rg <C-R><C-W><CR>
vnoremap <leader>* :<C-u>call VisualStarSearchSet('/')<CR>:Rg <C-R><C-/><CR>
" }}}
" theme: gruvbox {{{
Plug 'morhetz/gruvbox'
let g:gruvbox_underline = 1
let g:gruvbox_undercurl = 1
" let g:gruvbox_italic = 1
let g:gruvbox_bold = 1
let g:gruvbox_contrast_dark = "medium"
let g:gruvbox_contrast_light = "medium"
let g:gruvbox_improved_strings = 0
let g:gruvbox_improved_warnings = 1
" }}}
" theme: iosvkem {{{
Plug 'neutaaaaan/iosvkem'
" }}}
" theme: solarized {{{
" let g:solarized_termcolors=256
Plug 'altercation/vim-colors-solarized'
" }}}
" tmux {{{
" Plug 'christoomey/vim-tmux-navigator'
" }}}
" tpope magic {{{
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-obsession'
" }}}
" git plugins {{{
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'
Plug 'samoshkin/vim-mergetool'

let g:mergetool_layout = 'mr,b'
let g:mergetool_prefer_revision = 'local'
nmap <leader>mm <plug>(MergetoolToggle)

function s:on_mergetool_set_layout(split)
  if a:split["layout"] ==# 'mr,b' && a:split["split"] ==# 'b'
    set nodiff
    set syntax=on
    resize 15
  endif
endfunction
let g:MergetoolSetLayoutCallback = function('s:on_mergetool_set_layout')

let g:github_enterprise_urls = ['https://git.corp.stripe.com']
" }}}
" linter {{{
if !exists('g:vscode')
Plug 'w0rp/ale'

" let g:ale_open_list = 1
" let g:ale_set_loclist = 1
let g:ale_disable_lsp = 1
" let g:ale_set_quickfix = 1
let g:ale_linters = {
      \ 'ruby': ['rubocop', 'sorbet'],
      \ 'javascriptreact': ['eslint'],
      \ 'typescript': ['eslint'],
      \ 'python': ['flake8'],
      \ }
let g:ale_fix_on_save = 1
let g:ale_fixers = { 
      \ 'javascriptreact': ['prettier'],
      \ 'json': ['prettier'],
      \ 'scala': ['scalafmt'],
      \ 'reason': ['refmt'],
      \ 'python': ['black'],
      \ }
let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
let g:ale_scala_scalafmt_executable = 'scalafmt-native'

let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
" let g:ale_command_wrapper = '/Users/haran/.bin/ale_command_wrapper.sh'

let g:ale_pattern_options = {
\ 'pay-server\/.*\.rb$': {
\   'ale_linters': ['rubocop', 'sorbet-payserver'],
\   'ale_ruby_rubocop_executable': 'scripts/bin/rubocop-daemon/rubocop',
\   'ale_set_quickfix': 1,
\ },
\}
let g:ale_pattern_options_enabled = 1

nmap <silent> <leader>d <Plug>(ale_go_to_definition)
endif
" }}}
" formatter {{{
" Plug 'sbdchd/neoformat'
" }}}
" completion {{{
if !exists('g:vscode')
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp' " dependency
" IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-gtags'
endif
" }}}
" easy align {{{
Plug 'junegunn/vim-easy-align'
xmap <leader>= <Plug>(LiveEasyAlign)
nmap <leader>= <Plug>(LiveEasyAlign)
" }}}
" buffer/window management {{{
Plug 'spolu/dwm.vim'
let g:dwm_map_keys = 0
let g:dwm_master_pane_width="66"
nmap <C-w>v <Plug>DWMNew
nmap <C-w>d <Plug>DWMClose
nmap <Leader><Leader> <Plug>DWMFocus
nmap <C-j> <C-W>w
nmap <C-k> <C-W>W
nmap <Leader><C-j> <Plug>DWMRotateCounterclockwise
nmap <Leader><C-k> <Plug>DWMRotateClockwise

augroup DWM
  au!
  autocmd VimResized * call DWM_ResizeMasterPaneWidth()
augroup END

Plug 'moll/vim-bbye'
nmap <Leader>Q :bufdo :Bdelete<CR>
nmap <Leader>q :Bdelete<CR>
cabbrev bd Bdelete
" }}}
" {{{ smartsplit
" Plug '~/Workspace/vim-smart-split'
" nmap vv <Plug>(SmartSplit)
" }}}
" lightline {{{
if !exists('g:vscode')
Plug 'itchyny/lightline.vim'
Plug 'maximbaz/lightline-ale'
set noshowmode

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

function! LightlineObsession()
    return '%{ObsessionStatus()}'
endfunction

let g:lightline = {
      \ 'mode_map': {
      \ 'n' : 'No',
      \ 'i' : 'In',
      \ 'R' : 'Re',
      \ 'v' : 'Vc',
      \ 'V' : 'Vl',
      \ "\<C-v>": 'Vb',
      \ 'c' : 'Co',
      \ 's' : 'Sc',
      \ 'S' : 'Sl',
      \ "\<C-s>": 'Sb',
      \ 't': 'T',
      \ },
      \ 'colorscheme': 'gruvbox',
      \ 'component_function': {
      \   'readonly': 'LightlineReadonly',
      \   'fugitive': 'LightlineFugitive',
      \   'cocstatus': 'coc#status',
      \   'currentfunction': 'CocCurrentFunction',
      \   'obsession': 'ObsessionStatus',
      \ },
      \ 'component_expand': {
      \   'linter_checking': 'lightline#ale#checking',
      \   'linter_infos': 'lightline#ale#infos',
      \   'linter_warnings': 'lightline#ale#warnings',
      \   'linter_errors': 'lightline#ale#errors',
      \   'linter_ok': 'lightline#ale#ok',
      \ },
      \ 'component_type': {
      \   'linter_checking': 'right',
      \   'linter_infos': 'right',
      \   'linter_warnings': 'warning',
      \   'linter_errors': 'error',
      \   'linter_ok': 'right',
      \ },
      \ 'active': {
      \   'left': [
      \     [ 'mode', 'paste' ],
      \     [ 'readonly', 'obsession', 'filename', 'modified', 'cocstatus', 'currentfunction' ],
      \   ],
      \   'right': [
      \     [ 'lineinfo' ],
      \     [ 'percent' ],
      \     [ 'fileformat', 'fileencoding', 'filetype' ],
      \     [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos' ],
      \   ],
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' }
      \ }
      " \ 'active': { 'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_infos', 'linter_ok' ]] },
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
endif
" }}}
" parens {{{
if !exists('g:vscode')
Plug 'tpope/vim-unimpaired'
Plug 'kien/rainbow_parentheses.vim'
endif
" }}}
" indent guides {{{
if !exists('g:vscode')
let g:indentLine_concealcursor = 'n'
Plug 'Yggdroot/indentLine'
endif
" }}}
" undotree {{{
Plug 'sjl/gundo.vim'
nmap <Leader>uu :GundoToggle<CR>
" }}}

" vim exchange {{{
Plug 'tommcdo/vim-exchange'
Plug 'machakann/vim-swap'
" }}}
" vim text blocks {{{
Plug 'wellle/targets.vim'
Plug 'bronson/vim-visual-star-search'
" }}}

" nvim-lsp {{{
" Plug 'neovim/nvim-lsp'

" nnoremap <silent> gd     <cmd>lua vim.lsp.buf.definition()<CR>
" nnoremap <silent> gD     <cmd>lua vim.lsp.buf.declaration()<CR>
" nnoremap <silent> gr     <cmd>lua vim.lsp.buf.references()<CR>
" nnoremap <silent> K      <cmd>lua vim.lsp.buf.hover()<CR>
" nnoremap <silent> <c-h>  <cmd>lua vim.lsp.buf.signature_help()<CR>
" nnoremap <silent> gT     <cmd>lua vim.lsp.buf.type_definition()<CR>
" nnoremap <silent> g<c-d> <cmd>lua vim.lsp.buf.implementation()<CR>
" }}}
" vim-lsc {{{
" Plug 'natebosch/vim-lsc'
" let g:lsc_server_commands = {
"  \  'ruby': {
"  \    'name': 'sorbet-payserver',
"  \    'command': '/Users/haran/.bin/sorbet-payserver',
"  \    'log_level': -1,
"  \    'suppress_stderr': v:true,
"  \  },
"  \ 'python': 'pyls',
"  \ 'javascript': 'npx flow lsp',
"  \ 'javascriptreact': 'npx flow lsp',
"  \}
" let g:lsc_trace_level = 'verbose'
" let g:lsc_auto_map = {
"     \ 'GoToDefinition': 'gd',
"     \ 'GoToDefinitionSplit': '<C-W>gd',
"     \ 'FindReferences': 'gr',
"     \ 'NextReference': '<C-n>',
"     \ 'PreviousReference': '<C-p>',
"     \ 'FindImplementations': 'gI',
"     \ 'FindCodeActions': 'ga',
"     \ 'Rename': 'gR',
"     \ 'ShowHover': v:true,
"     \ 'DocumentSymbol': 'go',
"     \ 'WorkspaceSymbol': 'gS',
"     \ 'SignatureHelp': 'gm',
"     \ 'Completion': 'completefunc',
"     \}
" }}}
" coc.vim {{{
Plug 'neoclide/coc.nvim', {'branch': 'release'}

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gT <Plug>(coc-type-definition)
nmap <silent> g<c-d> <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

inoremap <silent><expr> <c-space> coc#refresh()
nnoremap <silent> K :call <SID>show_documentation()<CR>
autocmd CursorHold * silent call CocActionAsync('highlight')

" " Remap for rename current word
" nmap <leader>rn <Plug>(coc-rename)

" " Remap for format selected region
" xmap <leader>f  <Plug>(coc-format-selected)
" nmap <leader>f  <Plug>(coc-format-selected)

" augroup mygroup
"   autocmd!
"   " Setup formatexpr specified filetype(s).
"   autocmd FileType scala setl formatexpr=CocAction('formatSelected')
"   " Update signature help on jump placeholder
"   autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
" augroup end

" " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
" xmap <leader>a  <Plug>(coc-codeaction-selected)
" nmap <leader>a  <Plug>(coc-codeaction-selected)
" " Remap for do codeAction of current line
" nmap <leader>ac  <Plug>(coc-codeaction)
" " Fix autofix problem of current line
" nmap <leader>qf  <Plug>(coc-fix-current)

" " Use `:Format` to format current buffer
" command! -nargs=0 Format :call CocAction('format')

" " Use `:Fold` to fold current buffer
" command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" " Trigger for code actions
" " Make sure `"codeLens.enable": true` is set in your coc config
" nnoremap <leader>cl :<C-u>call CocActionAsync('codeLensAction')<CR>

" " Show all diagnostics
" nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" " Manage extensions
" nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" " Show commands
" nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" " Find symbol of current document
" nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" " Search workspace symbols
" nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
" nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" " Do default action for previous item.
" nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" " Resume latest coc list
" nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

" " Notify coc.nvim that <enter> has been pressed.
" " Currently used for the formatOnType feature.
" inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"       \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" " Toggle panel with Tree Views
" nnoremap <silent> <space>t :<C-u>CocCommand metals.tvp<CR>
" " Toggle Tree View 'metalsPackages'
" nnoremap <silent> <space>tp :<C-u>CocCommand metals.tvp metalsPackages<CR>
" " Toggle Tree View 'metalsCompile'
" nnoremap <silent> <space>tc :<C-u>CocCommand metals.tvp metalsCompile<CR>
" " Toggle Tree View 'metalsBuild'
" nnoremap <silent> <space>tb :<C-u>CocCommand metals.tvp metalsBuild<CR>
" " Reveal current current class (trait or object) in Tree View 'metalsPackages'
" nnoremap <silent> <space>tf :<C-u>CocCommand metals.revealInTreeView metalsPackages<CR>

" }}}

Plug 'liuchengxu/vista.vim'
nmap <Leader>vv :Vista!!<CR>


" [java/type]script {{{
Plug 'leafgarland/typescript-vim'
" }}}
" puppet {{{
Plug 'rodjek/vim-puppet'
" }}}
" ledger {{{
" Plug 'nathangrigg/vim-beancount'
" Plug 'ledger/vim-ledger'
" }}}
" reasonML {{{
Plug 'reasonml-editor/vim-reason-plus'
" }}}
" markdown {{{
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'
" }}}

" org-mode {{{
Plug 'tpope/vim-speeddating'
Plug 'vim-scripts/utl.vim'
Plug 'jceb/vim-orgmode'
" }}}

Plug 'junegunn/goyo.vim'
Plug 'luochen1990/rainbow'
