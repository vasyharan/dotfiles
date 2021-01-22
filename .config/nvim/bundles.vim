" fzf {{{
" let $FZF_DEFAULT_OPTS = '--layout=reverse'
let g:fzf_action = { 'ctrl-s': 'split', }
let g:fzf_buffers_jump = 1

Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" command! -bang -nargs=? -complete=dir Files
"     \ call fzf#vim#files(<q-args>, {'options': ['--layout=reverse', '--info=inline']}, <bang>0)
command! -bang -nargs=? -complete=dir DirFiles
      \ call fzf#vim#files(fnamemodify(expand("%"), ':p:h'),
      \ fzf#vim#with_preview(), <bang>0)
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
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
command! Todos :Rg todo[^:]*haran[^:]*


" execute "set <M-p>=\ep"
" nnoremap <M-p> :<c-u>Files<cr>

map <c-p>f :<c-u>Files<cr>
map <c-p>d :<c-u>DirFiles<cr>
map <c-p>b :<c-u>Buffers<cr>
map <c-p>t :<c-u>Todos<cr>
" map <c-p>g :<c-u>Rg<cr>
map <c-p>l :<c-u>BLines<cr>
map <c-p>m :<c-u>Marks<cr>
map <c-p><c-l> :<c-u>Lines<cr>

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
let g:solarized_termcolors=256
Plug 'altercation/vim-colors-solarized'
Plug 'lifepillar/vim-solarized8'
" }}}
" " tmux {{{
" " Plug 'christoomey/vim-tmux-navigator'
" " }}}
" tpope magic {{{
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-vinegar'
" }}}
" git plugins {{{
Plug 'tpope/vim-fugitive'
Plug 'tommcdo/vim-fubitive'
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
      \ 'javascriptreact': [],
      \ 'typescript': [],
      \ 'python': ['flake8', 'pylint'],
      \ }
let g:ale_fix_on_save = 1
let g:ale_fixers = {
      \ 'javascriptreact': [],
      \ 'typescript': [],
      \ 'json': ['prettier'],
      \ 'scala': ['scalafmt'],
      \ 'reason': ['refmt'],
      \ }
      " \ 'python': ['isort', 'black'],
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
nmap <C-w>c <Plug>DWMClose
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
      \ 'colorscheme': 'solarized',
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
Plug 'mbbill/undotree'
nmap <Leader>uu :UndotreeToggle<CR>
" }}}
" vim exchange {{{
Plug 'tommcdo/vim-exchange'
Plug 'machakann/vim-swap'
" }}}
" vim text blocks {{{
Plug 'wellle/targets.vim'
Plug 'bronson/vim-visual-star-search'
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

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <C-space> coc#refresh()
else
  inoremap <silent><expr> <C-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
imap <silent><expr> <C-f> pumvisible() ? coc#_select_confirm() : "<Right>"
" inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
"                               \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


let g:coc_enable_locationlist = 0
autocmd User CocLocationsChange CocList --normal location

nmap <silent> g[ <Plug>(coc-diagnostic-prev)
nmap <silent> g] <Plug>(coc-diagnostic-next)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> <C-w>gd :call CocAction('jumpDefinition', 'vsplit')<cr>
nmap <silent> gT <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nnoremap <silent> gh :call <sid>show_documentation()<cr>
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>gR <Plug>(coc-rename)

" Remap for format selected region
xmap gf  <Plug>(coc-format-selected)
nmap gf  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType scala setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>,ap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>,  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>.   <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold   :call CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OrganizeImports :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Remap for do codeAction of selected region
function! s:cocActionsOpenFromSelected(type) abort
  execute 'CocCommand actions.open ' . a:type
endfunction
xmap <silent> <leader>a :<C-u>execute 'CocCommand actions.open ' . visualmode()<CR>
nmap <silent> <leader>a :<C-u>set operatorfunc=<SID>cocActionsOpenFromSelected<CR>g@

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <C-p>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <C-p>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <C-p>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <C-p>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <C-p>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <C-p>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <C-p>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <C-p>r  :<C-u>CocListResume<CR>

" }}}
" visual tools {{{
Plug 'junegunn/goyo.vim'
Plug 'luochen1990/rainbow'
" }}}

" [java/type]script {{{
Plug 'leafgarland/typescript-vim'
Plug 'peitalin/vim-jsx-typescript'
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
" yaml {{{
Plug 'pedrohdz/vim-yaml-folds'
" }}}
" org-mode {{{
Plug 'tpope/vim-speeddating'
Plug 'vim-scripts/utl.vim'
Plug 'jceb/vim-orgmode'
" }}}
" jsonnet {{{
Plug 'google/vim-jsonnet'
" }}}

" {{{ Vista
let g:vista_default_executive = 'coc'
let g:vista_icon_indent = ["▸ ", ""]
" let g:vista#renderer#enable_icon = 1
" let g:vista#renderer#icons = {
" \   "function": "\uf794",
" \   "variable": "\uf71b",
" \  }
Plug 'liuchengxu/vista.vim'
" }}}
