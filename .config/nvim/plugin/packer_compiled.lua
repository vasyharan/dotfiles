-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/Users/haran/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/haran/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/haran/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/haran/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/haran/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  NeoSolarized = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/NeoSolarized"
  },
  ["completion-nvim"] = {
    config = { "\27LJ\2\n◊\2\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0∑\2      \" Use <Tab> and <S-Tab> to navigate through popup menu\n      inoremap <expr> <Tab>   pumvisible() ? \"\\<C-n>\" : \"\\<Tab>\"\n      inoremap <expr> <S-Tab> pumvisible() ? \"\\<C-p>\" : \"\\<S-Tab>\"\n\n      imap <silent> <c-p> <Plug>(completion_trigger)\n      imap <silent> <c-n> <Plug>(completion_trigger)\n      \bcmd\bvim\0" },
    loaded = true,
    needs_bufread = false,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/opt/completion-nvim"
  },
  ["csv.vim"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/csv.vim"
  },
  gruvbox = {
    loaded = true,
    needs_bufread = false,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/opt/gruvbox"
  },
  kommentary = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/kommentary"
  },
  ["lsp-status.nvim"] = {
    after = { "nvim-lspconfig", "nvim-metals" },
    loaded = true,
    only_config = true
  },
  ["lspsaga.nvim"] = {
    config = { "\27LJ\2\nË\t\0\0\4\0\17\00016\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\1\3\0014\3\0\0B\1\2\1\18\1\0\0005\3\4\0B\1\2\1\18\1\0\0005\3\5\0B\1\2\1\18\1\0\0005\3\6\0B\1\2\1\18\1\0\0005\3\a\0B\1\2\1\18\1\0\0005\3\b\0B\1\2\1\18\1\0\0005\3\t\0B\1\2\1\18\1\0\0005\3\n\0B\1\2\1\18\1\0\0005\3\v\0B\1\2\1\18\1\0\0005\3\f\0B\1\2\1\18\1\0\0005\3\r\0B\1\2\1\18\1\0\0005\3\14\0B\1\2\1\18\1\0\0005\3\15\0B\1\2\1\18\1\0\0005\3\16\0B\1\2\1K\0\1\0\1\4\1\0\6n\a]eH<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<cr>\vsilent\2\1\4\1\0\6n\a[eH<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<cr>\vsilent\2\1\4\1\0\6n\agHE<cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<cr>\vsilent\2\1\4\1\0\6n\aghG<cmd>lua require'lspsaga.diagnostic'.show_cursor_diagnostics()<cr>\vsilent\2\1\4\1\0\6v\ag.C<cmd>lua require('lspsaga.codeaction').range_code_action()<cr>\vsilent\2\1\4\1\0\6n\ag.=<cmd>lua require('lspsaga.codeaction').code_action()<cr>\vsilent\2\1\4\1\0\6n\n<C-b>F<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>\vsilent\2\1\4\1\0\6n\n<C-f>E<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>\vsilent\2\1\4\0\0\6n\agp@<cmd>lua require'lspsaga.provider'.preview_definition()<cr>\1\4\1\0\6i\n<C-k>A<cmd>lua require'lspsaga.signaturehelp'.signature_help()<cr>\vsilent\2\1\4\1\0\6n\6K;<cmd>lua require'lspsaga.hover'.render_hover_doc()<cr>\vsilent\2\1\4\1\0\6n\agR2<cmd>lua require'lspsaga.rename'.rename()<cr>\vsilent\2\1\4\1\0\6n\agf:<cmd>lua require('lspsaga.provider').lsp_finder()<cr>\vsilent\2\18init_lsp_saga\flspsaga\vkeymap\frequire\0" },
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/lspsaga.nvim"
  },
  ["lualine.nvim"] = {
    config = { "\27LJ\2\në\6\0\0\t\0(\0?6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\t\0005\4\3\0005\5\4\0=\5\5\0045\5\6\0=\5\a\0044\5\0\0=\5\b\4=\4\n\0035\4\r\0004\5\3\0006\6\0\0'\b\v\0B\6\2\0029\6\f\6>\6\1\5=\5\14\0045\5\15\0=\5\16\0045\5\17\0=\5\18\0045\5\26\0005\6\19\0005\a\20\0=\a\21\0065\a\22\0=\a\23\6>\6\1\0056\6\0\0'\b\24\0B\6\2\0029\6\25\6>\6\2\5=\5\27\0045\5\28\0=\5\29\0045\5\30\0=\5\31\4=\4 \0035\4!\0004\5\0\0=\5\14\0044\5\0\0=\5\16\0045\5\"\0=\5\18\0045\5#\0=\5\27\0044\5\0\0=\5\29\0044\5\0\0=\5\31\4=\4$\0034\4\0\0=\4%\0035\4&\0=\4'\3B\1\2\1K\0\1\0\15extensions\1\4\0\0\rfugitive\rnerdtree\rquickfix\ftabline\22inactive_sections\1\2\0\0\rlocation\1\2\0\0\rfilename\1\0\0\rsections\14lualine_z\1\2\0\0\rlocation\14lualine_y\1\2\0\0\rprogress\14lualine_x\1\5\0\0\0\0\rencoding\rfiletype\rprogress\26lsp-status/statusline\fsymbols\1\0\4\nerror\tÔÜà \thint\tÔÉ´ \twarn\tÔÅ± \tinfo\tÔëâ \fsources\1\2\0\0\rnvim_lsp\1\2\0\0\16diagnostics\14lualine_c\1\2\0\0\rfilename\14lualine_b\1\2\0\0\vbranch\14lualine_a\1\0\0\tmode\14nvim-mode\foptions\1\0\0\23disabled_filetypes\25component_separators\1\3\0\0\bÓÇ±\bÓÇ≥\23section_separators\1\3\0\0\bÓÇ∞\bÓÇ≤\1\0\2\ntheme\fgruvbox\18icons_enabled\2\nsetup\flualine\frequire\0" },
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/lualine.nvim"
  },
  nerdtree = {
    config = { "\27LJ\2\n*\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0\v      \bcmd\bvim\0" },
    loaded = true,
    needs_bufread = false,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/opt/nerdtree"
  },
  ["nvim-lspconfig"] = {
    config = { "\27LJ\2\nc\0\1\6\0\5\0\t6\1\0\0'\3\1\0B\1\2\0029\2\2\1'\4\3\0'\5\4\0B\2\3\2\18\4\0\0D\2\2\0\19pyproject.toml\t.git\17root_pattern\19lspconfig.util\frequireõ\15\1\0\14\0E\0ñ\0016\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0026\2\3\0009\2\4\0029\2\5\0026\3\3\0009\3\4\0039\3\a\0036\5\3\0009\5\4\0059\5\b\0059\5\t\0055\6\n\0005\a\v\0=\a\f\6B\3\3\2=\3\6\0029\2\r\0009\2\14\0025\4\15\0006\5\16\0=\5\16\0046\5\3\0009\5\17\5'\a\18\0004\b\0\0009\t\19\1B\5\4\2=\5\19\4B\2\2\0019\2\20\0009\2\14\0025\4\21\0006\5\16\0=\5\16\0046\5\3\0009\5\17\5'\a\18\0004\b\0\0009\t\19\1B\5\4\2=\5\19\4B\2\2\0019\2\22\0009\2\14\0025\4\23\0006\5\16\0=\5\16\0045\5\24\0=\5\25\4B\2\2\0019\2\26\0009\2\14\0025\4\27\0006\5\16\0=\5\16\4B\2\2\0019\2\28\0009\2\14\0025\4\29\0006\5\16\0=\5\16\0045\5!\0005\6\31\0005\a\30\0=\a \6=\6\"\5=\5#\4B\2\2\0019\2$\0009\2\14\0025\4%\0006\5\16\0=\5\16\4B\2\2\0015\2&\0005\3'\0005\4(\0005\5)\0=\5*\0045\5+\0005\6,\0005\a-\0=\a*\0065\a.\0005\b/\0=\b*\a9\b0\0009\b\14\b5\n2\0005\v1\0=\v3\n5\v4\0=\v\25\n3\v5\0=\v6\n6\v\16\0=\v\16\n5\vC\0005\f7\0004\r\3\0>\a\1\r=\r8\f4\r\4\0>\2\1\r>\3\2\r>\4\3\r=\r\"\f4\r\3\0>\5\1\r>\6\2\r=\r9\f4\r\3\0>\5\1\r>\6\2\r=\r:\f4\r\3\0>\5\1\r>\6\2\r=\r;\f4\r\3\0>\5\1\r>\6\2\r=\r<\f4\r\3\0>\5\1\r=\r=\f4\r\3\0>\5\1\r=\r>\f4\r\3\0>\5\1\r=\r?\f4\r\3\0>\5\1\r=\r@\f4\r\3\0>\5\1\r=\rA\f4\r\3\0>\5\1\r=\rB\f=\fD\v=\v#\nB\b\2\1K\0\1\0\14languages\1\0\0\rmarkdown\bcss\tscss\thtml\tjson\tyaml\20javascriptreact\20typescriptreact\15javascript\15typescript\6=\1\0\0\rroot_dir\0\1\f\0\0\vpython\15typescript\15javascript\20typescriptreact\20javascriptreact\tyaml\tjson\thtml\tscss\bcss\rmarkdown\17init_options\1\0\0\1\0\1\23documentFormatting\2\befm\1\2\0\0\17%f:%l:%c: %m\1\0\4\16lintCommand\rmisspell\23lintIgnoreExitCode\2\15lintSource\rmisspell\14lintStdin\2\1\3\0\0\27%f(%l,%c): %tarning %m\24%f(%l,%c): %rror %m\1\0\4\16lintCommand?eslint_d -f visualstudio --stdin --stdin-filename ${INPUT}\23lintIgnoreExitCode\2\15lintSource\veslint\14lintStdin\2\1\0\1\18formatCommand!./node_modules/.bin/prettier\16lintFormats\1\4\0\0\25%f:%l:%c: %trror: %m\27%f:%l:%c: %tarning: %m\24%f:%l:%c: %tote: %m\1\0\2\16lintCommand8mypy --show-column-numbers --ignore-missing-imports\15lintSource\tmypy\1\0\2\18formatCommand%isort --stdout --profile black -\16formatStdin\2\1\0\2\18formatCommand.black --stdin-filename ${INPUT} --quiet -\16formatStdin\2\1\0\0\16terraformls\rsettings\vpython\1\0\0\ranalysis\1\0\1\rvenvPath\22~/.pyenv/versions\1\0\3\19diagnosticMode\14workspace\20autoSearchPaths\2\27useLibraryCodeForTypes\2\1\0\0\fpyright\1\0\0\vsvelte\14filetypes\1\4\0\0\15typescript\20typescriptreact\19typescript.tsx\1\0\0\rtsserver\1\0\0\18rust_analyzer\17capabilities\tkeep\15tbl_extend\14on_attach\1\0\0\nsetup\ngopls\17virtual_text\1\0\2\fspacing\3\4\vprefix\bÔÜ≤\1\0\2\14underline\2\nsigns\2\27on_publish_diagnostics\15diagnostic\twith$textDocument/publishDiagnostics\rhandlers\blsp\bvim\15lsp-status\14lspconfig\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/opt/nvim-lspconfig"
  },
  ["nvim-metals"] = {
    config = { "\27LJ\2\nÈ\2\0\0\a\0\15\0$6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\1\3\0017\1\4\0006\1\4\0006\2\6\0009\2\a\2'\4\b\0004\5\0\0009\6\5\0B\2\4\2=\2\5\0016\1\4\0006\2\t\0=\2\t\0016\1\6\0009\1\n\1'\3\v\0B\1\2\0016\1\6\0009\1\n\1'\3\f\0B\1\2\0016\1\6\0009\1\n\1'\3\r\0B\1\2\0016\1\6\0009\1\n\1'\3\14\0B\1\2\1K\0\1\0\16augroup endTau FileType scala,sbt lua require(\"metals\").initialize_or_attach(metals_config)\bau!\19augroup Metals\bcmd\14on_attach\tkeep\15tbl_extend\bvim\17capabilities\18metals_config\16bare_config\vmetals\15lsp-status\frequire\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/opt/nvim-metals"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n∆\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\21ensure_installed\1\v\0\0\tbash\ago\tjava\15javascript\blua\vpython\nscala\15typescript\ttoml\vsvelte\14highlight\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["targets.vim"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/targets.vim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n«\v\0\0\v\0004\0T6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0026\2\0\0'\4\3\0B\2\2\0026\3\0\0'\5\4\0B\3\2\0029\3\5\0035\5\27\0005\6\a\0009\a\6\1=\a\b\0065\a\n\0005\b\t\0=\b\v\a=\a\f\0065\a\r\0005\b\14\0=\b\15\a5\b\16\0=\b\17\a5\b\18\0=\b\19\a=\a\20\0065\a\24\0005\b\22\0009\t\21\2=\t\23\b=\b\25\a=\a\26\6=\6\28\0055\6$\0005\a\29\0005\b#\0005\t\31\0009\n\30\2=\n \t9\n!\2=\n\"\t=\t\25\b=\b\26\a=\a%\6=\6&\0055\6(\0005\a'\0=\a)\6=\6*\5B\3\2\0016\3\0\0'\5\4\0B\3\2\0029\3+\3'\5)\0B\3\2\1\18\3\0\0005\5,\0B\3\2\1\18\3\0\0005\5-\0B\3\2\1\18\3\0\0005\5.\0B\3\2\1\18\3\0\0005\5/\0B\3\2\1\18\3\0\0005\0050\0B\3\2\1\18\3\0\0005\0051\0B\3\2\1\18\3\0\0005\0052\0B\3\2\1\18\3\0\0005\0053\0B\3\2\1K\0\1\0\1\4\0\0\6n\agr=<cmd>lua require'telescope.builtin'.lsp_references()<cr>\1\4\0\0\6n\agiB<cmd>lua require'telescope.builtin'.lsp_implementations()<cr>\1\4\0\0\6n\agd><cmd>lua require'telescope.builtin'.lsp_definitions()<cr>\1\4\0\0\6n\a,h:<cmd>lua require('telescope.builtin').help_tags()<cr>\1\4\0\0\6n\a,b8<cmd>lua require('telescope.builtin').buffers()<cr>\1\4\0\0\6n\a,*a<cmd>lua require('telescope.builtin').grep_string({ search = vim.fn.expand(\"<cword>\") })<cr>\1\4\0\0\6n\a,g:<cmd>lua require('telescope.builtin').live_grep()<cr>\1\4\0\0\6n\a,f><cmd>lua require('telescope.builtin').find_files(ivy)<cr>\19load_extension\15extensions\bfzf\1\0\0\1\0\4\25override_file_sorter\2\28override_generic_sorter\1\nfuzzy\2\14case_mode\15smart_case\fpickers\fbuffers\1\0\0\1\0\0\n<C-s>\22select_horizontal\n<C-x>\1\0\0\18delete_buffer\1\0\1\18sort_lastused\2\rdefaults\1\0\0\rmappings\6i\1\0\0\n<esc>\1\0\0\nclose\16borderchars\fresults\1\2\0\0\6 \vprompt\1\t\0\0\b‚îÄ\6 \6 \6 \b‚îÄ\b‚îÄ\6 \6 \fpreview\1\t\0\0\b‚îÄ\b‚îÇ\b‚îÄ\b‚îÇ\b‚îå\b‚îê\b‚îò\b‚îî\1\2\0\0\6z\18layout_config\16bottom_pane\1\0\0\1\0\3\18preview_title\5\14max_lines\3\20\vheight\3\25\16file_sorter\1\0\3\vborder\2\20layout_strategy\16bottom_pane\21sorting_strategy\14ascending\19get_fzy_sorter\nsetup\14telescope\22telescope.actions\22telescope.sorters\vkeymap\frequire\0" },
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["vim-abolish"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-abolish"
  },
  ["vim-bbye"] = {
    config = { "\27LJ\2\nS\0\0\4\0\3\0\a6\0\0\0'\2\1\0B\0\2\2\18\1\0\0005\3\2\0B\1\2\1K\0\1\0\1\4\0\0\6n\r<space>q\22<cmd>:Bdelete<cr>\vkeymap\frequire\0" },
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-bbye"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-obsession"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-obsession"
  },
  ["vim-rhubarb"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-rhubarb"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["vim-swap"] = {
    config = { "\27LJ\2\nÀ\1\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0´\1      omap i, <Plug>(swap-textobject-i)\n      xmap i, <Plug>(swap-textobject-i)\n      omap a, <Plug>(swap-textobject-a)\n      xmap a, <Plug>(swap-textobject-a)\n      \bcmd\bvim\0" },
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-swap"
  },
  ["vim-terraform"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-terraform"
  },
  ["vim-visual-star-search"] = {
    loaded = true,
    path = "/Users/haran/.local/share/nvim/site/pack/packer/start/vim-visual-star-search"
  }
}

time([[Defining packer_plugins]], false)
-- Setup for: gruvbox
time([[Setup for gruvbox]], true)
try_loadstring("\27LJ\2\nû\2\0\0\2\0\v\0\0296\0\0\0009\0\1\0)\1\1\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0006\0\0\0009\0\1\0)\1\1\0=\1\4\0006\0\0\0009\0\1\0'\1\6\0=\1\5\0006\0\0\0009\0\1\0'\1\b\0=\1\a\0006\0\0\0009\0\1\0)\1\0\0=\1\t\0006\0\0\0009\0\1\0)\1\1\0=\1\n\0K\0\1\0\30gruvbox_improved_warnings\29gruvbox_improved_strings\vmedium\27gruvbox_contrast_light\thard\26gruvbox_contrast_dark\17gruvbox_bold\22gruvbox_undercurl\22gruvbox_underline\6g\bvim\0", "setup", "gruvbox")
time([[Setup for gruvbox]], false)
time([[packadd for gruvbox]], true)
vim.cmd [[packadd gruvbox]]
time([[packadd for gruvbox]], false)
-- Setup for: completion-nvim
time([[Setup for completion-nvim]], true)
try_loadstring("\27LJ\2\n»\1\0\0\4\0\v\0\0206\0\0\0009\0\1\0)\1\0\0=\1\2\0006\0\0\0009\0\3\0005\1\5\0=\1\4\0006\0\0\0009\0\3\0009\0\6\0\18\2\0\0009\0\a\0'\3\b\0B\0\3\2\18\2\0\0009\0\t\0'\3\n\0B\0\3\1K\0\1\0\6c\vappend\6F\vremove\14shortmess\1\4\0\0\tmenu\rnoinsert\rnoselect\16completeopt\15opt_global!completion_enable_auto_popup\6g\bvim\0", "setup", "completion-nvim")
time([[Setup for completion-nvim]], false)
time([[packadd for completion-nvim]], true)
vim.cmd [[packadd completion-nvim]]
time([[packadd for completion-nvim]], false)
-- Setup for: nvim-lspconfig
time([[Setup for nvim-lspconfig]], true)
try_loadstring("\27LJ\2\nπ\1\0\0\3\0\5\0\r6\0\0\0009\0\1\0'\2\2\0B\0\2\0016\0\0\0009\0\1\0'\2\3\0B\0\2\0016\0\0\0009\0\1\0'\2\4\0B\0\2\1K\0\1\0,hi! link LspReferenceWrite CursorColumn+hi! link LspReferenceRead CursorColumn+hi! link LspReferenceText CursorColumn\bcmd\bvim\0", "setup", "nvim-lspconfig")
time([[Setup for nvim-lspconfig]], false)
-- Setup for: nerdtree
time([[Setup for nerdtree]], true)
try_loadstring("\27LJ\2\nY\0\0\2\0\4\0\t6\0\0\0009\0\1\0)\1\1\0=\1\2\0006\0\0\0009\0\1\0)\1\1\0=\1\3\0K\0\1\0\24NERDTreeMinimalMenu\24NERDTreeHijackNetrw\6g\bvim\0", "setup", "nerdtree")
time([[Setup for nerdtree]], false)
time([[packadd for nerdtree]], true)
vim.cmd [[packadd nerdtree]]
time([[packadd for nerdtree]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
try_loadstring("\27LJ\2\n«\v\0\0\v\0004\0T6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0026\2\0\0'\4\3\0B\2\2\0026\3\0\0'\5\4\0B\3\2\0029\3\5\0035\5\27\0005\6\a\0009\a\6\1=\a\b\0065\a\n\0005\b\t\0=\b\v\a=\a\f\0065\a\r\0005\b\14\0=\b\15\a5\b\16\0=\b\17\a5\b\18\0=\b\19\a=\a\20\0065\a\24\0005\b\22\0009\t\21\2=\t\23\b=\b\25\a=\a\26\6=\6\28\0055\6$\0005\a\29\0005\b#\0005\t\31\0009\n\30\2=\n \t9\n!\2=\n\"\t=\t\25\b=\b\26\a=\a%\6=\6&\0055\6(\0005\a'\0=\a)\6=\6*\5B\3\2\0016\3\0\0'\5\4\0B\3\2\0029\3+\3'\5)\0B\3\2\1\18\3\0\0005\5,\0B\3\2\1\18\3\0\0005\5-\0B\3\2\1\18\3\0\0005\5.\0B\3\2\1\18\3\0\0005\5/\0B\3\2\1\18\3\0\0005\0050\0B\3\2\1\18\3\0\0005\0051\0B\3\2\1\18\3\0\0005\0052\0B\3\2\1\18\3\0\0005\0053\0B\3\2\1K\0\1\0\1\4\0\0\6n\agr=<cmd>lua require'telescope.builtin'.lsp_references()<cr>\1\4\0\0\6n\agiB<cmd>lua require'telescope.builtin'.lsp_implementations()<cr>\1\4\0\0\6n\agd><cmd>lua require'telescope.builtin'.lsp_definitions()<cr>\1\4\0\0\6n\a,h:<cmd>lua require('telescope.builtin').help_tags()<cr>\1\4\0\0\6n\a,b8<cmd>lua require('telescope.builtin').buffers()<cr>\1\4\0\0\6n\a,*a<cmd>lua require('telescope.builtin').grep_string({ search = vim.fn.expand(\"<cword>\") })<cr>\1\4\0\0\6n\a,g:<cmd>lua require('telescope.builtin').live_grep()<cr>\1\4\0\0\6n\a,f><cmd>lua require('telescope.builtin').find_files(ivy)<cr>\19load_extension\15extensions\bfzf\1\0\0\1\0\4\25override_file_sorter\2\28override_generic_sorter\1\nfuzzy\2\14case_mode\15smart_case\fpickers\fbuffers\1\0\0\1\0\0\n<C-s>\22select_horizontal\n<C-x>\1\0\0\18delete_buffer\1\0\1\18sort_lastused\2\rdefaults\1\0\0\rmappings\6i\1\0\0\n<esc>\1\0\0\nclose\16borderchars\fresults\1\2\0\0\6 \vprompt\1\t\0\0\b‚îÄ\6 \6 \6 \b‚îÄ\b‚îÄ\6 \6 \fpreview\1\t\0\0\b‚îÄ\b‚îÇ\b‚îÄ\b‚îÇ\b‚îå\b‚îê\b‚îò\b‚îî\1\2\0\0\6z\18layout_config\16bottom_pane\1\0\0\1\0\3\18preview_title\5\14max_lines\3\20\vheight\3\25\16file_sorter\1\0\3\vborder\2\20layout_strategy\16bottom_pane\21sorting_strategy\14ascending\19get_fzy_sorter\nsetup\14telescope\22telescope.actions\22telescope.sorters\vkeymap\frequire\0", "config", "telescope.nvim")
time([[Config for telescope.nvim]], false)
-- Config for: nerdtree
time([[Config for nerdtree]], true)
try_loadstring("\27LJ\2\n*\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0\v      \bcmd\bvim\0", "config", "nerdtree")
time([[Config for nerdtree]], false)
-- Config for: vim-bbye
time([[Config for vim-bbye]], true)
try_loadstring("\27LJ\2\nS\0\0\4\0\3\0\a6\0\0\0'\2\1\0B\0\2\2\18\1\0\0005\3\2\0B\1\2\1K\0\1\0\1\4\0\0\6n\r<space>q\22<cmd>:Bdelete<cr>\vkeymap\frequire\0", "config", "vim-bbye")
time([[Config for vim-bbye]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\n∆\1\0\0\4\0\b\0\v6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\2B\0\2\1K\0\1\0\21ensure_installed\1\v\0\0\tbash\ago\tjava\15javascript\blua\vpython\nscala\15typescript\ttoml\vsvelte\14highlight\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: lspsaga.nvim
time([[Config for lspsaga.nvim]], true)
try_loadstring("\27LJ\2\nË\t\0\0\4\0\17\00016\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\1\3\0014\3\0\0B\1\2\1\18\1\0\0005\3\4\0B\1\2\1\18\1\0\0005\3\5\0B\1\2\1\18\1\0\0005\3\6\0B\1\2\1\18\1\0\0005\3\a\0B\1\2\1\18\1\0\0005\3\b\0B\1\2\1\18\1\0\0005\3\t\0B\1\2\1\18\1\0\0005\3\n\0B\1\2\1\18\1\0\0005\3\v\0B\1\2\1\18\1\0\0005\3\f\0B\1\2\1\18\1\0\0005\3\r\0B\1\2\1\18\1\0\0005\3\14\0B\1\2\1\18\1\0\0005\3\15\0B\1\2\1\18\1\0\0005\3\16\0B\1\2\1K\0\1\0\1\4\1\0\6n\a]eH<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<cr>\vsilent\2\1\4\1\0\6n\a[eH<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<cr>\vsilent\2\1\4\1\0\6n\agHE<cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<cr>\vsilent\2\1\4\1\0\6n\aghG<cmd>lua require'lspsaga.diagnostic'.show_cursor_diagnostics()<cr>\vsilent\2\1\4\1\0\6v\ag.C<cmd>lua require('lspsaga.codeaction').range_code_action()<cr>\vsilent\2\1\4\1\0\6n\ag.=<cmd>lua require('lspsaga.codeaction').code_action()<cr>\vsilent\2\1\4\1\0\6n\n<C-b>F<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>\vsilent\2\1\4\1\0\6n\n<C-f>E<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>\vsilent\2\1\4\0\0\6n\agp@<cmd>lua require'lspsaga.provider'.preview_definition()<cr>\1\4\1\0\6i\n<C-k>A<cmd>lua require'lspsaga.signaturehelp'.signature_help()<cr>\vsilent\2\1\4\1\0\6n\6K;<cmd>lua require'lspsaga.hover'.render_hover_doc()<cr>\vsilent\2\1\4\1\0\6n\agR2<cmd>lua require'lspsaga.rename'.rename()<cr>\vsilent\2\1\4\1\0\6n\agf:<cmd>lua require('lspsaga.provider').lsp_finder()<cr>\vsilent\2\18init_lsp_saga\flspsaga\vkeymap\frequire\0", "config", "lspsaga.nvim")
time([[Config for lspsaga.nvim]], false)
-- Config for: lsp-status.nvim
time([[Config for lsp-status.nvim]], true)
try_loadstring("\27LJ\2\nD\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\1\2\0B\1\1\1K\0\1\0\22register_progress\15lsp-status\frequire\0", "config", "lsp-status.nvim")
time([[Config for lsp-status.nvim]], false)
-- Config for: lualine.nvim
time([[Config for lualine.nvim]], true)
try_loadstring("\27LJ\2\në\6\0\0\t\0(\0?6\0\0\0'\2\1\0B\0\2\0029\1\2\0005\3\t\0005\4\3\0005\5\4\0=\5\5\0045\5\6\0=\5\a\0044\5\0\0=\5\b\4=\4\n\0035\4\r\0004\5\3\0006\6\0\0'\b\v\0B\6\2\0029\6\f\6>\6\1\5=\5\14\0045\5\15\0=\5\16\0045\5\17\0=\5\18\0045\5\26\0005\6\19\0005\a\20\0=\a\21\0065\a\22\0=\a\23\6>\6\1\0056\6\0\0'\b\24\0B\6\2\0029\6\25\6>\6\2\5=\5\27\0045\5\28\0=\5\29\0045\5\30\0=\5\31\4=\4 \0035\4!\0004\5\0\0=\5\14\0044\5\0\0=\5\16\0045\5\"\0=\5\18\0045\5#\0=\5\27\0044\5\0\0=\5\29\0044\5\0\0=\5\31\4=\4$\0034\4\0\0=\4%\0035\4&\0=\4'\3B\1\2\1K\0\1\0\15extensions\1\4\0\0\rfugitive\rnerdtree\rquickfix\ftabline\22inactive_sections\1\2\0\0\rlocation\1\2\0\0\rfilename\1\0\0\rsections\14lualine_z\1\2\0\0\rlocation\14lualine_y\1\2\0\0\rprogress\14lualine_x\1\5\0\0\0\0\rencoding\rfiletype\rprogress\26lsp-status/statusline\fsymbols\1\0\4\nerror\tÔÜà \thint\tÔÉ´ \twarn\tÔÅ± \tinfo\tÔëâ \fsources\1\2\0\0\rnvim_lsp\1\2\0\0\16diagnostics\14lualine_c\1\2\0\0\rfilename\14lualine_b\1\2\0\0\vbranch\14lualine_a\1\0\0\tmode\14nvim-mode\foptions\1\0\0\23disabled_filetypes\25component_separators\1\3\0\0\bÓÇ±\bÓÇ≥\23section_separators\1\3\0\0\bÓÇ∞\bÓÇ≤\1\0\2\ntheme\fgruvbox\18icons_enabled\2\nsetup\flualine\frequire\0", "config", "lualine.nvim")
time([[Config for lualine.nvim]], false)
-- Config for: completion-nvim
time([[Config for completion-nvim]], true)
try_loadstring("\27LJ\2\n◊\2\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0∑\2      \" Use <Tab> and <S-Tab> to navigate through popup menu\n      inoremap <expr> <Tab>   pumvisible() ? \"\\<C-n>\" : \"\\<Tab>\"\n      inoremap <expr> <S-Tab> pumvisible() ? \"\\<C-p>\" : \"\\<S-Tab>\"\n\n      imap <silent> <c-p> <Plug>(completion_trigger)\n      imap <silent> <c-n> <Plug>(completion_trigger)\n      \bcmd\bvim\0", "config", "completion-nvim")
time([[Config for completion-nvim]], false)
-- Config for: vim-swap
time([[Config for vim-swap]], true)
try_loadstring("\27LJ\2\nÀ\1\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0´\1      omap i, <Plug>(swap-textobject-i)\n      xmap i, <Plug>(swap-textobject-i)\n      omap a, <Plug>(swap-textobject-a)\n      xmap a, <Plug>(swap-textobject-a)\n      \bcmd\bvim\0", "config", "vim-swap")
time([[Config for vim-swap]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd nvim-metals ]]

-- Config for: nvim-metals
try_loadstring("\27LJ\2\nÈ\2\0\0\a\0\15\0$6\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0029\1\3\0017\1\4\0006\1\4\0006\2\6\0009\2\a\2'\4\b\0004\5\0\0009\6\5\0B\2\4\2=\2\5\0016\1\4\0006\2\t\0=\2\t\0016\1\6\0009\1\n\1'\3\v\0B\1\2\0016\1\6\0009\1\n\1'\3\f\0B\1\2\0016\1\6\0009\1\n\1'\3\r\0B\1\2\0016\1\6\0009\1\n\1'\3\14\0B\1\2\1K\0\1\0\16augroup endTau FileType scala,sbt lua require(\"metals\").initialize_or_attach(metals_config)\bau!\19augroup Metals\bcmd\14on_attach\tkeep\15tbl_extend\bvim\17capabilities\18metals_config\16bare_config\vmetals\15lsp-status\frequire\0", "config", "nvim-metals")

vim.cmd [[ packadd nvim-lspconfig ]]

-- Config for: nvim-lspconfig
try_loadstring("\27LJ\2\nc\0\1\6\0\5\0\t6\1\0\0'\3\1\0B\1\2\0029\2\2\1'\4\3\0'\5\4\0B\2\3\2\18\4\0\0D\2\2\0\19pyproject.toml\t.git\17root_pattern\19lspconfig.util\frequireõ\15\1\0\14\0E\0ñ\0016\0\0\0'\2\1\0B\0\2\0026\1\0\0'\3\2\0B\1\2\0026\2\3\0009\2\4\0029\2\5\0026\3\3\0009\3\4\0039\3\a\0036\5\3\0009\5\4\0059\5\b\0059\5\t\0055\6\n\0005\a\v\0=\a\f\6B\3\3\2=\3\6\0029\2\r\0009\2\14\0025\4\15\0006\5\16\0=\5\16\0046\5\3\0009\5\17\5'\a\18\0004\b\0\0009\t\19\1B\5\4\2=\5\19\4B\2\2\0019\2\20\0009\2\14\0025\4\21\0006\5\16\0=\5\16\0046\5\3\0009\5\17\5'\a\18\0004\b\0\0009\t\19\1B\5\4\2=\5\19\4B\2\2\0019\2\22\0009\2\14\0025\4\23\0006\5\16\0=\5\16\0045\5\24\0=\5\25\4B\2\2\0019\2\26\0009\2\14\0025\4\27\0006\5\16\0=\5\16\4B\2\2\0019\2\28\0009\2\14\0025\4\29\0006\5\16\0=\5\16\0045\5!\0005\6\31\0005\a\30\0=\a \6=\6\"\5=\5#\4B\2\2\0019\2$\0009\2\14\0025\4%\0006\5\16\0=\5\16\4B\2\2\0015\2&\0005\3'\0005\4(\0005\5)\0=\5*\0045\5+\0005\6,\0005\a-\0=\a*\0065\a.\0005\b/\0=\b*\a9\b0\0009\b\14\b5\n2\0005\v1\0=\v3\n5\v4\0=\v\25\n3\v5\0=\v6\n6\v\16\0=\v\16\n5\vC\0005\f7\0004\r\3\0>\a\1\r=\r8\f4\r\4\0>\2\1\r>\3\2\r>\4\3\r=\r\"\f4\r\3\0>\5\1\r>\6\2\r=\r9\f4\r\3\0>\5\1\r>\6\2\r=\r:\f4\r\3\0>\5\1\r>\6\2\r=\r;\f4\r\3\0>\5\1\r>\6\2\r=\r<\f4\r\3\0>\5\1\r=\r=\f4\r\3\0>\5\1\r=\r>\f4\r\3\0>\5\1\r=\r?\f4\r\3\0>\5\1\r=\r@\f4\r\3\0>\5\1\r=\rA\f4\r\3\0>\5\1\r=\rB\f=\fD\v=\v#\nB\b\2\1K\0\1\0\14languages\1\0\0\rmarkdown\bcss\tscss\thtml\tjson\tyaml\20javascriptreact\20typescriptreact\15javascript\15typescript\6=\1\0\0\rroot_dir\0\1\f\0\0\vpython\15typescript\15javascript\20typescriptreact\20javascriptreact\tyaml\tjson\thtml\tscss\bcss\rmarkdown\17init_options\1\0\0\1\0\1\23documentFormatting\2\befm\1\2\0\0\17%f:%l:%c: %m\1\0\4\16lintCommand\rmisspell\23lintIgnoreExitCode\2\15lintSource\rmisspell\14lintStdin\2\1\3\0\0\27%f(%l,%c): %tarning %m\24%f(%l,%c): %rror %m\1\0\4\16lintCommand?eslint_d -f visualstudio --stdin --stdin-filename ${INPUT}\23lintIgnoreExitCode\2\15lintSource\veslint\14lintStdin\2\1\0\1\18formatCommand!./node_modules/.bin/prettier\16lintFormats\1\4\0\0\25%f:%l:%c: %trror: %m\27%f:%l:%c: %tarning: %m\24%f:%l:%c: %tote: %m\1\0\2\16lintCommand8mypy --show-column-numbers --ignore-missing-imports\15lintSource\tmypy\1\0\2\18formatCommand%isort --stdout --profile black -\16formatStdin\2\1\0\2\18formatCommand.black --stdin-filename ${INPUT} --quiet -\16formatStdin\2\1\0\0\16terraformls\rsettings\vpython\1\0\0\ranalysis\1\0\1\rvenvPath\22~/.pyenv/versions\1\0\3\19diagnosticMode\14workspace\20autoSearchPaths\2\27useLibraryCodeForTypes\2\1\0\0\fpyright\1\0\0\vsvelte\14filetypes\1\4\0\0\15typescript\20typescriptreact\19typescript.tsx\1\0\0\rtsserver\1\0\0\18rust_analyzer\17capabilities\tkeep\15tbl_extend\14on_attach\1\0\0\nsetup\ngopls\17virtual_text\1\0\2\fspacing\3\4\vprefix\bÔÜ≤\1\0\2\14underline\2\nsigns\2\27on_publish_diagnostics\15diagnostic\twith$textDocument/publishDiagnostics\rhandlers\blsp\bvim\15lsp-status\14lspconfig\frequire\0", "config", "nvim-lspconfig")

time([[Sequenced loading]], false)
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
