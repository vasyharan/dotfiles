-- bootstrap packer {{{
local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end
-- }}}

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
function on_attach(client, bufnr) -- {{{
  local map = require('keymap')
  local protocol = require('vim.lsp.protocol')

  -- local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  -- map { 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<cr>', silent=true }
  map { 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', silent=true }
  -- map { 'n', 'K', '<cmd> lua vim.lsp.buf.hover()<cr>', silent = true }
  -- buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
  -- buf_set_keymap('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  -- buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  -- buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  -- buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  -- buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  -- buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  -- buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  -- buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  --buf_set_keymap('n', '<C-j>', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  -- buf_set_keymap('n', '<S-C-j>', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  -- buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  map { 'n', '<space>F', "<cmd>lua vim.lsp.buf.formatting()<CR>", silent = true }

  -- formatting
  if client.resolved_capabilities.document_formatting or
    client.resolved_capabilities.document_range_formatting then
  -- if client.resolved_capabilities.document_formatting then
    vim.api.nvim_command [[augroup Format]]
    vim.api.nvim_command [[autocmd! * <buffer>]]
    vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting()]]
    vim.api.nvim_command [[augroup END]]
  end

  require'completion'.on_attach(client, bufnr)
  require'lsp-status'.on_attach(client)

  --protocol.SymbolKind = { }
  protocol.CompletionItemKind = { -- {{{
    '', -- Text
    '', -- Method
    '', -- Function
    '', -- Constructor
    '', -- Field
    '', -- Variable
    '', -- Class
    'ﰮ', -- Interface
    '', -- Module
    '', -- Property
    '', -- Unit
    '', -- Value
    '', -- Enum
    '', -- Keyword
    '﬌', -- Snippet
    '', -- Color
    '', -- File
    '', -- Reference
    '', -- Folder
    '', -- EnumMember
    '', -- Constant
    '', -- Struct
    '', -- Event
    'ﬦ', -- Operator
    '', -- TypeParameter
  } -- }}}
end -- }}}

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  use { 'morhetz/gruvbox', -- {{{
  setup = function()
      vim.g.gruvbox_underline = 1
      vim.g.gruvbox_undercurl = 1
      -- vim.g.gruvbox_italic = 1
      vim.g.gruvbox_bold = 1
      vim.g.gruvbox_contrast_dark = "hard"
      vim.g.gruvbox_contrast_light = "medium"
      vim.g.gruvbox_improved_strings = 0
      vim.g.gruvbox_improved_warnings = 1
    end
  } -- }}}
  use { 'overcache/NeoSolarized', }

  use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', }
  use { 'nvim-telescope/telescope.nvim', --- {{{
    requires = { {'nvim-lua/plenary.nvim'} },
    config = function()
      local map = require('keymap')
      local sorters = require('telescope.sorters')
      local actions = require('telescope.actions')

      require('telescope').setup{
        defaults = {
          file_sorter = sorters.get_fzy_sorter,
          sorting_strategy = "ascending",
          layout_strategy = "bottom_pane",
          layout_config = {
            bottom_pane = {
              preview_title = "",
              height = 25,
              max_lines = 20,
            },
          },
          border = true,
          borderchars = {
            "z",
            -- ┐┌ ┘└
            -- preview = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
            -- results = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
            -- results = { "─", "│", "─", "│", "├", "┤", "╯", "╰" },
            preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
            prompt = { "─", " ", " ", " ", "─", "─", " ", " " },
            results = { " " },
          },
          mappings = {
            i = {
              ["<esc>"] = actions.close,
            },
          },
        },
        pickers = {
          buffers = {
            sort_lastused = true,
            mappings = {
              i = {
                ["<C-x>"] = actions.delete_buffer,
                ["<C-s>"] = actions.select_horizontal,
              },
            },
          },
        },
        extensions = {
          fzf = {
            fuzzy = true,                    -- false will only do exact matching
            override_generic_sorter = false, -- override the generic sorter
            override_file_sorter = true,     -- override the file sorter
            case_mode = "smart_case",        -- or "ignore_case" or "respect_case" the default case_mode is "smart_case"
          }
        }
      }
      require('telescope').load_extension('fzf')

      map { 'n', ',f', "<cmd>lua require('telescope.builtin').find_files(ivy)<cr>" }
      map { 'n', ',g', "<cmd>lua require('telescope.builtin').live_grep()<cr>" }
      map { 'n', ',*', "<cmd>lua require('telescope.builtin').grep_string({ search = vim.fn.expand(\"<cword>\") })<cr>" }
      map { 'n', ',b', "<cmd>lua require('telescope.builtin').buffers()<cr>" }
      map { 'n', ',h', "<cmd>lua require('telescope.builtin').help_tags()<cr>" }

      map { 'n', 'gd', "<cmd>lua require'telescope.builtin'.lsp_definitions()<cr>" }
      map { 'n', 'gi', "<cmd>lua require'telescope.builtin'.lsp_implementations()<cr>" }
      map { 'n', 'gr', "<cmd>lua require'telescope.builtin'.lsp_references()<cr>" }
    end
  }
  -- }}}

  use { 'b3nj5m1n/kommentary', -- {{{
    require('kommentary.config').configure_language("default", {
      prefer_single_line_comments = true,
    })
  } -- }}}
  use { 'tpope/vim-surround', }
  use { 'tpope/vim-obsession', }
  use { 'tpope/vim-abolish', }
  use { 'tpope/vim-fugitive', }
  use { 'tpope/vim-rhubarb', }
  use { 'wellle/targets.vim', }
  use { 'bronson/vim-visual-star-search', }
  use { 'machakann/vim-swap', -- {{{
    config = function()
      vim.cmd [[
      omap i, <Plug>(swap-textobject-i)
      xmap i, <Plug>(swap-textobject-i)
      omap a, <Plug>(swap-textobject-a)
      xmap a, <Plug>(swap-textobject-a)
      ]]
    end
  } -- }}}

  use { 'hoob3rt/lualine.nvim', -- {{{
    config = function()
      local lualine = require('lualine')
      lualine.setup {
        options = {
          icons_enabled = true,
          theme = 'gruvbox',
          section_separators = {'', ''},
          component_separators = {'', ''},
          disabled_filetypes = {}
        },
        sections = {
          lualine_a = {require'nvim-mode'.mode},
          lualine_b = {'branch'},
          lualine_c = {'filename'},
          lualine_x = {
            {
              'diagnostics',
              sources = {"nvim_lsp"},
              symbols = {error = ' ', warn = ' ', info = ' ', hint = ' '}
            },
            require'lsp-status/statusline'.progress,
            'encoding',
            'filetype'
          },
          lualine_y = {'progress'},
          lualine_z = {'location'}
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {'filename'},
          lualine_x = {'location'},
          lualine_y = {},
          lualine_z = {}
        },
        tabline = {},
        extensions = {'fugitive', 'nerdtree', 'quickfix'}
      }
    end
  } -- }}}
  use { 'nvim-lua/lsp-status.nvim', -- {{{
      config = function()
        local lsp_status = require('lsp-status')
        lsp_status.register_progress()
      end
  } -- }}}
  use { 'glepnir/lspsaga.nvim', -- {{{
    config = function()
      local map = require('keymap')
      require'lspsaga'.init_lsp_saga {
      }
      map { 'n', 'gf', "<cmd>lua require('lspsaga.provider').lsp_finder()<cr>", silent = true }
      map { 'n', 'gR', "<cmd>lua require'lspsaga.rename'.rename()<cr>", silent = true }
      map { 'n', 'K', "<cmd>lua require'lspsaga.hover'.render_hover_doc()<cr>", silent = true }
      map { 'i', '<C-k>', "<cmd>lua require'lspsaga.signaturehelp'.signature_help()<cr>", silent = true }
      map { 'n', 'gp', "<cmd>lua require'lspsaga.provider'.preview_definition()<cr>" }
      map { 'n', '<C-f>', "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<cr>", silent = true }
      map { 'n', '<C-b>', "<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<cr>", silent = true }

      map { 'n', 'g.', "<cmd>lua require('lspsaga.codeaction').code_action()<cr>", silent = true }
      map { 'v', 'g.', "<cmd>lua require('lspsaga.codeaction').range_code_action()<cr>", silent = true }

      map { 'n', 'gh', "<cmd>lua require'lspsaga.diagnostic'.show_cursor_diagnostics()<cr>", silent = true }
      map { 'n', 'gH', "<cmd>lua require'lspsaga.diagnostic'.show_line_diagnostics()<cr>", silent = true }
      map { 'n', '[e', "<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_prev()<cr>", silent = true }
      map { 'n', ']e', "<cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<cr>", silent = true }
    end
  } -- }}}
  use { 'nvim-lua/completion-nvim',  -- {{{
    setup = function()
      vim.g.completion_enable_auto_popup = 0
      vim.opt_global.completeopt = { "menu", "noinsert", "noselect" }
      vim.opt_global.shortmess:remove("F"):append("c")
    end,
    config = function()
      vim.cmd [[
      " Use <Tab> and <S-Tab> to navigate through popup menu
      inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
      inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

      imap <silent> <c-p> <Plug>(completion_trigger)
      imap <silent> <c-n> <Plug>(completion_trigger)
      ]]
    end,
  } -- }}}
  use { 'neovim/nvim-lspconfig', -- {{{
    after = 'lsp-status.nvim',
    setup = function()
      vim.cmd([[hi! link LspReferenceText CursorColumn]])
      vim.cmd([[hi! link LspReferenceRead CursorColumn]])
      vim.cmd([[hi! link LspReferenceWrite CursorColumn]])
    end,
    config = function()
      local lspconfig = require('lspconfig')
      local lsp_status = require('lsp-status')

      -- icon {{{
      vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        signs = true,
        underline = true,
	-- virtual_text = false,
        virtual_text = {
          spacing = 4,
          prefix = ''
        }
      })
      -- }}}
      lspconfig.gopls.setup { -- {{{
        on_attach = on_attach,
        capabilities = vim.tbl_extend('keep', {}, lsp_status.capabilities)
      } -- }}}
      lspconfig.rust_analyzer.setup{ -- {{{
        on_attach = on_attach,
        capabilities = vim.tbl_extend('keep', {}, lsp_status.capabilities)
      } -- }}}
      lspconfig.tsserver.setup { -- {{{
        on_attach = on_attach,
        filetypes = { "typescript", "typescriptreact", "typescript.tsx" }
      } -- }}}
      lspconfig.svelte.setup { -- {{{
        on_attach = on_attach,
      } -- }}}
      lspconfig.pyright.setup { -- {{{
        on_attach = on_attach,
        settings = {
          python = {
            analysis = {
              autoSearchPaths = true,
              diagnosticMode = "workspace",
              useLibraryCodeForTypes = true,
            },
            venvPath = "~/.pyenv/versions",
          },
        },
      } -- }}}
      lspconfig.terraformls.setup { -- {{{
        on_attach = on_attach,
      } -- }}}

      -- lspconfig.efm.setup {{{
      -- local vint = require "efm/vint"
      -- local luafmt = require "efm/luafmt"
      -- local golint = require "efm/golint"
      -- local goimports = require "efm/goimports"
      local black = {
        formatCommand = "black --stdin-filename ${INPUT} --quiet -",
        formatStdin = true
      }
      local isort = {
        formatCommand = "isort --stdout --profile black -",
        formatStdin = true
      }
      local mypy = {
        lintCommand = "mypy --show-column-numbers --ignore-missing-imports",
        lintFormats = {
          "%f:%l:%c: %trror: %m",
          "%f:%l:%c: %tarning: %m",
          "%f:%l:%c: %tote: %m"
        },
        lintSource = "mypy"
      }
      local prettier = {
        formatCommand = "./node_modules/.bin/prettier"
      }
      local eslint = {
        lintCommand = "eslint_d -f visualstudio --stdin --stdin-filename ${INPUT}",
        lintIgnoreExitCode = true,
        lintStdin = true,
        lintFormats = {
          "%f(%l,%c): %tarning %m",
          "%f(%l,%c): %rror %m"
        },
        lintSource = "eslint"
      }
      -- local shellcheck = require "efm/shellcheck"
      -- local shfmt = require "efm/shfmt"
      -- local terraform = require "efm/terraform"
      local misspell = {
        lintCommand = "misspell",
        lintIgnoreExitCode = true,
        lintStdin = true,
        lintFormats = {"%f:%l:%c: %m"},
        lintSource = "misspell"
      }
      lspconfig.efm.setup {
        -- cmd = {"efm-langserver", "-loglevel", "5", "-logfile", "/Users/haran/efm.log"},
        init_options = {documentFormatting = true},
        filetypes = { 'python', 'typescript', 'javascript', 'typescriptreact', 'javascriptreact', 'yaml', 'json', 'html', 'scss', 'css', 'markdown' },
        root_dir = function(fname)
          local util = require('lspconfig.util')
          return util.root_pattern(".git", "pyproject.toml")(fname)
        end,
        on_attach = on_attach,
        settings = {
          -- rootMarkers = {".git/", "pyproject.toml" ,},
          languages = {
            ["="] = {misspell},
            -- vim = {vint},
            -- lua = {luafmt},
            -- go = {golint, goimports},
            python = {black, isort, mypy},
            typescript = {prettier, eslint},
            javascript = {prettier, eslint},
            typescriptreact = {prettier, eslint},
            javascriptreact = {prettier, eslint},
            yaml = {prettier},
            json = {prettier},
            html = {prettier},
            scss = {prettier},
            css = {prettier},
            markdown = {prettier},
            -- sh = {shellcheck, shfmt},
            -- tf = {terraform},
          }
        }
      } -- }}}
    end,
  } -- }}}
  use { 'scalameta/nvim-metals', -- {{{
    after = 'lsp-status.nvim',
    config = function()
      local lsp_status = require('lsp-status')
      metals_config = require("metals").bare_config
      metals_config.capabilities = vim.tbl_extend('keep', {}, lsp_status.capabilities)
      metals_config.on_attach = on_attach

      vim.cmd [[augroup Metals]]
      vim.cmd [[au!]]
      vim.cmd [[au FileType scala,sbt lua require("metals").initialize_or_attach(metals_config)]]
      vim.cmd [[augroup end]]
    end
  } -- }}}
  use { 'nvim-treesitter/nvim-treesitter', -- {{{
    run = ':TSUpdate',
    config = function()
      require'nvim-treesitter.configs'.setup {
        highlight = { enable = true, },
        ensure_installed = {
          "bash",
          "go",
          "java",
          "javascript",
          "lua",
          "python",
          "scala",
          "typescript",
          "toml",
          "svelte",
        },
      }
    end
  } -- }}}

  use { 'preservim/nerdtree', -- {{{
    -- ▸▾
    -- TODO: replace with lua tree
    setup = function()
      -- vim.g.NERDTreeQuitOnOpen = 1
      vim.g.NERDTreeHijackNetrw = 1
      vim.g.NERDTreeMinimalMenu = 1
    end,
    config = function()
      vim.cmd [[
      ]]
    end
  } -- }}}

  use { 'hashivim/vim-terraform', }
  use { 'chrisbra/csv.vim', }
  use { 'moll/vim-bbye', -- {{{
    config = function()
      local map = require('keymap')
      map { 'n', '<space>q', "<cmd>:Bdelete<cr>" }
    end
  } -- }}}
end)
