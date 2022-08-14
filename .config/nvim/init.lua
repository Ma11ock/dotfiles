require('plugins')

local g = vim.g   -- Global options.
local o = vim.o   -- Vim options.
local A = vim.api -- Vim API.
local wo = vim.wo -- Window options.

local absGrp = A.nvim_create_augroup("AbsoluteGroup", { clear = true }) -- Autocmd group

-- Editor options.
-- Do not save when switching buffers
-- o.hidden = true

-- Decrease update time
o.timeoutlen = 500
o.updatetime = 200

-- Show 80'th column.
vim.wo.colorcolumn = "80"

-- TODO this in lua
vim.cmd 'hi ColorColumn ctermbg=gray guibg=gray'

-- Number of screen lines to keep above and below the cursor
o.scrolloff = 8
o.number = true

-- Line numbers.
o.relativenumber = true
o.cursorline = true
o.signcolumn = 'yes'

-- Better editing experience
o.expandtab = true
-- o.smarttab = true
o.cindent = true
-- o.autoindent = true
o.wrap = true
o.textwidth = 300
o.tabstop = 4
o.shiftwidth = 0
o.softtabstop = -1 -- If negative, shiftwidth value is used

-- Undo and backup options
o.backup = false
o.writebackup = false
o.undofile = true
o.swapfile = false

-- Remember 69 items in commandline history
o.history = 69

-- Better buffer splitting
o.splitright = true
o.splitbelow = true

-- Grahics options.
o.termguicolors = true

-- Better whitespace.
g.strip_whitespace_on_save=1

--local ok, _ = pcall(vim.cmd, 'colorscheme sakura')

-- LSP

lsp = require 'lspconfig'

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local bufopts = { noremap=true, silent=true, buffer=bufnr }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
    vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, bufopts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
    vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)
end

local lsp_flags = {
    -- This is the default in Nvim 0.7+
    debounce_text_changes = 150,
}
lsp['pyright'].setup{
    on_attach = on_attach,
    flags = lsp_flags,
}
lsp['tsserver'].setup{
    on_attach = on_attach,
    flags = lsp_flags,
}
lsp['rust_analyzer'].setup{
    on_attach = on_attach,
    flags = lsp_flags,
    -- Server-specific settings...
    settings = {
        ["rust-analyzer"] = {}
    }
}
lsp.clangd.setup{
    on_attach = on_attach,
    flags = lsp_flags,
}

-- YouCompleteMe
--
g.ycm_language_server = {{
    name = 'c',
    cmdline = {'/usr/bin/clangd'},
    filetypes = {'c'},
    project_root_files = {'Makefile', 'compile_commands.json'}
}}

-- Clang-format

-- Autostart.
A.nvim_create_autocmd("FileType", {
    command = 'ClangFormatAutoEnable',
    group = absGrp,
    pattern = 'c,cpp,objc'
})


-- Code formatting.

-- If nvim version >= 0.7, use native lua functionality to handle autocmd.
-------------------------------------- Note: This does not work. ---------------------------------
if vim.fn.has "nvim-0.7" then
    vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = '.*',
        callback = function()
            vim.schedule(AbsCodeFormat)
        end,
    })
else
    vim.cmd "autocmd BufWritePre * lua AbsCodeFormat()"
end

-- Function to format the code in the current buffer.
function AbsCodeFormat()
    local bufnr = vim.api.nvim_get_current_buf()
    local ft = vim.api.nvim_buf_get_option(bufnr, "filetype")
    local fname = vim.fn.expand "%:p:t"
    local keymap_c = {}

    --        if ft == 'c' or ft == 'cpp' or ft == 'objc' then
    --            ClangFormat()
    --        end
end
