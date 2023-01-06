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

