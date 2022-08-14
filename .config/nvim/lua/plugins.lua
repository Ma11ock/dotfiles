
-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim' -- The package manager.

    use {'dracula/vim', as = 'dracula'} -- Dracula theme

    use {
        -- Sakura theme.
        'numToStr/Sakura.nvim',
        config = function()
            require('Sakura').load()
        end,
    }

    use({
        {
            -- Lualine modeline plugin.
            'nvim-lualine/lualine.nvim',
            after = 'Sakura.nvim',
            event = 'BufEnter',
            config = function()
                require('numToStr.plugins.lualine')
            end,
        },
        {
            -- Lualine stuff.
            'j-hui/fidget.nvim',
            after = 'lualine.nvim',
            config = function()
                require('fidget').setup()
            end,
        },
    })

    use 'neovim/nvim-lspconfig' -- Configurations for Nvim LSP


    use 'https://github.com/tpope/vim-commentary' -- use gcc & gc to comment and uncomment code.

    --use 'vim-airline/vim-airline' -- Arline plugin

    use 'Raimondi/delimitMate' -- Delim matching.

    use 'https://github.com/ryanoasis/vim-devicons' -- Developer icons.

    use 'https://github.com/vim-python/python-syntax' -- Better python mode than nvim's default.

    use 'https://github.com/Yggdroot/indentLine' -- Better python mode than nvim's default.

    use 'https://github.com/terryma/vim-multiple-cursors' -- Ctrl+N for multiple cursors

    use 'ntpeters/vim-better-whitespace' -- Show trailing whitespace.
end)

