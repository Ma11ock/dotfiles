
-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use {
        'wbthomason/packer.nvim' -- The package manager.
        --as = 'wbthomason.packer.nvim'
    }

    use {'dracula/vim', as = 'dracula'} -- Dracula theme

    use {
        'numToStr/Sakura.nvim', -- Sakura theme.
        as = 'numToStr.Sakura.nvim',
        config = function()
            require('Sakura').load()
        end,
    }

    use({
        {
            'nvim-lualine/lualine.nvim', -- Lualine modeline plugin.
            as = 'nvim-lualine.lualine.nvim',
            after = 'Sakura.nvim',
            event = 'BufEnter',
            config = function()
                require('numToStr.plugins.lualine')
            end,
        },
        {
            'j-hui/fidget.nvim', -- Lualine stuff.
            as = 'j-hui.fidget.nvim',
            after = 'lualine.nvim',
            config = function()
                require('fidget').setup()
            end,
        },
    })

    use {
        'neovim/nvim-lspconfig', -- Configurations for Nvim LSP
        as = 'neovim.nvim-lspconfig'
    }


    use {
        'https://github.com/tpope/vim-commentary', -- use gcc & gc to comment and uncomment code.
        as = 'tpope.vim-commentary'
    }

    use {
        'Raimondi/delimitMate', -- Delim matching.
        as = 'Raimondi.delimitMate'
    }

    use {
        'https://github.com/ryanoasis/vim-devicons', -- Developer icons.
        as = 'ryanoasis.vim-devicons'
    }

    use {
        'https://github.com/vim-python/python-syntax', -- Better python mode than nvim's default.
        as = 'vim-python.python-syntax'
    }

    use {
        'https://github.com/Yggdroot/indentLine', -- Better python mode than nvim's default.
        as = 'Yggdroot.indentLine'
    }

    use {
        'terryma/vim-multiple-cursors', -- Ctrl+N for multiple cursors
        as = 'terryma.vim-multiple-cursors'
    }

    use {
        'ntpeters/vim-better-whitespace', -- Show trailing whitespace.
        as = 'ntpeters.vim-better-whitespace'
    }

    use {
        'ycm-core/YouCompleteMe',
        as = 'ycm-core.YouCompleteMe'
    }

    use {
        'rhysd/vim-clang-format',
        as = 'rhysd.vim-clang-format'
    }

    use {
        'Shougo/vimproc.vim',
        as = 'Shougo.vimproc.vim'
    }
end)

