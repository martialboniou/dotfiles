return {
	
  -- telescope (may change back to fzf or lf)
  {
	  'nvim-telescope/telescope.nvim',
	  tag = '0.1.2',
	  dependencies = { {'nvim-lua/plenary.nvim'} },
  },

  -- colorscheme
  {
	  'rose-pine/neovim',
	  name = 'rose-pine',
	  config = function()
		  vim.cmd('colorscheme rose-pine')
	  end
  },

  -- treesitter
  {
	  'nvim-treesitter/nvim-treesitter',
  	  build = ':TSUpdate'
  },
  { 'nvim-treesitter/playground' },

  -- harpoon
  'theprimeagen/harpoon',

  -- undotree
  'mbbill/undotree',

  -- LSP (beware: packer format)
--  use {
--	  'VonHeikemen/lsp-zero.nvim',
--	  branch = 'v2.x',
--	  requires = {
--		  -- LSP Support
--		  {'neovim/nvim-lspconfig'},             -- Required
--		  {                                      -- Optional
--		  'williamboman/mason.nvim',
--		  run = function()
--			  pcall(vim.cmd, 'MasonUpdate')
--		  end,
--	  },
--	  {'williamboman/mason-lspconfig.nvim'}, -- Optional
--
--	  -- Autocompletion
--	  {'hrsh7th/nvim-cmp'},     -- Required
--	  --{'hrsh7th/cmp-buffer'},   -- Optional
--	  --{'hrsh7th/cmp-path'},     -- Optional
--	  {'hrsh7th/cmp-nvim-lsp'}, -- Required
--	  --{'hrsh7th/cmp-nvim-lua'}, -- Optional
--
--	  -- Snippets
--	  {'L3MON4D3/LuaSnip'},     -- Required
--	  --{'saadparwaiz1/cmp_luasnip'}, -- Optional
--	  {'rafamadriz/friendly-snippets'}, -- Optional
--  	}
--  }

}

