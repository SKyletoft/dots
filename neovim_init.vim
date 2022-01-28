syntax on

noremap a h
noremap s j
noremap w k
noremap d l

noremap <C-a> b
noremap <C-d> w
noremap <C-w> 5<C-y>
noremap <C-s> 5<C-e>

noremap <C-c> y
noremap <C-x> d
noremap <C-v> p


nnoremap <tab> >>
noremap <S-tab> <<

noremap <C-f> /
noremap <C-z> <C-,>

noremap <C-z> u

noremap h a
noremap j s
noremap k w
noremap l d

noremap <C-p> :e 

set tabstop=8
set shiftwidth=8
set noexpandtab
set number
set mouse=a

let mapleader = " "

"autocmd! Filetype rust noremap <C-o> :!cargo fmt
"autocmd! FileType haskell noremap <C-o> :!hindent % && stylish-haskell -i %

autocmd! FileType rust    setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd! FileType fortran setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd! FileType python  setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd! FileType nim     setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd! FileType kotlin  setlocal tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
autocmd! FileType fut     setlocal tabstop=2 softtabstop=2 shiftwidth=0 noexpandtab
autocmd! FileType haskell setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab

colorscheme monokai_pro
xmap <leader>A <Plug>(coc-codeaction-selected)
nmap <leader>A <Plug>(coc-codeaction-selected)
nmap <leader>g <Plug>(coc-definition)
nmap <leader>a <Plug>(coc-codeaction-cursor)
nnoremap <silent> <leader>f :call <SID>show_documentation()<CR>
noremap f <Plug>Lightspeed_f
noremap F <Plug>Lightspeed_F

function! s:show_documentation()
	if (index(['vim', 'help'], &filetype) >= 0)
		execute 'h '.expand('<cword')
	elseif (coc#rpc#ready())
		call CocActionAsync('doHover')
	else
		execute '!' . &keywordrg . " " . expand('<cword>')
	endif
endfunction

lua <<EOF
require'nvim-treesitter.configs'.setup {
	-- One of "all", "maintained" (parsers with maintainers), or a list of languages
	ensure_installed = "maintained",

	-- Install languages synchronously (only applied to `ensure_installed`)
	sync_install = false,

	-- List of parsers to ignore installing
	ignore_install = {},

	highlight = {
		-- `false` will disable the whole extension
		enable = true,

		-- list of language that will be disabled
		disable = { "rust" },

		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = false,
	},
}
EOF

