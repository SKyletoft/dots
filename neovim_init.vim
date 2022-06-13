" Basic controls
noremap a h
noremap s gj
noremap w gk
noremap d l

" Larger movements
noremap <C-a> b
noremap <C-d> w
noremap <C-w> 5<C-y>
noremap <C-s> 5<C-e>

" Copy, cut, paste for normal people
noremap <C-c> y
noremap <C-x> d
noremap <C-v> p

" Indents
nnoremap <tab> >>
noremap <S-tab> <<
vmap <tab> >gv
vmap <S-tab> <gv

" Find
noremap <C-f> /

" Undo and redo
noremap <C-z> u
noremap <C-M-z> <C-r>

" And preserve wasd behaviour somewhere
noremap h a
noremap j s
noremap k w
noremap l d

" Open file
noremap <C-o> :e 

" End of line and start of line inserts
nnoremap A I
nnoremap D A

" Delete rest of line
noremap X D

" Unmap undo because it's poorly placed
noremap u <Nop>

" Pane management
noremap <C-e> <C-w>v
noremap <C-q> <C-w>s
noremap <C-M-W> <C-w>k
noremap <C-M-S> <C-w>j
noremap <C-M-A> <C-w>h
noremap <C-M-D> <C-w>l

filetype plugin indent off
set tabstop=8
set shiftwidth=8
set noexpandtab
set number
set mouse=a
set scrolloff=8
set foldmethod=syntax
set foldnestmax=10
set nofoldenable
set foldlevel=1

autocmd! FileType rust    setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd! FileType fortran setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd! FileType python  setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd! FileType nim     setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd! FileType kotlin  setlocal tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
autocmd! FileType fut     setlocal tabstop=2 softtabstop=2 shiftwidth=0 noexpandtab
autocmd! FileType haskell setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd! FileType nix     setlocal tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
autocmd! FileType toml    setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd! FileType yaml    setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab

autocmd! FileType rust    nmap <leader>i :!cargo +nightly fmt<CR>
autocmd! FileType c       nmap <leader>i :!clang-format -i %
autocmd! FileType js      nmap <leader>i :!clang-format -i %
autocmd! FileType java    nmap <leader>i :!clang-format -i %

colorscheme monokai_pro

let mapleader = " "
xmap <leader>A <Plug>(coc-codeaction-selected)
nmap <leader>A <Plug>(coc-codeaction-selected)
nmap <leader>g <Plug>(coc-definition)
nmap <leader>a <Plug>(coc-codeaction-cursor)
nnoremap <silent> <leader>f :call <SID>show_documentation()<CR>
map f <Plug>Sneak_s
map F <Plug>Sneak_S
map ' ;

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
	ensure_installed = "all",

	-- Install languages synchronously (only applied to `ensure_installed`)
	sync_install = false,

	-- List of parsers to ignore installing
	ignore_install = {},

	highlight = {
		-- `false` will disable the whole extension
		enable = true,

		-- list of language that will be disabled
		disable = {},

		-- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- Instead of true it can also be a list of languages
		additional_vim_regex_highlighting = true,
	},
}
EOF

