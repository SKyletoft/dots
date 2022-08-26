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
noremap <C-g> :%s/

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
noremap <C-M-tab> gt

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

" Terminal managerment
noremap <C-t> :terminal<CR>:set nonumber<CR>i
tnoremap <Esc> <C-\><C-n>

filetype plugin indent off
set tabstop=8
set shiftwidth=8
set noexpandtab
set number
set mouse=a
set scrolloff=8
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=1
set path+=**
set wildmenu
colorscheme monokai_pro

autocmd FileType rust    setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd FileType fortran setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd FileType python  setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd FileType nim     setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd FileType kotlin  setlocal tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
autocmd FileType fut     setlocal tabstop=2 softtabstop=2 shiftwidth=0 noexpandtab
autocmd FileType haskell setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd FileType nix     setlocal tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
autocmd FileType toml    setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd FileType yaml    setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab

" Formatter shortcuts
autocmd FileType rust     nmap <leader>i :!cargo +nightly fmt<CR>
autocmd FileType c        nmap <leader>i :!clang-format -i %<CR>
autocmd FileType cpp      nmap <leader>i :!clang-format -i %<CR>
autocmd FileType js       nmap <leader>i :!clang-format -i %<CR>
autocmd FileType java     nmap <leader>i :!clang-format -i %<CR>
autocmd FileType haskell  nmap <leader>i :!hindent % && stylish-haskell -i %<CR>
autocmd FileType markdown nmap <leader>i vipgq

" Table mode in text files
autocmd FileType txt      TableModeEnable
autocmd FileType markdown TableModeEnable

let mapleader = " "

" CoC
nmap <leader>d <Plug>(coc-git-chunkinfo)
xmap <leader>A <Plug>(coc-codeaction-selected)
nmap <leader>A <Plug>(coc-codeaction-selected)
nmap <leader>g <Plug>(coc-definition)
nmap <leader>a <Plug>(coc-codeaction-cursor)
nmap <leader>r <Plug>(coc-rename)
nmap <F2>      <Plug>(coc-rename)
nmap <leader>t :CocCommand rust-analyzer.toggleInlayHints<CR>
nnoremap <silent> <leader>f :call <SID>show_documentation()<CR>

function! s:show_documentation()
	if (index(['vim', 'help'], &filetype) >= 0)
		execute 'h '.expand('<cword')
	elseif (coc#rpc#ready())
		call CocActionAsync('doHover')
	else
		execute '!' . &keywordrg . " " . expand('<cword>')
	endif
endfunction

" Vim Slime
let g:slime_target = "neovim"
xmap <C-b><C-b> <Plug>SlimeRegionSend
nmap <C-b><C-b> <Plug>SlimeParagraphSend
nmap <C-b>v     <Plug>SlimeConfig
autocmd FileType haskell so ~/dots/vim-slime/haskell.vim
autocmd FileType python  so ~/dots/vim-slime/python.vim

" Vimspector
nmap <F5>  <Plug>VimspectorContinue
nmap <F3>  <Plug>VimspectorStop
nmap <F4>  <Plug>VimspectorRestart
nmap <F6>  <Plug>VimspectorPause
nmap <C-B> <Plug>VimspectorToggleBreakpoint
nmap <F9>  <Plug>VimspectorStepOver
nmap <F10> <Plug>VimspectorStepInto
nmap <F12> <Plug>VimspectorStepOut

" Lua configs
lua <<EOF

require("nvim-treesitter.configs").setup {
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

require("zen-mode").setup {
	-- https://github.com/folke/zen-mode.nvim#%EF%B8%8F-configuration
	window = {
		width = 83
	}
}

EOF

