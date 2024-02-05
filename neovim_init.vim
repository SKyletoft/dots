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
noremap  <S-tab> <<
vmap     <tab> >gv
vmap     <S-tab> <gv

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
nnoremap S i
nnoremap W ciw

" Delete rest of line
noremap X D

" Unmap undo because it's poorly placed
noremap u <Nop>

" Pane management
noremap <C-e>   <C-w>v
noremap <C-q>   <C-w>s
noremap <C-M-w> <C-w>k
noremap <C-M-s> <C-w>j
noremap <C-M-a> <C-w>h
noremap <C-M-d> <C-w>l

" Terminal managerment
noremap <C-t> :terminal<CR>:set nonumber nocursorline<CR>i
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
set cursorline

if exists('g:started_by_firenvim')
	colorscheme zellner
	nnoremap <Esc><Esc> :call firenvim#focus_page()<CR>
else
	colorscheme monokai_pro
endif

autocmd FileType rust    setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd FileType fortran setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd FileType python  setlocal tabstop=8 softtabstop=8 shiftwidth=0 noexpandtab
autocmd FileType nim     setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd FileType kotlin  setlocal tabstop=4 softtabstop=4 shiftwidth=0 noexpandtab
autocmd FileType fut     setlocal tabstop=2 softtabstop=2 shiftwidth=0 noexpandtab
autocmd FileType haskell setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
autocmd FileType fsharp  setlocal tabstop=2 softtabstop=2 shiftwidth=0   expandtab
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
autocmd FileType tex      nmap <leader>i vipgq

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

"https://github.com/neoclide/coc.nvim/pull/3862
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
inoremap <silent><expr> <C-x><C-z> coc#pum#visible() ? coc#pum#stop() : "\<C-x>\<C-z>"
" remap for complete to use tab and <cr>
inoremap <silent><expr> <Tab>
	\ coc#pum#visible() ? coc#pum#next(1):
	\ CheckBackspace() ? "\<Tab>" :
	\ coc#refresh()
inoremap <expr><S-Tab> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
inoremap <silent><expr> <c-space> coc#refresh()

function! CheckBackspace() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1]  =~# '\s'
endfunction
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
" noremap <F5>      <Plug>VimspectorContinue
" noremap <F3>      <Plug>VimspectorStop
" noremap <F4>      <Plug>VimspectorRestart
" noremap <F6>      <Plug>VimspectorPause
" noremap <leader>b <Plug>VimspectorToggleBreakpoint
" noremap <F9>      <Plug>VimspectorStepOver
" noremap <F10>     <Plug>VimspectorStepInto
" noremap <F12>     <Plug>VimspectorStepOut

" nvim-dap
noremap <leader>b :lua require("dap").toggle_breakpoint()<CR>
noremap <leader>n :lua require("dap").step_over()<CR>
noremap <leader>s :lua require("dap").step_into()<CR>
noremap <leader>c :lua require("dap").continue()<CR>
noremap <leader>o :lua require("dapui").toggle()<CR>
noremap <F5>      :lua require("dapui").open()<CR>:lua require("dap").run_last()<CR>

" LiveShare
let g:instant_username = "u3836"

" Lua configs
lua <<EOF

-- require("nvim-treesitter.configs").setup {
	-- -- One of "all", "maintained" (parsers with maintainers), or a list of languages
	-- -- ensure_installed = "all",
 
	-- -- Install languages synchronously (only applied to `ensure_installed`)
	-- sync_install = false,
 
	-- -- List of parsers to ignore installing
	-- ignore_install = {},
 
	-- highlight = {
		-- -- `false` will disable the whole extension
		-- enable = true,

		-- -- list of language that will be disabled
		-- disable = {
			-- "markdown",
			-- "lua",
			-- "vim",
			-- "bash",
			-- "nix"
		-- },

		-- -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
		-- -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
		-- -- Using this option may slow down your editor, and you may see some duplicate highlights.
		-- -- Instead of true it can also be a list of languages
		-- additional_vim_regex_highlighting = true,
	-- },
-- }

require("zen-mode").setup {
	-- https://github.com/folke/zen-mode.nvim#%EF%B8%8F-configuration
	window = {
		width = 83,
		backdrop = 1.0
	}
}
EOF

