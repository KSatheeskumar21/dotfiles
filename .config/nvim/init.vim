set nocompatible
set splitbelow splitright
set number
set undodir=~/.config/nvim/undodir
filetype off

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

" Plugins

call plug#begin('~/.vim/plugged')
Plug 'gmarik/Vundle.vim' " Vundle

" Colour Schemes
Plug 'ghifarit53/tokyonight-vim'
Plug 'romgrk/doom-one.vim'

" Status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
"Plug 'itchyny/lightline.vim'

" Start page
" Plug 'mhinz/vim-startify'

" File manager
Plug 'preservim/nerdtree'
Plug 'ryanoasis/vim-devicons'

" Completion
" Plug 'nvim-lua/completion-nvim'
" Plug 'neovim/nvim-lspconfig'
Plug 'nvim-treesitter/nvim-treesitter'

Plug 'neoclide/coc.nvim', { 'branch': 'master', 'do': 'yarn install --frozen-lockfile' }

" NeOrg
Plug 'nvim-neorg/neorg' | Plug 'nvim-lua/plenary.nvim'

" Org mode
Plug 'jceb/vim-orgmode'

"Junegunn Choice Plugins
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/vim-emoji'

" Syntax highlighting
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'kovetskiy/sxhkd-vim'
Plug 'vim-python/python-syntax'
Plug 'ap/vim-css-color'
Plug 'rstacruz/vim-closer'
Plug 'tpope/vim-endwise'

call plug#end()

filetype plugin indent on

set termguicolors

let g:tokyonight_style = 'night'
let g:tokyonight_enable_italic = 1
let g:airline_theme = "tokyonight"
" Set this to 1 for arrows 
let g:airline_powerline_fonts = 1

" Statusline theme
" let g:lightline = {
" 		\ 'colorscheme': 'darcula',
" 		\}

" Always show statusline
set laststatus=2
set noshowmode

" Colorscheme of choice
colorscheme tokyonight

" Remapping Splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Mouse
set mouse=nicr
set mouse=a

" Cursor Style
set guicursor=

" Nerdtree 
" autocmd vimenter * NERDTree
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
let g:NERDTreeDirArrowExpandable = '→'
let g:NERDTreeDirArrowCollapsible = '↓'
let NERDTreeShowLineNumbers=1
let NERDTreeShowHidden=1
let NERDTreeMinimalUI = 1
let NERDTreeWinSize = 38

" Startify Stuff
" Session Directory
" let g:startify_session_dir="~/.config/nvim/session"

" Home page
" let g:startify_lists = [
" 			\ { 'type' : 'files',   'header': [' Files' ] },
" 			\ { 'type' : 'sessions',     'header': [' Sessions' ] },
" 			\ { 'type' : 'bookmarks',     'header': [' Bookmarks' ] },
" 			\ ]
" Bookmarks
" let g:startify_bookmarks = [
" 			\ { 'l' : '~/.config/leftwm/config.toml' },
" 			\ { 'i' : '~/.config/nvim/init.vim' },
" 			\ { 'f' : '~/.config/fish/config.fish' },
" 			\ { 'p' : '~/.config/qtile/config.py' },
" 			\ { 'a' : '~/.config/alacritty/alacritty.yml' },
" 			\ ]

" let g:startify_session_reload = 1

" Completion-nvim
" autocmd BufEnter * lua require 'completion'.on_attach()

" Nvim-lspconfig
" lua << EOF
" require'lspconfig'.pyright.setup{}
" require'lspconfig'.rust_analyzer.setup{}
" require'lspconfig'.haskell-language-server.setup{}
" require'lspconfig'.gopls.setup{}
" require'lspconfig'.tsserver.setup{}
" EOF

" let g:completion_enable_snippet = 'NeoSnippets'

" Completion

" vim-lsp-settings won't detect hls automatically as of today (2020-10-26). Let's teach it:
if (executable('haskell-language-server-wrapper'))
  au User lsp_setup call lsp#register_server({
      \ 'name': 'haskell-language-server-wrapper',
      \ 'cmd': {server_info->['haskell-language-server-wrapper', '--lsp']},
      \ 'whitelist': ['haskell'],
      \ })
endif

" Decorations
augroup lsp_install
    au!
    let g:lsp_signs_enabled = 1         " enable signs
    let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode
    let g:lsp_signs_error = {'text': '✗'}
    let g:lsp_signs_warning = {'text': '‼'}
    let g:lsp_highlight_references_enabled = 1
    highlight clear LspWarningLine
    highlight lspReference ctermfg=red guifg=red ctermbg=green guibg=green
    highlight lspReference guibg=#303010

    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

lua <<EOF
require 'nvim-treesitter.configs'.setup {
	ensure_installed = "python", "haskell", "go", "rust", "typescript",
	sync_install = false,
	highlight = {
		enable = true,
		addition_vim_regex_highlighting = false,
	},
}
EOF

" Sizing Bug with Alacritty
autocmd VimEnter * :silent exec "!kill -s SIGWINCH $PPID"

" Other
let g:python_highlight_all = 1
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org	     call org#SetOrgFileType()
" set guifont=Mononoki\ Nerd\ Font:h15
" let g:neovide_transparency=0.95
