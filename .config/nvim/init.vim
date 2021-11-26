set nocompatible
set splitbelow splitright
set number
filetype off

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

call plug#begin('~/.config/nvim/plugged')
	
	Plug 'gmarik/Vundle.vim' " Vundle
	"Colour Schemes
	Plug 'ghifarit53/tokyonight-vim'
        Plug 'romgrk/doom-one.vim'
	" Status line
	"Plug 'vim-airline/vim-airline'
	Plug 'itchyny/lightline.vim'
	" Start page
	Plug 'mhinz/vim-startify'
	" File manager
	Plug 'preservim/nerdtree'
	Plug 'ryanoasis/vim-devicons'
	" Completion
	Plug 'nvim-lua/completion-nvim'
	"Junegunn Choice Plugins
	Plug 'junegunn/goyo.vim'
	Plug 'junegunn/limelight.vim'
	Plug 'junegunn/vim-emoji'
	" Syntax highlighting
	Plug 'PotatoesMaster/i3-vim-syntax'
	Plug 'kovetskiy/sxhkd-vim'
	Plug 'vim-python/python-syntax'
	Plug 'ap/vim-css-color'

call plug#end()

filetype plugin indent on

set termguicolors

let g:tokyonight_style = 'night'
let g:tokyonight_enable_italic = 1
"let g:airline_theme = "tokyonight"
"let g:airline_powerline_fonts = 0

" Statusline theme
let g:lightline = {
		\ 'colorscheme': 'material',
		\}

" Always show statusline
set laststatus=2
set noshowmode

" Colorscheme of choice
colorscheme doom-one

" Remapping Splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

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
let g:startify_session_dir="~/.config/nvim/session"

" Home page
let g:startify_lists = [
			\ { 'type' : 'files',   'header': [' Files' ] },
			\ { 'type' : 'sessions',     'header': [' Sessions' ] },
			\ { 'type' : 'bookmarks',     'header': [' Bookmarks' ] },
			\ ]
" Bookmarks
let g:startify_bookmarks = [
			\ { 'l' : '~/.config/leftwm/config.toml' },
			\ { 'i' : '~/.config/nvim/init.vim' },
			\ { 'f' : '~/.config/fish/config.fish' },
			\ { 'p' : '~/.config/qtile/config.py' },
			\ { 'a' : '~/.config/alacritty/alacritty.yml' },
			\ ]

let g:startify_session_reload = 1

" Completion-nvim
autocmd BufEnter * lua require 'completion'.on_attach()

let g:completion_enable_snippet = 'NeoSnippets'

" Sizing Bug with Alacritty
autocmd VimEnter * :silent exec "!kill -s SIGWINCH $PPID"

" Other
let g:python_highlight_all = 1
au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org	     call org#SetOrgFileType()
set guifont=Mononoki\ Nerd\ Font:h15
let g:neovide_transparency=0.95
