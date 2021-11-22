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
	
	Plug 'ghifarit53/tokyonight-vim'
	Plug 'vim-airline/vim-airline'
	Plug 'mhinz/vim-startify'
	Plug 'preservim/nerdtree'
	Plug 'nvim-lua/completion-nvim'

call plug#end()


set termguicolors

let g:tokyonight_style = 'night' 
let g:tokyonight_enable_italic = 1
let g:airline_theme = "tokyonight"
let g:airline_powerline_fonts = 1

colorscheme tokyonight


" Remapping Splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Nerdtree bindings
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>

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

let g:completion_enable_snippet = 'UltiSnips'
