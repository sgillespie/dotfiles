" Load vim-plug {{{
set shell=/bin/sh
set nocompatible

filetype off
call plug#begin(expand('~/.vim/plugged'))

Plug 'fatih/vim-go'
Plug 'fholgado/minibufexpl.vim'
Plug 'nlknguyen/papercolor-theme'
Plug 'scrooloose/nerdtree', {'do' : 'make'}
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tfnico/vim-gradle'

call plug#end()
filetype plugin indent on

" Basic settings
scriptencoding utf-8
set autoindent                      " Indent like previous line
set autoread                        " Automatically reload changed files
set backspace=indent,eol,start
set clipboard=unnamed               " Use system clipboard
set copyindent                      " Copy indentation
set cursorline                      " Highlight current line
set directory=/tmp                  " Directory for swap files
set encoding=utf-8                  " Set file encoding
set expandtab                       " Expand tabs to spaces
set fillchars=vert:║
set gdefault                        " Set default to global
set guioptions+=e                   " Use GUI tabs
set guioptions-=L                   " Remove scrollbar
set guioptions-=T                   " Remove toolbar
set guioptions-=r                   " Remove scrollbar
set hidden                          " Hide buffer when abandoned
set hlsearch                        " Highlight current matches
set ignorecase                      " Ignore case when searching
set incsearch                       " Incremental search
set laststatus=2                    " Always show status line
set modelines=0
set nobackup                        " Don't create a backup of a file
set noerrorbells                    " Turn off error bells
set nolist                          " Don't show $ at ends of lines
set noswapfile                      " Don't create a swap file
set nowrap                          " Don't wrap lines
set number                          " Show line numbers
set pastetoggle=<F3>                " Key to toggle paste mode
set ruler                           " Show current line and column
set scrolloff=3                     " Minimum number of lines above/below cursor
set shiftround                      " Round indentation to multiple of shiftwidth
set shiftwidth=2                    " Number of spaces to indent
set showcmd                         " Show command at bottom of screen
set showmatch                       " Show matching bracker
set showmode                        " Show the current mode
set smartcase                       " Override ignorecase when search string has upper case characters
set smarttab                        " Use shiftwidth when inserting tabs at beginning of line
set softtabstop=2                   " Number of spaces for a tab when editing
set t_Co=256                        " Set 256 colors for terminal vim
set tabstop=2                       " Number of spaces for a tab
set title                           " Set titlebar to current file
set ttyfast                         " Fast terminal connection (faster redraw)
set visualbell                      " Use a visual bell instead of audible bell
set wildmenu                        " Enhanced command-line completion
set wildmode=list:longest           " List all matches

if has("gui_macvim")
  set macmeta                       " Enable Option key for key bindings
endif

set background=dark
:colorscheme PaperColor

" Set up GUI options
if has("gui_running")
  if has("gui_gtk2")
    :set guifont=Source\ Code\ Pro\ for\ Powerline\ 18
  else
    " I don't have these fonts!
    " :set guifont=Sauce\ Code\ Powerline\ Plus\ Nerd\ File\ Types\ Plus\ Font\ Awesome\ Plus\ Octicons\ Plus\ Pomicons:h18
    :set guifont=Source\ Code\ Pro:h14
  endif
endif


" Source .vimrc on save
augroup vimrc_changed
  autocmd!
  autocmd! bufwritepost $MYVIMRC nested source $MYVIMRC
augroup END

" NERDTree settings 
let NERDTreeBookmarksFile=expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeShowBookmarks=1
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=0
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=2
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFocus<CR>
nnoremap <leader>tc :NERDTreeCWD<CR>
nnoremap <leader>tf :NERDTreeClose<CR>:NERDTreeFind<CR>

" Minibuf Explorer settings
nnoremap <leader>bt :MBEToggle<CR>
nnoremap <leader>bf :MBEFocus<CR>
nnoremap <leader>bd :MBEbd

" Dispatch settings
nnoremap <leader>dd :Dispatch 
nnoremap <leader>do :Copen<CR>

augroup vim_startup
  "Skip this for now
  "autocmd!
  "autocmd StdinReadPre * let s:std_in=1
  "autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
augroup END

function! NERDTreeHighlightFile(extension, fg, bg, guifg, guibg)
exec 'autocmd FileType nerdtree highlight ' . a:extension .' ctermbg='. a:bg .' ctermfg='. a:fg .' guibg='. a:guibg .' guifg='. a:guifg
exec 'autocmd FileType nerdtree syn match ' . a:extension .' #^\s\+.*'. a:extension .'$#'
endfunction

call NERDTreeHighlightFile('jade', 'green', 'none', 'green', '#151515')
call NERDTreeHighlightFile('ini', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('md', 'blue', 'none', '#3366FF', '#151515')
call NERDTreeHighlightFile('yml', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('config', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('conf', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('json', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('html', 'yellow', 'none', 'yellow', '#151515')
call NERDTreeHighlightFile('styl', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('css', 'cyan', 'none', 'cyan', '#151515')
call NERDTreeHighlightFile('coffee', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('js', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('jsx', 'Red', 'none', '#ffa500', '#151515')
call NERDTreeHighlightFile('cjsx', 'Red', 'none', 'red', '#151515')
call NERDTreeHighlightFile('php', 'Magenta', 'none', '#ff00ff', '#151515')
call NERDTreeHighlightFile('go', 'cyan', 'none', 'cyan', '#151515')
"
