" Load vim-plug {{{
set shell=/bin/sh
set nocompatible

filetype off
call plug#begin(expand('~/.vim/plugged'))

Plug 'ap/vim-buftabline'
Plug 'diepm/vim-rest-console'
Plug 'fatih/vim-go'
Plug 'nlknguyen/papercolor-theme'
Plug 'scrooloose/nerdtree', {'do' : 'make'}
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'w0ng/vim-hybrid'

" Experimental
Plug 'rhysd/committia.vim'

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
set guioptions-=L                   " Remove scrollbar
set guioptions-=m                   " Remove menubar
set guioptions-=T                   " Remove toolbar
set guioptions-=r                   " Remove scrollbar
set hidden                          " Hide buffer when abandoned
set hlsearch                        " Highlight current matches
set ignorecase                      " Ignore case when searching
set incsearch                       " Incremental search
set iskeyword+=-                    " - is not a word separator
set laststatus=2                    " Always show status line
set modelines=0
set nobackup                        " Don't create a backup of a file
set noerrorbells                    " Turn off error bells
set nolist                          " Don't show $ at ends of lines
set noswapfile                      " Don't create a swap file
set nowrap                          " Don't wrap lines
set nonumber                        " Hide line numbers
set pastetoggle=<F3>                " Key to toggle paste mode
set ruler                           " Show current line and column
set scrolloff=0                     " Minimum number of lines above/below cursor
set shiftround                      " Round indentation to multiple of shiftwidth
set shiftwidth=4                    " Number of spaces to indent
set showcmd                         " Show command at bottom of screen
set showmatch                       " Show matching bracker
set showmode                        " Show the current mode
set smartcase                       " Override ignorecase when search string has upper case characters
set smarttab                        " Use shiftwidth when inserting tabs at beginning of line
set softtabstop=2                   " Number of spaces for a tab when editing
set t_Co=256                        " Set 256 colors for terminal vim
set tabstop=4                       " Number of spaces for a tab
set title                           " Set titlebar to current file
set ttyfast                         " Fast terminal connection (faster redraw)
set visualbell                      " Use a visual bell instead of audible bell
set wildmenu                        " Enhanced command-line completion
set wildmode=list:longest           " List all matches

if has("gui_macvim")
  set macmeta                       " Enable Option key for key bindings
endif

set background=dark
:colorscheme hybrid

" Set up GUI options
if has("gui_running")
  :set guifont=Source\ Code\ Pro\ 13
endif

" Source .vimrc on save
augroup vimrc_changed
  autocmd!
  autocmd! bufwritepost $MYVIMRC nested source $MYVIMRC
augroup END

" NERDTree settings 
let NERDTreeBookmarksFile=expand("$HOME/.vim/NERDTreeBookmarks")
let NERDTreeShowBookmarks=0
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeQuitOnOpen=0
let NERDTreeHighlightCursorline=1
let NERDTreeMouseMode=2
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <leader>f :NERDTreeFocus<CR>
nnoremap <leader>tc :NERDTreeCWD<CR>
nnoremap <leader>tf :NERDTreeClose<CR>:NERDTreeFind<CR>

" Dispatch settings
nnoremap <leader>dd :Dispatch 
nnoremap <leader>dm :Dispatch make<CR>
nnoremap <leader>do :Copen<CR>

" Buffer bindings
nnoremap <leader>bn :enew<cr>
nnoremap <c-p> :bprevious<cr>
nnoremap <c-n> :bnext<cr>
nnoremap <leader>bd :bprevious <bar> bdelete #<cr>

" Go settings
let g:go_fmt_command = "goimports"

" Haskell settings
augroup haskell
    autocmd!
    autocmd FileType haskell setlocal tabstop=8 expandtab softtabstop=4 shiftwidth=4 shiftround
augroup END

" HTML settings
augroup html
    autocmd!
    autocmd FileType html setlocal tabstop=2 expandtab softtabstop=2 shiftwidth=2 shiftround
augroup END

" ReST settings
let g:vrc_allow_get_request_body = 1
nnoremap <leader>rc :set filetype=rest<cr> 

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

