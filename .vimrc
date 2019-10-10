if filereadable($VIMRUNTIME . '/vimrc_example.vim')
  source $VIMRUNTIME/vimrc_example.vim
en

set hidden
set nobackup
"set noundofile
if !isdirectory(expand('~/.vim/undodir')) | call mkdir(expand('~/.vim/undodir'), 'p') | endif
let &undodir = expand('~/.vim/undodir')
set noswapfile
set nowrap
set linebreak
set autochdir " auto change working dir to the current buffer
set selection=old " prevent selection from including carriage return
set number " shows line number
"set showtabline=2 " always shows tab line
"set clipboard-=autoselect " do not copy when selecting text
"if !has('gui_running')
" set tabline=%!MyTabLine() " sets up tabline
"endif
set autoread " auto read if file has changed
set nowrapscan " don't loop back to the beginning when (/|?)'ing
set foldmethod=marker " read folds from a mark
set wildignorecase " ignore case when completing filename
set laststatus=2 " always shows status line
set scrolloff=0 " keeps {k,j}'ing until cursor reach at the top or end of the window
set wildmenu " displays completion in status line
set ttimeoutlen=0 " disables waiting after <Esc> was inputted
set splitbelow splitright " the way vim put new splitted window sure is annoys me
set history=10000 " maximum ":" commands to be remembered
set viminfo+=/10000 " maximum "/" commands to be remembered
set vb t_vb= " disables beeping

" Set how a <Tab> should looks like
set tabstop=4
set shiftwidth=4
set noexpandtab

" Indentation line
let &listchars = 'tab:¦ '
set list
" #808080
autocmd VimEnter * highlight SpecialKey ctermfg=244

packadd indentLine
let g:indentLine_char = '¦'
let g:indentLine_first_char = '¦'
let g:indentLine_showFirstIndentLevel = 1
if !has('gui_running')
  let g:indentLine_color_term = 244 " #808080
en

" Disables beeping
autocmd VimEnter * se t_vb=

" Deletes undofiles
autocmd VimLeave * for b in getbufinfo() | call delete(&undodir . '/' . fnamemodify(b.name, ':gs?/?%?')) | endfor

" Remembers current line number in l
fu Line()
  let b:l = line('.')
endf

" CRUX Pkgfile
au BufNewFile,BufRead *Pkgfile set filetype=sh

" Default keyword for *
if v:version >= 800
  " 'iskeyword' requires version 8.0 or higher
  au TabNew,BufRead * set iskeyword&
en

" Do not jump automatically when pressing *
nnoremap * *``

" EnhancedJumps plugin
au VimEnter * nmap <C-o> :<C-u>call EnhancedJumps#Jump(0,'local')<CR>
au VimEnter * nmap <C-i> :<C-u>call EnhancedJumps#Jump(1,'local')<CR>

" Allows to j, k, 0, g on wrapped line
au BufEnter * if &wrap|nnoremap <buffer> j gj|en
au BufEnter * if &wrap|nnoremap <buffer> k gk|en
au BufEnter * if &wrap|nnoremap <buffer> 0 g0|en
au BufEnter * if &wrap|nnoremap <buffer> $ g$|en

" Stop using arrow keys
"nnoremap <Up> <Nop>
"nnoremap <Down> <Nop>
"nnoremap <Left> <Nop>
"nnoremap <Right> <Nop>
"inoremap <Up> <Nop>
"inoremap <Down> <Nop>
"inoremap <Left> <Nop>
"inoremap <Right> <Nop>

" Set tabstop and shiftwidth for a specific &filetype
autocmd FileType man setlocal tabstop=8 shiftwidth=8
"autocmd FileType sh setlocal tabstop=2 shiftwidth=2 expandtab

" vim -b : edit binary using xxd-format!
function ReadBinary()
  let &bin=1

  if &bin
    %!xxd
    set ft=xxd
  endif
endfunction
function WriteBinary()
  if &bin
    %!xxd -r
  endif

  if &bin
    %!xxd
    set nomod
  endif
endfunction

" Abbreviations
"cnoreabbrev u up
"cnoreabbrev man Man

" Behave c_Ctrl-{N,P} like c_Up
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" NFO viewer
"e ++enc=cp437
"se fencs=cp437

" Allows to read man page using :Man
source $VIMRUNTIME/ftplugin/man.vim
nnoremap K :Man <cword><CR>

" Sets colorscheme
"if &term =~ 'rxvt' || &term =~ 'xterm' || &term =~ 'st-'
"  set background=dark
"  colorscheme bubblegum-256-dark
"endif
if !has('gui_running')
set term=xterm
set background=dark
let g:airline_symbols_ascii = 1
let g:airline_left_sep='>'
let g:airline_right_sep='<'
packadd airline
let g:airline#extensions#whitespace#mixed_indent_algo = 2
let g:airline#extensions#whitespace#checks = ['indent', 'trailing']
endif

" Prevents from breaking line
augroup vimrcEx
  au!
augroup END

" Creates some abbreviations for NERDTree
"cnoreabbrev to NERDTree
"cnoreabbrev tc NERDTreeClose

" Sets up cursor line looks
"if !has('gui_running')
"  highlight CursorLine term=bold cterm=bold
"endif

" Sets up tabline looks
"if has('gui_running')
"if g:colors_name
"  if g:colors_name == 'solarized'
    "highlight TabLine gui=NONE
    "highlight TabLineSel gui=reverse
    "highlight TabLineFill gui=NONE

    "highlight TabLine     term=underline cterm=underline ctermfg=15 ctermbg=8 gui=underline guifg=#839496 guibg=#073642 guisp=#839496
    "highlight TabLineSel  term=underline,reverse cterm=bold gui=underline,reverse guifg=#586e75 guibg=#eee8d5 guisp=#839496
    "highlight TabLineFill term=underline cterm=reverse gui=underline guifg=#839496 guibg=#073642 guisp=#839496
"  endif
"endif
"endif

" Fixs term color
"if !has('gui_running')
"  if has("terminfo")
"    set t_Co=8
"    set t_Sf=<Esc>[3%p1%dm
"    set t_Sb=<Esc>[4%p1%dm
"  else
"    set t_Co=8
"    set t_Sf=<Esc>[3%dm
"    set t_Sb=<Esc>[4%dm
"  endif
"endif

" Store meteorid into black hole
" so it will not affects the earth
"nnoremap x  "_x
"nnoremap d  "_d
"nnoremap dd "_dd
"
"nnoremap D  "_D
"
"vnoremap d  "_d
"vnoremap D  "_D

" Set up tabline {{{
"if !has('gui_running')
"  " Remove underline
"  highlight TabLine cterm=NONE
"
"  function MyTabLine()
"    let s = ''
"    for i in range(tabpagenr('$'))
"      let active = 0
"      " select the highlighting
"      if i + 1 == tabpagenr()
"        let active = 1
"        let s .= '%#TabLineSel#'
"      else
"        let s .= '%#TabLine#'
"      endif
"
"      " set the tab page number (for mouse clicks)
"      let s .= '%' . (i + 1) . 'T'
"
"      " the label is made by MyTabLabel()
"      let s .= ' %{MyTabLabel(' . (i + 1) . ')} '
"
"      " append some flags, e.g. [+], [-], [RO]
"      if active
"        let s .= '%m'
"        let s .= '%r'
"        "let s .= '%h'
"        let s .= '%w'
"      endif
"
"      " is there any tab that has the same label?
"      "let tabpagenr = tabpagenr('$')
"      "if tabpagenr > 1
"      "  let j = 0
"      "  let label = []
"      "  while j < tabpagenr
"      "    if MyTabLabel(i + 1) != MyTabLabel(tabpagenr - j)
"      "      call add(label, MyTabLabel(tabpagenr - j))
"      "    endif
"      "    let j += 1
"      "  endwhile
"
"      "  for item in label
"      "    if MyTabLabel(i + 1) == item
"      "      let s .= 'UCme?'
"      "    endif
"      "  endfor
"      "endif
"    endfor
"
"    " after the last tab fill with TabLineFill and reset tab page nr
"    let s .= '%#TabLineFill#%T'
"
"    " right-align the label to close the current tab page
"    if tabpagenr('$') > 1
"      let s .= '%=%#TabLine#%999XX'
"    endif
"
"    return s
"  endfunction
"
"  function MyTabLabel(n)
"    let buflist = tabpagebuflist(a:n)
"    let winnr = tabpagewinnr(a:n)
"    let bufname = bufname(buflist[winnr - 1])
"
"    " default name for empty buffer
"    if bufname == ''
"      let bufname = '[No Name]'
"    endif
"
"    " strip path
"    let bufname = fnamemodify(bufname, ':t')
"
"    return bufname
"  endfunction
"endif "}}}

" vim:ts=2:sw=2:et
