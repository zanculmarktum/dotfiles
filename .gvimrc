se go-=egimrLtT
se gfn=Operator\ Mono\ 10
se co=150
se lines=50
se gcr=n-v-c:blinkoff0
se t_vb=

"se ssop=tabpages
"fu SaveTabs()
"  let l:tabfile = expand('~/.vimtabs')
"  cal delete(l:tabfile)
"  exe 'mks! ' . l:tabfile
"  let l:lines = []
"  for l:l in readfile(l:tabfile)
"    if l:l =~ '^edit\s' || l:l =~ '^tabedit\s' || l:l =~ '^set split\(below\|right\)' || l:l =~ '^wincmd\s' || l:l =~ '^tabnext\s'
"      cal add(l:lines, l:l)
"    en
"  endfo
"  cal add(l:lines, '" vim: set ft=vim :')
"  cal writefile(l:lines, l:tabfile)
"endf
"",TabEnter
"if v:version >= 800
"  " TabNew and TabClosed requires 8.0 or higher
"  au VimLeave,TabNew,TabClosed * cal SaveTabs()
"el
"  au VimLeave * cal SaveTabs()
"en

fu SaveBuffers()
  if !isdirectory(expand('~/.vim/sessions'))
    cal mkdir(expand('~/.vim/sessions'), 'p')
  en
  let l:buffile = expand('~/.vim/sessions/buffers.vim')
  cal delete(l:buffile)
  let l:lines = []
  cal add(l:lines, "if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''")
  cal add(l:lines, "  let s:wipebuf = bufnr('%')")
  cal add(l:lines, "endif")
  cal add(l:lines, "")
  let l:empty = 0
  for l:buf in getbufinfo({'buflisted': 1})
    if l:buf.name == ''
      let l:empty = 1
      break
    en
    cal add(l:lines, 'badd ' . l:buf.name)
  endfo
  if !l:empty
    cal add(l:lines, "")
    cal add(l:lines, "if exists('s:wipebuf')")
    cal add(l:lines, "  silent exe 'bwipe ' . s:wipebuf")
    cal add(l:lines, "endif")
    cal add(l:lines, "unlet! s:wipebuf")
    cal add(l:lines, "")
    cal add(l:lines, '" vim: set ft=vim :')
    cal writefile(l:lines, l:buffile)
  en
endf
au VimLeave,BufAdd * cal SaveBuffers()
au VimEnter * if filereadable(expand('~/.vim/sessions/buffers.vim')) | exe 'so ' . expand('~/.vim/sessions/buffers.vim') | en

set bg=dark
"colo bubblegum-256-dark
"colo base16-onedark
colo onedark

pa airline
let g:airline#extensions#whitespace#mixed_indent_algo = 2
" Defaults to ['indent', 'trailing', 'mixed-indent-file']
let g:airline#extensions#whitespace#checks = ['indent', 'trailing']

pa airline-themes
if g:colors_name == 'bubblegum-256-dark'
  let g:airline_theme='bubblegum'
en
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''

" pa indentLine
if g:colors_name == 'bubblegum-256-dark'
  let g:indentLine_color_gui = '#808080'
elsei g:colors_name == 'base16-onedark'
  let g:indentLine_color_gui = '#545862'
elsei g:colors_name == 'onedark'
  let g:indentLine_color_gui = '#3b4048'
en

" vim:ts=2:sw=2:et
