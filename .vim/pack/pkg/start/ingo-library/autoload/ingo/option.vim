" ingo/option.vim: Functions for dealing with Vim options.
"
" DEPENDENCIES:
"
" Copyright: (C) 2014-2016 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.028.004	31-Oct-2016	Add ingo#option#Join().
"   1.024.003	06-Mar-2015	Add ingo#option#SplitAndUnescape().
"   1.021.002	12-Jun-2014	Add ingo#option#Contains() and
"				ingo#option#ContainsOneOf().
"   1.020.001	03-Jun-2014	file creation

function! ingo#option#Split( optionValue, ... )
    return call('split', [a:optionValue, '\%(\%(^\|[^\\]\)\%(\\\\\)*\\\)\@<!,'] + a:000)
endfunction
function! ingo#option#SplitAndUnescape( optionValue, ... )
    return map(call('ingo#option#Split', [a:optionValue] + a:000), 'ingo#escape#Unescape(v:val, ",\\")')
endfunction

function! ingo#option#Contains( optionValue, expr )
    return (index(ingo#option#Split(a:optionValue), a:expr) != -1)
endfunction
function! ingo#option#ContainsOneOf( optionValue, list )
    let l:optionValues = ingo#option#Split(a:optionValue)
    for l:expr in a:list
	if (index(l:optionValues, l:expr) != -1)
	    return 1
	endif
    endfor
    return 0
endfunction

function! ingo#option#Join( val1, ... )
    let l:result = a:val1
    for l:val in a:000
	if empty(l:result)
	    let l:result = l:val
	elseif ! empty(l:val)
	    let l:result .= ',' . l:val
	endif
    endfor
    return l:result
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
