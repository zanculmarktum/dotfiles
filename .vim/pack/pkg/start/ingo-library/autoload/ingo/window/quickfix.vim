" ingo/window/quickfix.vim: Functions for the quickfix window.
"
" DEPENDENCIES:
"
" Copyright: (C) 2010-2015 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.023.003	07-Feb-2015	Improve heuristics of
"				ingo#window#quickfix#IsQuickfixList() to also
"				handle empty location list (with non-empty
"				quickfix list).
"				Add
"				ingo#window#quickfix#TranslateVirtualColToByteCount()
"				from autoload/QuickFixCurrentNumber.vim.
"   1.016.002	10-Dec-2013	Add ingo#window#quickfix#GetList() and
"				ingo#window#quickfix#SetList().
"   1.004.001	08-Apr-2013	file creation from autoload/ingowindow.vim

function! ingo#window#quickfix#IsQuickfixList( ... )
"******************************************************************************
"* PURPOSE:
"   Check whether the current window is the quickfix window or a location list.
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"* EFFECTS / POSTCONDITIONS:
"   None.
"* INPUTS:
"   determineType   Flag whether it should also be attempted to determine the
"		    type (global quickfix / local location list).
"* RETURN VALUES:
"   Boolean when no a:determineType is given. Else:
"   1 if the current window is the quickfix window.
"   2 if the current window is a location list window.
"   0 for any other window.
"******************************************************************************
    if &buftype !=# 'quickfix'
	return 0
    elseif a:0
	" Try to determine the type.
	" getloclist(0) inside a location list returns the displayed location
	" list. A quickfix window cannot have a location list, so we can use
	" that to determine that we're in a quickfix window.
	if empty(getloclist(0))
	    " Cornercase: We may be in an empty location list window; do not
	    " fall back to the quickfix list, then.
	    if line('$') == 1 && ! empty(getqflist())
		return 2
	    else
		return 1
	    endif
	else
	    return 2
	endif
    else
	return 1
    endif
endfunction
function! ingo#window#quickfix#ParseFileFromQuickfixList()
    return (ingo#window#quickfix#IsQuickfixList() ? matchstr(getline('.'), '^.\{-}\ze|') : '')
endfunction

function! ingo#window#quickfix#GetList()
"******************************************************************************
"* PURPOSE:
"   Return a list with all the quickfix / location list errors of the current
"   window.
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"* EFFECTS / POSTCONDITIONS:
"   None.
"* INPUTS:
"   None.
"* RETURN VALUES:
"   List.
"   Throws exception if the current window is no quickfix / location list.
"******************************************************************************
    let l:quickfixType = ingo#window#quickfix#IsQuickfixList(1)
    if l:quickfixType == 0
	throw 'GetList: Not in quickfix window'
    elseif l:quickfixType == 1
	return getqflist()
    elseif l:quickfixType == 2
	return getloclist(0)
    else
	throw 'ASSERT: Invalid quickfix type: ' . l:quickfixType
    endif
endfunction
function! ingo#window#quickfix#SetList( ... )
"******************************************************************************
"* PURPOSE:
"   Change or replace the quickfix / location list errors of the current window.
"* ASSUMPTIONS / PRECONDITIONS:
"   None.
"* EFFECTS / POSTCONDITIONS:
"   None.
"* INPUTS:
"   a:list      Error list, like |setqflist()|.
"   a:action    Optional action, like |setqflist()|.
"* RETURN VALUES:
"   Returns zero for success, -1 for failure.
"   Throws exception if the current window is no quickfix / location list.
"******************************************************************************
    let l:quickfixType = ingo#window#quickfix#IsQuickfixList(1)
    if l:quickfixType == 0
	throw 'SetList: Not in quickfix window'
    elseif l:quickfixType == 1
	return call('setqflist', a:000)
    elseif l:quickfixType == 2
	return call('setloclist', [0] + a:000)
    else
	throw 'ASSERT: Invalid quickfix type: ' . l:quickfixType
    endif
endfunction

function! ingo#window#quickfix#TranslateVirtualColToByteCount( qfEntry )
    let l:bufNr = a:qfEntry.bufnr
    if l:bufNr == 0 || ! a:qfEntry.vcol
	" As the buffer doesn't exist, we can't do any translation. Just return
	" the byte index (even if it may in fact be a virtual column).
	" If vcol isn't set, no need for translation.
	return a:qfEntry.col
    endif

    let l:neededTabstop = getbufvar(l:bufNr, '&tabstop')
    if l:neededTabstop != &tabstop
	let l:save_tabstop = &l:tabstop
	let &l:tabstop = l:neededTabstop
    endif
	let l:translatedCol = len(matchstr(getbufline(l:bufNr, a:qfEntry.lnum)[0], '^.*\%<'.(a:qfEntry.col + 1).'v'))
    if exists('l:save_tabstop')
	let &l:tabtop = l:save_tabstop
    endif

    return l:translatedCol
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
