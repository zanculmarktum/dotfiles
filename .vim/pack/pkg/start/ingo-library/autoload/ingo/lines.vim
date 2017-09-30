" ingo/lines.vim: Functions for line manipulation.
"
" DEPENDENCIES:
"
" Copyright: (C) 2012-2015 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.024.005	12-Feb-2015	FIX: Also correctly set change marks when
"				replacing entire buffer with
"				ingo#lines#Replace(). (The :delete clobbered
"				them.)
"   1.019.004	19-May-2014	ENH: Make ingo#lines#Replace() handle
"				replacement with nothing (empty List) and
"				replacing the entire buffer (without leaving an
"				additional empty line).
"   1.004.003	04-Apr-2013	Drop the :silent in ingolines#PutWrapper().
"				Move into ingo-library.
"	002	02-Sep-2012	ENH: Avoid clobbering the expression register.
"	001	16-Aug-2012	file creation from LineJuggler.vim autoload
"				script.

function! ingo#lines#PutWrapper( lnum, putCommand, lines )
"******************************************************************************
"* PURPOSE:
"   Insert a:lines into the current buffer at a:lnum without clobbering the
"   expression register.
"* SEE ALSO:
"   If you don't need the 'report' message, setting of change marks, and
"   handling of a string containing newlines, you can just use built-in
"   append().
"* ASSUMPTIONS / PRECONDITIONS:
"   Current buffer is modifiable.
"* EFFECTS / POSTCONDITIONS:
"   To suppress a potential message based on 'report', invoke this function with
"   :silent.
"   Sets change marks '[,'] to the inserted lines.
"* INPUTS:
"   a:lnum  Address for a:putCommand.
"   a:putCommand    The :put[!] command that is used.
"   a:lines         List of lines or string (where lines are separated by \n
"		    characters).
"* RETURN VALUES:
"   None.
"******************************************************************************
    if v:version < 703 || v:version == 703 && ! has('patch272')
	" Fixed by 7.3.272: ":put =list" does not add empty line for trailing
	" empty item
	if type(a:lines) == type([]) && len(a:lines) > 1 && empty(a:lines[-1])
	    " XXX: Vim omits an empty last element when :put'ting a List of lines.
	    " We can work around that by putting a newline character instead.
	    let a:lines[-1] = "\n"
	endif
    endif

    " Avoid clobbering the expression register.
    let l:save_register = getreg('=', 1)
	execute a:lnum . a:putCommand '=a:lines'
    let @= = l:save_register
endfunction
function! ingo#lines#PutBefore( lnum, lines )
    if a:lnum == line('$') + 1
	call ingo#lines#PutWrapper((a:lnum - 1), 'put',  a:lines)
    else
	call ingo#lines#PutWrapper(a:lnum, 'put!',  a:lines)
    endif
endfunction
function! ingo#lines#Replace( startLnum, endLnum, lines, ... )
"******************************************************************************
"* PURPOSE:
"   Replace the range of a:startLnum,a:endLnum with the List of lines (or string
"   where lines are separated by \n characters).
"* ASSUMPTIONS / PRECONDITIONS:
"   Current buffer is modifiable.
"* EFFECTS / POSTCONDITIONS:
"   Sets change marks '[,'] to the replaced lines.
"* INPUTS:
"   a:startLnum     First line to be replaced. Use ingo#range#NetStart() if
"		    necessary.
"   a:endLnum       Last line to be replaced. Use ingo#range#NetEnd() if
"		    necessary.
"   a:lines         List of lines or string (where lines are separated by \n
"		    characters).
"   a:register      Optional register to store the replaced lines. By default
"		    goes into black-hole.
"* RETURN VALUES:
"   None.
"******************************************************************************
    let l:isEntireBuffer = (a:startLnum <= 1 && a:endLnum == line('$'))
    silent execute printf('%s,%sdelete %s', a:startLnum, a:endLnum, (a:0 ? a:1 : '_'))
    if ! empty(a:lines)
	silent call ingo#lines#PutBefore(a:startLnum, a:lines)
	if l:isEntireBuffer
	    silent $delete _

	    call setpos("'[", [0, 1, 1, 0])
	    call setpos("']", [0, line('$'), 1, 0])
	endif
    endif
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
