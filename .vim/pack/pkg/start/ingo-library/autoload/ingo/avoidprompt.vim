" ingo/avoidprompt.vim: Functions for echoing text without the hit-enter prompt.
"
" DESCRIPTION:
"   When using the :echo or :echomsg commands with a long text, Vim will show a
"   'Hit ENTER' prompt (|hit-enter|), so that the user has a chance to actually
"   read the entire text. In most cases, this is good; however, some mappings
"   and custom commands just want to echo additional, secondary information
"   without disrupting the user. Especially for mappings that are usually
"   repeated quickly "/foo<CR>, n, n, n", a hit-enter prompt would be highly
"   irritating.
"   This script provides an :echo replacement which truncates lines so that the
"   hit-enter prompt doesn't happen. The echoed line is too long if it is wider
"   than the width of the window, minus cmdline space taken up by the ruler and
"   showcmd features. The non-standard widths of <Tab>, unprintable (e.g. ^M)
"   and double-width characters (e.g. Japanese Kanji) are taken into account.

" DEPENDENCIES:
"   - ingo/compat.vim autoload script
"   - ingo/strdisplaywidth.vim autoload script
"
" TODO:
"   - Consider 'cmdheight', add argument isSingleLine.
"
" Copyright: (C) 2008-2016 Ingo Karkat
"   The VIM LICENSE applies to this script; see ':help copyright'.
"
" Maintainer:	Ingo Karkat <ingo@karkat.de>
"
" REVISION	DATE		REMARKS
"   1.028.003	18-Nov-2016	ENH: Add optional a:reservedColumns also to
"				ingo#avoidprompt#TruncateTo(), and pass this
"				from ingo#avoidprompt#Truncate().
"				ingo#avoidprompt#TruncateTo(): The strright()
"				cannot precisely account for the rendering of
"				tab widths. Check the result, and if necessary,
"				remove further characters until we go below the
"				limit.
"   1.026.002	11-Aug-2016	ENH: ingo#avoidprompt#TruncateTo() has a
"				configurable ellipsis string
"				g:IngoLibrary_TruncateEllipsis, now defaulting
"				to a single-char UTF-8 variant if we're in such
"				encoding. Thanks to Daniel Hahler for sending a
"				patch! It also handles pathologically small
"				lengths that only show / cut into the ellipsis.
"   1.008.001	07-Jun-2013	file creation from EchoWithoutScrolling.vim

function! ingo#avoidprompt#MaxLength()
    let l:maxLength = &columns

    " Account for space used by elements in the command-line to avoid
    " 'Hit ENTER' prompts.
    " If showcmd is on, it will take up 12 columns.
    " If the ruler is enabled, but not displayed in the status line, it
    " will in its default form take 17 columns.  If the user defines
    " a custom &rulerformat, they will need to specify how wide it is.
    if has('cmdline_info')
	if &showcmd == 1
	    let l:maxLength -= 12
	else
	    let l:maxLength -= 1
	endif
	if &ruler == 1 && has('statusline') && ((&laststatus == 0) || (&laststatus == 1 && winnr('$') == 1))
	    if &rulerformat == ''
		" Default ruler is 17 chars wide.
		let l:maxLength -= 17
	    elseif exists('g:rulerwidth')
		" User specified width of custom ruler.
		let l:maxLength -= g:rulerwidth
	    else
		" Don't know width of custom ruler, make a conservative
		" guess.
		let l:maxLength -= &columns / 2
	    endif
	endif
    else
	let l:maxLength -= 1
    endif
    return l:maxLength
endfunction

function! ingo#avoidprompt#TruncateTo( text, length, ... )
"*******************************************************************************
"* PURPOSE:
"   Truncate a:text to a maximum of a:length virtual columns by dropping text in
"   the middle of a:text if necessary. This is based on what Vim does when for
"   example echoing a very long search pattern during n/N.
"* SEE ALSO:
"   - ingo#strdisplaywidth#TruncateTo() does something similar, but truncates at
"     the end, and doesn't account for buffer-local tabstop values.
"* ASSUMPTIONS / PRECONDITIONS:
"   The ellipsis can be configured by g:IngoLibrary_TruncateEllipsis.
"* EFFECTS / POSTCONDITIONS:
"   none
"* INPUTS:
"   a:text	Text which may be truncated to fit.
"   a:length	Maximum virtual columns for a:text.
"   a:reservedColumns	Optional number of columns that are already taken in the
"			line (before a:text, this matters for tab rendering); if
"			specified, a:text will be truncated to (MaxLength() -
"			a:reservedColumns).
"* RETURN VALUES:
"   Truncated a:text.
"*******************************************************************************
    if a:length <= 0
	return ''
    endif
    let l:reservedColumns = (a:0 > 0 ? a:1 : 0)
    let l:reservedPadding = repeat(' ', l:reservedColumns)

    " The \%<23v regexp item uses the local 'tabstop' value to determine the
    " virtual column. As we want to echo with default tabstop 8, we need to
    " temporarily set it up this way.
    let l:save_ts = &l:tabstop
    setlocal tabstop=8

    let l:text = a:text
    try
	if ingo#strdisplaywidth#HasMoreThan(l:reservedPadding . l:text, a:length + l:reservedColumns)
	    let l:ellipsisLength = ingo#compat#strchars(g:IngoLibrary_TruncateEllipsis)

	    " Handle pathological cases.
	    if a:length == l:ellipsisLength
		return g:IngoLibrary_TruncateEllipsis
	    elseif a:length < l:ellipsisLength
		return ingo#compat#strcharpart(g:IngoLibrary_TruncateEllipsis, 0, a:length)
	    endif

	    " Consider the length of the (configurable) "..." ellipsis.
	    " 1 must be added because columns start at 1, not 0.
	    let l:length = a:length - l:ellipsisLength + 1
	    let l:frontCol = l:length / 2
	    let l:backCol  = (l:length % 2 == 0 ? (l:frontCol - 1) : l:frontCol)
"**** echomsg '**** ' a:length ':' l:frontCol '-' l:backCol
	    while 1
		let l:fullText =
		\   ingo#strdisplaywidth#strleft(l:reservedPadding . l:text, l:frontCol) .
		\   g:IngoLibrary_TruncateEllipsis .
		\   ingo#strdisplaywidth#strright(l:text, l:backCol)

		" The strright() cannot precisely account for the rendering of
		" tab widths. Check the result, and if necessary, remove further
		" characters until we go below the limit.
		if ! ingo#strdisplaywidth#HasMoreThan(l:fullText, a:length + l:reservedColumns)
		    let l:text = strpart(l:fullText, l:reservedColumns)
		    break
		endif
		if l:backCol > 0
		    let l:backCol -= 1
		else
		    let l:frontCol -= 1
		endif
	    endwhile
	endif
    finally
	let &l:tabstop = l:save_ts
    endtry
    return l:text
endfunction
function! ingo#avoidprompt#Truncate( text, ... )
"*******************************************************************************
"* PURPOSE:
"   Truncate a:text so that it can be echoed to the command line without causing
"   the "Hit ENTER" prompt (if desired by the user through the 'shortmess'
"   option). Truncation will only happen in (the middle of) a:text.
"* ASSUMPTIONS / PRECONDITIONS:
"   none
"* EFFECTS / POSTCONDITIONS:
"   none
"* INPUTS:
"   a:text	Text which may be truncated to fit.
"   a:reservedColumns	Optional number of columns that are already taken in the
"			line (before a:text, this matters for tab rendering); if
"			specified, a:text will be truncated to (MaxLength() -
"			a:reservedColumns).
"* RETURN VALUES:
"   Truncated a:text.
"*******************************************************************************
    if &shortmess !~# 'T'
	" People who have removed the 'T' flag from 'shortmess' want no
	" truncation.
	return a:text
    endif

    let l:reservedColumns = (a:0 > 0 ? a:1 : 0)
    let l:maxLength = ingo#avoidprompt#MaxLength() - l:reservedColumns

    return ingo#avoidprompt#TruncateTo( a:text, l:maxLength, l:reservedColumns )
endfunction

function! ingo#avoidprompt#TranslateLineBreaks( text )
"*******************************************************************************
"* PURPOSE:
"   Translate embedded line breaks in a:text into a printable characters to
"   avoid that a single-line string is split into multiple lines (and thus
"   broken over multiple lines or mostly obscured) by the :echo command and
"   ingo#avoidprompt#Echo() functions.
"
"   For the :echo command, strtrans() is not necessary; unprintable characters
"   are automatically translated (and shown in a different highlighting, an
"   advantage over indiscriminate preprocessing with strtrans()). However, :echo
"   observes embedded line breaks (in contrast to :echomsg), which would mess up
"   a single-line message that contains embedded \n = <CR> = ^M or <LF> = ^@.
"
"   For the :echomsg and :echoerr commands, neither strtrans() nor this function
"   are necessary; all translation is done by the built-in command.
"
"* LIMITATIONS:
"   When :echo'd, the translated line breaks are not rendered with the typical
"   'SpecialKey' highlighting.
"
"* ASSUMPTIONS / PRECONDITIONS:
"	? List of any external variable, control, or other element whose state affects this procedure.
"* EFFECTS / POSTCONDITIONS:
"	? List of the procedure's effect on each external variable, control, or other element.
"* INPUTS:
"   a:text	Text.
"* RETURN VALUES:
"   Text with translated line breaks; the text will :echo into a single line.
"*******************************************************************************
    return substitute(a:text, "[\<CR>\<LF>]", '\=strtrans(submatch(0))', 'g')
endfunction

function! ingo#avoidprompt#Echo( text )
    echo ingo#avoidprompt#Truncate(a:text)
endfunction
function! ingo#avoidprompt#EchoAsSingleLine( text )
    echo ingo#avoidprompt#Truncate(ingo#avoidprompt#TranslateLineBreaks(a:text))
endfunction

" vim: set ts=8 sts=4 sw=4 noexpandtab ff=unix fdm=syntax :
