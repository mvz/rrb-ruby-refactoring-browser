ruby require 'rrb/vim_interface.rb'

" Refactoring Commands
command -nargs=1 RRBRenameVariable :call s:RRBRenameVariable(expand("<args>"))
command -nargs=1 -range RRBExtractMethod :call s:RRBExtractMethod(expand("<args>"), <line1>, <line2>)
command -nargs=1 RRBRenameMethodAll :call s:RRBRenameMethodAll(expand("<args>"))


" Global variables
let g:RRBMessage = ""
let g:RRBError = ""

" Functions
function s:RRBClearMessage()
  let g:RRBMessage = ""
  let g:RRBError = ""
endfunction

function s:RRBRenewBuffers()
  let cur = bufnr("%")
  let last = bufnr("$")
  let i = 1
  while i <= last
    if bufexists(i) && (match(bufname(i), "\\.rb$") != -1)
      execute("buffer " . i)
      silent edit
    endif
    let i = i + 1
  endwhile
  execute("buffer " . cur)
endfunction

function s:RRBRefactor(exe)
  " initialize
  call s:RRBClearMessage()
  " refactoring
  execute "ruby " . a:exe
  " output
  if g:RRBError == ""
    silent edit
    call s:RRBRenewBuffers()
    echo g:RRBMessage
  else
    echo g:RRBError
  endif
endfunction

function s:RRBRenameVariable(var)
  call s:RRBRefactor("RRB::VimInterface.rename_var(\"" . a:var . "\");")
endfunction

function s:RRBExtractMethod(method, line1, line2)
  call s:RRBRefactor("RRB::VimInterface.extract_method(\"" . a:method . "\"," . a:line1 . "," . a:line2 . ")")
endfunction

function s:RRBRenameMethodAll(method)
  call s:RRBRefactor("RRB::VimInterface.rename_method_all(\"" . a:method . "\");")
endfunction

" call rrbcui
" !rrbcui -w *.rb