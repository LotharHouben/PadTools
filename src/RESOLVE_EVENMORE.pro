; RESOLVE_EVENMORE
; Modification of RSI's RESOLVE_ALL.
; (This routine is a more persistent version of the original)
; Call:  RESOLVE_EVENMORE [,QUIET=quiet]
;          QUIET:  Boolean.   Set it to prevent console messages.
; Triggers an error if one or more procs/funcs can't be resolved.
; Peter Mason, CSIRO DEM, Nov. 1996.
; Use at your own risk.

; -------------------------------
; Support function...
pro rslvvnmr_doit,names,count,unres,nunres,funcs=funcs
funcs=keyword_set(funcs)
nnames=n_elements(names)
catch,e
if (e ne 0) then begin;error compiling a routine
; print,'Failed to compile '+n+' (this time)'
  unres=[temporary(unres),n]
  nunres=nunres+1L;add routine to bad list
  if (i lt (nnames-1L)) then begin;carry on resolutely
    i=i+1L &goto,redo;..with the next name
  endif else return;none left - return
endif
i=0L
while (i lt nnames) do begin;process given routine names
redo:
  n=names(i)
  if (nunres gt 0L) then try=(max(unres eq n) eq 0) else try=1
  if try then begin;only try names not on our bad list
    resolve_routine,n,is_func=funcs
    count=count+1L
  endif
  i=i+1L
endwhile
return
end

; -------------------------------
PRO resolve_evenmore, QUIET=quiet
on_error,2
if n_elements(quiet) ne 0 then begin
    quiet_save=!quiet &!quiet=quiet
endif else quiet=!quiet
iter=0 &maxiter=2;allow for some "nesting of unknowns"
redo:
unresp='' &nunresp=0L
unresf='' &nunresf=0L;initialise bookkeeping lists
repeat begin
  cnt=0L
  a=routine_names(/proc,/unresolved)
  if strlen(a(0)) gt 0 then begin
    if quiet eq 0 then print,'Resolving procedures: ', a
    rslvvnmr_doit,a,cnt,unresp,nunresp
  endif
  a=routine_names(/func,/unresolved)
  if strlen(a(0)) gt 0 then begin
    if quiet eq 0 then print,'Resolving functions: ', a
    rslvvnmr_doit,a,cnt,unresf,nunresf,/func
  endif
endrep until cnt eq 0L
if ((nunresp gt 0L) or (nunresf gt 0L)) and (iter lt maxiter) then begin
  iter=iter+1
  goto,redo;one more time, with feeling
endif
if n_elements(quiet_save) ne 0 then !quiet=quiet_save
; At this point we're no longer concerned with our (private) unresolved lists.
; Rather, we check to see whether IDL (itself) still "thinks" some routines
; are unresolved.
unresp=routine_names(/proc,/unresolved)
nunresp=n_elements(unresp)*keyword_set(unresp(0))
unresf=routine_names(/func,/unresolved)
nunresf=n_elements(unresf)*keyword_set(unresf(0))
if (nunresp gt 0) or (nunresf gt 0) then begin;fail with an error message
  if (quiet eq 0) and (nunresp gt 0) then print,'Unresolved procedures:  ',unresp
  if (quiet eq 0) and (nunresf gt 0) then print,'Unresolved functions:   ',unresf
  m='RESOLVE_EVENMORE failed to resolve '
  if (nunresp gt 0) then m=temporary(m)+strtrim(nunresp,2)+' procedures'
  if (nunresp gt 0) and (nunresf gt 0) then m=temporary(m)+' and '
  if (nunresf gt 0) then m=temporary(m)+strtrim(nunresf,2)+' functions.'
  message,m
endif
return
end
