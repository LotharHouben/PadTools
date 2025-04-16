FUNCTION CurrentlyManagedWidgets
  ; Makes sure all the widgets in the list of managed widgets are still
  ; valid, and removes those that aren't.

  COMPILE_OPT hidden
  COMMON managed,	ids, $		; IDs of widgets being managed
  			names, $	; and their names
			modalList	; list of active modal widgets

  ; initialize the lists
  IF (NOT keyword_set(ids)) THEN BEGIN
    ids = 0L
    names = 0
  ENDIF

  ; if the list is empty, it's valid
  IF (ids[0] EQ 0L) THEN RETURN, hash()

  ; which ones are valid?
  valid = where(widget_info(ids, /managed))

  ; build new lists from those that were valid in the old lists
  IF (valid[0] EQ -1) THEN BEGIN
    ids = 0L
    names = 0
  ENDIF ELSE BEGIN
    ids = ids[valid]
    names = names[valid]
 ENDELSE
  return, hash(names,ids)
END
