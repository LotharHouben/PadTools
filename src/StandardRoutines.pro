FUNCTION GetStandardPointers, ptr, p, e, ppd 
  A = SCOPE_TRACEBACK()
  ErrPrefix="% "+MyString(A[N_ELEMENTS(A)-2])+": "
  ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
;; there is no root pointer
   printtocon, ErrPrefix+"Root pointer is invalid."
   return, 0
END
p=(*ptr).current
IF (NOT(PTR_VALID(p))) THEN BEGIN
;; there is no data in the list or nothing is highlighted
   printtocon, ErrPrefix+"Current data list pointer is invalid."
   return, 0
END
e=(*p).datap
IF (NOT(PTR_VALID(e))) THEN BEGIN
;; there is no structure of type DATA
   printtocon,  ErrPrefix+"Pointer to structure data in current data list element is invalid."
   return, 0
END
ppd=(*e).data
IF (NOT(PTR_VALID(ppd))) THEN BEGIN
;; there is no data array
   printtocon,  ErrPrefix+"Pointer to to data array is invalid."
return, 0
END
return, 1
END 


FUNCTION GetTiltAxisP, e, INIT=init
   IF OBJ_VALID((*e).radongeometry) THEN BEGIN 
     ;; geometry is defined for the stack
     TAxisP=(*e).radongeometry
  END ELSE BEGIN
     IF keyword_set(init) THEN BEGIN
        ;; geometry is not defined for this stack 
        (*e).radongeometry=OBJ_NEW('TiltSeriesObj')
        TAxisP=(*e).radongeometry
        TAxisP->set_rho, AXSTR='y',PRESET=[-FLOOR(((*e).SzY)/2),1.,(*e).SzY]
        TAxisP->set_stack, AXSTR='z', NUM=(*e).SzZ
        TAxisP->set, AXSTR='x',PRESET=[-FLOOR(((*e).SzX)/2),(*e).xsamp,(*e).SzX]
     END ELSE BEGIN
        A = SCOPE_TRACEBACK()
        ErrPrefix="% "+MyString(A[N_ELEMENTS(A)-2])+": "
        printtocon,  ErrPrefix+"Radon geometry is not defined."
        return, 0
     END
  END
  return, TAxisP
END
