;; define data structure type for three-d array
FUNCTION GetEmptyDataP
  print, "% GetEmptyDataP: creating data structure"
  p=PTR_NEW(CREATE_STRUCT(name='Data', $
                          ['id', $
                           'window', $
                           'binning', $
                           'binningx', $
                           'binningy', $
                           'binningz', $
                           'type', $
                           'filenames', $
                           'voltage', $
                           'SzX', $
                           'SzY', $
                           'SzZ', $
                           'xsamp', $
                           'ysamp', $
                           'zsamp', $
                           'xunit', $
                           'yunit', $
                           'zunit', $
                           'zcoord',$
                           'slice', $
                           'slicewidth', $
                           'contrastmode', $
                           'contrastsubmode', $
                           'contrastscaling', $
                           'contrastroi', $
                           'contrastsdev', $
                           'contrast', $
                           'radongeometry', $ the radon space geometry object
                           'DisplayScaleBar', $
                           'ScaleBar', $ pointer to scalebar structure
                           'data', $  ;; the data pointer
                           'extra', $ ;; extra object such as empad
                           'note', $
                           'plink'], $  ;; pointer link to another stack     
                           'data', $    ;; the id, could be 'empad'
                          -1, $
                          1.0, $
                          1.0, $
                          1.0, $
                          1.0, $
                          1.0, $
                          PTR_NEW(), $
                          200000, $ 
                          256L, $
                          256L, $
                          256L, $
                          1.0, $
                          1.0, $
                          1.0, $
                          '1/nm', $
                          '1/nm', $
                          '1/nm', $
                          3,   $
                          0L,   $
                           1L,   $
                          'auto', $  ;; contrastmode
                          'sdev', $  ;; contrastsubmode
                          'lin', $  ;; contrastscaling 'lin','log','exp'
                          'quarter', $  ;; contrastroi
                          2.,   $  ;; contrastsdev
                          [0.0,1.0], $  ;; contrast
                          OBJ_NEW(), $
                          0B, $
                          PTR_NEW(), $
                          PTR_NEW(), $
                          OBJ_NEW(), $
                          OBJ_NEW('StaticTextBuffer', 'Image Note', MAXLINES=25), $
                          PTR_NEW() $
  ))
  return, p
END 

FUNCTION DataList_Init, name
  ;; create a new pointer list which will carry the 3D data cubes
  d=PList_New(name)
  IF (NOT(PTR_VALID(d))) THEN print, "% DataList_Init: Could not create data list"
  return, d
END

FUNCTION DataList_CreateElement, p, name, DESCR=descr
;; creates a new empty stack element in the list 
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% DataList_CreateElement:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     return, 0
 END
  IF PTR_VALID(p) THEN BEGIN
     e=GetEmptyDataP()
     IF PTR_VALID(e) THEN BEGIN
        IF PList_Add(p,e,name, DESCRSTRING=descr) THEN return, e
     END ELSE BEGIN
        print, "% DataList_CreateElement: Could not create data instance "+name
        return, e
     END
  END else BEGIN
     print, "% DataList_CreateElement: Datalist "+name+" is unknown"
     return, p
  END
END

FUNCTION DataList_DeleteCurrentElement, p
;; deletes a  stack element in the list 
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      Print, "%  DataList_DeleteCurrentElement:  Fatal error "
      Print, "%   Error status  - " + STRING(error_status)
      Print, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return, 0
   END
END
  IF PTR_VALID(p) THEN BEGIN
     d=(*p).current
     IF (NOT(PTR_VALID(d))) THEN BEGIN
        print, "% DataList_DeleteCurrentElement: current pointer is invalid"
        return, 1
     END
     e=(*d).datap
     IF PTR_VALID(e) THEN BEGIN
        IF PList_Delete(p,(*d).name) THEN BEGIN
           ;; deleting geometry object
           IF PTR_VALID((*e).radongeometry) THEN obj_destroy, (*e).radongeometry
           ;; deleting data
           PTR_FREE, (*e).data
           ;; store window number
           Wdow=(*e).window 
           ;; deleting data pointer
           PTR_FREE, e
           ;; delete window (might cause exception)
           IF (wdow GE 0) THEN WDELETE, wdow
           return, 0
        END
     END ELSE BEGIN
        print, "% DataList_DeleteCurrentElement: Could not delete data instance "+name
        return, e
     END
  END ELSE BEGIN
        print, "% DataList_DeleteCurrentElement: Data list "+name+" is unknown"
        return, p
     END
END

FUNCTION DataList_DeleteElement, list, p
;; deletes a  stack element in the list 
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "%  DataList_DeleteCurrentElement:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     return, 0
  END
 ;; store current
  IF PTR_VALID(list) THEN BEGIN
     d=(*list).current
     IF (NOT(PTR_VALID(d))) THEN BEGIN
        print, "% DataList_DeleteElement: current list pointer is invalid"
        return, 1
     END
     IF (NOT(PTR_VALID(p))) THEN BEGIN
        print, "% DataList_DeleteElement: list pointer is invalid"
        return, 1
     END
     e=(*p).datap
     IF (NOT(PTR_VALID(e))) THEN BEGIN
        print, "% DataList_DeleteElement: list pointer has empty data"
        return, 1
     END
     IF PTR_VALID(e) THEN BEGIN
        IF PList_Delete(list,(*p).name) THEN BEGIN
           ;; deleting geometry object
           IF PTR_VALID((*e).radongeometry) THEN obj_destroy, (*e).radongeometry
           ;; deleting data
           PTR_FREE, (*e).data
           ;; store window number
           Wdow=(*e).window 
           ;; deleting data pointer
           PTR_FREE, e
           ;; delete window (might cause exception)
           IF (wdow GE 0) THEN WDELETE, wdow
           return, 0
        END
     END ELSE BEGIN
        print, "% DataList_DeleteCurrentElement: Could not delete data instance "+name
        return, e
     END
  END ELSE BEGIN
        print, "% DataList_DeleteCurrentElement: Data list "+name+" is unknown"
        return, list
     END
END


FUNCTION DataList_Destroy, p
;; destroy the whole data list
  e=PList_DeleteLast(p)
  While (PTR_VALID(e)) Do BEGIN
     IF PTR_VALID((*e).data) THEN PTR_FREE, (*e).data
     IF PTR_VALID((*e).filenames) THEN PTR_FREE, (*e).filenames
     IF PTR_VALID((*e).radongeometry) THEN obj_destroy, (*e).radongeometry
     PTR_FREE, e
     e=PList_DeleteLast(p)
  END
  return, PList_Destroy(p)
END

FUNCTION DataList_GetNames, p
;; returns a list with the name of all data stacks in the stack list
  IF PTR_VALID(p) THEN BEGIN
     n=0
     t=(*p).first
     WHILE (Ptr_VALID(t)) DO BEGIN
        n=n+1
        t=(*t).next
     END
     IF (n GT 0) THEN BEGIN
        res=STRARR(n)
        t=(*p).first
        i=0
        WHILE (Ptr_VALID(t)) DO BEGIN
           res[i]=(*t).name
           t=(*t).next
           i=i+1
        END
        return, res
     END
  END
  res=["no data available"]
  return, res 
END

FUNCTION DataList_SearchName, p, name
;; searches for the first occurence of an entry with name name in the
;; stack list p
;; sets current data pointer to this element
  ret=0
  IF PTR_VALID(p) THEN BEGIN
     n=0
     t=(*p).first
     WHILE (Not(PTR_VALID(ret)) And Ptr_VALID(t)) DO BEGIN
        IF ((*t).name eq name) THEN BEGIN
           ret=t
           (*p).current=t
        END
        n=n+1
        t=(*t).next
     END
  END  
  return, ret 
END 

FUNCTION Data_GetEmptyArrayP, p
;; creates a data array on the heap and returns the pointer to it
;; p is a pointer to a data stack, like the one returned by
;; GetEmptyDataP
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% Data_GetEmptyArrayP:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     return, 0
 END
IF PTR_VALID(p) THEN BEGIN
  
  case (*p).type of
	1 : BEGIN
             APtr=PTR_NEW(BYTARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	2 : BEGIN
             APtr=PTR_NEW(INTARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	3 : BEGIN
             APtr=PTR_NEW(LONARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	4 : BEGIN
             APtr=PTR_NEW(FLTARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	5 : BEGIN
             APtr=PTR_NEW(DBLARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	6 : BEGIN
             APtr=PTR_NEW(COMPLEXARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	9 : BEGIN
             APtr=PTR_NEW(DCOMPLEXARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	12 : BEGIN
             APtr=PTR_NEW(UINTARR((*p).SzX,(*p).SzY,(*p).SzZ))
            END
	else: BEGIN
                print, "% Data_GetEmptyArrayP: invalid data type " + String((*p).type)
                return,  0
              END
  endcase
END
return, APtr
END

