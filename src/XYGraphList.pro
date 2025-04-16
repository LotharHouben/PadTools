;; define data structure type for three-d array
FUNCTION GetXYGraphP
  print, "% GetXYGraphP: creating xy graph data structure"
  o=obj_new()
  p=PTR_NEW(CREATE_STRUCT('id', "xygraph",$ ;; the id to identify the list item
                           'name', "", $ ;; the name of the list item
                           'pobj', o))   ;; the graph object pointer 
  return, p
END 

FUNCTION XYGraphList_Init, name
  ;; create a new pointer list which will hold the xy plot window pointers
  d=PList_New(name)
  IF (NOT(PTR_VALID(d))) THEN print, "% XYGraphList_Init: Could not create XYGraph data list"
  return, d
END

FUNCTION XYGraphList_CreateElement, p, name 
;; creates a new empty element in the list with the name name
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% XYGraphList_CreateElement:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     return, 0
 END
  IF PTR_VALID(p) THEN BEGIN
     e=GetXYGraphP() ;; get a pointer to a structure
     IF PTR_VALID(e) THEN BEGIN
        IF PList_Add(p,e,name) THEN return, e
     END ELSE BEGIN
        print, "% XYGraphList_CreateElement: Could not create data instance "+name
        return, e
     END
  END else BEGIN
     print, "% XYGraphList_CreateElement: Datalist "+name+" is unknown"
     return, p
  END
END

FUNCTION XYGraphList_DeleteCurrentElement, p, PURGE=purge
;; deletes an element in the list 
;; p is the root pointer
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "%  XYGraphList_DeleteCurrentElement:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     return, 0
 END
  IF PTR_VALID(p) THEN BEGIN
     d=(*p).current
     IF (NOT(PTR_VALID(d))) THEN BEGIN
        print, "% XYGraphList_DeleteCurrentElement: current pointer is invalid"
        return, 1
     END
     e=(*d).datap ;; pointer to the xygraph structure
     IF PTR_VALID(e) THEN BEGIN
        IF PList_Delete(p,(*d).name) THEN BEGIN
           ;; store window number
           IF keyword_set(purge) THEN BEGIN
              tmp=(*e).pobj
              IF OBJ_VALID(tmp) THEN BEGIN
                 tmp->TidyUp
                 obj_destroy, tmp
              END
           END
           return, 0
        END
     END ELSE BEGIN
        print, "% XYGraphList_DeleteCurrentElement: Could not delete data instance "+name
        return, e
     END
  END ELSE BEGIN
        print, "% XYGraphList_DeleteCurrentElement: Data list "+name+" is unknown"
        return, p
     END
END

FUNCTION XYGraphList_DeleteOther, p
;; destroy the whole data list, except for the current graph
  IF PTR_VALID(p) THEN BEGIN
     d=(*p).current
     IF (NOT(PTR_VALID(d))) THEN BEGIN
        print, "% XYGraphList_DeleteCurrentElement: current pointer is invalid"
        return, 1
     END
     ;; keep the current pointer
     ;; delete all other entries
     prev=(*d).previous
     IF (PTR_VALID(prev)) THEN BEGIN
        (*prev).next=(*d).next
     END ELSE BEGIN
        ;; no previous element, the first element needs to be
        ;; redefined
        (*p).first=(*d).next
     END
     next=(*d).next
     IF (PTR_VALID(next)) THEN BEGIN
        (*next).previous=(*d).previous
        
     END ELSE BEGIN
        ;; no next element, the last element needs to be
        ;; redefined
        (*p).last=(*d).previous
     END
     ;; now we have a list without the current element d
     ;; we can delete the whole list
     e=PList_DeleteLast(p)
     While (PTR_VALID(e)) Do BEGIN
       tmp=(*e).pobj 
       IF OBJ_VALID(tmp) THEN BEGIN 
          tmp->TidyUp
          obj_destroy, tmp
       END
       PTR_FREE, e
       e=PList_DeleteLast(p)
    END
     ;; now reestablish list with element d
     (*p).first=d
     (*p).current=d
     (*p).last=d
  END
  return, 0
END

FUNCTION XYGraphList_Destroy, p
;; destroy the whole data list
  e=PList_DeleteLast(p)
  While (PTR_VALID(e)) Do BEGIN
     tmp=(*e).pobj 
     IF OBJ_VALID(tmp) THEN BEGIN 
        tmp->TidyUp
        obj_destroy, tmp
     END
     PTR_FREE, e
     e=PList_DeleteLast(p)
  END
  return, PList_Destroy(p)
END

FUNCTION XYGraphList_GetNames, p
;; returns a list with the name of all xy plot list elements in list
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

FUNCTION XYGraphList_SearchName, p, name
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

FUNCTION GetNewXYGraphContainer, fname
  ptr=GetRootPXYGraph() ;; datalistpointer
  names=DataList_GetNames(ptr)
  Selected=WHERE(names EQ fname)
  IF (Selected EQ -1) THEN BEGIN
     e=XYGraphList_CreateElement(ptr, fname)
  END ELSE BEGIN
     i=2
     REPEAT BEGIN
       altname=fname+ " id:" + MyString(i)
       Selected=WHERE(names EQ altname)
       i=i+1
     END UNTIL (Selected EQ -1)
     e=XYGraphList_CreateElement(ptr, altname)
  END
  IF PTR_VALID(e) THEN return, e ELSE return, PTR_NEW()
END 
