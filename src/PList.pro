;; Create a structure element, consisting of a name, a pointer to the data 
;; and previous and next pointers


function PList_New, IdString
;; 
;; create the header: 
;; has fields: 
;; NAME
;; FIRST
;; LAST
;; CURRENT
;;

  p=Ptr_New(CREATE_STRUCT(['name','first', 'last', 'current'], IdString, PTR_NEW(), PTR_NEW(), PTR_NEW()))
         return, p
      END

function PList_Add, p, e, IdString, DESCRSTRING=descrstring
;; add an element e to p
;; returns 0 if wrong 

  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print,"% PList_Add: pointer list does not exist" 
     return, 0
  END
;; create a new data storage pointer
  IF NOT(keyword_set(descrstring)) THEN descrstring=''
  t=Ptr_New(CREATE_STRUCT(['previous','name','descriptor','datap', 'next'], PTR_NEW(), IdString,  descrstring, e, PTR_NEW()))
  IF (NOT(PTR_VALID(t))) THEN BEGIN
     print, "% Plist_Add: could not create element container" 
     return, 0
  END
  s=(*p).last
  IF (NOT(PTR_VALID(s))) THEN BEGIN
     ;; no last means no first and no current too
     (*p).first=t & (*p).last=t & (*p).current=t
     return, 1
  END
  ;; last is defined, we'll append e
  ;; first remains, last changes to new element
  (*s).next=t & (*t).previous=s & (*p).last=t 
  (*p).current=t
  ;;
  return, 1
END

function PList_Delete, p, IdString
;; returns the element pointer 
;; that has to be deleted separately!
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print,"% Plist_Delete: pointer list does not exist" 
     return, 0
  END
  ;; search for element with name IdString
  s=(*p).first
  While ((PTR_VALID(s)) AND ((*s).name NE IdString)) DO s=(*s).next
  IF (NOT(PTR_VALID(s))) THEN BEGIN
     ;; not found
          print,"% Plist_Delete: Element with name " + IdString + " not found"
     return, 0
  END
  ;; element exists, unlink the element
  nn=(*s).next & pp=(*s).previous

  IF (PTR_VALID(nn)) THEN BEGIN
     (*nn).previous=pp
  END
  IF (PTR_VALID(pp)) THEN (*pp).next=nn
  e=(*s).datap 
  ;; take care of current pointer
  IF ((*p).current EQ s) THEN BEGIN
     IF PTR_VALID(nn) THEN (*p).current=nn ELSE (*p).current=pp
  END
  ;; take care of last pointer
  IF ((*p).last EQ s) THEN BEGIN
     (*p).last=pp
  END
  ;; take care of first pointer
  IF ((*p).first EQ s) THEN BEGIN
     (*p).first=nn
  END
  ;; free ptr
  PTR_FREE, s
  return, e
END

function PList_DeleteLast, p
;; returns the element pointer 
;; that has to be deleted separately!
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print,"% PList_DeleteLast: pointer list does not exist" 
     return, 0
  END
  ;; search for element with name IdString
  s=(*p).last
  IF (NOT(PTR_VALID(s))) THEN BEGIN
     ;; not found
          print,"% PList_DeleteLast: Empty list"
     return, 0
  END
  ;; element exists, unlink the element
  nn=(*s).next & pp=(*s).previous

  IF (PTR_VALID(nn)) THEN BEGIN
     (*nn).previous=pp
  END
  IF (PTR_VALID(pp)) THEN (*pp).next=nn
  e=(*s).datap 
  ;; take care of current pointer
  IF ((*p).current EQ s) THEN BEGIN
     IF PTR_VALID(nn) THEN (*p).current=nn ELSE (*p).current=pp
  END
  ;; take care of last pointer
  IF ((*p).last EQ s) THEN BEGIN
     (*p).last=pp
  END
  ;; take care of last pointer
  IF ((*p).first EQ s) THEN BEGIN
     (*p).first=nn
  END
  ;; free ptr
  PTR_FREE, s
  return, e
END

function PList_GetDataP, p, IdString
;; returns the element pointer 
;; that has to be deleted separately!
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print,"% PList_GetDataP: pointer list does not exist" 
     return, 0
  END
  ;; search for element with name IdString
  s=(*p).first
  While ((PTR_VALID(s)) AND ((*s).name NE IdString)) DO s=(*s).next
  IF (NOT(PTR_VALID(s))) THEN BEGIN
     ;; not found
          print,"% PList_GetDataP: Element with name " + IdString + " not found"
     return, 0
  END 
  return, (*s).datap
END 

function PList_GetCurrentDataP, p
;; returns the element pointer 
;; that has to be deleted separately!
  IF PTR_VALID(p) THEN BEGIN
     s=(*p).current
     IF (PTR_VALID(s)) THEN return, (*s).datap
  END
  return, 0
END

function PList_GetNextDataP, p
;; returns the element pointer 
;; that has to be deleted separately!
  IF PTR_VALID(p) THEN BEGIN
     t=(*p).current
     IF PTR_VALID(t) THEN BEGIN
       IF PTR_VALID((*t).next) THEN BEGIN
          (*p).current=(*t).next
           t=(*p).current
          return, (*t).datap
       END 
    END
  END  
  return, 0
END 

function PList_GetPreviousDataP, p
;; returns the element pointer 
;; that has to be deleted separately!
  IF PTR_VALID(p) THEN BEGIN
     t=(*p).current
     IF PTR_VALID(t) THEN BEGIN
       IF PTR_VALID((*t).previous) THEN BEGIN
          (*p).current=(*t).previous
           t=(*p).current
          return, (*t).datap
       END
    END
  END 
  return, 0
END 

function PList_Destroy, p
;; returns the element pointer 
;; that has to be deleted separately!
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print,"% PList_Destroy: pointer list does not exist" 
     return, 0
  END
  ;; search for element with name IdString
  s=(*p).last
  WHILE (PTR_VALID(s)) DO BEGIN 
     ;; check whether data has been destroyed
     e=(*s).datap 
     IF PTR_VALID(e) THEN BEGIN
        print,"% PList_Destroy: Destroy data first!" 
        print,"%                List element "+(*s).name +" contains data!"
        return, 0
     END
     ;; element exists, unlink the element
     nn=(*s).next & pp=(*s).previous
     IF (PTR_VALID(nn)) THEN BEGIN
        (*nn).previous=pp
     END
     IF (PTR_VALID(pp)) THEN (*pp).next=nn
     ;; take care of current pointer
     IF ((*p).current EQ s) THEN BEGIN
        IF PTR_VALID(nn) THEN (*p).current=nn ELSE (*p).current=pp
     END
     ;; take care of last pointer
     IF ((*p).last EQ s) THEN BEGIN
        (*p).last=pp
     END
     ;; take care of last pointer
     IF ((*p).first EQ s) THEN BEGIN
        (*p).first=nn
     END
     ;; free ptr
     PTR_FREE, s
  END
  return, 1
END
