FUNCTION NGET_IMAGE_PTRLIST, first
 p=first
 list=['empty'] ; empty list
 WHILE PTR_VALID(p) DO BEGIN
    list=[p, list]
    p=(*p).next             
  END
 return, list
END

FUNCTION NGET_LISTPTR, first, index
;; returns listptr if index is found in the image list starting with first
;; a non valid pointer is returned otherwise 
 ptr=PTR_NEW()
 IF (PTR_VALID(first)) THEN BEGIN
  p=first
  p_index=0
  WHILE (p_index NE index) DO BEGIN
      p_index=p_index+1
      p=(*p).next
      IF NOT(PTR_VALID(p)) THEN return, ptr             
  END
  ptr=p
 END
 return, ptr
END

FUNCTION NGET_LISTPOS, first, p
;; returns -1 if p ist not found in the image list starting with first
;;         the index position in the list otherwise
 index=-1
 IF (PTR_VALID(p) AND PTR_VALID(first)) THEN BEGIN
     index=0
     pindex=first
     WHILE (pindex NE p) DO BEGIN
         index=index+1
         pindex=(*pindex).next
         IF NOT(PTR_VALID(pindex)) THEN return, -1             
     END
 END ELSE BEGIN
     PrintToCon, "NGET_LISTPOS: invalid list or item pointer, list="+MyString(first) + " item=" +MyString(p)
 END
 return, index
END

FUNCTION NGET_IMAGE_PTRLIST, first
;; returns a list of names for the list starting with first
 p=first
 IF PTR_Valid(p) THEN BEGIN
        list=[p]
        p=(*p).next
        WHILE PTR_VALID(p) DO BEGIN
           list=[list, p]
         p=(*p).next             
        END
 ENDIF ELSE list=['empty'];
 return, list
END

FUNCTION NGET_IMAGE_NAMELIST, first
;; returns a list of names for the list starting with first
  p=first
  IF PTR_Valid(p) THEN BEGIN
        list=[(*p).name]
        p=(*p).next
        WHILE PTR_VALID(p) DO BEGIN
         list=[list, (*p).name] 
         p=(*p).next             
        END
 ENDIF ELSE list=['empty'];
 return, list
END


PRO FocusToImage, p
;; set the window focus to image p without redrawing 
 IF PTR_VALID(p) THEN BEGIN 
  IF ((*p).id EQ 'image') THEN WSET, (*p).wdow
  IF ((*p).id EQ 'group') THEN BEGIN
    IF PTR_VALID((*p).group) THEN WSET, (*(*p).group).wdow
  END 
END 
END







