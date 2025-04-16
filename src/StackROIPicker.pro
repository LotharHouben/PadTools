Function StackROIPicker
;;
;; a template for a processing routine
;; 
IF (GetDebugFlag() EQ 0) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% StackROIPicker:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      ErrMsg, !ERROR_STATE.MSG
      return, 0
   END
END
ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
;; there is no root pointer
   printtocon, "% StackROIPicker: Root pointer is invalid."
   return, 0
END
p=(*ptr).current
pdirectdisplayp=(*ptr).current
IF (NOT(PTR_VALID(p))) THEN BEGIN
;; there is no data in the list or nothing is highlighted
   printtocon, "% StackROIPicker: Current data list pointer is invalid."
   return, 0
END
e=(*p).datap
IF (NOT(PTR_VALID(e))) THEN BEGIN
;; there is no structure of type DATA
   printtocon, "% StackROIPicker: Pointer to structure data in current data list element is invalid."
   return, 0
END
ppd=(*e).data
IF (NOT(PTR_VALID(ppd))) THEN BEGIN
;; there is no data array
   printtocon, "% StackROIPicker: Pointer to to data array is invalid."
   return, 0
END
;;
;; to get the parameters
;; get number of group images
CASE (*e).zcoord OF
1: BEGIN
   NIm=(*e).SzX
   hdim=(*e).SzY & vdim=(*e).SzZ
END
2: BEGIN
   NIm=(*e).SzY
   hdim=(*e).SzX & vdim=(*e).SzZ
END
3: BEGIN 
   NIm=(*e).SzZ
   hdim=(*e).SzX & vdim=(*e).SzY
END
ELSE: BEGIN
   printtocon, "% StackROIPicker: Viewing direction is invalid."
   printtocon, "%    value is "+MyString((*e).zcoord)
   return, 0
END
END
;; allocate array to hold the ROI coorddinates [x0:x1,y0:y1]
;; and y 
SliceROI=INTARR(NIm,4)
;; now start the
;; loop over all images
;; and let the user mark the data points
loop=(*e).slice   ;; focus on the current image in the group
x0=FLOOR(hdim*0.25)& x1=x0+FIX(hdim/2) & y0=FLOOR(vdim*0.25) & y1=y0+FIX(vdim/2)

;; Get roi data for all slices
count=1
roi=[x0,x1,y0,y1]
WHILE (count LE NIm) DO BEGIN
   (*e).slice=loop
   TVDisplay, DirectP=pdirectdisplayp
   result=XBox(x0, y0, x1, y1, bin=(*e).binning, SX=hdim, SY=vdim, TITLE="Mark ROI")
   IF (result GT 0) THEN BEGIN 
      roi=[x0,x1,y0,y1]
      printtocon, "% "+MyString(loop) + " (" + MyString(x0)+":" + MyString(x1)+"," + MyString(y0)+":" + MyString(y1)+")"
      SliceROI[loop,*]=roi 
   END ELSE BEGIN
      print, "% StackROIPicker: cancelled." 
      return, 0
   END 
   loop=loop+1
   IF (loop GE NIm) THEN loop=0
   count = count+1
END     
Update_XTabControl
XConsole_PopState
return, SliceROI
END      
