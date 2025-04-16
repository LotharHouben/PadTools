PRO ROINormalize
;;
;; Example for the interactive selection of a rectangular ROI 
;; and normalization 
;; dereference all pointers to get to the data array in the currently
;; highlighted 3D data array list item
;; it is a good policy to check for undefined or NIL pointers 
ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
   ;; there is no root pointer
   printtocon, "%  ROINormalize: Root pointer is invalid." 
   return
END
p=(*ptr).current
IF (NOT(PTR_VALID(p))) THEN BEGIN
   ;; there is no data in the list or nothing is highlighted 
   printtocon, "%  ROINormalize: Current data list pointer is invalid." 
   return
END
e=(*p).datap
IF (NOT(PTR_VALID(e))) THEN BEGIN
   ;; there is no structure of type DATA
   printtocon, "% ROINormalize: Pointer to structure data in current data list element is invalid." 
   return
END
ppd=(*e).data
IF (NOT(PTR_VALID(ppd))) THEN BEGIN
   ;; there is no data array
   printtocon, "% ReportArrayStatistics: Pointer to to data array is invalid." 
   return
END
roi=StackROIPicker()
NRoi=Size(roi, /Dimensions)
;; N must match the current number of slices in the 
Case (*e).zcoord of 
1: nslices=(*e).SzX
2: nslices=(*e).SzY
3: nslices=(*e).SzZ
ELSE: BEGIN
   printtocon, "% ROINormalize: Current viewing direction is invalid." 
   printtocon, "%     Value is "+MyString((*e).zcoord)+"." 
   return
END
END 
IF (NRoi[0] NE nslices) THEN BEGIN
   printtocon, "% ROINormalize: Mismatch between stack slices and number of ROI." 
   return
END
 ;; Calculate moments
 i=0
 ;; min, max, mean, sum, var, sdev, mean absolute dev
 stat=FLTARR(nslices,7)
 ;; 
 printtocon, "% ROINormalize: slice, min, max, mean, sum, var, sdev, mean absolute dev"
 ;;
 FOR i=0, nslices-1 DO BEGIN
    x0=roi[i,0] & x1=roi[i,1] 
    y0=roi[i,2] & y1=roi[i,3] 
    CASE (*e).zcoord of 
    1: BEGIN 
       m=Moment((*ppd)[i,x0:x1,y0:y1], MDEV=mdev, SDEV=sdev)
       mmin=Min((*ppd)[i,x0:x1,y0:y1],MAX=mmax)
       tot=Total((*ppd)[i,x0:x1,y0:y1])
    END
    2: BEGIN 
       m=Moment((*ppd)[x0:x1,i,y0:y1], MDEV=mdev, SDEV=sdev)
       mmin=Min((*ppd)[x0:x1,i,y0:y1],MAX=mmax)
       tot=Total((*ppd)[x0:x1,i,y0:y1])
    END
    3: BEGIN 
       m=Moment((*ppd)[x0:x1,y0:y1,i], MDEV=mdev, SDEV=sdev)
       mmin=Min((*ppd)[x0:x1,y0:y1,i], MAX=mmax)
       tot=Total((*ppd)[x0:x1,y0:y1,i])
    END

 END 
  printtocon, " "+MyString(i) $ 
              +" "+MyString(mmin) $ 
              +" "+MyString(mmax) $ 
              +" "+MyString(m[0]) $ 
              +" "+MyString(tot) $ 
              +" "+MyString(m[1]) $ 
              +" "+MyString(sdev) $ 
              +" "+MyString(mdev) 
   stat[i,0]=mmin
   stat[i,1]=mmax
   stat[i,2]=m[0]
   stat[i,3]=tot
   stat[i,4]=m[1]
   stat[i,5]=sdev
   stat[i,6]=mdev           
END
 ;; ++++++++++++++++++++++++++++++
  ;; operator parameters
  ;; ++++++++++++++++++++++++++++++
  selFn=[" Divide by Minimum "," Divide by Maximum "," Divide by Mean "," Divide by  Sum "," Divide by Variance"," Divide by Standard Deviation "," Divide by Mean Absolute Deviation ", " Subtract Minimum ", " Subtract Maximum ", " Subtract Mean "]
  Op=XMChoiceDialog(selFn,"Choose Normalization Method")
  IF (Op EQ "") THEN BEGIN
     printtocon, "% ROINormalize: cancelled"
     return
  END
 ;;Create a new stack
  name="ROINormalize("+(*e).id+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  CASE (*e).type OF
     1: BEGIN
        (*pp).type=4
     END
     2: BEGIN
        (*pp).type=4
     END
     3: BEGIN
        (*pp).type=4
     END
     12: BEGIN
        (*pp).type=4
     END
     13: BEGIN
        (*pp).type=4
     END
     ELSE: (*pp).type=(*e).type
  END
  ;; get dimensions for the projection image stack dimensions
  (*pp).SzZ=(*e).SzZ
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX & (*pp).SzY=(*e).SzY
  (*pp).xsamp=(*e).xsamp &  (*pp).ysamp=(*e).xsamp
  (*pp).zcoord=(*e).zcoord
  (*pp).slice=(*e).slice
  nppd=(Data_GetEmptyArrayP(pp))
  (*pp).data=nppd
  IF NOT(PTR_VALID(nppd)) THEN BEGIN
     print, "% ROINormalize: Failed to create data array for the normalized images."
     return
  END 
  NormFact=FLTARR(nslices)
  NormFact[*]=1.
  AddFact=FLTARR(nslices)
  AddFact[*]=0
  CASE Op OF
     " Divide by Minimum ": BEGIN
        NormFact[*]=1./stat[*,0]
     END
     " Divide by Maximum ": BEGIN
        NormFact[*]=1./stat[*,1]
     END
     " Divide by Mean ": BEGIN
        NormFact[*]=1./stat[*,2]
     END
     " Divide by  Sum ": BEGIN
        NormFact[*]=1./stat[*,3]
     END
     " Divide by Variance": BEGIN
        NormFact[*]=1./stat[*,4]
     END
     " Divide by Standard Deviation ": BEGIN
        NormFact[*]=1./stat[*,5]
     END
     " Divide by Mean Absolute Deviation ": BEGIN
        NormFact[*]=1./stat[*,6]
     END
     " Subtract Minimum ": BEGIN
        AddFact[*]=-stat[*,0]
     END
     " Subtract Maximum ": BEGIN
        AddFact[*]=-stat[*,1]
     END
     " Subtract Mean ": BEGIN
        AddFact[*]=-stat[*,2]
     END
     ELSE: BEGIN
        print, "% ROIMormalize: Unknown normalization, normalization factors set to 1."             
     END
  END
  ;; STOP
  printtocon, "% ROINormalize: Affin linear transformation I <- a*I + b. "
  printtocon, "%     slice, a,  b "
  For i=0,(nslices-1) DO BEGIN
     CASE (*e).zcoord OF
     1: (*nppd)[i,*,*]=NormFact[i]* (*ppd)[i,*,*] + AddFact[i]
     2: (*nppd)[*,i,*]=NormFact[i]* (*ppd)[*,i,*] + AddFact[i]
     3: (*nppd)[*,*,i]=NormFact[i]* (*ppd)[*,*,i] + AddFact[i]
     ELSE:
  END 
     printtocon, " "+MyString(i) $ 
              +" "+MyString(NormFact[i]) $ 
             +" "+MyString(AddFact[i]) 
  END 
  CreateWindow
  TVDisplay
  Update_XTabControl
END 
