PRO NormalizeVol, MODE=mode
;; Normalizes the 3D intensity data
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% NormalizeVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% NormalizeVol: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% NormalizeVol: current data pointer is invalid" 
     return
  END
  ;;Create a new stack
  IF NOT(keyword_set(mode)) THEN mode='subtractmean'
  name="Normalize("+(*e).id+","+MyString(mode)+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=(*e).type
  ;; get dimensions for the rotated image stack dimensions
  (*pp).SzZ=(*e).SzZ 
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX & (*pp).SzY=(*e).SzY
  (*pp).xsamp=(*e).xsamp &  (*pp).ysamp=(*e).ysamp
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     printtocon, "% NormalizeVol: failed to create data array for the rotated volume" 
     return
  END
  (*pp).data=ppd
  (*ppd)=(*(*e).data)
  ;; stack is copied, pointer to copy is ppd
  ;; normalize copied data
 CASE mode OF
     'subtractmean': BEGIN
        ;; calculate moments
        printtocon, "% NormalizeVol: subtracting 3D volume mean value" 
        norm=Mean((*ppd))
        printtocon, "%     mean value = "+MyString(norm) 
        IF ((norm NE 0) AND Finite(norm)) THEN BEGIN 
           (*ppd)=(*ppd)-norm
        END ELSE BEGIN
           printtocon, "%     error: mean value is not a finite number " 
        END
     END
     'dividebymean': BEGIN
        ;; calculate moments
        printtocon, "% NormalizeVol: dividing by 3D volume mean value" 
        norm=Mean((*ppd))
        printtocon, "%     mean value = "+MyString(norm) 
        IF ((norm NE 0) AND Finite(norm)) THEN BEGIN 
           (*ppd)=1./norm*(*ppd)
        END ELSE BEGIN
           printtocon, "%     error: mean value is not a finite number " 
        END
     END
     'dividebystddev': BEGIN
        ;; calculate moments
        printtocon, "% NormalizeVol: dividing by 3D volume standard deviation" 
        norm=STDDEV((*ppd))
        printtocon, "%     standard deviation = "+MyString(norm) 
        IF ((norm NE 0) AND Finite(norm)) THEN BEGIN 
           (*ppd)=(*ppd)-norm
        END ELSE BEGIN
           printtocon, "%     error: standard deviation is not a finite number " 
        END
     END
     ELSE: 
  END
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=0
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  Update_XTabControl
END 


