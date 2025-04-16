
PRO TypeCastVol, target
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% TypeCastVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% TypeCastVol: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     print, "% TypeCastVol: current data pointer is invalid" 
     return
  END
  parr=(*e).data
  IF (NOT(PTR_VALID(parr))) THEN BEGIN
     print, "% TypeCastVol: current data array pointer is invalid" 
     return
  END

  ;;Create a new stack
  CASE target OF
     'byte': BEGIN
        type=1
     END
     'int': BEGIN
        type=2
     END
     'long': BEGIN
        type=3
     END
     'float': BEGIN
        type=4
     END
     ELSE: BEGIN
        print, "% TypeCastVol: target identifier is invalid" 
     return
     END
  END
  TypeID=type_spec(type)
  name=TypeID+"("+(*e).id+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=type
  ;; get dimensions for the projection image stack dimensions
  (*pp).SzZ=(*e).SzZ 
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX & (*pp).SzY=(*e).SzY
  (*pp).xsamp=(*e).xsamp &  (*pp).ysamp=(*e).xsamp
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% TypeCastVol: Failed to create data array." 
     return
  END
  (*pp).data=ppd
  ;; Don't accumulate errors, don't do incremental tilt
  ;;
  CASE type OF
     1: BEGIN
        ;; get contrast data from current stack 
        (*ppd)=BYTSCL((*(*e).data), MIN=(*e).contrast[0],MAX=(*e).contrast[1])
        (*pp).contrast=[0,255]
        PrintToCon, "% TypeCastVol: Type Cast to Byte, Contrast range = " + MyString((*e).contrast[0])+":"+MyString((*e).contrast[1]) 
        ;; 
     END
     2: BEGIN
        (*ppd)=FIX(*(*e).data)
        PrintToCon, "% TypeCastVol: Type Cast to Integer" 
     END
     3: BEGIN
        (*ppd)=LONG(*(*e).data)
        PrintToCon, "% TypeCastVol: Type Cast to Longword Integer" 
     END
     4: BEGIN
        (*ppd)=FLOAT(*(*e).data)        
        PrintToCon, "% TypeCastVol: Type Cast to Floating Point" 
        END
     ELSE: 
  END   
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=0
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  Update_XTabControl
END 


Pro TypeCast
  selFn=["Byte","Integer","Longword Integer","Floating Point"]
  res=XMChoiceDialog(selFn,"Choose the Target Type")
  IF (res EQ "") THEN BEGIN
      print, "% TypeCast: cancelled"
      return
   END
  CASE res OF
     "Byte": TypeCastVol, "byte"
     "Integer": TypeCastVol, "int"
     "Longword Integer": TypeCastVol, "long"
     "Floating Point": TypeCastVol, "float"
     ELSE: printtocon, "% TypeCast: Unknown target data type: "+MySTRING(res)+"."
  END
END
