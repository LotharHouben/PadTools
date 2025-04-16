PRO MorphologicalTF, GRADIENT=gradient, TOPHAT=tophat
;; calculates a morphological transformation

  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% MorphologicalTF: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% MorphologicalTF: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     print, "% MorphologicalTF: current data pointer is invalid" 
     return
  END
  parr=(*e).data
  IF (NOT(PTR_VALID(parr))) THEN BEGIN
     print, "% MorphologicalTF: current data array pointer is invalid" 
     return
  END

  ;; check whether the data type is byte
  IF NOT((*e).type EQ 1) THEN BEGIN
     ErrMsg, "% MorphologicalTF: Data type is not Byte, type cast to Byte first!" 
     return
  END

  hlp=[" ",$
       " Set the radius of the disc shaped structuring element for the",$
       " morphological dilation/erosion operation.", $
       " The radius is measured in pixels.",$
       " "]
  r=1
  IF (XDataField(r, 'Radius of structuring circle (pix): ', Title="Input", Help=hlp) EQ 1) THEN r=FIX(r) ELSE r=1
  disc=SHIFT(DIST(2*r+1),r,r) LE r
  
  IF keyword_set(gradient) THEN name='MorphGrad'
  IF keyword_set(tophat) THEN name='MorphTopHat'
  name=name+"("+(*e).id+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=(*e).type
  ;; get dimensions for the projection image stack dimensions
  (*pp).SzZ=(*e).SzZ 
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX & (*pp).SzY=(*e).SzY
  (*pp).xsamp=(*e).xsamp &  (*pp).ysamp=(*e).xsamp
  (*pp).contrast=(*e).contrast
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% MorphologicalTF: Failed to create data array." 
     return
  END
  (*pp).data=ppd
  CASE (*e).zcoord OF
     1: BEGIN
        For j=0,((*e).SzX-1) DO BEGIN
           IF keyword_set(tophat) THEN (*ppd)[j,*,*]=MORPH_TOPHAT(REFORM((*(*e).data)[j,*,*]),disc)
           IF keyword_set(gradient) THEN (*ppd)[j,*,*]=MORPH_GRADIENT(REFORM((*(*e).data)[j,*,*]),disc)
        END
     END
     2: BEGIN
        For j=0,((*e).SzY-1) DO BEGIN
           IF keyword_set(tophat) THEN (*ppd)[*,j,*]=MORPH_TOPHAT(REFORM((*(*e).data)[*,j,*]),disc)
           IF keyword_set(gradient) THEN (*ppd)[*,j,*]=MORPH_GRADIENT(REFORM((*(*e).data)[*,j,*]),disc)
        END
     END
     3: BEGIN
        For j=0,((*e).SzZ-1) DO BEGIN
           IF keyword_set(tophat) THEN (*ppd)[*,*,j]=MORPH_TOPHAT(REFORM((*(*e).data)[*,*,j]),disc)
           IF keyword_set(gradient) THEN (*ppd)[*,*,j]=MORPH_GRADIENT(REFORM((*(*e).data)[*,*,j]),disc)
        END
     END
     ELSE: 
  END   
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  Update_XTabControl
END 


Pro MorphologicalTransform
  selFn=[" Tophat "," Gradient "]
  res=XMChoiceDialog(selFn,"Morphological Operator")
  IF (res EQ "") THEN BEGIN
      print, "% MorphologicalTransform: cancelled"
      return
   END
  CASE res OF
     " Tophat ": MorphologicalTF, /TOPHAT
     " Gradient ": MorphologicalTF, /GRADIENT
     ELSE: printtocon, "% MorphologicalTransform: Unknown operator type: "+MySTRING(res)+"."
  END
END
