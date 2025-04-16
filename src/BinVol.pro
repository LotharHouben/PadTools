
PRO BinVol, Menu=menu
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% BinVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% BinVol: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% BinVol: current data pointer is invalid" 
     return
  END
  fact='2'
  IF keyword_set(menu) THEN BEGIN
;; ++++++++++++++++++++++++++++++
  ;; FFT parameters
  ;; ++++++++++++++++++++++++++++++     
     selFn=['2','3','4','5','6']
     fact=XMChoiceDialog(selFn,"Choose binning factor")
     IF (fact EQ "") THEN BEGIN
        printtocon, "% BinVol: cancelled"
        return
     END  
  END
  fact=FIX(fact)
  ;;Create a new stack
  name="Binnned("+(*e).id+","+MyString(Op)+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  (*pp).type=(*e).type
  ;; get dimensions for the projection image stack dimensions
  (*pp).SzZ= (*e).SzZ/fact
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY/fact
  (*pp).xsamp=(*e).xsamp 
  (*pp).ysamp=(*e).ysamp*fact
  (*pp).zsamp=(*e).zsamp*fact
  ppd=(Data_GetEmptyArrayP(pp))
  (*pp).data=ppd
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% BnVol: Failed to create data array for the binned images." 
     return
  END
  InitProgressBar, MINV=0, MAXV=(*pp).SzX-1, TEXT="Binning Image Nr."
  maxy=((*pp).SzY*fact)-1
  maxz=((*pp).SzZ*fact)-1
  For k=0,(*pp).SzX-1 DO BEGIN
     ProgressBarSet, 0, k, TEXT=STRING(k)
     (*(*pp).data)[k,*,*] = Rebin(REFORM((*(*e).data)[k,0:maxy,0:maxz]),(*pp).SzY,(*pp).SzZ)
  END
  DestroyProgressBar
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  Update_XTabControl
END 
