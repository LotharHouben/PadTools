PRO HanningVol
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% HanningVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% HanningVol: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% HanningVol: current data pointer is invalid" 
     return
  END
  alphax=0.5 & alphay=0.5 & alphaz=0.5
  ;;
  ;; get alpha from dialog
  ;; 
  mydialog=obj_new("ListDialog")
  mydialog->AddOption, "Alpha X", 0.5, RANGE=[0.5,1.]
  mydialog->AddOption, "Alpha Y", 0.5, RANGE=[0.5,1.]
  mydialog->AddOption, "Alpha Z", 0.5, RANGE=[0.5,1.]
  mydialog->SetTitle, "Hanning Window Parameters"
  IF (mydialog->UserDialog() GT 0) THEN BEGIN
     IF (mydialog->Value("Alpha X",x)) THEN alphax=x ELSE printtocon, "% HanningVol: Alpha X not found, setting value to 0.5.
     IF (mydialog->Value("Alpha Y",x)) THEN alphay=x ELSE printtocon, "% HanningVol: Alpha Y not found, setting value to 0.5.
     IF (mydialog->Value("Alpha Z",x)) THEN alphaz=x ELSE printtocon, "% HanningVol: Alpha Z not found, setting value to 0.5."
END ELSE BEGIN
   print, "% HanningVol: User cancelled, returning."
   return
   obj_destroy, mydialog
   
END
  obj_destroy, mydialog
;; 
  name="Hanning("+(*e).id+","+MyString(alphax)+","+MyString(alphay)+","+MyString(alphaz)+")"
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
     6: BEGIN
        (*pp).type=6
     END
     12: BEGIN
        (*pp).type=4
     END
     13: BEGIN
        (*pp).type=4
     END
     ELSE: (*pp).type=4
  END
  ;; get dimensions for the projection image stack dimensions
  Nx=(*e).SzX & & Ny=(*e).SzY & Nz=(*e).SzZ 
  (*pp).SzX=Nx &   (*pp).SzY=Ny & (*pp).SzZ=Nz
  (*pp).xsamp=(*e).xsamp & (*pp).ysamp=(*e).xsamp & (*pp).zsamp=(*e).zsamp 
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% HanningVol: failed to create data array for the projected images" 
     return
  END
  ;;
  (*pp).data=ppd
  (*ppd)=(*(*e).data)
  InitProgressBar, MINV=[1], MAXV=[3], TEXT=["Processing Dimension"] 
  ;; mask all x components
  Einsy=INDGEN(Ny) & Einsy[*]=1.
  hgx=Hanning(Nx,ALPHA=alphax)
  hgy=Hanning(Ny,ALPHA=alphay)
  hgz=Hanning(Nz,ALPHA=alphaz)
  ProgressBarSet, 0, 1, TEXT=MyString("X, Y")
  tmp=hgx#TRANSPOSE(hgy)
  For i=0,(Nz-1) Do BEGIN
     (*ppd)[*,*,i] *= tmp
  END
  ProgressBarSet, 0, 2, TEXT=MyString("Z")
  tmp=Einsy#TRANSPOSE(hgz)
  For i=0,(Nx-1) Do BEGIN
     (*ppd)[i,*,*] *= tmp
  END
  DestroyProgressBar
  ;;
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  Update_XTabControl
END 


PRO FFTVol, FWD=fwd, BWD=bwd, CENTRE=centre, MENU=menu
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% PaddVol: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% FFTVol: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% FFTVol: current data pointer is invalid" 
     return
  END
  IF keyword_set(menu) THEN BEGIN
;; ++++++++++++++++++++++++++++++
  ;; FFT parameters
  ;; ++++++++++++++++++++++++++++++
     selFn=["forward","forward and centred","inverse","inverse and centred"]
     Op=XMChoiceDialog(selFn,"Choose FFT Option")
     IF (Op EQ "") THEN BEGIN
        printtocon, "% FFTVol: cancelled"
        return
     END
     CASE Op of
        "forward": BEGIN 
           bwd=0 & centre=0
        END
        "forward and centred": BEGIN 
           bwd=0 & centre=1
        END
        "inverse": BEGIN 
           bwd=1 & centre=0
        END
        "inverse and centred": BEGIN 
           bwd=1 & centre=1
        END
     END 
  END  
  ;;Create a new stack
  IF keyword_set(bwd) THEN name="InvFFT("+(*e).id+")" ELSE name="FFT("+(*e).id+")"
  pp=DataList_CreateElement(ptr, name)
  ;; make sure we will not get an overflow when adding data 
  CASE (*e).type OF
     1: BEGIN
        (*pp).type=6
     END
     2: BEGIN
        (*pp).type=6
     END
     3: BEGIN
        (*pp).type=6
     END
     12: BEGIN
        (*pp).type=6
     END
     13: BEGIN
        (*pp).type=6
     END
     ELSE: (*pp).type=6
  END
  ;; get dimensions for the projection image stack dimensions
  (*pp).SzZ= (*e).SzZ
  (*pp).zsamp=(*e).zsamp 
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY
  (*pp).xsamp=(*e).xsamp 
  (*pp).ysamp=(*e).xsamp
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% FFTVol: failed to create data array for the projected images" 
     return
  END
  (*pp).data=ppd
  ;; Don't accumulate errors, don't do incremental tilt
  i=0
  ;;
  IF keyword_set(centre) THEN BEGIN
     IF keyword_set(bwd) THEN (*ppd)=SHIFT(FFT(*(*e).data,1),-(FLOOR((*pp).SzX/2)+1),-(FLOOR((*pp).SzY/2)+1),-(FLOOR((*pp).SzZ/2)+1)) ELSE (*ppd)=SHIFT(FFT(*(*e).data,-1),-(FLOOR((*pp).SzX/2)+1),-(FLOOR((*pp).SzY/2)+1),-(FLOOR((*pp).SzZ/2)+1))
  END ELSE BEGIN
     IF keyword_set(bwd) THEN (*ppd)=FFT(*(*e).data,1) ELSE (*ppd)=FFT(*(*e).data,-1)
  END 
  ;; current stack is *pp, display the projections on the screen  
  (*pp).slice=(*e).slice
  (*pp).zcoord=(*e).zcoord
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay
  
  Update_XTabControl
END 

