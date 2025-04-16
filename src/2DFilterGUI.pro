;+
; NAME:
;
;
;
; PURPOSE: Select rectangular ROIs within a three-dimensional data set
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE: Two views are presented and in both views two rectangles
; are shown. View 1 defines four box coordinates, the fifth and sixth
; coordinate is taken from teh second view 
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
FUNCTION CosRampFilter, dimx, dimy, pkx, pky, pu, w
  ;; returns a ramp filter, multiplied by an sync smoothing
  ;; function
  ;; dimx: image dimension x
  ;; dimy: image dimension y
  ;; pkx: Pointer, kx^2 array - will be returned if not valid upon input
  ;; pky: Pointer, ky^2 array
  ;; pu: Pointer, sqrt(kx^2+ky^2) array (ramp filter)
  IF (NOT(PTR_VALID(pkx)) OR NOT(PTR_VALID(pky)) OR NOT(PTR_VALID(pu)) OR NOT(PTR_VALID(pu2))) THEN BEGIN
     ;; first create the integer arrays kx_ij=i ky_ij=j
     kx = LINDGEN(dimx)
     EinsX= INTARR(dimx)+1
     ky = LINDGEN(dimy)
     Einsy = INTARR(dimY)+1
     kx = kx-dimx/2
     maxx=MAX(kx)  
     ky = ky-dimy/2
     maxy=MAX(ky)
     pkx = PTR_NEW(DOUBLE(EinsY##kx))
     (*pkx)=(*pkx)*(*pkx)
     pky = PTR_NEW(DOUBLE(ky##EinsX))
     (*pky)=(*pky)*(*pky)
     pu=PTR_NEW(Sqrt((*pkx)))
  END 
  ;; exponential mask
  wx=0.5*w*dimx & wy=0.5*w*dimy
  invwx2=1/wx/wx & invwy2=1/wy/wy
  FX=invwx2*(*pkx)
  FY=invwy2*(*pky)
  Filt=(*pu)*exp(-(FX+FY))
  return, SHIFT(Filt,dimx/2,dimy/2)
END


;; w = [-pi:(2*pi)/Length:pi-(2*pi)/Length];
;; rn1 = abs(2/a*sin(a.*w./2));
;; rn2 = sin(a.*w./2);
;; rd = (a*w)./2;
;; r = rn1*(rn2/rd)^2;


FUNCTION RampFilter, dimx, dimy, pkx, pky, pu, w, expnt
  ;; returns a ramp filter, multiplied by an exponential smoothing
  ;; function
  ;; dimx: image dimension x
  ;; dimy: image dimension y
  ;; pkx: Pointer, kx^2 array - will be returned if not valid upon input
  ;; pky: Pointer, ky^2 array
  ;; pu: Pointer, sqrt(kx^2+ky^2) array (ramp filter)
  IF (NOT(PTR_VALID(pkx)) OR NOT(PTR_VALID(pky)) OR NOT(PTR_VALID(pu)) OR NOT(PTR_VALID(pu2))) THEN BEGIN
     ;; first create the integer arrays kx_ij=i ky_ij=j
     kx = LINDGEN(dimx)
     EinsX= INTARR(dimx)+1
     ky = LINDGEN(dimy)
     Einsy = INTARR(dimY)+1
     kx = kx-dimx/2
     maxx=MAX(kx)  
     ky = ky-dimy/2
     maxy=MAX(ky)
     pkx = PTR_NEW(DOUBLE(EinsY##kx))
     (*pkx)=(*pkx)*(*pkx)
     pky = PTR_NEW(DOUBLE(ky##EinsX))
     (*pky)=(*pky)*(*pky)
     ;; note: the ramp is only in horizontal direction!
     pu=PTR_NEW(Sqrt((*pkx)))
  END 
  ;; exponential mask
  wx=0.5*w*dimx & wy=0.5*w*dimy
  invwx2=1/wx/wx & invwy2=1/wy/wy
  FX=invwx2*(*pkx)
  FY=invwy2*(*pky)
;;  print, "wx,wy=", wx, ",",  wy
;;  print, "exponent=", expnt
;;  STOP
  Filt=(*pu) ;; ramp
  if (expnt EQ 0.) THEN BEGIN
     B=WHERE((FX+FY) GT 1, count)
     if (count GT 0) THEN Filt[B]=0
     return, SHIFT(Filt,dimx/2,dimy/2)
  END
  if (expnt EQ 0.5) THEN BEGIN
     Filt=Filt*exp(-SQRT(FX+FY))
     return, SHIFT(Filt,dimx/2,dimy/2)
  END
  if (expnt EQ 1) THEN BEGIN 
        Filt=Filt*exp(-(FX+FY))
        return, SHIFT(Filt,dimx/2,dimy/2)
  END
  Filt=Filt*exp(-(FX+FY)^expnt) 
  return, SHIFT(Filt,dimx/2,dimy/2)
END


FUNCTION FilterGUI2D

IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% FilterGUI2D:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
hlptxt=[" ",$
        "PROCEDURE 2D Filter GUI", $
        " ", $
        "PURPOSE ", $
        " ", $
        "Apply a 2D FFT filter to data slices.", $
        " ", $
        "PARAMETERS ", $
        " ", $
        " ", $
        " " $
   ]
  ;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;; data pointer
  ;; e = pointer to the tilt series list entry
  ;; (*e).data = pointer to the data array
  ;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% GridSearchAlignmentXY: Root pointer is invalid" 
     return, 0
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% GridSearchAlignmentXY: Current stack pointer is invalid." 
     return, 0
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% GridSearchAlignmentXY: Current data pointer is invalid." 
     return, 0
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3] 
  
  ;; draw the widget
  x0=0
  Case (*e).zcoord OF 
      1: x0=(*e).slice
      ELSE: BEGIN
         printtocon, "% FilterGUI2D: Projection is not along X (proj=1)." 
         return, 0
      END
   END
  ;; Window, 1, XSIZE=TotalDimX, YSIZE=TotalDimY
  ;; binning=TVBIN(TotalDimX,TotalDimY)
  ;;
  name="PreviewSlice("+(*c).name+")"
  pp=DataList_CreateElement(ptr, name)
  ;; calculate the central slice to get the right dimensions
  cslice=0
  (*pp).type=4
  (*pp).binning=(*e).binning
  (*pp).SzX=DimY
  (*pp).SzY=DimZ
  (*pp).SzZ=1
  (*pp).xsamp=1
  (*pp).ysamp=1
  (*pp).zsamp=1
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  (*pp).data=PTR_NEW(REFORM((*datap)[x0,*,*]))
  IF NOT(PTR_VALID((*pp).data)) THEN BEGIN
     print, "% FilterGUI2D: Failed to create data array for the preview slice." 
     return, 0
  END
  ;; current stack is *pp, display the projections on the screen  
  CreateWindow
  TVDisplay
  Update_XTabControl
  ;; 
  ;; define the slider widget
  ;;
  title='2D Filter GUI'
  base=widget_base(Title=title, /Column)
  ;;
  ;; Filter
  ;;
  
  ;; default:  filter
  filterstack=1B
  pobj=GetDLGParObject()
  width=pobj->get_token_value('RAMPFILTER','MASKWIDTH', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      width=0.5
      printtocon, "% 2DFilterGUI: Error reading RAMPFILTER::MASK."
   END
  expnt=pobj->get_token_value('RAMPFILTER','MASKEXPONENT', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      expnt=1.
      printtocon, "% 2DFilterGUI: Error reading RAMPFILTER::MASKEXPONENT."
   END
   cosfactor=pobj->get_token_value('RAMPFILTER','COSFACTOR', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      cosfactor=100
      printtocon, "% 2DFilterGUI: Error reading RAMPFILTER::COSFACTOR."
   END
  ;;
  title='2D Filter GUI'
  base=widget_base(Title=title, /COLUMN)
  row1=widget_base(base, /ROW)
  dummy=WIDGET_LABEL(row1,VALUE='       ')
  slider_x=WIDGET_SLIDER(row1, MINIMUM=0, XSIZE=300, MAXIMUM=DimX-1, Value=x0, TITLE="Slice")
  colbase=widget_base(base, /ROW)


  col1=widget_base(colbase, /COLUMN)

  buttons=widget_base(col1, /COLUMN, /EXCLUSIVE)
  button_nofilter = Widget_Button(buttons, Value='No Filter')
  button_filter = Widget_Button(buttons, Value='Ramp Exponential', YSIZE=60)
  button_cosfilter = Widget_Button(buttons, Value='Ramp Cosine')
  ;;
  col2=widget_base(colbase, /COLUMN)
  dummy=WIDGET_LABEL(col2,VALUE='')
  dummy=WIDGET_LABEL(col2,VALUE='')
  dummy=WIDGET_LABEL(col2,VALUE='')
  slider_width=WIDGET_SLIDER(col2, MINIMUM=0, MAXIMUM=100, Value=FIX(width*100), TITLE='width')
  slider_cosfact=WIDGET_SLIDER(col2, MINIMUM=0, MAXIMUM=100, Value=cosfactor, TITLE='attenuation factor')

  ;;
  col3=widget_base(colbase, /COLUMN)
  dummy=WIDGET_LABEL(col3,VALUE='')
  dummy=WIDGET_LABEL(col3,VALUE='')
  dummy=WIDGET_LABEL(col3,VALUE='')
  slider_expnt=WIDGET_SLIDER(col3, MINIMUM=0, MAXIMUM=10, Value=FIX(2*expnt), TITLE='exp')
  ;;
  ; filter: Create filter
  pkx=PTR_NEW() & pky=PTR_NEW() & pu=PTR_NEW()
  ;; deactivate all sliders, no filter by default
  WIDGET_CONTROL, slider_expnt, SENSITIVE=0
  WIDGET_CONTROL, slider_width, SENSITIVE=0
  WIDGET_CONTROL, slider_cosfact, SENSITIVE=0
  Widget_Control, button_nofilter, /Set_Button
  filterstack=0B
  ;; create a filter to pre-calculate kx, ky and u=kx^2+ky^2 arrays
  filterA=RampFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
  IF (filterstack EQ 1) THEN BEGIN
     (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
     TVDISPLAY
  END
  ;;

  dummy = WIDGET_BASE(base,/Column)
  text = WIDGET_LABEL(dummy,VALUE='')
  buttons = WIDGET_BASE(base,/Row)
  apply = WIDGET_BUTTON(buttons, value='Accept', UVALUE='apply')
  cancel = WIDGET_BUTTON(buttons, value='Cancel', UVALUE='cancel')
  hlp = WIDGET_BUTTON(buttons, value='Help', UVALUE='hlp')

  WIDGET_CONTROL, base, /REALIZE
    
  quit=0
  REPEAT BEGIN
     ev = WIDGET_Event(base)
;;     print, ev
;;     STOP
     CASE ev.id OF
        button_nofilter: BEGIN
           ;; toggle button
           filterstack=0B 
           Widget_Control, button_nofilter, /Set_Button
           WIDGET_CONTROL, slider_expnt, SENSITIVE=0
           WIDGET_CONTROL, slider_width, SENSITIVE=0
           WIDGET_CONTROL, slider_cosfact, SENSITIVE=0
           (*(*pp).data)=REFORM((*datap)[x0,*,*])
           TVDisplay
        END
        button_filter: BEGIN
           ;; toggle button
           filterstack=1B
           Widget_Control, button_filter, /Set_Button
           WIDGET_CONTROL, slider_expnt, SENSITIVE=1
           WIDGET_CONTROL, slider_width, SENSITIVE=1
           WIDGET_CONTROL, slider_cosfact, SENSITIVE=0
           ;; filter: Create filter
           ;;
           filterA=RampFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        button_cosfilter: BEGIN
           ;; toggle button
           filterstack=1B
           Widget_Control, button_cosfilter, /Set_Button
           WIDGET_CONTROL, slider_expnt, SENSITIVE=0
           WIDGET_CONTROL, slider_width, SENSITIVE=0
           WIDGET_CONTROL, slider_cosfact, SENSITIVE=1
           ;; filter: Create filter
           ;;
           filterA=CosRampFilter(DimY, DimZ, pkx, pky, pu, cosfactor)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        slider_expnt: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_expnt, GET_VALUE=tmpx
           expnt=0.5*FLOAT(tmpx)
           filterA=RampFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END 
        slider_width: BEGIN
           ;; read filter low pass cutoff
           WIDGET_CONTROL, slider_width, GET_VALUE=tmpx
           width=0.01*FLOAT(tmpx)
           filterA=RampFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END
        slider_cosfact: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_cosfact, GET_VALUE=tmpx
           cosfactor=0.01*FLOAT(tmpx)
           filterA=CosRampFilter(DimY, DimZ, pkx, pky, pu, cosfactor)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END 
        slider_x: BEGIN
           ;; changing x, we have to change view B (Y-Z)
           ;; read new x slice
           tmpx=x0
           WIDGET_CONTROL, slider_x, GET_VALUE=tmpx
           x0=tmpx
           ;; change viewB
           IF (filterstack EQ 1) THEN BEGIN
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           END ELSE BEGIN
              (*(*pp).data)=REFORM((*datap)[x0,*,*])
           END
           TVDisplay
           ;; update rulers, note that the ruler in viewA changes
        END
        apply: BEGIN
           quit=1
           result=1
        END
        cancel: BEGIN
           quit=1
           result=0
        END
        hlp: BEGIN
           ;;STOP
          res=Dialog_Message(hlptxt, /INFORMATION)
        END
        ELSE: 
     END 
  ENDREP UNTIL (quit EQ 1)
  WIDGET_CONTROL, base, /DESTROY
  (*(*pp).data)=filterA
  TVDISPLAY
  if (result EQ 1) THEN BEGIN
     printtocon, "% 2DFilterGUI: Applying filter" 
     (*ptr).current=c
     CopyStack
     cnew=(*ptr).current
     (*cnew).name="RampFilter("+(*c).name+")"
     d=(*cnew).datap
;; display a prograss bar
     InitProgressBar, MINV=0, MAXV=(*d).SzX-1, TEXT="Applying Filter to Image Nr."
     For k=0,(*d).SzX-1 DO BEGIN
        ProgressBarSet, 0, k, TEXT=STRING(k)
        (*(*d).data)[k,*,*] = ABS(ApplyFilter(filterA,REFORM((*(*d).data)[k,*,*])))
     END
     DestroyProgressBar
  END 
  TVDISPLAY
  PTR_FREE, pkx
  PTR_FREE, pky
  PTR_FREE, pu
  pobj->set_token_value,'RAMPFILTER','MASKWIDTH', width, ERRSTATE=err
  pobj->set_token_value, 'RAMPFILTER','MASKEXPONENT', expnt, ERRSTATE=err
  return, result
END



FUNCTION BandPassFilterGUI2D, APPLYTOSTACK=applytostack

IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% FilterGUI3D:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
hlptxt=[" ",$
        "PROCEDURE 2D Filter GUI", $
        " ", $
        "PURPOSE ", $
        " ", $
        "Apply a 2D FFT filter to data slices.", $
        " ", $
        "PARAMETERS ", $
        " ", $
        " ", $
        " " $
   ]
  ;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ;; data pointer
  ;; e = pointer to the tilt series list entry
  ;; (*e).data = pointer to the data array
  ;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% GridSearchAlignmentXY: Root pointer is invalid" 
     return, 0
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% GridSearchAlignmentXY: Current stack pointer is invalid." 
     return, 0
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% GridSearchAlignmentXY: Current data pointer is invalid." 
     return, 0
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3] 
  
  ;; draw the widget
  x0=0
  Case (*e).zcoord OF 
      1: x0=(*e).slice
      ELSE: BEGIN
         printtocon, "% FilterGUI2D: Projection is not along X (proj=1)." 
         return, 0
      END
   END
  ;; Window, 1, XSIZE=TotalDimX, YSIZE=TotalDimY
  ;; binning=TVBIN(TotalDimX,TotalDimY)
  ;;
  name="PreviewSlice("+(*c).name+")"
  pp=DataList_CreateElement(ptr, name)
  ;; calculate the central slice to get the right dimensions
  cslice=0
  (*pp).type=4
  (*pp).binning=(*e).binning
  (*pp).SzX=DimY
  (*pp).SzY=DimZ
  (*pp).SzZ=1
  (*pp).xsamp=1
  (*pp).ysamp=1
  (*pp).zsamp=1
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  (*pp).data=PTR_NEW(REFORM((*datap)[x0,*,*]))
  IF NOT(PTR_VALID((*pp).data)) THEN BEGIN
     print, "% FilterGUI2D: Failed to create data array for the preview slice." 
     return, 0
  END
  ;; current stack is *pp, display the projections on the screen  
  CreateWindow
  TVDisplay
  Update_XTabControl
  ;; 
  ;; define the slider widget
  ;;
  title='2D Filter GUI'
  base=widget_base(Title=title, /Column)
  ;;
  ;; Filter
  ;;
  
  ;; default:  filter
  filterstack=1B
  pobj=GetDLGParObject()
  bplo=pobj->get_token_value('BANDPASS','LOWNQFRAC', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      bplo=0.1
      printtocon, "% 2DFilterGUI: Error reading BANDPASS::LOWNQFRAC."
   END
  bphi=pobj->get_token_value('BANDPASS','HINQFRAC', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      bphi=0.5
      printtocon, "% 2DFilterGUI: Error reading BANDPASS::HINQFRAC."
   END
   bpexp=pobj->get_token_value('BANDPASS','EXPONENT', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      bpexp=1
      printtocon, "% 2DFilterGUI: Error reading BANDPASS::EXPONENT."
   END
    bptype=pobj->get_token_value('BANDPASS','TYPE', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      bptype="BUTTERWORTH"
      printtocon, "% 2DFilterGUI: Error reading BANDPASS::TYPE."
   END
   mv=pobj->get_token_value('BANDPASS','MEANVALUE', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
     mv=1B 
      printtocon, "% 2DFilterGUI: Error reading BANDPASS::MEANVALUE."
   END
  ;;
  title='2D Filter GUI'
  base=widget_base(Title=title, /COLUMN)
  colbase=widget_base(base, /ROW)


  col1=widget_base(colbase, /COLUMN)
  frame1=widget_base(colbase, /COLUMN, FRAME=1)
  buttons=widget_base(frame1, /ROW, /EXCLUSIVE)
  button_nofilter = Widget_Button(buttons, Value='Unfiltered')
  button_gaussfilter = Widget_Button(buttons, Value='Gaussian')
  button_bwfilter = Widget_Button(buttons, Value='Butterworth')
  ;;
  slidergranularity=1.  
  row2=widget_base(base, /COLUMN)
  slider_lo=CW_FSLIDER(row2, XSIZE=300, MINIMUM=0, MAXIMUM=slidergranularity, FRAME=0, Value=bplo*slidergranularity, EDIT=1, TITLE='Low frequency cut-off (fraction of Nyquist)')
  slider_hi=CW_FSLIDER(row2, XSIZE=300, MINIMUM=0, MAXIMUM=slidergranularity, FRAME=0, Value=bphi*slidergranularity, EDIT=1, TITLE='High frequency cut-off (x Ny)')
  slider_exp=WIDGET_SLIDER(row2, MINIMUM=1, MAXIMUM=4, Value=FIX(bpexp), TITLE='Shape exponent')
  ;;
  ; filter: Create filter
  pkx=PTR_NEW() & pky=PTR_NEW() & pu=PTR_NEW()
  ;; deactivate all sliders, no filter by default
  WIDGET_CONTROL, slider_hi, SET_VALUE=bphi*slidergranularity
  WIDGET_CONTROL, slider_lo, SET_VALUE=bplo*slidergranularity
  WIDGET_CONTROL, slider_exp, SENSITIVE=0
  WIDGET_CONTROL, slider_hi, SENSITIVE=0
  WIDGET_CONTROL, slider_lo, SENSITIVE=0

  Widget_Control, button_nofilter, /Set_Button
  CASE bptype OF
     "GAUSS": BEGIN
        gaussian=1
        Widget_Control, button_gaussfilter, /Set_Button
        WIDGET_CONTROL, slider_exp, SENSITIVE=1
        WIDGET_CONTROL, slider_lo, SENSITIVE=1
        WIDGET_CONTROL, slider_hi, SENSITIVE=1
     END
     "BUTTERWORTH": BEGIN
        gaussian=0
        Widget_Control, button_bwfilter, /Set_Button
        WIDGET_CONTROL, slider_exp, SENSITIVE=1
        WIDGET_CONTROL, slider_lo, SENSITIVE=1
        WIDGET_CONTROL, slider_hi, SENSITIVE=1
     END
     ELSE: bptype="NONE"
  END
  ;;STOP
  filterstack=0B
  ;; create a filter to pre-calculate kx, ky and u=kx^2+ky^2 arrays
  ;; use RampFilter pro for that
  pkx=PTR_NEW()
  pky=PTR_NEW()
  pu=PTR_NEW()
  PrepareRecSampGrid, DimY, DimZ, pkx, pky, pu
  filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian)
  ;;
  IF (filterstack EQ 1) THEN BEGIN
     (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
     TVDISPLAY
  END
  ;;

  row41 = WIDGET_BASE(base,/Row)
  text=Widget_Label(row41, VALUE='Preserve Mean Value')
  tilttogglebase = WIDGET_BASE(row41, /Row, /EXCLUSIVE)
  mv_id=INTARR(2)
  mv_yes = WIDGET_BUTTON(tilttogglebase, VALUE="yes",  UVALUE="mvyes", /NO_RELEASE)
  mv_no = WIDGET_BUTTON(tilttogglebase, VALUE="no", UVALUE="mvno", /NO_RELEASE)
  If (mv EQ 1) THEN  WIDGET_CONTROL, mv_yes, /SET_BUTTON ELSE  WIDGET_CONTROL, mv_no, /SET_BUTTON
  ;;
row1=widget_base(base, /ROW)
slider_x=WIDGET_SLIDER(row1, MINIMUM=0, XSIZE=300, MAXIMUM=DimX-1, Value=x0, TITLE="Slice")
 
 buttons = WIDGET_BASE(base,/Row)
  apply = WIDGET_BUTTON(buttons, value='Accept', UVALUE='apply')
  cancel = WIDGET_BUTTON(buttons, value='Cancel', UVALUE='cancel')
  hlp = WIDGET_BUTTON(buttons, value='Help', UVALUE='hlp')
  
  WIDGET_CONTROL, base, /REALIZE
  quit=0
  REPEAT BEGIN
     ev = WIDGET_Event(base)
    ;; print, ev
    ;;  STOP
     CASE ev.id OF
        button_nofilter: BEGIN
           ;; toggle button
           filterstack=0B 
           Widget_Control, button_nofilter, /Set_Button
           bptpe="NONE"
           WIDGET_CONTROL, slider_exp, SENSITIVE=0
           WIDGET_CONTROL, slider_lo, SENSITIVE=0
           WIDGET_CONTROL, slider_hi, SENSITIVE=0
           (*(*pp).data)=REFORM((*datap)[x0,*,*])
           TVDisplay
        END
        button_gaussfilter: BEGIN
           ;; toggle button
           filterstack=1B
           bptype="GAUSS"
           gaussian=1
           Widget_Control, button_gaussfilter, /Set_Button
           WIDGET_CONTROL, slider_exp, SENSITIVE=1
           WIDGET_CONTROL, slider_lo, SENSITIVE=1
           WIDGET_CONTROL, slider_hi, SENSITIVE=1
           ;; filter: Create filter
           ;;
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        button_bwfilter: BEGIN
           filterstack=1B
           bptype="BUTTERWORTH"
           gaussian=0
           Widget_Control, button_bwfilter, /Set_Button
           WIDGET_CONTROL, slider_exp, SENSITIVE=1
           WIDGET_CONTROL, slider_lo, SENSITIVE=1
           WIDGET_CONTROL, slider_hi, SENSITIVE=1
           ;; filter: Create filter
           ;;
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        slider_exp: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_exp, GET_VALUE=tmpx
           bpexp=FIX(tmpx)
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END 
        slider_lo: BEGIN
           ;; read filter low pass cutoff
           WIDGET_CONTROL, slider_lo, GET_VALUE=tmpx
           bplo=1./slidergranularity*FLOAT(tmpx)
           print, "bplo=", bplo
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END
        slider_hi: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_hi, GET_VALUE=tmpx
           bphi=1./slidergranularity*FLOAT(tmpx)
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END 
        slider_x: BEGIN
           ;; changing x, we have to change view B (Y-Z)
           ;; read new x slice
           tmpx=x0
           WIDGET_CONTROL, slider_x, GET_VALUE=tmpx
           x0=tmpx
           ;; change viewB
           IF (filterstack EQ 1) THEN BEGIN
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           END ELSE BEGIN
              (*(*pp).data)=REFORM((*datap)[x0,*,*])
           END
           TVDisplay
           ;; update rulers, note that the ruler in viewA changes
        END
        apply: BEGIN
           quit=1
           result=1
        END
        cancel: BEGIN
           quit=1
           result=0
        END
        hlp: BEGIN
          res=Dialog_Message(hlptxt, /INFORMATION)
       END
        mv_yes: BEGIN
           mv=1B
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        mv_no: BEGIN
           mv=0B
           filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian, MEANVALUE=mv)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        ELSE: 
     END 
  ENDREP UNTIL (quit EQ 1)
  WIDGET_CONTROL, base, /DESTROY
  IF (filterstack EQ 1) THEN BEGIN
     (*(*pp).data)=filterA
     TVDISPLAY
  END ELSE BEGIN
     res=DataList_DeleteCurrentElement(GetRootP())
  END
  if (PTR_VALID(pp) AND (result EQ 1) AND (keyword_set(applytostack))) THEN BEGIN
     printtocon, "% 2DFilterGUI: Applying filter" 
     (*ptr).current=c
     CopyStack
     cnew=(*ptr).current
     (*cnew).name="BandpassFilter("+(*c).name+")"
     d=(*cnew).datap
;; display a prograss bar
     InitProgressBar, MINV=0, MAXV=(*d).SzX-1, TEXT="Applying Filter to Image Nr."
     For k=0,(*d).SzX-1 DO BEGIN
        ProgressBarSet, 0, k, TEXT=STRING(k)
        (*(*d).data)[k,*,*] = ABS(ApplyFilter(filterA,REFORM((*(*d).data)[k,*,*])))
     END
     DestroyProgressBar
  END 
  PTR_FREE, pkx
  PTR_FREE, pky
  PTR_FREE, pu
  pobj->set_token_value,'BANDPASS','HINQFRAC', bphi, ERRSTATE=err
  pobj->set_token_value,'BANDPASS','LOWNQFRAC', bplo, ERRSTATE=err
  pobj->set_token_value,'BANDPASS','EXPONENT', bpexp, ERRSTATE=err
  pobj->set_token_value,'BANDPASS','TYPE', bptype, ERRSTATE=err
  pobj->set_token_value,'BANDPASS','MEANVALUE', mv, ERRSTATE=err
  return, pp
END

PRO TestBPFilter
  XDim=512 & YDim=256
  WINDOW, XSIZE=XDim, YSize=YDim
  STOP
  pkx=PTR_NEW()
  pky=PTR_NEW()
  pu=PTR_NEW()
  PrepareRecSampGrid, XDim, YDim, pkx, pky, pu
  bpexp=2.
  bplo=0.1
  bphi=0.5
  gaussian=0
  filterA=GetBPNQFilterPreCalcK(pkx, pky, pu, bplo, bphi, bpexp, Gaussian=gaussian)
  TVSCL, filterA 
END
  
