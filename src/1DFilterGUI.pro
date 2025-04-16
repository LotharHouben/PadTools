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

FUNCTION RaisedCos1DFilter, dim, pk, pu, alpha
  ;; returns a 1D rasised cosine filter, multiplied by a cosine smoothing
  ;; function
  ;; dim: image dimension
  ;; pk: Pointer, k^2 array - will be returned if not valid upon input
  ;; pu: Pointer, sqrt(kx^2) array (ramp filter)
  IF (NOT(PTR_VALID(pk)) OR NOT(PTR_VALID(pu))) THEN BEGIN
     ;; first create the integer arrays kx_ij=i ky_ij=j
     kx = LINDGEN(dim)
     kx = kx-dim/2
     maxx=MAX(kx)  
     pk = PTR_NEW(DOUBLE(kx))
     (*pk)=(*pk)*(*pk)
     ;; note: the ramp is only in horizontal direction!
     pu=PTR_NEW(Sqrt((*pk)))
  END 
  ;; raised cosine mask
  kc=Fix(MAX(*pu)/2) ;; half Nyquist frequency
  w=FLTARR(dim) & w(*)=1.
  If (alpha EQ 0) THEN BEGIN
     ;; simple step shape
     B=WHERE((*pu) GT kc, count)
     if (count GT 0) THEN w(B)=0
  END ELSE BEGIN
     klow=kc*(1-alpha) & khigh=kc*(1+alpha)
     B=WHERE((*pu) GT klow, count)
     if (count GT 0) THEN w[B]=0.5*(1.+Cos(!PI*(((*pu)[B]-klow)/2/alpha/kc)))
     B=WHERE((*pu) GT khigh, count)
     if (count GT 0) THEN w[B]=0
  END
  return, SHIFT(w,dim/2)
END

FUNCTION CosRamp1DFilter, dim, pk, pu, alpha
  ;; returns a 1D ramp filter, multiplied by a cosine smoothing
  ;; function
  ;; dim: image dimension
  ;; pk: Pointer, k^2 array - will be returned if not valid upon input
  ;; pu: Pointer, sqrt(kx^2) array (ramp filter)
  IF (NOT(PTR_VALID(pk)) OR NOT(PTR_VALID(pu))) THEN BEGIN
     ;; first create the integer arrays kx_ij=i ky_ij=j
     kx = LINDGEN(dim)
     kx = kx-dim/2
     maxx=MAX(kx)  
     pk = PTR_NEW(DOUBLE(kx))
     (*pk)=(*pk)*(*pk)
     ;; note: the ramp is only in horizontal direction!
     pu=PTR_NEW(Sqrt((*pk)))
  END 
  ;; raised cosine mask
  kc=Fix(MAX(*pu)/2) ;; half Nyquist frequency
  w=FLTARR(dim) & w(*)=1.
  If (alpha EQ 0) THEN BEGIN
     ;; simple step shape
     B=WHERE((*pu) GT kc, count)
     if (count GT 0) THEN w(B)=0
  END ELSE BEGIN
     klow=kc*(1-alpha) & khigh=kc*(1+alpha)
     B=WHERE((*pu) GT klow, count)
     if (count GT 0) THEN w[B]=0.5*(1.+Cos(!PI*(((*pu)[B]-klow)/2/alpha/kc)))
     B=WHERE((*pu) GT khigh, count)
     if (count GT 0) THEN w[B]=0
  END
  Filt=(*pu)*w 
  return, SHIFT(Filt,dim/2)
END


FUNCTION Ramp1DFilter, dim, pk, pu, w, expnt
  ;; returns a 1D ramp filter, multiplied by an exponential smoothing
  ;; function
  ;; dim: image dimension
  ;; pk: Pointer, k^2 array - will be returned if not valid upon input
  ;; pu: Pointer, sqrt(kx^2) array (ramp filter)
  IF (NOT(PTR_VALID(pk)) OR NOT(PTR_VALID(pu))) THEN BEGIN
     ;; first create the integer arrays kx_ij=i ky_ij=j
     kx = LINDGEN(dim)
     kx = kx-dim/2
     maxx=MAX(kx)  
     pk = PTR_NEW(DOUBLE(kx))
     (*pk)=(*pk)*(*pk)
     ;; note: the ramp is only in horizontal direction!
     pu=PTR_NEW(Sqrt((*pk)))
  END 
  ;; exponential mask
  wx=0.5*w*dim
  invwx2=1/wx/wx 
  FX=invwx2*(*pk)
;;  print, "wx,wy=", wx, ",",  wy
;;  print, "exponent=", expnt
;;  STOP
  Filt=(*pu) ;; ramp
  if (expnt EQ 0.) THEN BEGIN
     B=WHERE((FX) GT 1, count)
     if (count GT 0) THEN Filt[B]=0
     return, SHIFT(Filt,dim/2)
  END
  if (expnt EQ 0.5) THEN BEGIN
     Filt=Filt*exp(-SQRT(FX))
     return, SHIFT(Filt,dim/2)
  END
  if (expnt EQ 1) THEN BEGIN 
        Filt=Filt*exp(-(FX))
        return, SHIFT(Filt,dim/2)
  END
  Filt=Filt*exp(-(FX)^expnt) 
  return, SHIFT(Filt,dim/2)
END


FUNCTION FilterGUI1D

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
        "PROCEDURE 1D Filter GUI", $
        " ", $
        "PURPOSE: Frequency filter to weigh the radial frequency components prior to reconstruction.", $
        "         Note that the data stack axes need to fulfill the convention for the reconstruction, ", $
        "         i.e. X is the dimension of tilt angles, Y is the distance from the tilt axis and ", $
        "         Z is the direction parallel to the tilt axis.", $
        "    ", $
        "FILTERS AND PARAMETERS: ", $
        " ", $
        "- Ramp Exponential Filter: Linear Frequency ramp along Y multiplied with an exponential decay to high frequencies. ", $
        "                           width is the width of the exponential function and corresponds to the high frequency ", $
        "                           cut-off. exp is the exponent of the exponential smoothing. exp=0 means no smoothing, i.e.", $
        "                           a pure ramp with sharp cut-off. exp=2 is a gaussian smoothing function. ", $
        "- Ramp Cosine Filter:      Linear Frequency ramp along Y multiplied with a raised cosine filter. The raised cosine", $
        "                           filter smoothes the cut-off at half Nyquist (http://www.filter-solutions.com/raised.html).",$
        "                           The roll-off factor determines the bandwidth of cosine smoothing around half the Nyquist. ", $
        "                           frequency. A roll off factor of 0 corresponds to a sharp cut-off, a value of 100 means no ", $
        "                           cosine smoothing and a ramp up to the Nyquist frequency.", $
        "- Raised Cosine Filter:    Similar to the Ramp Cosine Filter without the frequency ramp. A pure low pass filter. ", $
        " ", $
        "OUTPUT: ", $
        " ", $
        "Two-dimensional frequency domain filter and filtered data stack. ", $
        "" $
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
         printtocon, "% FilterGUI1D: Projection is not along X (proj=1)." 
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
     print, "% FilterGUI1D: Failed to create data array for the preview slice." 
     return, 0
  END
  ;; current stack is *pp, display the projections on the screen  
  CreateWindow, BIN=(*pp).binning
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
      printtocon, "% 1DFilterGUI: Error reading RAMPFILTER::MASK."
   END
  expnt=pobj->get_token_value('RAMPFILTER','MASKEXPONENT', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      expnt=1.
      printtocon, "% 1DFilterGUI: Error reading RAMPFILTER::MASKEXPONENT."
   END
   cosfactor=pobj->get_token_value('RAMPFILTER','COSFACTOR', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      cosfactor=1.
      printtocon, "% 1DFilterGUI: Error reading RAMPFILTER::COSFACTOR."
   END
   raisedcosfactor=pobj->get_token_value('RAMPFILTER','RAISEDCOSFACTOR', ERRSTATE=err)
   IF (err NE 0) THEN BEGIN 
      raisedcosfactor=1.
      printtocon, "% 1DFilterGUI: Error reading RAMPFILTER::RAISEDCOSFACTOR."
   END
  ;;
  title='1D Filter GUI'
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
  button_raisedcosfilter = Widget_Button(buttons, Value='Raised Cosine')
  ;;
  col2=widget_base(colbase, /COLUMN)
  dummy=WIDGET_LABEL(col2,VALUE='')
  dummy=WIDGET_LABEL(col2,VALUE='')
  dummy=WIDGET_LABEL(col2,VALUE='')
  slider_width=WIDGET_SLIDER(col2, MINIMUM=0, MAXIMUM=100, Value=FIX(width*100), TITLE='width')
  slider_cosfact=WIDGET_SLIDER(col2, MINIMUM=0, MAXIMUM=100, Value=FIX(cosfactor*100), TITLE='roll off factor')
  slider_raisedcosfact=WIDGET_SLIDER(col2, MINIMUM=0, MAXIMUM=100, Value=FIX(raisedcosfactor*100), TITLE='roll off factor')

  ;;
  col3=widget_base(colbase, /COLUMN)
  dummy=WIDGET_LABEL(col3,VALUE='')
  dummy=WIDGET_LABEL(col3,VALUE='')
  dummy=WIDGET_LABEL(col3,VALUE='')
  slider_expnt=WIDGET_SLIDER(col3, MINIMUM=0, MAXIMUM=10, Value=FIX(2*expnt), TITLE='exp')
  ;;
  ; filter: Create filter
  pk=PTR_NEW() & pu=PTR_NEW()
  EinsZ=1.+FLTARR(DimZ)
  ;; deactivate all sliders, no filter by default
  WIDGET_CONTROL, slider_expnt, SENSITIVE=0
  WIDGET_CONTROL, slider_width, SENSITIVE=0
  WIDGET_CONTROL, slider_cosfact, SENSITIVE=0
  WIDGET_CONTROL, slider_raisedcosfact, SENSITIVE=0
  Widget_Control, button_nofilter, /Set_Button
  filterstack=0B

  ;; create a filter to pre-calculate kx, ky and u=kx^2+ky^2 arrays
  filterA=(Ramp1DFilter(DimY, pk, pu, width, expnt)#EinsZ)
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
           WIDGET_CONTROL, slider_raisedcosfact, SENSITIVE=0
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
           WIDGET_CONTROL, slider_raisedcosfact, SENSITIVE=0
           ;; filter: Create filter
           ;;
           filterA=(Ramp1DFilter(DimY, pk, pu, width, expnt)#(EinsZ))
           ;; filterA=Ramp1DFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
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
           WIDGET_CONTROL, slider_raisedcosfact, SENSITIVE=0
           WIDGET_CONTROL, slider_cosfact, GET_VALUE=tmpx
           cosfactor=0.01*FLOAT(tmpx)
           ;; filter: Create filter
           ;;
           filterA=(CosRamp1DFilter(DimY, pk, pu, cosfactor)#(EinsZ))
           ;; filterA=CosRamp1DFilter(DimY, DimZ, pkx, pky, pu, cosfactor)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        button_raisedcosfilter: BEGIN
           ;; toggle button
           filterstack=1B
           Widget_Control, button_raisedcosfilter, /Set_Button
           WIDGET_CONTROL, slider_expnt, SENSITIVE=0
           WIDGET_CONTROL, slider_width, SENSITIVE=0
           WIDGET_CONTROL, slider_cosfact, SENSITIVE=0
           WIDGET_CONTROL, slider_raisedcosfact, SENSITIVE=1
           ;; filter: Create filter
           ;;
           filterA=(RaisedCos1DFilter(DimY, pk, pu, raisedcosfactor)#(EinsZ))
           ;; filterA=CosRamp1DFilter(DimY, DimZ, pkx, pky, pu, cosfactor)
           (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
           TVDisplay
        END
        slider_expnt: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_expnt, GET_VALUE=tmpx
           expnt=0.5*FLOAT(tmpx)
           filterA=(Ramp1DFilter(DimY, pk, pu, width, expnt)#(EinsZ))
;;           filterA=Ramp1DFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
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
           filterA=(Ramp1DFilter(DimY, pk, pu, width, expnt)#(EinsZ))
;;           filterA=Ramp1DFilter(DimY, DimZ, pkx, pky, pu, width, expnt)
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
           filterA=(CosRamp1DFilter(DimY, pk, pu, cosfactor)#(EinsZ))
;;            filterA=CosRamp1DFilter(DimY, DimZ, pkx, pky, pu, cosfactor)
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*(*pp).data)=ABS(ApplyFilter(filterA,REFORM((*datap)[x0,*,*])))
              ;;
              TVDisplay
           END
        END 
        slider_raisedcosfact: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_raisedcosfact, GET_VALUE=tmpx
           raisedcosfactor=0.01*FLOAT(tmpx)
           filterA=(RaisedCos1DFilter(DimY, pk, pu, raisedcosfactor)#(EinsZ))
;;            filterA=CosRamp1DFilter(DimY, DimZ, pkx, pky, pu, cosfactor)
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
     (*cnew).name="Ramp1DFilter("+(*c).name+")"
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
  Update_XTabControl
  PTR_FREE, pk
  PTR_FREE, pu
  pobj->set_token_value,'RAMPFILTER','MASKWIDTH', width, ERRSTATE=err
  pobj->set_token_value, 'RAMPFILTER','MASKEXPONENT', expnt, ERRSTATE=err
  pobj->set_token_value, 'RAMPFILTER','COSFACTOR', cosfactor, ERRSTATE=err
  pobj->set_token_value, 'RAMPFILTER','RAISEDCOSFACTOR', raisedcosfactor, ERRSTATE=err
  return, result
END
