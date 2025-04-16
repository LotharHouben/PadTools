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




FUNCTION FilterGUI3D, BWLO=bwlo, BWHI=bwhi

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
  ;; define view frames
  TotalDimX=DimX+DimY & TotalDimY=DimZ
  EinsY=FLTARR(TotalDimY)+1
  GradX=FINDGEN(TotalDimX)
  view=PTR_NEW(EinsY##GradX) ;; the image that will be displayed
  ;; it has two views: ViewA is a X-Z slice, ViewB is a Y-Z slice 
  viewA=WHERE((*view) LT DimX)
  viewB=WHERE((*view) GE DimX)
  ;; you can later set the view frames
  ;; view[viewA]=REFORM(*datap[x,*,z]) 
  ;; where x and z are the slice coordinates
  ;; preset display image
  x=Fix(DimX/2) & y=Fix(DimY/2) & z=Fix(DimZ/2)
  Case (*e).zcoord OF 
      1: x=(*e).slice
      2: y=(*e).slice
      ELSE: z=(*e).slice
   END
  (*view)[viewA]=REFORM(REFORM((*datap)[*,y,*],DimX*DimZ,1,1))
  (*view)[viewB]=REFORM(REFORM((*datap)[x,*,*],DimY*DimZ,1,1))
;;  Window, 1, XSIZE=TotalDimX, YSIZE=TotalDimY
;;  TVSCL, view
  binning=TVBIN(TotalDimX,TotalDimY)
  viewszx=CEIL(TotalDimX/binning) & viewszx=CEIL(TotalDimY/binning)
  ;; select a slice and create the Radon transform 
  ;;
  name="VolumeSlices("+(*c).name+")"
  pp=DataList_CreateElement(ptr, name)
  ;; calculate the central slice to get the right dimensions
  cslice=0
  (*pp).type=4
  (*pp).SzX=TotalDimX
  (*pp).SzY=TotalDimY
  (*pp).SzZ=1
  (*pp).xsamp=1
  (*pp).ysamp=1
  (*pp).zsamp=1
  (*pp).slice=0
  (*pp).zcoord=3
  (*pp).contrastmode="auto"
  ppd=PTR_NEW(view)
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% FilterGUI3D: failed to create data array for the slice view." 
     return, 0
  END
  (*pp).data=view
  ;; current stack is *pp, display the projections on the screen  
  CreateWindow
  TVDisplay
  Update_XTabControl
  ;; 
  ;; create rulers
  myrulers=obj_new('Ruler', SCREENSCOPE=[0.,(TotalDimX-1),0,(TotalDimY-1)])
  ;;
  ;; add a ruler for the z-coordinate, this is free to choose
  z0=FIX(DimZ/2) & dz=FIX(DimZ/20)
  myrulers->AddRuler, 'Z-Ruler', z0, dz, ORIENTATION=0, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[0.,(TotalDimX-1),0.,TotalDimY], /SINGLE
  ;; add a ruler for the x-coordinate, x0 is the projection on the
  ;; right side
  x0=x & dx=FIX(DimX/20)
  myrulers->AddRuler, 'X-Ruler', x0, dx, ORIENTATION=1, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[0.,(DimX-1),0.,TotalDimY], /SINGLE
;; add a ruler for the x-coordinate, x0 is the projection on the
  ;; right side
  y0=y & dy=FIX(DimY/20)
  myrulers->AddRuler, 'Y-Ruler', y0+DimX, dy, ORIENTATION=1, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[DimX,(TotalDimX-1),0.,TotalDimY], /SINGLE
 
  ;;
  ;; define the slider widget
  ;;
  title='3D Filter GUI'
  base=widget_base(Title=title, /Column)
  ;;
  ;; Filter
  ;;
  bwexp=1.
  IF NOT(keyword_set(bwloexp)) THEN bwloexp=bwexp
  IF NOT(keyword_set(bwhiexp)) THEN bwhiexp=bwexp
  IF NOT(keyword_set(bwlo)) THEN bwlo=0.06
  IF NOT(keyword_set(bwhi)) THEN bwhi=0.7
  ;;
  IF ((bwlo LT 0.) OR (bwlo GE 1.)) THEN bwlo=0.
  IF ((bwhi LT 0.) OR (bwhi GT 1.)) THEN bwhi=1.
  IF (bwhi LT bwlo) THEN BEGIN
     tmp=bwhi & bwhi=bwlo & bwlo=tmp
  END

  ;; default: no filter
  filterstack=0B
  ;;
  frame_filter = WIDGET_BASE(base,/COLUMN)
  frame_filter_button=Widget_Base(frame_filter, /NONEXCLUSIVE)
  button_filter = Widget_Button(frame_filter_button, Value='Apply Bandpass Filter')
  Widget_Control, button_filter, Set_Button=filterstack
  frame_filter_sliders = WIDGET_BASE(frame_filter,/ROW)
  label=WIDGET_LABEL(frame_filter_sliders,VALUE='Low Cut-off')
  slider_low=Widget_SLIDER(frame_filter_sliders, MINIMUM=0, MAXIMUM=100, Value=Fix(bwlo*100))
  label=WIDGET_LABEL(frame_filter_sliders,VALUE='High Cut-off')
  slider_hi=Widget_SLIDER(frame_filter_sliders, MINIMUM=0, MAXIMUM=100, Value=Fix(bwhi*100))
  label=WIDGET_LABEL(frame_filter_sliders,VALUE='Exponent')
  slider_exp=Widget_SLIDER(frame_filter_sliders, MINIMUM=1, MAXIMUM=10., Value=FIX(bwexp))
; filter: Create filter
  filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
  filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])  ;;
  slider_frame=WIDGET_BASE(base,/Row)
  frame_X = WIDGET_BASE(slider_frame,/Row)
  label=WIDGET_LABEL(frame_X,VALUE='X:')
  slider_x=WIDGET_SLIDER(frame_X, MINIMUM=0, MAXIMUM=DimX-1, Value=x0)
  ;;
  frame_Y = WIDGET_BASE(slider_frame,/Row)
  label=WIDGET_LABEL(frame_Y,VALUE='Y:')
  slider_y=WIDGET_SLIDER(frame_Y, MINIMUM=0, MAXIMUM=DimY-1, Value=y0)
  ;;
  frame_Z = WIDGET_BASE(slider_frame,/Row)
  label=WIDGET_LABEL(frame_Z,VALUE='Z:')
  slider_z=WIDGET_SLIDER(frame_Z, MINIMUM=0, MAXIMUM=DimZ-1, Value=z0)
  ;;
;; define roi list
  roilist=list()
  proi=PROIStructure3D()
  (*proi).ID=0
  (*proi).X0=FIX(x0-0.5*dx)
  (*proi).X1=FIX(x0+0.5*dx)
  (*proi).Y0=FIX(y0-0.5*dy)
  (*proi).Y1=FIX(y0+0.5*dy)
  (*proi).Z0=FIX(z0-0.5*dz)
  (*proi).Z1=FIX(z0+0.5*dz)
  roilist.add, proi
  roi_frame=WIDGET_BASE(base,/Column)
  ;; widget list to create and administrate regions
  framelist = WIDGET_BASE(roi_frame,/Row)
  listitems = STRARR(roilist.Count())
  FOR i=0,roilist.Count() DO BEGIN
     listitems[i-1]=StringROIStructure3D(roilist[i-1])
  END
  ;;
;;  buttonslist=WIDGET_BASE(roi_frame,/Row)
;;  new = WIDGET_BUTTON(buttonslist, value='Create New ROI', UVALUE='new')
;;  delete = WIDGET_BUTTON(buttonslist, value='Delete Selected ROI', UVALUE='delete')

  dummy = WIDGET_BASE(base,/Column)
  text = WIDGET_LABEL(dummy,VALUE='')
  buttons = WIDGET_BASE(base,/Row)
  apply = WIDGET_BUTTON(buttons, value='Accept', UVALUE='apply')
  cancel = WIDGET_BUTTON(buttons, value='Cancel', UVALUE='cancel')

  WIDGET_CONTROL, base, /REALIZE
    
  quit=0
  REPEAT BEGIN
     ev = WIDGET_Event(base)
;;     print, ev
;;     STOP
     CASE ev.id OF
        button_filter: BEGIN
           ;; toggle button
           IF filterstack THEN filterstack=0B ELSE filterstack=1B
           Widget_Control, button_filter, Set_Button=filterstack
           ;; redisplay
           ;; hide all rulers
           myrulers->HideRuler, 'X-Ruler'
           myrulers->HideRuler, 'Y-Ruler'
           myrulers->HideRuler, 'Z-Ruler'
           ;; update views
           IF (filterstack EQ 0) THEN BEGIN
              ;; no filter
              (*view)[viewB]=REFORM(REFORM((*datap)[x0,*,*],DimY*DimZ,1,1))
              (*view)[viewA]=REFORM(REFORM((*datap)[*,y0,*],DimX*DimZ,1,1))
           END ELSE BEGIN
              ;; filter: Create filter
              filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
              filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
              ;;
              
              (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
              (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
           END
           TVDisplay
           ;; update rulers, note that the ruler in viewA changes
           myrulers->ModifyRuler, 'X-Ruler', X=x0, Y=dx
           myrulers->ShowRuler, 'X-Ruler'
           myrulers->ModifyRuler, 'Z-Ruler', X=z0, Y=dz
           myrulers->ShowRuler, 'Z-Ruler'
           myrulers->ModifyRuler, 'Y-Ruler', X=y0+DimX, Y=dy
           myrulers->ShowRuler, 'Y-Ruler'
        END
        slider_exp: BEGIN
;; read filter low pass cutoff
           WIDGET_CONTROL, slider_exp, GET_VALUE=tmpx
           tmpx=FLOAT(tmpx)
           bwexp=tmpx
           bwloexp=bwexp & bwhiexp=bwexp
           filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
           filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
           IF (filterstack EQ 1) THEN BEGIN
              ;;
              (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
              (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
              ;;
              TVDisplay
           END
        END 
        slider_low: BEGIN
           ;; read filter low pass cutoff
           WIDGET_CONTROL, slider_low, GET_VALUE=tmpx
           tmpx=FLOAT(tmpx)/100.
           if (tmpx LT bwhi) THEN BEGIN
              bwlo=tmpx
              Widget_Control, slider_low, SET_VALUE=(bwlo*100)           
              IF (filterstack EQ 1) THEN BEGIN
                 filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
                 filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
                 ;;
                 (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
                 (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
                 ;;
                 TVDisplay
              END
           END ELSE BEGIN
              Widget_Control, slider_low, SET_VALUE=FIX(bwlo*100)           
           END
        END
        slider_hi: BEGIN
           ;; read filter low pass cutoff
           WIDGET_CONTROL, slider_hi, GET_VALUE=tmpx
           tmpx=FLOAT(tmpx)/100.
           if (tmpx GT bwlo) THEN BEGIN
              bwhi=tmpx
              Widget_Control, slider_hi, SET_VALUE=FIX(bwhi*100)           
              IF (filterstack EQ 1) THEN BEGIN
                 filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
                 filterB=GetBWNQFilter(DimY, DimZ,  BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
                 ;;
                 (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
                 (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
                 ;;
                 TVDisplay
              END
           END ELSE BEGIN
              Widget_Control, slider_hi, SET_VALUE=FIX(bwhi*100)           
           END
        END
        slider_x: BEGIN
           ;; changing x, we have to change view B (Y-Z)
           ;; read new x slice
           tmpx=x0
           WIDGET_CONTROL, slider_x, GET_VALUE=tmpx
           x0=tmpx
           ;; hide rulers
           myrulers->HideRuler, 'X-Ruler'
           myrulers->HideRuler, 'Y-Ruler'
           myrulers->HideRuler, 'Z-Ruler'
           ;; change viewB
           IF (filterstack EQ 1) THEN BEGIN
              (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
           END ELSE BEGIN
              (*view)[viewB]=REFORM(REFORM((*datap)[x0,*,*],DimY*DimZ,1,1))
           END
           TVDisplay
           ;; update rulers, note that the ruler in viewA changes
           myrulers->ShowRuler, 'Y-Ruler'
           myrulers->ShowRuler, 'Z-Ruler'
           myrulers->ModifyRuler, 'X-Ruler', X=x0, Y=dx
           myrulers->ShowRuler, 'X-Ruler'
           tmpdx=dx
           IF myrulers->GetRuler('X-Ruler', X=tmpx, Y=tmpdx) THEN BEGIN
              x0=tmpx & dx=tmpdx
              Widget_Control, slider_x, SET_VALUE=x0
           END
           ;;
        END
        slider_y: BEGIN
           ;; changing x, we have to change view A (X-Z)
           ;; read new x slice
           tmpy=y0
           WIDGET_CONTROL, slider_y, GET_VALUE=tmpy
           y0=tmpy
           ;; hide rulers
           myrulers->HideRuler, 'X-Ruler'
           myrulers->HideRuler, 'Y-Ruler'
           myrulers->HideRuler, 'Z-Ruler'
           ;; change viewA
           IF (filterstack EQ 1) THEN BEGIN
              (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
           END ELSE BEGIN
              (*view)[viewA]=REFORM(REFORM((*datap)[*,y0,*],DimX*DimZ,1,1))
           END
           TVDisplay
           ;; update rulers, note that the ruler in viewA changes
           myrulers->ShowRuler, 'X-Ruler'
           myrulers->ShowRuler, 'Z-Ruler'
           myrulers->ModifyRuler, 'Y-Ruler', X=y0+DimX, Y=dy
           myrulers->ShowRuler, 'Y-Ruler'
           tmpdy=dy
           IF myrulers->GetRuler('Y-Ruler', X=tmpy, Y=tmpdy) THEN BEGIN
              y0=tmpy-DimX & dy=tmpdy
              Widget_Control, slider_y, SET_VALUE=y0
           END
        END
        slider_z: BEGIN
           ;; changing z, we have to change the ruler only
           ;; read new z slice
           tmpz=z0
           WIDGET_CONTROL, slider_z, GET_VALUE=tmpz
           z0=tmpz
           ;; modify rulers
           myrulers->ModifyRuler, 'Z-Ruler', X=z0, Y=dz
           ;;
           tmpz=z0
           IF myrulers->GetRuler('Z-Ruler', X=tmpz, Y=tmpdz) THEN BEGIN
              z0=tmpz & dz=tmpdz
              Widget_Control, slider_z, SET_VALUE=z0
           END
           ;;
        END
        apply: BEGIN
           quit=1
           result=1
        END
        cancel: BEGIN
           quit=1
           result=0
        END
        ELSE: 
     END 
     ENDREP UNTIL (quit EQ 1)
     WIDGET_CONTROL, base, /DESTROY
  obj_destroy, myrulers
  if (result EQ 1) THEN BEGIN
     printtocon, "% FilterGUI3D: Applying filter" 
     
     For i=1,roilist.Count() DO BEGIN
        printtocon, "   low, high cutoff frequency (% Nq): "+MyString(bwlo*100)+", "  +MyString(bwhi*100)
        printtocon, "   exponent                         : "  +MyString(bwexp)
     END
     bwhiexp=bwexp
     bwloexp=bwexp
     ; filter: Create filter
     filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp])
     (*ptr).current=c
     CopyStack
     cnew=(*ptr).current
     (*cnew).name="Filter("+(*c).name+")"
     d=(*cnew).datap
;; display a prograss bar
     InitProgressBar, MINV=0, MAXV=(*d).SzY-1, TEXT="Applying Filter to Image Nr."
     For k=0,(*d).SzY-1 DO BEGIN
        ProgressBarSet, 0, k, TEXT=STRING(k)
        (*(*d).data)[*,k,*] = ApplyFilter(filterA,(*(*d).data)[*,k,*])
     END
     DestroyProgressBar
  END 
  return, result
END
