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


FUNCTION PROIStructure3D
s1 = CREATE_STRUCT('ID', -1,'X0', 0, 'X1', 0,'Y0', 0, 'Y1', 0,'Z0', 0, 'Z1', 0)
return, PTR_NEW(s1)
END

FUNCTION StringROIStructure3D, proi
s='X: '+MyString((*proi).X0)+"-"+MyString((*proi).X1) 
s += ' Y: '+MyString((*proi).Y0)+"-"+MyString((*proi).Y1) 
s += ' Z: '+MyString((*proi).Z0)+"-"+MyString((*proi).Z1) 
return, s 
END

FUNCTION ROIListToArray, roilist
;; convert ROI list to an array for downwards compatibility of the
;; core routine with older IDL versions
;;      roiarray: INTARR(nROI,6)
;;   
roiarray=0
nROI=roilist.Count()
if (nROI LE 0) THEN BEGIN
   PrintToCon, "% ROIListToArray: ROI list is empty "
   return, 0
END
;; convert ROI list to array for downwards compatibility of the
;; core routine with older IDL versions
roiarray=INTARR(nROI,6)
For i=0,nROI-1 DO BEGIN
   roiarray[i,0]=(*roilist[i]).X0
   roiarray[i,1]=(*roilist[i]).X1
   roiarray[i,2]=(*roilist[i]).Y0
   roiarray[i,3]=(*roilist[i]).Y1
   roiarray[i,4]=(*roilist[i]).Z0
   roiarray[i,5]=(*roilist[i]).Z1
END
return, roiarray
END 


PRO ROIListToTable, roilist
;; convert ROI list to atable object
nROI=roilist.Count()
if (nROI LE 0) THEN BEGIN
   PrintToCon, "% ROIListToTable: ROI list is empty "
   return
END
;; convert ROI list to array for downwards compatibility of the
;; core routine with older IDL versions
;create table
atable=NewTable('ROI table',13,1)
atable->SetColumnLabels, ["index","X0","X1","Y0","Y1","Z0","Z1","CX","CY","CZ","WX","WY","WZ"]
atable->SetLayerNames, ["Layer 1"]
atable->SetDescriptor, ["Region of Interest Table","", ""] 
atable->Setlayer, 1
;; fill table data: each row is a list of values
For i=0,(nROI-1) DO BEGIN
   CX=0.5*((*roilist[i]).X0+(*roilist[i]).X1)
   CY=0.5*((*roilist[i]).Y0+(*roilist[i]).Y1)
   CZ=0.5*((*roilist[i]).Z0+(*roilist[i]).Z1)
   WX=ABS((*roilist[i]).X0-(*roilist[i]).X1)+1
   WY=ABS((*roilist[i]).Y0-(*roilist[i]).Y1)+1
   WZ=ABS((*roilist[i]).Z0-(*roilist[i]).Z1)+1
   atable->AddRow, list(i,(*roilist[i]).X0,(*roilist[i]).X1,(*roilist[i]).Y0,(*roilist[i]).Y1,(*roilist[i]).Z0,(*roilist[i]).Z1,CX,CY,CZ,WX,WY,WZ)
END
return
END 

FUNCTION ROIArrayToList, roiarray
;; convert ROI array to a list  downwards compatibility of the
;; core routine with older IDL versions
;;      roiarray: INTARR(nROI,6)
;;   
N=Size(roiarray)
NRoi=N(1) 
roilist=list() 
For i=0,nROI-1 DO BEGIN
   proi=PROIStructure3D()
   (*proi).X0=roiarray[i,0]
   (*proi).X1=roiarray[i,1]
   (*proi).Y0=roiarray[i,2]
   (*proi).Y1=roiarray[i,3]
   (*proi).Z0=roiarray[i,4]
   (*proi).Z1=roiarray[i,5]
   roilist.add, proi           
END
RETURN, roilist
END



FUNCTION VolumePicker3D, ROILIST=roilist, BWLO=bwlo, BWHI=bwhi, Filter=filter, ERRSTATE=errstate, STACKS=stacks, TOTABLE=totable
;;
;; stacks: list, VolumePicker adds the newly created data stacks here
;; 
IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% VolumePicker3D:    Fatal error "
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
 
;;  Window, 1, XSIZE=TotalDimX, YSIZE=TotalDimY
;;  TVSCL, view
  binning=(*e).binning ;TVBIN(TotalDimX,TotalDimY)
  viewszx=CEIL(TotalDimX/binning) & viewszx=CEIL(TotalDimY/binning)
  ;; select a slice and create the Radon transform 
  ;;
  name="VolumeSlices("+(*c).name+")"
  pp=DataList_CreateElement(ptr, name)
  ;; calculate the central slice to get the right dimensions
  cslice=0
  (*pp).type=4
  (*pp).binning=binning
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
     print, "% VolumePicker3D: failed to create data array for the slice view." 
     return, 0
  END
  (*pp).data=view
  ;; current stack is *pp, display the projections on the screen  
  CreateWindow, BIN=binning
  TVDisplay
  Update_XTabControl
  IF keyword_set(stacks) THEN stacks.add, (*ptr).current
  ;; 
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
  ;; apply filter?
  IF NOT(keyword_set(filter)) then filter='N'
  if (STRUPCASE(filter) EQ 'N') THEN filterstack=0B else filterstack=1B
  ;;
  ; filter: Create filter
  filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
  filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
  ;;
  IF (filterstack EQ 0) THEN BEGIN
     ;; no filter
     (*view)[viewB]=REFORM(REFORM((*datap)[x,*,*],DimY*DimZ,1,1))
     (*view)[viewA]=REFORM(REFORM((*datap)[*,y,*],DimX*DimZ,1,1))
  END ELSE BEGIN
     ;; filter: Create filter
     filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
     filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
     ;;
     
     (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x,*,*])),DimY*DimZ,1,1))
     (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y,*])),DimX*DimZ,1,1))
  END
  TVDisplay
  ;;
  ;; create rulers
  myrulers=obj_new('Ruler', SCREENSCOPE=[0.,(TotalDimX-1),0,(TotalDimY-1)])
  ;;
  ;; add a ruler for the z-coordinate, this is free to choose
  z0=FIX(DimZ/2) & dz=FIX(DimZ/20)
  myrulers->AddRuler, 'Z-Ruler', z0, dz, ORIENTATION=0, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[0.,(TotalDimX-1),0.,TotalDimY]
  ;; add a ruler for the x-coordinate, x0 is the projection on the
  ;; right side
  x0=x & dx=FIX(DimX/20)
  myrulers->AddRuler, 'X-Ruler', x0, dx, ORIENTATION=1, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[0.,(DimX-1),0.,TotalDimY]
;; add a ruler for the x-coordinate, x0 is the projection on the
  ;; right side
  y0=y & dy=FIX(DimY/20)
  myrulers->AddRuler, 'Y-Ruler', y0+DimX, dy, ORIENTATION=1, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[DimX,(TotalDimX-1),0.,TotalDimY]
  ;; define roi list
  IF keyword_set(roilist) THEN BEGIN
     ;; Do a range check for the ROI
     jmax=roilist.Count()-1
     for j=jmax, 0, -1 DO BEGIN
        printtocon, "% VolumePicker3D: Passed ROI #"+MyString(j)+" - "+StringROIStructure3D(roilist[j])
        rangeviolation=0
        proi=roilist[j]
        IF ((*proi).X0 GT DIMX) THEN rangeviolation=1
        IF ((*proi).X1 GT DIMX) THEN rangeviolation=1
        IF ((*proi).Y0 GT DIMY) THEN rangeviolation=1
        IF ((*proi).Y1 GT DIMY) THEN rangeviolation=1
        IF ((*proi).Z0 GT DIMZ) THEN rangeviolation=1
        IF ((*proi).Z1 GT DIMZ) THEN rangeviolation=1
        IF rangeviolation then  BEGIN
           PrintToCon, "%    Removing ROI #"+MyString(j)+" because of range violation."
           proi=roilist.remove(j)
        END 
     END 
     if NOT(roilist.Count() GE 1) THEN BEGIN
        ErrMsg, "% VolumePicker3D: Warning - Passed ROI list is empty."
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
     END
  END ELSE BEGIN
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
  END
  ;;
  ;; define the slider widget
  ;;
  title='3D Volume Picker'
  base=widget_base(Title=title, /Column)
  slider_frame=WIDGET_BASE(base,/Column, /FRAME)
  frame_X = WIDGET_BASE(slider_frame,/Row)
  label=WIDGET_LABEL(frame_X,VALUE='X:  Pos ')
  slider_x=WIDGET_SLIDER(frame_X, MINIMUM=0, MAXIMUM=DimX-1, Value=x0)
  label=WIDGET_LABEL(frame_X,VALUE=' Width ')
  slider_dx=WIDGET_SLIDER(frame_X, MINIMUM=0, MAXIMUM=FLOOR(DimX), Value=dx)
  ;;
  frame_Y = WIDGET_BASE(slider_frame,/Row)
  label=WIDGET_LABEL(frame_Y,VALUE='Y:  Pos ')
  slider_y=WIDGET_SLIDER(frame_Y, MINIMUM=0, MAXIMUM=DimY-1, Value=y0)
  label=WIDGET_LABEL(frame_Y,VALUE=' Width ')
  slider_dy=WIDGET_SLIDER(frame_Y, MINIMUM=0, MAXIMUM=FLOOR(DimY), Value=dy)
  ;;
  frame_Z = WIDGET_BASE(slider_frame,/Row)
  label=WIDGET_LABEL(frame_Z,VALUE='Z:  Pos ')
  slider_z=WIDGET_SLIDER(frame_Z, MINIMUM=0, MAXIMUM=DimZ-1, Value=z0)
  label=WIDGET_LABEL(frame_Z,VALUE=' Width ')
  slider_dz=WIDGET_SLIDER(frame_Z, MINIMUM=0, MAXIMUM=FLOOR(DimZ), Value=dz)
  ;;
  roi_frame=WIDGET_BASE(base,/Column, /FRAME)
  dummy = WIDGET_BASE(roi_frame,/Column)
  text = WIDGET_LABEL(dummy,VALUE='')
  text = WIDGET_LABEL(dummy,VALUE='Regions of Interest (ROI)')
  ;; widget list to create and administrate regions
  framelist = WIDGET_BASE(roi_frame,/Row)
  listitems = STRARR(roilist.Count())
  FOR i=0,roilist.Count() DO BEGIN
     listitems[i-1]=StringROIStructure3D(roilist[i-1])
  END
  rlist=widget_list(framelist, VALUE=listitems, YSize=5, XSIZE=50)
  ;; set selected item to the first roi
  selected=0
  widget_control, rlist, SET_LIST_SELECT=selected
  ;;
  buttonslist=WIDGET_BASE(roi_frame,/Row)
  new = WIDGET_BUTTON(buttonslist, value='Create New ROI', UVALUE='new')
  delete = WIDGET_BUTTON(buttonslist, value='Delete Selected ROI', UVALUE='delete')
 
  frame_filter = WIDGET_BASE(base,/COLUMN, /FRAME)
  frame_filter_button=Widget_Base(frame_filter, /NONEXCLUSIVE)
  button_filter = Widget_Button(frame_filter_button, Value='Apply Bandpass Filter')
  Widget_Control, button_filter, Set_Button=filterstack
  ;;
  ;;
  frame_filter_sliders = WIDGET_BASE(frame_filter,/ROW)
  slider_low=Widget_SLIDER(frame_filter_sliders, MINIMUM=0, MAXIMUM=100, Value=Fix(bwlo*100))
  slider_hi=Widget_SLIDER(frame_filter_sliders, MINIMUM=0, MAXIMUM=100, Value=Fix(bwhi*100))
  slider_exp=Widget_SLIDER(frame_filter_sliders, MINIMUM=1, MAXIMUM=10., Value=FIX(bwexp))

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
              filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
              filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
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
           filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
           filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
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
                 filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
                 filterB=GetBWNQFilter(DimY, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
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
                 filterA=GetBWNQFilter(DimX, DimZ, BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
                 filterB=GetBWNQFilter(DimY, DimZ,  BWLO=[bwlo,bwloexp], BWHI=[bwhi,bwhiexp], /MEANVALUE)
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
              Widget_Control, slider_dx, SET_VALUE=dx
           END
           selected=widget_info(rlist, /LIST_SELECT)
           IF (selected GE 0) THEN BEGIN
              (*roilist[selected]).X0=FIX(x0-0.5*dx)
              (*roilist[selected]).X1=FIX(x0+0.5*dx)
              listitems[selected]=StringROIStructure3D(roilist[selected])
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
           END
           ;;
        END
        slider_dx: BEGIN
           ;; changing dx, we have to change the ruler
           ;; read new dx value
           tmpdx=dx
           WIDGET_CONTROL, slider_dx, GET_VALUE=tmpdx
           dx=tmpdx
           ;; modify rulers
           myrulers->ModifyRuler, 'X-Ruler', X=x0, Y=dx
           ;;
           tmpx=x0
           IF myrulers->GetRuler('X-Ruler', X=tmpx, Y=tmpdx) THEN BEGIN
              x0=tmpx & dx=tmpdx
              Widget_Control, slider_x, SET_VALUE=x0
              Widget_Control, slider_dx, SET_VALUE=dx
           END
           selected=widget_info(rlist, /LIST_SELECT)
           IF (selected GE 0) THEN BEGIN
              (*roilist[selected]).X0=FIX(x0-0.5*dx)
              (*roilist[selected]).X1=FIX(x0+0.5*dx)
              listitems[selected]=StringROIStructure3D(roilist[selected])
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
           END
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
              Widget_Control, slider_dy, SET_VALUE=dy
           END
           selected=widget_info(rlist, /LIST_SELECT)
           IF (selected GE 0) THEN BEGIN
              (*roilist[selected]).Y0=FIX(y0-0.5*dy)
              (*roilist[selected]).Y1=FIX(y0+0.5*dy)
              listitems[selected]=StringROIStructure3D(roilist[selected])
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
           END
           ;;
        END
        slider_dy: BEGIN
           ;; changing dx, we have to change the ruler
           ;; read new dx value
           tmpdy=dy
           WIDGET_CONTROL, slider_dy, GET_VALUE=tmpdy
           dy=tmpdy
           ;; modify rulers
           myrulers->ModifyRuler, 'Y-Ruler', X=y0+DimX, Y=dy
           ;;
           tmpy=y0
           IF myrulers->GetRuler('Y-Ruler', X=tmpy, Y=tmpdy) THEN BEGIN
              y0=tmpy-DimX & dy=tmpdy
              Widget_Control, slider_y, SET_VALUE=y0
              Widget_Control, slider_dy, SET_VALUE=dy
           END
           selected=widget_info(rlist, /LIST_SELECT)
           IF (selected GE 0) THEN BEGIN
              (*roilist[selected]).Y0=FIX(y0-0.5*dy)
              (*roilist[selected]).Y1=FIX(y0+0.5*dy)
              listitems[selected]=StringROIStructure3D(roilist[selected])
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
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
              Widget_Control, slider_dz, SET_VALUE=dz
           END
           selected=widget_info(rlist, /LIST_SELECT)
           IF (selected GE 0) THEN BEGIN
              (*roilist[selected]).Z0=FIX(z0-0.5*dz)
              (*roilist[selected]).Z1=FIX(z0+0.5*dz)
              listitems[selected]=StringROIStructure3D(roilist[selected])
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
           END
           ;;
        END
        slider_dz: BEGIN
           ;; changing dx, we have to change the ruler
           ;; read new dx value
           tmpdz=dz
           WIDGET_CONTROL, slider_dz, GET_VALUE=tmpdz
           dz=tmpdz
           ;; modify rulers
           myrulers->ModifyRuler, 'Z-Ruler', X=z0, Y=dz
           ;;
           tmpz=z0
           IF myrulers->GetRuler('Z-Ruler', X=tmpz, Y=tmpdz) THEN BEGIN
              z0=tmpz & dz=tmpdz
              Widget_Control, slider_z, SET_VALUE=z0
              Widget_Control, slider_dz, SET_VALUE=dz
           END
           selected=widget_info(rlist, /LIST_SELECT)
           IF (selected GE 0) THEN BEGIN
              (*roilist[selected]).Z0=FIX(z0-0.5*dz)
              (*roilist[selected]).Z1=FIX(z0+0.5*dz)
              listitems[selected]=StringROIStructure3D(roilist[selected])
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
           END
        END
        new: BEGIN
           ;; create a new region of interest
           proi=PROIStructure3D()
           (*proi).ID=roilist.Count()
           (*proi).X0=(*roilist[selected]).X0
           (*proi).X1=(*roilist[selected]).X1
           (*proi).Y0=(*roilist[selected]).Y0
           (*proi).Y1=(*roilist[selected]).Y1
           (*proi).Z0=(*roilist[selected]).Z0
           (*proi).Z1=(*roilist[selected]).Z1
           roilist.add, proi
           ;; copy data from the current set
           listitems=[listitems,StringROIStructure3D(proi)]
           selected=N_Elements(listitems)-1
           Widget_Control, rlist, SET_VALUE=listitems
           widget_control, rlist, SET_LIST_SELECT=selected
           ;;
           ;; no need to update view
           ;;
        END 
        delete: BEGIN
           ;; delete current ROI
           selected=widget_info(rlist, /LIST_SELECT)
           print, "deleting selected ROI at position ", selected
           if (roilist.Count() GT 1) THEN BEGIN
              ;; delete from widget list
              print, "ROI widget list descriptor: ", listitems[selected]
              print, "Full listitems list: ", listitems 
              listitems=DeleteElement(listitems, selected)
              print, "Processed listitems list: ", listitems 
              ;; delete from ROI list
              proi=roilist[selected]
              proi=roilist.remove(selected)
              print, "Deleted element in ROI pointer list: ", *proi
              PTR_FREE, proi
              ;; set selected and the region
              selected=roilist.Count()-1
              proi=roilist[selected]
              print, "Selected element in ROI pointer list: ", *proi
              x0=FIX(0.5*((*proi).X0+(*proi).X1))
              y0=FIX(0.5*((*proi).Y0+(*proi).Y1))
              z0=FIX(0.5*((*proi).Z0+(*proi).Z1))
              dx=(*proi).X1-(*proi).X0+1
              dy=(*proi).Y1-(*proi).Y0+1
              dz=(*proi).Z1-(*proi).Z0+1
              ;; select in widget list
              Widget_Control, rlist, SET_VALUE=listitems
              widget_control, rlist, SET_LIST_SELECT=selected
              ;;
              proi=roilist[selected]
              ;; update views and rulers
              ;; hide all rulers
              myrulers->HideRuler, 'X-Ruler'
              myrulers->HideRuler, 'Y-Ruler'
              myrulers->HideRuler, 'Z-Ruler'
              ;; update views
              IF (filterstack EQ 1) THEN BEGIN
                 ;;
                 (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
                 (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
                 ;;
              END
              (*view)[viewB]=REFORM(REFORM((*datap)[x0,*,*],DimY*DimZ,1,1))
              (*view)[viewA]=REFORM(REFORM((*datap)[*,y0,*],DimX*DimZ,1,1))
              TVDisplay
              ;; update rulers, note that the ruler in viewA changes
              myrulers->ModifyRuler, 'X-Ruler', X=x0, Y=dx
              myrulers->ShowRuler, 'X-Ruler'
              myrulers->ModifyRuler, 'Z-Ruler', X=z0, Y=dz
              myrulers->ShowRuler, 'Z-Ruler'
              myrulers->ModifyRuler, 'Y-Ruler', X=y0+DimX, Y=dy
              myrulers->ShowRuler, 'Y-Ruler'
              ;; update sliders
              Widget_Control, slider_x, SET_VALUE=x0
              Widget_Control, slider_dx, SET_VALUE=dx
              Widget_Control, slider_y, SET_VALUE=y0
              Widget_Control, slider_dy, SET_VALUE=dy
              Widget_Control, slider_z, SET_VALUE=z0
              Widget_Control, slider_dz, SET_VALUE=dz
           END  ELSE  BEGIN
              ErrMsg, "Bummer: can only delete when there is more than one region." 
           END
           ;;
        END
        rlist: BEGIN
           tmpselected=widget_info(rlist, /LIST_SELECT)
           ;;STOP
           IF ((tmpselected GE 0) AND (tmpselected NE selected)) THEN BEGIN
              selected=tmpselected
              proi=roilist[selected]
              ;; print, "Selected element in ROI pointer list: ", *proi
              x0=FIX(0.5*((*proi).X0+(*proi).X1))
              y0=FIX(0.5*((*proi).Y0+(*proi).Y1))
              z0=FIX(0.5*((*proi).Z0+(*proi).Z1))
              dx=(*proi).X1-(*proi).X0+1
              dy=(*proi).Y1-(*proi).Y0+1
              dz=(*proi).Z1-(*proi).Z0+1
              ;; update views and rulers
              ;; hide all rulers
              myrulers->HideRuler, 'X-Ruler'
              myrulers->HideRuler, 'Y-Ruler'
              myrulers->HideRuler, 'Z-Ruler'
              ;; update views
              IF (filterstack EQ 1) THEN BEGIN
                 ;;
                 (*view)[viewB]=REFORM(REFORM(ABS(ApplyFilter(filterB,(*datap)[x0,*,*])),DimY*DimZ,1,1))
                 (*view)[viewA]=REFORM(REFORM(ABS(ApplyFilter(filterA,(*datap)[*,y0,*])),DimX*DimZ,1,1))
                 ;;
              END
              (*view)[viewB]=REFORM(REFORM((*datap)[x0,*,*],DimY*DimZ,1,1))
              (*view)[viewA]=REFORM(REFORM((*datap)[*,y0,*],DimX*DimZ,1,1))
              TVDisplay
              ;; update rulers, note that the ruler in viewA changes
              myrulers->ModifyRuler, 'X-Ruler', X=x0, Y=dx
              myrulers->ShowRuler, 'X-Ruler'
              myrulers->ModifyRuler, 'Z-Ruler', X=z0, Y=dz
              myrulers->ShowRuler, 'Z-Ruler'
              myrulers->ModifyRuler, 'Y-Ruler', X=y0+DimX, Y=dy
              myrulers->ShowRuler, 'Y-Ruler'
              ;; update sliders
              Widget_Control, slider_x, SET_VALUE=x0
              Widget_Control, slider_dx, SET_VALUE=dx
              Widget_Control, slider_y, SET_VALUE=y0
              Widget_Control, slider_dy, SET_VALUE=dy
              Widget_Control, slider_z, SET_VALUE=z0
              Widget_Control, slider_dz, SET_VALUE=dz
           END
        END
        apply: BEGIN
           quit=1
           errstate=0
        END
        cancel: BEGIN
           quit=1
           errstate=1
           return, 0
        END
        ELSE: 
     END 
     ENDREP UNTIL (quit EQ 1)
     WIDGET_CONTROL, base, /DESTROY
  obj_destroy, myrulers
  printtocon, "% VolumePicker3D" 
  For i=1,roilist.Count() DO BEGIN
     printtocon, "   ROI #"+MyString(i-1)+" - "+StringROIStructure3D(roilist[i-1])
  END
  if filterstack then filter='y' ELSE filter='n'
  IF keyword_set(totable) THEN RoiListToTable, roilist
  return, roilist  
END
