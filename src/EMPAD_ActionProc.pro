FUNCTION BRESENHAM, PA, PB
; Bresenham Linien-Algorithmus
; gibt ein Array mit den Punktkoordinaten einer Geraden zwischen
; PA und PB  zurueck
   DX = PB.X-PA.X
   DY = PB.Y-PA.Y
   Pos_SLOPE = (DX GT 0)
   k = 0
   IF (DY LT 0) THEN Pos_Slope = NOT Pos_Slope
   IF (ABS(DX) GE ABS(DY)) THEN BEGIN
      A = Replicate({IntIntPoint, X:0, Y :0},ABS(DX)+1)
      IF (DX GT 0) THEN BEGIN
         C = PA.X
         R = PA.Y
         F = PB.X
      ENDIF ELSE BEGIN
         C = PB.X
         R = PB.Y
         F = PA.X
      ENDELSE
      INC1 = 2*ABS(DY)
      INC2 = 2*(ABS(DY)-ABS(DX))
      G = 2*ABS(DY)-ABS(DX)
      IF Pos_Slope THEN BEGIN
         WHILE (C LE F) DO BEGIN
            A(k).X = C
            A(k).Y = R
            k = k + 1
            C = C+1
            IF (G GE 0) THEN BEGIN
               R = R + 1
               G = G + INC2
            ENDIF ELSE G = G + INC1
         ENDWHILE
      ENDIF ELSE BEGIN
         WHILE (C LE F) DO BEGIN
            A(k).X = C
            A(k).Y = R
            k = k + 1
            C = C+1
            IF (G GE 0) THEN BEGIN
               R = R - 1
               G = G + INC2
            ENDIF ELSE G = G + INC1
         ENDWHILE
      ENDELSE
   ENDIF ELSE BEGIN
      B = BRESENHAM({IntIntPoint, X:PA.Y, Y:PA.X},{IntIntPoint, X:PB.Y, Y:PB.X} )
      A = Replicate({IntIntPoint, X:0, Y :0},ABS(DY)+1)
      A(*).X = B(*).Y
      A(*).Y = B(*).X
    ENDELSE
   return, A   
END 


FUNCTION DisplayROIDiff, FUNCARGS=funcargs, ROI=roi, STORE=store, AUTOFILESAVE=autofilesave, LOG=log, SCALEBAR=scalebar, ROTEMPAD=rotempad
  ;; funcargs is a hash that contains all the necessary information
  ;; for the function
  ;; funcargs["EMPADObj"] is the object pointer
  ;; funcargs["outputp"] is the preview image pointer
  ;; funcargs["capturep"] is the capture frame for ROI selection
  ;; funcargs["cumulativemaskp"] is the cumulative mask for all stored selections
  ;; convention: roi=[x1,y1,x2,y2]
  ;; store: if set then a copy of the output will be stored
  IF NOT(GetDebugFlag()) THEN BEGIN 
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% XActionBoxDisplayROIDiff:    Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END
  ;;print, "called  DisplayROIDiff: - ", roi
  ;;IF keyword_set(store) THEN print, "store set"
  ;;IF keyword_set(autofilesave) THEN print, "autosave set"
  ;;IF keyword_set(log) THEN print, "log set"
  self=funcargs["EMPADObj"] ;; the EMPAD object
  
  IF NOT(OBJ_VALID(self)) THEN BEGIN
     printtocon, "%DisplayROIDiff: EMPADOBJ not defined. Returning."
     return, 0
  END
     p=self.GETDATAPOINTER()
  IF NOT(PTR_VALID(p)) THEN BEGIN
     printtocon, "%DisplayROIDiff: EMPADOBJ data pointer is not defined. Returning."
     return, 0
  END
  ;; analyze the data array size
  N=SIZE(*p)
  ;; N= 3         128         130       65536
  ;; extract slices 
  ;; EMPAD data is stored in a linear sequence of diffraction frames
;;
;; DimX, DimY: dimensions of a single diffraction pattern
;; DimX * DimY * 4: Size of a diffraction pattern in bytes
;;
  DimX=LONG(N[1]) & DimY=LONG(N[2])
  wX=roi[2]-roi[0]+1 & wY=roi[3]-roi[1]+1
  DimZ=LONG(wX)*LONG(wY) ;; number of the diffraction patterns that will be extractd
  offset=ULONG64(0)
  APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
  LinLen=ULONG64(DimX*DimY*4) ;; length of one diffraction pattern, 128 x 130 x 4byte
  z0=0
  offset=LONG(roi[0]) ;; match z coordinate of diffraction pattern
  scansizex=256L & scansizey=256L
  scansizex=self.GetScanSize(/X)
  scansizey=self.GetScanSize(/Y)
  norm=1./DimZ
  For i=0L,wY-1 DO BEGIN  
     ;; i_data = ASSOC(LUN, LONARR(DimX,DimY,wX),offset)
     offset=(roi[1]+i)*scansizex+roi[0]
     (*(APtr))[*,*,z0:z0+wX-1] = norm*REFORM((*p)[*,*,offset:(offset+wX-1)])
     z0 = z0+wx
  END
  proj=PTR_NEW(Total((*APtr),3))
  ;; STOP
  IF keyword_set(log) THEN BEGIN
     ScalarArithmetics, proj, /LOG
  END
  IF keyword_set(rotempad) THEN BEGIN 
     tmp=(*proj)
     tmp=REVERSE(tmp,1) ;; flip x
     tmp=ROTATE(tmp,3)  ;; rotate -90 degrees (or +270 degrees)
     (*proj)[*,*]=0
     frameX=self.GetFrameSize(/X)
     frameY=self.GetFrameSize(/Y)
     max=Min([frameX,frameY])-1
     (*proj)[*,*]=0
     (*proj)[0:max,0:max]=tmp[0:max,0:max]
  END
  o=funcargs["outputp"]
  captp=funcargs["capturep"]
  maskp=funcargs["cumulativemaskp"]
  if keyword_set(scalebar) THEN BEGIN
     d=(*o).datap
     IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
        (*(*d).ScaleBar).color = !D.N_COLORS-1
     END ELSE BEGIN
        (*d).DisplayScaleBar=1
        IF PTR_VALID((*d).ScaleBar) THEN PTR_FREE, (*d).ScaleBar 
        (*d).ScaleBar=PTR_NEW(GetScaleBar(((*d).SzX),((*d).xsamp), UNIT=((*d).xunit)))
     END
  END ELSE BEGIN
     d=(*o).datap
     IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
           (*d).DisplayScaleBar=0
           ;; get rid of object
           PTR_FREE, (*d).ScaleBar
        END    
  END
  ;; update preview and set output window
  (*(*o).datap).data=proj
  Wdow=(*(*o).datap).Window
  WSET, wdow
  ERASE
  ;; autocontrast on ROI DIFF according to conrtrast settings in
  ;; preview image under pointer o
  TVDisplay
  ;;hilo=GetAutoContrastValues()
  hilo=XTabContrastDialogGetContrastRange()
  IF keyword_set(store) THEN BEGIN
     IF PTR_VALID(maskp) THEN BEGIN
        ;; update cumulative selection mask
        (*(*(*maskp).datap).data)[roi[0]:roi[2],roi[1]:roi[3]]=1      
     END
     ;; make an image with the marker selection 
     origw=!D.WINDOW
     myp=funcargs["capturep"]
     myp_=(*myp).datap
     im=BYTSCL(*((*(myp_)).data))
     im[roi[0],roi[1]:roi[3]]= (im[roi[0],roi[1]:roi[3]] XOR 255)
     im[roi[2],roi[1]:roi[3]]= im[roi[2],roi[1]:roi[3]] XOR 255
     im[roi[0]:roi[2],roi[1]]= im[roi[0]:roi[2],roi[1]] XOR 255
     im[roi[0]:roi[2],roi[3]]= im[roi[0]:roi[2],roi[3]] XOR 255
     filename=self.GetSetName()
     theDirectory=" " & theExtension=" "
     thefile=FSC_Base_Filename(filename, Directory=theDirectory, Extension=theExtension)
     name=theDirectory+thefile+"_ROI_"+Mystring(roi[0])+"-"+Mystring(roi[2])+"_"+Mystring(roi[1])+"-"+Mystring(roi[3])
     ;; show and optionally export the image with the roi marker
     ThrowImage, im,  TITLE=name,BIN=0.5, SAMP=[self.GetScanSampling(/X),self.GetScanSampling(/Y),1.], UNIT=['nm','nm','nm']
     ;; set contrast to min-max
      SetStackContrastMode, /AUTO, /MINMAX, /FULL 
     TVDisplay ;; this should autoscale the contrast and set the stack contrast values
     IF keyword_set(autofilesave) THEN BEGIN
        printtocon, "% FileExport: HDF5 export to "+ name+".h5" 
        dummy=Write_hdf5(GetCurrentP(), name+".h5")
        printtocon, "%             PNG export to "+ name+".png" 
        ExportDisplay, name+".png"
     END
     WSET, origw
     name=theDirectory+thefile+"_ROIDiff_"+Mystring(roi[0])+"-"+Mystring(roi[2])+"_"+Mystring(roi[1])+"-"+Mystring(roi[3])
     ThrowImage, *proj,TITLE=name, BIN=0.5, SAMP=[self.GetDetectorSampling(/X),self.GetDetectorSampling(/Y),1.], UNIT=['1/nm','1/nm','nm']
     saveobj=GetCurrentP()
     ;; define contrastmode
     d=(*saveobj).datap
     (*d).contrastmode=(*(*o).datap).contrastmode
     (*d).contrastsubmode=(*(*o).datap).contrastsubmode
     (*d).contrastscaling=(*(*o).datap).contrastscaling
     (*d).contrastroi=(*(*o).datap).contrastroi
     (*d).contrastsdev=(*(*o).datap).contrastsdev
     
     (*d).contrast=hilo
     
     IF PTR_VALID((*(*o).datap).ScaleBar) THEN BEGIN
        (*d).displayscalebar=1
       (*d).ScaleBar=PTR_NEW(GetScaleBar((*d).SzX,(*d).xsamp, UNIT=(*d).xunit)) 
     END
     ;; IF keyword_set(log) THEN (*(*saveobj).datap).contrastroi="quarter" ELSE (*(*saveobj).datap).contrastroi="diff"
     WSET, (*(*saveobj).datap).Window
  ;; autocontrast on ROI DIFF
     TVDisplay
     s=["Region of interest diffraction", "Parent data: "+self.GetSetName(), "ROI: "+MyString(roi[0])+":"+MyString(roi[2])+","+MyString(roi[1])+":"+MyString(roi[3]),"Number of pixels: "+MyString(DimZ), "Mode: "+"Average"]
     ;;STOP
     ;; Optionally export the image
     PrintToNote, (*saveobj).datap, s, /TIMESTAMP
     IF keyword_set(autofilesave) THEN BEGIN
        printtocon, "% FileExport: HDF5 export to "+ name+".h5" 
        dummy=Write_hdf5(GetCurrentP(), name+".h5")
        printtocon, "%             PNG export to "+ name+".png"
        ExportDisplay, name+".png"
     END
     ;; Capture the ROI selection
     ;;
     ;;currentwdow=!D.window
     ;;Capture, captp     
     ;;WSET, currentwdow
;;
;; now we need to restore the preview data pointer as the current
     ;; image pointer
     o_as_it_was=DataList_SearchName(GetRootP(), (*o).name)
     Update_XTabControl
  END
  
END


FUNCTION DisplayROILineDiff, FUNCARGS=funcargs, ROI=roi, LOG=log,  SCALEBAR=scalebar, ROTEMPAD=rotempad
  ;; funcargs is a hash that contains all the necessary information
  ;; for the function
  ;; funcargs["EMPADObj"] is the object pointer
  ;; funcargs["outputp"] is the pointer to the output data stack
  ;; funcargs["last"] is a pointer to a four-element array that holds
  ;; the first point
  ;; the last roi
  ;; convention: roi=[x1,y1,x2,y2]
  IF NOT(GetDebugFlag()) THEN BEGIN 
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% DisplayROILineDiff:    Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        return, 0
     END
  END 
  self=funcargs["EMPADObj"]
  IF NOT(OBJ_VALID(self)) THEN BEGIN
     printtocon, "% DisplayROILineDiff: EMPADOBJ not defined. Returning."
     return, 0
  END
  o=funcargs["outputp"]
  IF NOT(PTR_VALID(o)) THEN BEGIN
     printtocon, "% DisplayROILineDiff: Output data stack not defined. Returning."
     return, 0
  END 
  p=self.GETDATAPOINTER()
  IF NOT(PTR_VALID(p)) THEN BEGIN
     printtocon, "%DisplayROIDiff: EMPADOBJ data pointer is not defined. Returning."
     return, 0
  END
  plast=funcargs["last"]
;;  IF NOT(PTR_VALID(plast)) THEN BEGIN
;;     printtocon, "% DisplayROILineDiff: Setting starting point."
;;     printtocon, "     (x1,y1,x2,y2)=("+ MySTRING(roi[0])+","+ MySTRING(roi[1])+","+ MySTRING(roi[2])+","+ MySTRING(roi[3])+")"
;;     copyroi=roi
;;     funcargs["last"]=PTR_NEW(copyroi)
;;     return, 0
;;  END
  ;; Now calculate line positions
  ;; use bresenham algorithm
  ;;
  printtocon, "% DisplayROILineDiff: starting and end point."
  printtocon, "     (x1,y1,x2,y2)=("+ MySTRING(roi[0])+","+ MySTRING(roi[1])+","+ MySTRING(roi[2])+","+ MySTRING(roi[3])+")"
  ;;PA = {IntIntPoint, X:FIX(((*plast)[0]+(*plast)[2])/2), Y:FIX(((*plast)[1]+(*plast)[3])/2)}
  ;;PB = {IntIntPoint, X:FIX((roi[0]+roi[2])/2), Y:FIX((roi[1]+roi[3])/2)}
  PA = {IntIntPoint, X:FIX(roi[0]), Y:FIX(roi[1])}
  PB = {IntIntPoint, X:FIX(roi[2]), Y:FIX(roi[3])}
  A=Bresenham(PA,PB)
  NumPix=N_Elements(A)
  printtocon, "% DisplayROILineDiff: Number of scan pixels on the profile= "+ STRING(NumPix)
  ;;
  o=funcargs["outputp"]
  ;; get rid of old data
  PTR_FREE, ((*(*o).datap).data)
  ;; and create a new stack
  ;; analyze the data array size
  N=SIZE(*p)
  ;; N= 3         128         130       65536
  ;; extract slices 
  ;; EMPAD data is stored in a linear sequence of diffraction frames
;;
;; DimX, DimY: dimensions of a single diffraction pattern
;; DimX * DimY * 4: Size of a diffraction pattern in bytes
;;
  DimX=LONG(N[1]) & DimY=LONG(N[2])
  PResultArray=PTR_NEW(FLTARR(DimX,DimY,NumPix))
  (*(*o).datap).data=PResultArray
  (*(*o).datap).SzZ=NumPix
  ;;
  ;; Loop over data points
  ;;
  wX=roi[2]-roi[0]+1 & wY=roi[3]-roi[1]+1 ;; memorize box width
  scansizex=256L & scansizey=256L
  scansizex=self.GetScanSize(/X)
  scansizey=self.GetScanSize(/Y)
  DimZ=NumPix ;; number of the diffraction patterns that will be extractd
  offset=ULONG64(0)
  ;; APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
  ;; LinLen=ULONG64(DimX*DimY*4) ;; length of one diffraction pattern, 128 x 130 x 4byte
  For j=0,NumPix-1 Do BEGIN
     ;;roi[0]=A[j].X-wX/2
     ;;roi[2]=roi[0]+wX-1
     ;;roi[1]=A[j].Y-wY/2
     ;;roi[3]=roi[0]+wY-1
     offset=A[j].Y*scansizex+A[j].X
     (*PResultArray)[*,*,j]=REFORM((*p)[*,*,offset])
     ;; z0=0
     ;; offset=LONG(roi[0]) ;; match z coordinate of diffraction pattern
     ;; norm=1./DimZ
     ;;For i=0L,wY-1 DO BEGIN  
        ;; i_data = ASSOC(LUN, LONARR(DimX,DimY,wX),offset)
        ;;offset=(roi[1]+i)*scansizex+roi[0]
        ;;(*(APtr))[*,*,z0:z0+wX-1] = norm*REFORM((*p)[*,*,offset:(offset+wX-1)])
       ;; z0 = z0+wx
     ;;END
     ;;proj=Total((*APtr),3)
     ;; (*PResultArray)[*,*,j]=proj
  END

  printtocon, "% DisplayROILineDiff: Finished. "
  o_as_it_was=DataList_SearchName(GetRootP(), (*o).name)
  Update_XTabControl

  WSET, (*(*o).datap).Window
  ;; autocontrast
  TVDisplay
END


