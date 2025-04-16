PRO ExportDisplay, fname  
  ptr=GetRootP() ;; dexit
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ExportDisplay: Root pointer is invalid"
     ErrMsg, "% ExportDisplay: Root pointer is invalid"
     return
     END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ExportDisplay: Current stack pointer is invalid"
     ErrMsg, "% ExportDisplay: Current stack pointer is invalid"
     return
  END
  e=(*c).datap
  IF (PTR_VALID(e))THEN BEGIN
     index=(*e).slice
     binSzX=(*e).SzX
     binSzY=(*e).SzY
     binSzZ=(*e).SzZ
     IF ((*e).binning NE 1) THEN BEGIN
        binSzX=FLOOR(((*e).SzX)/(*e).binning)
        binSzY=FLOOR(((*e).SzY)/(*e).binning)
        binSzZ=FLOOR(((*e).SzZ)/(*e).binning)
        X1=((*e).binning*binSzX)-1
        Y1=((*e).binning*binSzY)-1
     END
     index=(*e).slice
     CASE 1 OF
           IN((*e).type,[1,2,3,4,5,12,13]): BEGIN
              IF ((*e).binning NE 1) THEN BEGIN
                 CASE (*e).zcoord OF
                    3: image=REBIN(REFORM(((*(*e).data)[0:X1,0:Y1,index])), binSzX,binSzY)
                    2: image=REBIN(REFORM(((*(*e).data)[0:X1,index,0:Z1])), binSzX,binSzZ)
                    1: image=REBIN(REFORM(((*(*e).data)[index,0:Y1,0:Z1])), binSzY,binSzZ)
                    ELSE:
                 ENDCASE
              ENDIF ELSE BEGIN
                 CASE (*e).zcoord OF
                    3: image=REFORM((*(*e).data)[*,*,index])
                    2: image=REFORM((*(*e).data)[*,index,*])
                    1: image=REFORM((*(*e).data)[index,*,*])
                    ELSE:
                 END
              ENDELSE
           END     
           IN((*e).type,[6,9]): BEGIN
              IF ((*e).binning NE 1) THEN BEGIN
                 CASE (*e).zcoord OF
                    3: image=REBIN(ABS(REFORM((*(*e).data)[0:X1,0:Y1,index])), binSzX,binSzY)
                    2: image=REBIN(ABS(REFORM((*(*e).data)[0:X1,index,0:Z1])), binSzX, binSzZ)
                    1: image=REBIN(ABS(REFORM((*(*e).data)[index,0:Y1,0:Z1])), binSzY,binSzZ)
                    ELSE:
                 ENDCASE
                 
              ENDIF  ELSE BEGIN
                 CASE (*e).zcoord OF
                    3: image=REFORM(ABS((*(*e).data)[*,*,index]))
                    2: image=REFORM(ABS((*(*e).data)[*,index,*]))
                    1: image=REFORM(ABS((*(*e).data)[index,*,*]))
                    ELSE:
                 END
              END
           END         
           ELSE: printtocon, "% ExportDisplay: error - unknown image type"
        ENDCASE
  END
  
  hilo=GetStackContrastValues() ;; Take the data from the ContrastInspector values.
 
  
  ;; Color model:
  ;; 8-bit or indexed color system
  ;; or 24 bit color system?
  mydevice = !D.NAME
  ;; analyse color settings
  DEVICE, GET_DECOMPOSED=decomposed
  DEVICE, GET_VISUAL_DEPTH=vd
  ;; Get color definition
  IF vd GT 8 THEN BEGIN
    TVLCT, r, g, b, /Get
    
 ENDIF ELSE BEGIN
    
    TVLCT, r, g, b, /Get
 ENDELSE
;;
N=Size(image)
SzX=N[1] & SzY=N[2]
mypalette = Obj_New('IDLgrPalette', r, g, b)
  view=OBJ_NEW('IDLgrView',viewplane_rect=[0,0,SzX,SzY])
  model=obj_new('IDLgrModel')
  view->add, model
  imageobj=obj_new('IDLgrImage', BYTSCL(image, MAX=hilo[1], Min=hilo[0], TOP=253)+1, Palette=mypalette)
  model->add, imageobj
  window=OBJ_NEW('IDLgrWindow',dimensions=[SzX,SzY])
  ;; plot into buffer and take a screenshot
   buffer = Obj_New('IDLgrBuffer', dimensions=[SzX,SzY])
   IF PTR_VALID((*e).ScaleBar) THEN BEGIN
      IF NOT(keyword_set(charsize)) THEN charsize=ROUND(0.04*SzY)
      IF (charsize LT 10) THEN charsize=10
      myfont = OBJ_NEW('IDLgrFont', GetFont(), SIZE=charsize)
      mytext = OBJ_NEW('IDLgrText', (*(*e).ScaleBar).label, LOCATION=[0,0], COLOR=(*(*e).ScaleBar).color, FONT=myfont)
     ;; COLOR=!D.Table_Size-1)
      mytext -> SetProperty, ENABLE_FORMATTING=1
      model -> Add, mytext
      ;;window->draw, view
      textdim=buffer.GetTextDimensions(mytext)
      pos=ScaleMarkPos(SzX,SzY,(*e).binning,(*(*e).ScaleBar).Size,textdim[0], textdim[1], VERTGAP=1.3)
      ;;pos.xbar=pos.xbar/(*e).binning
      ;;pos.ybar=pos.ybar/(*e).binning
     mytext -> SetProperty, LOCATION=[pos.xlab,pos.ylab]
     myline = OBJ_NEW('IDLgrPlot', [pos.xbar,pos.xbar+(*(*e).ScaleBar).Size/(*e).binning],[pos.ybar,pos.ybar], LINESTYLE=0, COLOR=(*(*e).ScaleBar).color)
     myline -> SetProperty, THICK=ROUND(4*charsize/24)
     model->Add, myline
     ;; window -> draw, view
  END
  buffer -> draw, view
  buffer -> GetProperty, Image_Data=snap
  IF PTR_VALID((*e).ScaleBar) THEN obj_destroy, [window,view,buffer,myfont,myline,mytext,mypalette,model,imageobj] ELSE  obj_destroy, [window,view,buffer,mypalette,model,imageobj]
  printtocon, "% ExportDisplay: Writing color png to "+fname
  printtocon, "%                ... "
  WRITE_PNG, fname, snap, r, g, b
  printtocon, "%                done "
  ;; obj_destroy,
  ;; [view,buffer,myfont,myline,mytext,palette,model,image]
  SET_PLOT, mydevice 
END

PRO ReadTvipsData, fname, swap
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadTvipsData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END
  ptr=GetRootP() ;; datalistpointer
  names=DataList_GetNames(ptr)
  Selected=WHERE(names EQ fname[0])
  IF (Selected EQ -1) THEN BEGIN
     e=DataList_CreateElement(ptr, fname[0])
  END ELSE BEGIN
     i=2
     REPEAT BEGIN
       altname=fname[0]+ "" + MyString(i)
       Selected=WHERE(names EQ altname)
       i=i+1
     END UNTIL (Selected EQ -1)
     e=DataList_CreateElement(ptr, altname)
  END
  IF PTR_VALID(e) THEN BEGIN
     x=0 & y=0 & z=0 & type=4
     print, read_tvips_stack(e, fname, x, y, z, dtype, SWAPENDIAN=swap)
     IF (PTR_VALID((*e).data)) THEN BEGIN
        (*e).SzX=x & (*e).SzY=y & (*e).SzZ=z
        (*e).id=fname[0]
        (*e).type=type
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        CreateWindow
        TVDisplay
        Update_XTabControl
     End
  END 
END  

PRO FileImport
  IF NOT(GetDebugFlag()) THEN BEGIN
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% FileImport:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
  END
 END
      swapend=0
      crgrpd=1
      type=0
      filter="*"
      tlist=filetypes(/READ,/PTRARRAY) ;; see FileIdent.pro
      ;;; ftypes=PTR_NEW(["EMPAD","HDF5","MRC","Raw Binary
      ;;; 3D-Data","Multiple Image TIFF","PIF"])
      suffixident=1B
            hlp=["", $
           "Select data file(s) to open", $
           " ",$
           "Browse through directories and select file(s) from a single subdirectory.", $
           "Select files by left mouse click. Select multiple files by holding down SHIFT or CTRL.", $
           "The DESKTOP BUTTON takes you to your desktop directory that you can configure under", $
           "File -> Settings. The CYCLE BUTTON lets you cycle through the history of the ten most", $
           "recently visited directories. The HOME BUTTON takes you to the empadTools installation directory.", $
           "The FILTER INPUT can be used to filter the results of the directory listing. Enter a filter", $
           "string an press Enter to update the listing. Checking the REVERSE BUTTON will put the directory", $
           "listing into reverse order.", $
           "Choose the FILE TYPE from the droplist below the directory listing. File types should be auto-detected", $
           "according to the suffix of the first file in the selection, if the checkbox SUGGEST TYPE is", $
           "active. You can always overwrite the (auto-detected) type by choosing the appropriate type from", $
           "the droplist.", $
           "Checking SWAP ENDIAN will force to change the byte order upon image loading, which is useful in case", $
           "you encounter an endian conflict. This applies for dat types where auto-detection is not supported.",$
           "Make sure that - before pressing on open - a file selection is displayed in SELECTION.", $ 
      ""]
            phlp=PTR_NEW(hlp)
            cancelled=0
      files = rmd_pickfile( $
              ;;group_leader = event.top,     
              filter_in = filter,              $
              path = GetWorkingDir(),         $
              get_path = out_path,          $
              cancelled = cancelled,        $
              swapen = swapend,             $
              suffixident = suffixident,             $
              type = type,             $
              ftypes = tlist,     $
              crgrp = crgrpd,             $
              help=phlp, $
              /multiple, $
              /open)
      if NOT(cancelled EQ 1) then begin
          IF (n_elements(files) EQ 0) THEN BEGIN
             printtocon, 'No files selected'
             return
          END
          SetWorkingDir, out_path
;;  now decide which type
          type=(*tlist)[type]
          printtocon, ["% FileImport: Selected file ","   "+files[0]]
          CASE type OF
             "PadTools Data": BEGIN ;; padtools header and mrc  data
                Read_ptd, files[0] 
             END
             "PadTools HDF5": BEGIN ;; hdf5 data
                Read_hdf5,files[0], VERBOSE=verbose, DEBUG=debug
             END
             "EMPAD": BEGIN ;; tvips images
                ReadEMPADData, files[0], SWAPENDIAN=swapend
             END
             "DECTRIS": BEGIN
                 ReadDectrisData, files[0], SWAPENDIAN=swapend
             END
             "TVIPS": BEGIN ;; tvips images
                ReadTvipsData, files, swapend
             END
             "Raw Binary 3D-Data": BEGIN  ;; raw 3D data
                ReadRaw3DData, files[0], swapend
             END
             "PIF": BEGIN  ;; raw 3D data
                dummy=ReadPIFData(files[0])
             END
             "Multiple Image TIFF": BEGIN
                ReadTiffData, files[0]
             END 
             "MRC": BEGIN
                ReadMRCData, files[0], /FEI
             END
             "Digital Micrograph Image": BEGIN
                ImportDM, files[0]
             END
             ELSE: 
          ;;print, "Swap Endian = ", swapend
          ;;print, "Type = ", type
          ;;print, "Filter = ", filter
          END 
       endif  ELSE printtocon, 'cancelled'
   end 

PRO ExportBitmap, DISPLAY=display
IF NOT(GetDebugFlag()) THEN BEGIN
  CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ExportBitmap  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
  END
 END
  swapend=0
  crgrpd=1
  type=0
  filter="*"
  ftypes=PTR_NEW(["PNG"])
  files = rmd_pickfile(  $
;;  group_leader = event.top,     $
          filter_in = filter,              $
          path=GetWorkingDir(), $
          get_path = out_path,          $
          cancelled = cancelled,        $
          swapen = swapend,             $
          type = type,             $
          ftypes = ftypes,     $
          crgrp = crgrpd,             $
          /multiple, $
          /save)
  if NOT(cancelled EQ 1) then begin
     if (n_elements(files) GE 1) THEN BEGIN
        SetWorkingDir, out_path
        fname=files[0]
        type=(*ftypes)[type]
          CASE type OF
             "PNG": BEGIN
                if keyword_set(display) THEN ExportDisplay, fname ELSE ExportDisplay, fname
             END 
             ELSE: BEGIN
                printtocon, "% ExportBitmap: export format unknown"
                ErrMsg, "% ExportBitmap: export format unknown."
             END
          ;;print, "Swap Endian = ", swapend
          ;;print, "Type = ", type
          ;;print, "Filter = ", filter
          END 
        
     end
;;     print, "Swap Endian = ", swapend
;;          print, "Create Group = ", crgrpd
;;     print, "Type = ", type
;;     print, "Filter = ", filter
  endif ELSE printtocon, '% FileExport: cancelled'
end



PRO FileExport, AUTOSAVE=autosave, GETFILENAME=getfilename
  IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% FileExport:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
  END
 END
  swapend=0
  crgrpd=1
  type=0
  filter="*"
  guessname=GuessFileName(GetCurrentP())
  ftypes=PTR_NEW(["PadTools Data","HDF5","MRC 3D-Data", "MRC 3D-Data (FEI Readable Header)", "Raw Binary 3D-Data","Multiple Image TIFF File"])
  files = rmd_pickfile(  $
;;  group_leader = event.top,     $
          filter_in = filter,              $
          path=GetWorkingDir(), $
          get_path = out_path,          $
          cancelled = cancelled,        $
          swapen = swapend,             $
          type = type,             $
          ftypes = ftypes,     $
          crgrp = crgrpd,             $
          /multiple, $
          /save)
  if NOT(cancelled EQ 1) then begin
     if (n_elements(files) GE 1) THEN BEGIN
        SetWorkingDir, out_path
        fname=files[0]
        if keyword_set(getfilename) THEN getfilename=files[0]
        type=(*ftypes)[type]
        CASE type OF
           "HDF5": BEGIN
              printtocon, "% FileExport: export to HDF5" 
              dummy=Write_hdf5(GetCurrentP(), files[0])
           END
           "PadTools Data": BEGIN
              printtocon, "% FileExport: export to PadToolsData"
              dummy=Write_ptd( GetCurrentP(),  files[0])
           END
             "MRC 3D-Data": BEGIN
                SaveBinData, files[0], FORMAT="mrc", AUTOSAVE=autosave
             END 
             "MRC 3D-Data (FEI Readable Header)": BEGIN
                SaveBinData, files[0], FORMAT="feimrc", AUTOSAVE=autosave
             END 
             "Raw Binary 3D-Data": BEGIN  ;; raw 3D data
                SaveBinData, files[0], FORMAT="raw", AUTOSAVE=autosave
             END
             "Multiple Image TIFF File": BEGIN ;; multiple image tiff export
                SaveMTiffData, files[0], AUTOSAVE=autosave
             END
             "Tvips Images": BEGIN ;; tvips images
                printtocon, "% FileExport: export format not yet available" 
                ErrMsg, "% FileExport: export format not yet available, use " + $
                "MRC format and convert with iMtools."              
             END
             ELSE: BEGIN
                printtocon, "% FileExport: export format unknown"
                ErrMsg, "% FileExport: export format unknown."
             END
          ;;print, "Swap Endian = ", swapend
          ;;print, "Type = ", type
          ;;print, "Filter = ", filter
          END 
        
     end
;;     print, "Swap Endian = ", swapend
;;          print, "Create Group = ", crgrpd
;;     print, "Type = ", type
;;     print, "Filter = ", filter
  endif ELSE printtocon, '% FileExport: cancelled'
  
end

PRO FileExport_NonInteractive
  ptr=GetRootP() ;; datalistpointer
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% SaveBinData: Root pointer is invalid"
     ErrMsg, "% SaveBinData: Root pointer is invalid"
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% SaveBinData: Current stack pointer is invalid"
     ErrMsg, "% SaveBinData: Current stack pointer is invalid"
     return
  END
  fname=(*c).name
   IF (FileIsWriteable(fname, /OVERWRITE) EQ 1) THEN SaveBinData, fname, FORMAT="mrc", /OVERWRITE
end


PRO Read_PadProp, fname
  ptr=GetRootP() ;; datalistpointer
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% Read_PadProp: Root pointer is invalid"
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% Read_PadProp: Current stack pointer is invalid"
     return
  END
  e=(*c).datap
  IF PTR_VALID(e) THEN BEGIN
     o=(*e).extra
     IF OBJ_VALID(o) THEN BEGIN
        ;; open file
        ;;
     ;; open file to write header
     ;;
     openr, LUN, fname, ERROR = o_err, /GET_LUN
     if (o_err NE 0 ) then begin
        printtocon, "% Read_PadProp: Error while trying to open file " + fname+ "," + !ERR_STRING
        return
     endif
     s=''
     readf, LUN, s
     close, LUN
     free_lun, LUN
     res=o.JSONToProps(s,/SETSHAPE) 
  END ELSE BEGIN
     printtocon, "% Read_PadProp: Data pointerPAD object is invalid, not PAD data?"
     return
  END 
END  ELSE BEGIN
   printtocon, "% Read_PadProp: Data pointer is invalid"
   return
END 
END 

PRO Write_PadProp, fname
ptr=GetRootP() ;; datalistpointer
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% Write_PadProp: Root pointer is invalid"
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% Write_PadProp: Current stack pointer is invalid"
     return
  END
  e=(*c).datap
  IF PTR_VALID(e) THEN BEGIN
     o=(*e).extra
     IF OBJ_VALID(o) THEN BEGIN
        ;; open file
        ;;
     ;; open file to write header
     ;;
     openw, LUN, fname, ERROR = o_err, /GET_LUN
     if (o_err NE 0 ) then begin
        printtocon, "% Write_PadProp: Error while trying to open file " + fname+ "," + !ERR_STRING
        return
     endif
     s=o.PropsToJSON()
     printf, LUN, s
     close, LUN
     free_lun, LUN
     printtocon, "% Write_PadProp: Wrote properties to file " + fname+ "."
  END ELSE BEGIN
     printtocon, "% Write_PadProp: Data pointerPAD object is invalid, not PAD data?"
     return
  END 
END  ELSE BEGIN
   printtocon, "% Write_PadProp: Data pointer is invalid"
   return
END  
END 



PRO PropFileIO, Export=export
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% PropFileImport:  Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
  swapend=0
  crgrpd=1
  type=0
  filter="*.json"
  tlist=PTR_NEW(["Pad Properties"])
  suffixident=0B
  cancelled=0
  IF not(keyword_set(export)) THEN BEGIN
     hlp=["", $
          "Select a properties file", $
          " ",$
          "Browse through directories and select a json properties file from a single subdirectory.", $
          "The DESKTOP BUTTON takes you to your desktop directory that you can configure under", $
          "File -> Settings. The CYCLE BUTTON lets you cycle through the history of the ten most", $
          "recently visited directories. The HOME BUTTON takes you to the PadTools installation directory.", $
          "The FILTER INPUT can be used to filter the results of the directory listing. Checking the", $
          " REVERSE BUTTON will put the directory listing into reverse order.", $         
          ""]
     phlp=PTR_NEW(hlp)
     
     files = rmd_pickfile( $
             ;;group_leader = event.top,     
             filter_in = filter,              $
             path = GetWorkingDir(),         $
             get_path = out_path,          $
             cancelled = cancelled,        $
             swapen = swapend,             $
             suffixident = suffixident,             $
             type = type,             $
             ftypes = tlist,     $
             crgrp = crgrpd,             $
             help=phlp, $
             /open)
  END ELSE BEGIN
     hlp=["", $
          "Select a directory and filename to write a properties file", $
          " ",$
          "The DESKTOP BUTTON takes you to your desktop directory that you can configure under", $
          "File -> Settings. The CYCLE BUTTON lets you cycle through the history of the ten most", $
          "recently visited directories. The HOME BUTTON takes you to the PadTools installation directory.", $
          "The FILTER INPUT can be used to filter the results of the directory listing. Checking the", $
          " REVERSE BUTTON will put the directory listing into reverse order.", $         
          ""]
     phlp=PTR_NEW(hlp)
     
     files = rmd_pickfile( $
             ;;group_leader = event.top,     
             filter_in = filter,              $
             path = GetWorkingDir(),         $
             get_path = out_path,          $
             cancelled = cancelled,        $
             swapen = swapend,             $
             suffixident = suffixident,             $
             type = type,             $
             ftypes = tlist,     $
             crgrp = crgrpd,             $
             help=phlp, $
             /save)
  END
  if NOT(cancelled EQ 1) then begin
     IF (n_elements(files) EQ 0) THEN BEGIN
        printtocon, 'No files selected'
        return
     END
     ;; SetWorkingDir, out_path
;;  now decide which type
     type=(*tlist)[type]
     CASE type OF
        "Pad Properties": BEGIN ;; hdf5 data
           IF keyword_set(export) THEN Write_PadProp, files[0] ELSE Read_PadProp, files[0]
        END
        ELSE: 
        ;;print, "Swap Endian = ", swapend
        ;;print, "Type = ", type
        ;;print, "Filter = ", filter
     END 
  endif  ELSE printtocon, 'cancelled'
end   
