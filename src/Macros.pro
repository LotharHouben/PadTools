FUNCTION DirGetBigFileList, BASEPATH=basepath, FILTER=filter, HLP=hlp
  if not(keyword_set(basepath)) THEN BEGIN
     IF NOT(keyword_set(hlp)) THEN  hlp=PTR_NEW(["",$
          "Select a top directory for file selection", $
          "",$
          "The directories will be parsed for files matching the filter selection." , $
          "The assumption is that each of these files refers to a series dataset.", $
          "" $
                                                ])
    ;;  res=dialog_pickfile(PATH=GetWorkingDir(),/DIRECTORY,title = "Select a directory", get_path = wdir )
     res=rmd_pickfile(  PATH=GetWorkingDir(), $
                        get_path = wdir,          $
                        title = "Select a directory",          $
                        help=hlp, $
    ;;                    filter_in=filter, $
    ;;                    filter_out=filter, $
                        /DIREC,$
                        cancelled = cancelled)
     PTR_FREE, hlp
     if (cancelled  EQ 1) THEN return, hash() ELSE basepath=wdir
  END
  IF NOT(keyword_set(filter)) THEN BEGIN
     filter='*.*'
     IF (XStringField(filter,"Pattern", XSIZE=32, TITLE="File search pattern") NE 1) THEN BEGIN
        printtocon,'% DirGetBigFileList: User cancelled.'
        return, hash("dir",basepath,"files",list() )
     END
  END
  printtocon, "% DirGetBigFileList: "+ MyString(basepath)
  res=File_Search(basepath, filter)
  l=list()
  IF ((N_Elements(res) GE 1) AND res[0] NE "") THEN BEGIN
     For i=0,N_Elements(res)-1 Do BEGIN
        SplitPath, res[i], direct, filename
        l.Add, filename
     END
     printtocon, "%   "+ MyString(l.Count()) + " files match the pattern "+MyString(filter)
     printtocon, "%    1st: "+ MyString(l[0])
     printtocon, "%   last: "+ MyString(l[-1]) 
  END
  printtocon, "%   No files match the pattern "+MyString(filter)
  return, hash("dir",basepath,"files",l)
END   


FUNCTION ProcessingDirList, BASEPATH=basepath, FILTER=filter, HLP=hlp
  l=list()
  if not(keyword_set(basepath)) THEN BEGIN
     IF NOT(keyword_set(hlp)) THEN  hlp=PTR_NEW(["",$
          "Select a top directory for recursive macro prosessing", $
          "",$
          "All subdirectories with a name acq* will be searched for xml descriptor files." , $
          "The assumption is that each of these files refers to a PAD dataset.", $
          "" $
                 ])
     res=rmd_pickfile(  PATH=GetWorkingDir(), $
                        get_path = wdir,          $
                        title = "Select a processing directory",          $
                        help=hlp, $
                        filter_in=filter, $
                        filter_out=filter, $
                       /DIREC,$
                        cancelled = cancelled)
     PTR_FREE, hlp
     if not(cancelled) THEN basepath=wdir ELSE return, l
  END
  res=File_Search(basepath, "acq*",/TEST_DIRECTORY)
  For i=0,N_Elements(res)-1 Do BEGIN
     xml=File_Search(res[i], "*.xml")
     FOR j=0,N_ELEMENTS(xml)-1 Do BEGIN
        SplitPath, xml[j], direct, filename
        l.Add, [direct,filename]
     END
  END

  return, l
END 

FUNCTION ProcessingFileList, pattern, BASEPATH=basepath
  hlp=PTR_NEW(["Recursive Directory File Search",$
               "",$
          "Select a top directory and a file pattern filter for directory recursive file search", $
          "All subdirectories with a name acq* will be searched for matching files." , $
          "" $
                 ])
  dirlist=ProcessingDirList(BASEPATH=basepath, FILTER=pattern, HLP=hlp)
  filelist=list()
  For i=1,dirlist.Count() DO BEGIN
     path=(dirlist[i-1])[0]+Path_Sep()
     res=File_Search(path, pattern)
     FOR j=0,N_ELEMENTS(res)-1 Do BEGIN
        SplitPath, res[j], direct, filename
        filelist.Add, [direct,filename]
     END
  END
  return, filelist
END 

FUNCTION MagCorrDataTable, l
;;
  IF (l.count() GT 0) THEN BEGIN
     mytable=NewTable("Meta Data",4,1)
     mytable->SetColumnLabels, ["Directory","Filename","Magnification(K)","CameraLength(mm)"]
     mytable->SetDescriptor, ["Magnification and camera length table.","The table holds the coreection values for the meta data in the files. ", "The value 0 means that the file data will be used."]
     mytable->Setlayer, 1
     For jj=1,l.count() DO  mytable->AddRow, list((l[jj-1])[0],(l[jj-1])[1],0.0,0.0)
     mytable->WidgetEditor
     s1=mytable.GetColumnData("Magnification(K)", LAYER=0)
     S2=mytable.GetColumnData("CameraLength(mm)", LAYER=0)
     ll=list()
     For jj=1,l.count() DO ll.Add, list((l[jj-1])[0], (l[jj-1])[1], s1[jj-1],s2[jj-1])
  END
  return, ll
END

FUNCTION GetFileListForBatchProc, Basepath=Basepath
;; by convention a line in the processing file should contain the
;; following fields:
;; directory filename magnification camera length
;; magnification is in K, camera length is in mm
;; 
l=list()
  if not(keyword_set(basepath)) THEN BEGIN
     IF NOT(keyword_set(hlp)) THEN  hlp=PTR_NEW(["",$
          "Select a processing table with filenames magnification and camera length information.", $
          "",$
          "Create this file if it does not exist under Macros -> Directory Recursion -> Prepare Filelist." , $
          "", $
          "" $
                 ])
     res=rmd_pickfile(  PATH=GetWorkingDir(), $
                        get_path = wdir,          $
                        title = "Select a filelist",          $
                        help=hlp, $
                        filter_in=filter, $
                        filter_out=filter, $
                        cancelled = cancelled)
     PTR_FREE, hlp
     if not(cancelled) THEN basepath=wdir ELSE return, l
  END
  Printtocon, "% GetFileListForBatchProc: Reading "+res[0]
  l=ReadBlockDataToList(res[0])
  ;; run a test to sort out invalid lines
  rm=list()
  For i=0,l.Count()-1 DO BEGIN
     ;; go through each line a check whether a file exists
     ln=l[i]
     fail=0B
     arr = ln.ToArray(TYPE='STRING')
     if (ln.Count() GE 2) THEN BEGIN
        dirname=arr[0]
        IF FILE_TEST(dirname, /DIRECTORY, /WRITE) THEN BEGIN
           filecheck=FILE_TEST(dirname+Path_Sep()+arr[1])
           IF NOT(filecheck) THEN BEGIN
              fail=1B
           END ELSE BEGIN
              ;; check for magnification and camera length field.
              If NOT(ln.Count() GT 2) THEN BEGIN
                 PrintToCon, "    Warning, line "+MyString(i)+": No magnification or camera length data, using descriptor data."
                 ln.add, " 0."
                 ln.add, " 0."
                 l[i]=ln
              END 
               If NOT(ln.Count() GT 3) THEN BEGIN
                  PrintToCon, "    Warning, line "+MyString(i)+": No camera length data, using descriptor data."
                  ln.add, " 0."
                 l[i]=ln
              END
           END           
        END ELSE fail=1B
     END ELSE fail=1B
     If fail THEN BEGIN
        s=""
        For k=1,N_elements(arr) Do s+=" "+arr[k-1]
        PrintToCon, "    Skipping line "+MyString(i)+":"+s
        rm.add, i
     END
  END
  If (rm.Count() GT 0) THEN l.remove, rm.ToArray()
  Printtocon, "% GetFileListForBatchProc: Number of remaining files = "+MyString(l.Count())
  return, l
END

  

PRO PurgeAll, KEEPCURR=keepcurr
  ptr=GetRootPXYGraph()
  void=XYGraphList_DeleteOther(ptr)                              ;; purge: delete also the object
  void=XYGraphList_DeleteCurrentElement(ptr, /PURGE)             ;; purge: delete also the object 
  Update_XTabControl
  ptr=GetRootPTable()
  void=TableList_DeleteOther(ptr)                           ;; purge: delete also the object
  void=TableList_DeleteCurrentElement(ptr, /PURGE)          ;; purge: delete also the object 
  Update_XTabControl
  ptr=GetRootP()
  f=(*ptr).first
             ;; print, "% Delete Other: first pointer ", keepp
  WHILE PTR_VALID(f) DO BEGIN
     (*ptr).current=f
                ;; print, "% Delete Other: current pointer ", keepp
     ;; print, "% Delete Other: next pointer ", (*f).next
                    ;; print, "% Delete Other: deleting data under pointer ", (*ptr).current
     res=DataList_DeleteCurrentElement(ptr)
     f=(*ptr).current
     ;; print, "% Delete Other: current pointer after deletion ", f
  END
  Update_XTabControl
  Update_XTabControl
END


PRO AutoSaveCurrent, thefile, CONTRASTMODE=contrastmode, ZOOMFACT=zoomfact, SCALEBARCOLMIN=scalebarcolmin
;;   PRO SetStackContrastMode, AUTO=auto, MANUAL=manual, SDEV=sdev,
;;     MDEV=mdev, MINMAX=minmax, QUARTER=quarter, FULL=full, SRANGE=srange
;; auto save BF
  imp=GetCurrentP()
  if keyword_set(zoomfact) THEN Zoom, zoomfact ELSE Zoom, 0.25
  d=(*imp).datap
  IF NOT(PTR_VALID(d)) THEN return
  IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
      (*(*d).ScaleBar).color = !D.N_COLORS-1
  END ELSE BEGIN
     (*d).DisplayScaleBar=1
     IF PTR_VALID((*d).ScaleBar) THEN PTR_FREE, (*d).ScaleBar 
     (*d).ScaleBar=PTR_NEW(GetScaleBar(((*d).SzX),((*d).xsamp), UNIT=((*d).xunit)))
  END
  IF keyword_set(scalebarcolmin) THEN BEGIN
     (*(*d).ScaleBar).color = 0  
  END
    x0=FLOOR((*d).SzX*0.25) & x1=x0+FLOOR((*d).SzX*0.5)-1
  y0=FLOOR((*d).SzY*0.25) & y1=y0+FLOOR((*d).SzY*0.5)-1
  SetStackContrastMode, /AUTO, /SDEV, /QUARTER ;; default is sdev
  IF keyword_set(contrastmode) THEN Begin
     Case contrastmode OF
        "minmax": BEGIN
           SetStackContrastMode, /AUTO, /MINMAX, /QUARTER
           ;;SetContrast, d, FRAME=[x0,x1,y0,y1], MODE='minmax'
        END
        "sdev": BEGIN
           SetStackContrastMode, /AUTO, /QUARTER
           ;;SetContrast, d, FRAME=[x0,x1,y0,y1], MODE='sdev'
        END
        "mdev": BEGIN
           SetStackContrastMode, /AUTO, /MDEV, /QUARTER
           ;;SetContrast, d, FRAME=[x0,x1,y0,y1], MODE='mdev'
        END
        "diff": BEGIN
           SetStackContrastMode, /AUTO, /DIFF, /QUARTER
           ;;SetContrast, d, FRAME=[x0,x1,y0,y1], MODE='diff'
        END
        ELSE: BEGIN
           SetStackContrastMode, /AUTO, /SDEV, /QUARTER
           ;; SetContrast, d, FRAME=[x0,x1,y0,y1], MODE='sdev'
        END 
     END
  END
  Update_XTabContrastDialog
  Update_XTabControl
  TVDisplay
  ;; Save display as
  ExportDisplay,  thefile
  printtocon, "% AutoSaveCurrent"
  printtocon, "    Saved to "+thefile
END
  

PRO Macro_ADF
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  ;; align microdiffraction 
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  ;; create ADF
  o.ADF, RADIUS=[10,40], DX=0, DY=0, MASK=0
  imp=GetCurrentP()
  Zoom,0.25
  d=(*imp).datap
  IF NOT(PTR_VALID(d)) THEN return
  IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
     (*(*d).ScaleBar).color = !D.N_COLORS-1
  END ELSE BEGIN
     (*d).DisplayScaleBar=1
     IF PTR_VALID((*d).ScaleBar) THEN PTR_FREE, (*d).ScaleBar 
     (*d).ScaleBar=PTR_NEW(GetScaleBar(((*d).SzX),((*d).xsamp), UNIT=((*d).xunit)))
  END
  SetStackContrastMode, /AUTO, /MINMAX, /QUARTER ;; default is sdev
  x0=FLOOR((*d).SzX*0.25) & x1=x0+FLOOR((*d).SzX*0.5)-1
  y0=FLOOR((*d).SzY*0.25) & y1=x0+FLOOR((*d).SzY*0.5)-1
  TVDisplay
  ;; Save display as
  ExportBitmap, /DISPLAY
  ;; Save HDF5
  FileExport
  ;;
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  
  ;;
END

PRO Macro_ROI_DIFF, P=p, FPREFIX=fprefix, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
  IF NOT(keyword_set(p)) THEN BEGIN
     if keyword_set(file) THEN BEGIN
        ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
        SplitPath, file, direct, filename
     END
     p=GetCurrentP()
  END
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  ;; align microdiffraction 
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  ;; creates ADF
  o.PickDiff
  ;; now we have a number of patterns to save
  ;; these are the last images in the list, except for the Preview
  
  IF keyword_set(fprefix) THEN BEGIN
     AutoSaveCurrent, direct+Path_Sep()+fprefix+"_NBED.png"
     ;; Save HDF5
     dummy=Write_hdf5(GetCurrentP(),direct+Path_Sep()+fprefix+"_NBED.h5")
  END ELSE BEGIN
    FileExport
  END
  ;;
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
 ;;
END




PRO LoadEMPAD, fname
 
END


PRO BatchProcessing, filelist, PROCLIST=proclist
;; Load files from list
  FOR i=1,filelist.Count() Do BEGIN
     ReadEMPADData, filelist[i-1], /NONINTERACTIVE
     p=GetCurrentP()
     Zoom, 0.5
     For j=1,proclist.Count() DO BEGIN
       CALL_PROCEDURE, proclist[j]
     END
  END
 
END


PRO Macro_QuadDodec, AUTO=auto, NONINTERACTIVE=noninteractive, QUADLARMOR=quadlarmor, DODECLARMOR=dodeclarmor, CLOCKWISE=clockwise, FILE=file, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, SAVE=save
  ;; simple use when a file is loaded: 
  ;; Macro_QuadDodec, /AUTO
  ;;
   IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
  END ELSE direct=GetWorkingDir()
  IF NOT(keyword_set(fprefix)) THEN fprefix="" ;; for auto save
  ;; detectors rotate clockwise by default
  ;; the first element starts symmetrically around 3 o'clock
  if not(keyword_set(quadlarmor)) THEN larmor_quad=-45.
  if not(keyword_set(dodeclarmor)) THEN larmor_dodec=-15.
  cwrot=0B
  if keyword_set(clockwise) THEN BEGIN
     cwrot=1B
     if not(keyword_set(quadlarmor)) THEN larmor_quad=45. ELSE larmor_quad=quadlarmor
     if not(keyword_set(dodeclarmor)) THEN larmor_dodec=15. ELSE larmor_dodec=dodeclarmor
  END ELSE BEGIN
     cwrot=0B
     if not(keyword_set(quadlarmor)) THEN larmor_quad=-45. ELSE larmor_quad=quadlarmor
     if not(keyword_set(dodeclarmor)) THEN larmor_dodec=-15. ELSE larmor_dodec=dodeclarmor    
  END
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  xi = FIX((*d).SzX/2)
  yi = FIX(((*d).SzY-2)/2)
  ri = FIX((*d).SzX/4)
  IF keyword_set(auto) THEN BEGIN
     ;; auto-detect diffraction disk and displacement of the detector
     dx=1 & dy=1
     radius=50
     o.DiscCentre,  SEARCHRADII=[5,FIX((*d).SzX/2-1)], DX=dx, DY=dy, /SPARSE, RADIUS=radius, /CORRECT
     dx=Round(dx) & dy=Round(dy)
     xi+=dx & yi+=dy
     ri=FIX(radius)
  END ELSE BEGIN
     dx=0 & dy=0
     radius=50
  END 
  s=list()
  s.Add, {value:FIX(ri/3),label:"Radii (pix)",newrow:0B}
  s.Add, {value:FIX(2*ri/3),label:"",newrow:0B}
  s.Add, {value:FIX(ri),label:"",newrow:0B}
  s.Add, {value:larmor_quad,label:"Larmor offsets (deg)",newrow:1B}
  s.Add, {value:larmor_dodec,label:"",newrow:0B}
  s.Add, {value:dx,label:"Off-center shift dx, dy (pix)",newrow:1B}
  s.Add, {value:dy,label:"",newrow:0B}
  c=list()
  c.Add, {value:1B,label:"Sharp mask",newrow:1B}
  c.Add, {value:cwrot,label:"Clockwise segment notation",newrow:0B}
  c.Add, {value:1B,label:"Show detector mask",newrow:0B}
  c.Add, {value:1B,label:"Merge Segment Stacks",newrow:0B}
  if not(keyword_set(noninteractive)) THEN BEGIN
     if not XCircle(xi,yi, ri, BIN=(*d).binning) THEN return
     dx=xi-FIX((*d).SzX/2)
     dy=yi-FIX(((*d).SzY-2)/2)
     larmor_quad=45.
     larmor_dodec=15.
  
     ;; Get inner radius and centre shift 
     help=["Four-Quadrant-Dodecapole Segmented Detector",$
        "", $ 
        "Parameters:", $
        "", $
        "Radii: inner collection radii of the four-quadrant-dodecapole detector.", $
        "Off-center shift: Center displacement in horizontal and vertical direction in pixel", $
        "Clockwise rotation: Segment notation is clockwise.", $
        "Show mask: Display the detector mask.", $ 
        ""]
    
     IF NOT (XMDataChoiceField(s, c,TITLE="QuadDodec parameters") EQ 1) THEN return
  END  
  o.BF, RADIUS=s[0].value, DX=s[4].value, DY=s[5].value
  IF keyword_set(save) THEN BEGIN
     ;; auto save the bright field image as CH0.mrc
     pp=GetCurrentP()
     thefile=direct+Path_Sep()+fprefix+"BF_CH0.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  o.SegmentDet, RADIUS=[s[0].value,s[1].value], SYMMETRY=4, LARMOR=s[3].value*!Pi/180., DX=s[5].value, DY=s[6].value, MASK=c[2].value, CLOCKWISE=c[1].value, TOPHAT=c[0].value, /NAN
  s1=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all quad segment images as CH1-4.mrc
     thefile=direct+Path_Sep()+fprefix+"QuadDodec_CH1-4.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  o.SegmentDet, RADIUS=[s[1].value,s[2].value], SYMMETRY=12, LARMOR=s[4].value*!Pi/180., DX=s[5].value, DY=s[6].value, MASK=c[2].value, CLOCKWISE=c[1].value, TOPHAT=c[0].value, /NAN
  s2=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all dodec segment images as CH5-16.mrc
     thefile=direct+Path_Sep()+fprefix+"QuadDodec_CH5-16.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  ;;
  if (c[3].value EQ 1B) THEN BEGIN
     q=Make_Array((*(*s1).datap).SzX,(*(*s1).datap).SzX, (*(*s1).datap).SzZ+(*(*s2).datap).SzZ,TYPE=FIX((*(*s1).datap).type))
     q[*,*,0:3]=*(*(*s1).datap).data
     q[*,*,4:15]=*(*(*s2).datap).data
     ThrowStack, PTR_NEW(q), "QuadDodec", BIN=0.5, TITLE="QuadDodec", SAMP=[(*(*s1).datap).xsamp,(*(*s1).datap).ysamp,(*(*s1).datap).zsamp], UNIT=[(*(*s1).datap).xunit,(*(*s1).datap).yunit,(*(*s1).datap).zunit]
      IF keyword_set(save) THEN BEGIN
     ;; auto save all segment images as CH1-16.mrc
         pp=GetCurrentP()
         thefile=direct+Path_Sep()+fprefix+"QuadDodec_CH1-16.mrc"
         SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
      END
   END
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  
  ;;
END

PRO Macro_QuadOctet, AUTO=auto, NONINTERACTIVE=noninteractive, QUADLARMOR=quadlarmor, OCTETLARMOR=octetlarmor, CLOCKWISE=clockwise, FILE=file, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, SAVE=save
  ;; simple use when a file is loaded: 
  ;; Macro_QuadDodec, /AUTO
  ;;
   IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
  END ELSE direct=GetWorkingDir()
  IF NOT(keyword_set(fprefix)) THEN fprefix="" ;; for auto save
  ;; detectors rotate clockwise by default
  ;; the first element starts symmetrically around 3 o'clock
  
  if not(keyword_set(octetlarmor)) THEN larmor_octet=-22.5
  if not(keyword_set(clockwise)) THEN BEGIN
     cwrot=0B
     if not(keyword_set(quadlarmor)) THEN larmor_quad=-45. ELSE larmor_quad=quadlarmor
     if not(keyword_set(octetlarmor)) THEN larmor_octet=-22.5 ELSE larmor_octet=octetlarmor
  END ELSE BEGIN
     cwrot=1B
     if not(keyword_set(quadlarmor)) THEN larmor_quad=45. ELSE larmor_quad=quadlarmor
     if not(keyword_set(octetlarmor)) THEN larmor_octet=22.5 ELSE larmor_octet=octetlarmor
  END
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  xi = FIX((*d).SzX/2)
  yi = FIX(((*d).SzY-2)/2)
  ri = FIX((*d).SzX/4)
  IF keyword_set(auto) THEN BEGIN
     ;; auto-detect diffraction disk and displacement of the detector
     dx=1 & dy=1
     radius=50
     o.DiscCentre,  SEARCHRADII=[5,FIX((*d).SzX/2-1)], DX=dx, DY=dy, /SPARSE, RADIUS=radius, /CORRECT
     dx=Round(dx) & dy=Round(dy)
     xi+=dx & yi+=dy
     ri=FIX(radius)
  END ELSE BEGIN
     dx=0 & dy=0
     radius=50
  END
  s=list()
  s.Add, {value:FIX(ri/3),label:"Radii (pix)",newrow:0B}
  s.Add, {value:FIX(2*ri/3),label:"",newrow:0B}
  s.Add, {value:FIX(ri),label:"",newrow:0B}
  s.Add, {value:larmor_quad,label:"Larmor offsets (deg)",newrow:1B}
  s.Add, {value:larmor_octet,label:"",newrow:0B}
  s.Add, {value:dx,label:"Off-center shift dx, dy (pix)",newrow:1B}
  s.Add, {value:dy,label:"",newrow:0B}
  c=list()
  c.Add, {value:1B,label:"Sharp mask",newrow:1B}
  c.Add, {value:cwrot,label:"Clockwise segment notation",newrow:0B}
  c.Add, {value:1B,label:"Show detector mask",newrow:0B}
  c.Add, {value:1B,label:"Merge Segment Stacks",newrow:0B}
  if not(keyword_set(noninteractive)) THEN BEGIN
     if not XCircle(xi,yi, ri, BIN=(*d).binning) THEN return
     dx=xi-FIX((*d).SzX/2)
     dy=yi-FIX(((*d).SzY-2)/2)
     larmor_quad=45.
     larmor_dodec=15.
  
     ;; Get inner radius and centre shift 
     help=["Four-QuadrantOctopole Segmented Detector",$
        "", $ 
        "Parameters:", $
        "", $
        "Radii: inner collection radii of the four-quadrant-dodecapole detector.", $
        "Off-center shift: Center displacement in horizontal and vertical direction in pixel", $
        "Clockwise rotation: Segment notation is clockwise.", $
        "Show mask: Display the detector mask.", $ 
        ""]
    
     IF NOT (XMDataChoiceField(s, c,TITLE="QuadOctet parameters") EQ 1) THEN return
  END  
  o.BF, RADIUS=s[0].value, DX=s[4].value, DY=s[5].value
  IF keyword_set(save) THEN BEGIN
     ;; auto save the bright field image as CH0.mrc
     pp=GetCurrentP()
     thefile=direct+Path_Sep()+fprefix+"BF_CH0.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  o.SegmentDet, RADIUS=[s[0].value,s[1].value], SYMMETRY=4, LARMOR=s[3].value*!Pi/180., DX=s[5].value, DY=s[6].value, MASK=c[2].value, CLOCKWISE=c[1].value, TOPHAT=c[0].value, /NAN
  s1=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all quad segment images as CH1-4.mrc
     thefile=direct+Path_Sep()+fprefix+"Quad_CH1-4.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  o.SegmentDet, RADIUS=[s[1].value,s[2].value], SYMMETRY=8, LARMOR=s[4].value*!Pi/180., DX=s[5].value, DY=s[6].value, MASK=c[2].value, CLOCKWISE=c[1].value, TOPHAT=c[0].value, /NAN
  s2=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all dodec segment images as CH5-12.mrc
     thefile=direct+Path_Sep()+fprefix+"Octet_CH5-12.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  ;;
  if (c[3].value EQ 1B) THEN BEGIN
     q=Make_Array((*(*s1).datap).SzX,(*(*s1).datap).SzX, (*(*s1).datap).SzZ+(*(*s2).datap).SzZ,TYPE=FIX((*(*s1).datap).type))
     q[*,*,0:3]=*(*(*s1).datap).data
     q[*,*,4:11]=*(*(*s2).datap).data
     ThrowStack, PTR_NEW(q), "QuadOctet", BIN=0.5, TITLE="QuadOctet", SAMP=[(*(*s1).datap).xsamp,(*(*s1).datap).ysamp,(*(*s1).datap).zsamp], UNIT=[(*(*s1).datap).xunit,(*(*s1).datap).yunit,(*(*s1).datap).zunit]
      IF keyword_set(save) THEN BEGIN
     ;; auto save all segment images as CH1-12.mrc
         pp=GetCurrentP()
         thefile=direct+Path_Sep()+fprefix+"QuadOctet_CH1-12.mrc"
         SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
      END
   END
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;
END



PRO Macro_QuadOctetDodec, AUTO=auto, FILE=file, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, SAVE=save
  ;; simple use when a file is loaded: 
  ;; Macro_QuadDodec, /AUTO
  ;;
   IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=magnificationoverwrite
  END ELSE direct=GetWorkingDir()
  IF NOT(keyword_set(fprefix)) THEN fprefix="" ;; for auto save
  ;; detectors rotate clockwise by default
  ;; the first element starts symmetrically around 3 o'clock
  larmor_quad=-45.
  larmor_octet=-22.5
  larmor_dodec=-15
  cwrot=0B
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  xi = FIX((*d).SzX/2)
  yi = FIX(((*d).SzY-2)/2)
  ri = FIX((*d).SzX/4)
  if keyword_set(auto) THEN BEGIN
     ;; auto-detect diffraction disk and displacement of the detector
     dx=1 & dy=1
     radius=50
     o.DiscCentre,  SEARCHRADII=[5,FIX((*d).SzX/2-1)], DX=dx, DY=dy, /SPARSE, RADIUS=radius, /CORRECT
     dx=Round(dx) & dy=Round(dy)
     xi+=dx & yi+=dy
     ri=FIX(radius)
  END
  o.BF, RADIUS=FIX(ri/3)
  IF keyword_set(save) THEN BEGIN
     ;; auto save the bright field image as CH0.mrc
     pp=GetCurrentP()
     thefile=direct+Path_Sep()+fprefix+"BF_CH0.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  o.SegmentDet, RADIUS=[FIX(ri/3),FIX(2*ri/3)], SYMMETRY=4, LARMOR=larmor_quad*!Pi/180., /MASK, /TOPHAT, /NAN
  s1=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all quad segment images as CH1-4.mrc
     thefile=direct+Path_Sep()+fprefix+"Quad_CH1-4.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  o.SegmentDet, RADIUS=[FIX(2*ri/3),FIX(ri)], SYMMETRY=8, LARMOR=larmor_octet*!Pi/180., /MASK, /TOPHAT, /NAN
  s2=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all dodec segment images as CH5-12.mrc
     thefile=direct+Path_Sep()+fprefix+"Octet_CH5-12.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  ;;
  q=Make_Array((*(*s1).datap).SzX,(*(*s1).datap).SzX, (*(*s1).datap).SzZ+(*(*s2).datap).SzZ,TYPE=FIX((*(*s1).datap).type))
  q[*,*,0:3]=*(*(*s1).datap).data
  q[*,*,4:11]=*(*(*s2).datap).data
  ThrowStack, PTR_NEW(q), "QuadOctet", BIN=0.5, TITLE="QuadOctet", SAMP=[(*(*s1).datap).xsamp,(*(*s1).datap).ysamp,(*(*s1).datap).zsamp], UNIT=[(*(*s1).datap).xunit,(*(*s1).datap).yunit,(*(*s1).datap).zunit]
  IF keyword_set(save) THEN BEGIN
     ;; auto save all segment images as CH1-12.mrc
     pp=GetCurrentP()
     thefile=direct+Path_Sep()+fprefix+"QuadOctet_CH1-12.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  
o.SegmentDet, RADIUS=[FIX(2*ri/3),FIX(ri)], SYMMETRY=12, LARMOR=larmor_dodec*!Pi/180., /MASK, /TOPHAT, /NAN
  s2=GetCurrentP()
  IF keyword_set(save) THEN BEGIN
     ;; auto save all dodec segment images as CH5-12.mrc
     thefile=direct+Path_Sep()+fprefix+"Dodec_CH5-16.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  ;;
  q=Make_Array((*(*s1).datap).SzX,(*(*s1).datap).SzX, (*(*s1).datap).SzZ+(*(*s2).datap).SzZ,TYPE=FIX((*(*s1).datap).type))
  q[*,*,0:3]=*(*(*s1).datap).data
  q[*,*,4:15]=*(*(*s2).datap).data
  ThrowStack, PTR_NEW(q), "QuadOctet", BIN=0.5, TITLE="QuadDodec", SAMP=[(*(*s2).datap).xsamp,(*(*s2).datap).ysamp,(*(*s2).datap).zsamp], UNIT=[(*(*s2).datap).xunit,(*(*s2).datap).yunit,(*(*s2).datap).zunit]
  IF keyword_set(save) THEN BEGIN
     ;; auto save all segment images as CH1-16.mrc
     pp=GetCurrentP()
     thefile=direct+Path_Sep()+fprefix+"QuadDodec_CH1-16.mrc"
     SaveBinData, thefile, FORMAT="mrc", AUTOSAVE=autosave
  END
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  
  ;;
END


PRO Macro_FriedelPairMap, spacing, angwidth, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, MEDIANWIDTH=medianwidth, THRESHOLD=threshold, MAXSYM=maxsym, SPATAVG=spatavg
  ;; spacing: lower and upper radius
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  ;; continue with fluctuation map
  IF keyword_set(spatavg) THEN o.SpatialAverage, FIX(spatavg)
  maps=1
  arg=1
  o.AzimuthCorr, RADIUS=[spacing[0],spacing[1]], ANGWIDTH=angwidth, MAPS=maps, MEDIANWIDTH=medianwidth, THRESHOLD=threshold, ARG=arg, MAXSYM=maxsym 
  IF SetCurrentP(maps[0]) THEN BEGIN
     Update_XTabControl
     tmp=MyString(spacing[0])+"-"+MyString(spacing[1])
     thefile=direct+Path_Sep()+fprefix+"spacing_"+tmp+"Friedel-2fold.png"
     AutoSaveCurrent, thefile, CONTRASTMODE="minmax"
     thefile=direct+Path_Sep()+fprefix+"spacing_"+tmp+"Friedel-2fold.h5"
     dummy=Write_hdf5(GetCurrentP(),thefile)
  END
  IF SetCurrentP(arg[0]) THEN BEGIN
     Update_XTabControl
     tmp=MyString(spacing[0])+"-"+MyString(spacing[1])
     thefile=direct+Path_Sep()+fprefix+"spacing_"+tmp+"Friedel-2fold-arg.png"
     AutoSaveCurrent, thefile, contrastmode="minmax"
     thefile=direct+Path_Sep()+fprefix+"spacing_"+tmp+"Friedel-2fold_arg.h5"
     dummy=Write_hdf5(GetCurrentP(),thefile)
  END
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;
END



PRO Macro_ReflectionMap, spacing, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix
  ;; spacing: lower and upper radius
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  o.BF, RADIUS=5, DX=0, DY=0
  thefile=direct+Path_Sep()+fprefix+"BF_5pix.png"
  AutoSaveCurrent, thefile
  ;; continue with fluctuation map
  o.FluctuationMap, RADIUS=[spacing[0],spacing[1]], DX=0, DY=0, MASK=0
  tmp=MyString(spacing[0])+"-"+MyString(spacing[1])
  thefile=direct+Path_Sep()+fprefix+"spacing_"+tmp+".png"
  AutoSaveCurrent, thefile
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;
END

PRO Macro_BFADF, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, CLOVERWRITE=cloverwrite
  ;; spacing: lower and upper radius
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     PrintToCon, "% MacroPeakSearch: Loading data"
     IF keyword_set(cloverwrite) THEN PrintToCon, "% Macro_BFADF: Camera length overwrite: "+Mystring(cloverwrite)+ " m"
     IF keyword_set(magnificationoverwrite) THEN PrintToCon, "% Macro_BFADF: Magnification overwrite: "+Mystring(magnificationoverwrite)
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, CLOVERWRITE=cloverwrite
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  o.BF, RADIUS=5, DX=0, DY=0
  thefile=direct+Path_Sep()+fprefix+"_BF_5pix.png"
  AutoSaveCurrent, thefile, contrastmode='minmax', /SCALEBARCOLMIN
  o.ADF, RADIUS=[40,100], DX=0, DY=0
  thefile=direct+Path_Sep()+fprefix+"_ADF_40pix+.png"
  AutoSaveCurrent, thefile, CONTRASTMODE='minmax'
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;
END

PRO Macro_DistilledDiffractionMap, spacing, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, LOWERBOUND=lowerbound
  ;; spacing: lower and upper radius
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite
     PrintToCon, "% MacroPeakSearch: Loading data"
        IF cloverwrite THEN PrintToCon, "  camera length overwrite: "+Mystring(cloverwrite)+ " m"
        IF keyword_set(magnificationoverwrite) THEN PrintToCon, " magnification overwrite: "+Mystring(magnificationoverwrite)
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  IF NOT(keyword_set(lowerbound)) THEN lowerbound=1000.
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  ;; create fluctuation map
  o.FluctuationMap, RADIUS=[spacing[0],spacing[1]], DX=0, DY=0, MASK=0
  tmp=MyString(spacing[0])+"-"+MyString(spacing[1])
  thefile=direct+Path_Sep()+fprefix+"spacing_"+tmp+".png"
  AutoSaveCurrent, thefile
  ;; distill diffraction data
  ss=1
  o.GetFluctuationMapSubset, lowerbound, SUBSET=ss
  ss.Average
  ;;thefile=direct+Path_Sep()+fprefix+"avgdiff_"+tmp+".png"
  ;;AutoSaveCurrent, thefile
  thefile=direct+Path_Sep()+fprefix+"avgdiff_"+tmp+".h5"
  dummy=Write_hdf5(GetCurrentP(),thefile)
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;
END

PRO AngularMap_To_Index, threshold,  FILE=file, FPREFIX=fprefix, MEDIANWIDTH=medianwidth
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  NAngles=(*d).SzZ
  im=BYTARR((*d).SzX,(*d).SzY)
  For i=0,NAngles-1 DO BEGIN
     IF (keyword_set(medianwidth) AND (medianwidth GE 2)) THEN BEGIN
        tmp=WHERE(Median((*(*d).data)[*,*,i],medianwidth) GT threshold, count)
     END ELSE tmp=WHERE((*(*d).data)[*,*,i] GT threshold, count)
     IF (count GE 1) THEN im[tmp]=1+FIX(i*1./Nangles*254) ;; colors range from 1 to 255, 0 is the non-match
  END
  name="Angular color coded map"
  ThrowImage, im, BIN=0.5,TITLE=name, SAMP=[(*d).xsamp,(*d).xsamp,(*d).zsamp], UNIT=[(*d).xunit,(*d).yunit,(*d).zunit]
  IF keyword_set(fprefix) THEN BEGIN
      IF keyword_set(file) THEN BEGIN
         SplitPath, file, direct, filename
         ;; save data
         thefile=direct+Path_Sep()+fprefix+"colorcodedangfluctmap.h5"
         dummy=Write_hdf5(GetCurrentP(),thefile)
      END
   END
  ;; Focus to original p
  dummy=SetCurrentP(porig)
END
  

PRO Macro_DistilledAngularDiffractionMap, spacing, DELTA=delta, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, LOWERBOUND=lowerbound, CLOVERWRITE=cloverwrite
  ;; spacing: lower and upper radius
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, CLOVERWRITE=cloverwrite
     PrintToCon, "% MacroPeakSearch: Loading data"
        IF cloverwrite THEN PrintToCon, "  camera length overwrite: "+Mystring(cloverwrite)+ " m"
        IF keyword_set(magnificationoverwrite) THEN PrintToCon, " magnification overwrite: "+Mystring(magnificationoverwrite)
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  IF NOT(keyword_set(lowerbound)) THEN lowerbound=3000.
  IF NOT(keyword_set(delta)) THEN delta=5.
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=1, UPSCALING=0
  ;; create angular fluctuation map 
  o.AngularFluctuationImages, DELTA=delta, RADIUS=[spacing[0],spacing[1]], DX=0, DY=0
  tmp=MyString(spacing[0])+"-"+MyString(spacing[1])
  thefile=direct+Path_Sep()+fprefix+"angfluctmap_"+tmp+".h5"
  dummy=Write_hdf5(GetCurrentP(),thefile)
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  printtocon, "% Macro_DistilledAngularDiffractionMap"
  printtocon, "    Saved to "+thefile
  ;;
END


PRO MacroDPC, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, FPREFIX=fprefix, RADIUS=radius, LARMOR=larmor, CLOCKWISE=clockwise, TYPE=type, CLOVERWRITE=cloverwrite
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, CLOVERWRITE=cloverwrite
     PrintToCon, "% MacroPeakSearch: Loading data"
        IF cloverwrite THEN PrintToCon, "  camera length overwrite: "+Mystring(cloverwrite)+ " m"
        IF keyword_set(magnificationoverwrite) THEN PrintToCon, " magnification overwrite: "+Mystring(magnificationoverwrite)
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  ;; align microdiffraction 
  o.CenterCorrection, SUBPIX=0, COMRADIUS=5, FITORDER=2, PIXWISE=0, UPSCALING=0
  ;; create DPC images
  if not(keyword_set(radius)) Then radius=[10,40]
  retp=1
  o.DPC, RADIUS=radius, DX=0, DY=0, Mask=0, LARMOR=larmor, CLOCKWISE=clockwise, TYPE=type, RETP=retp
  IF SetCurrentP(retp[0]) THEN BEGIN
     Update_XTabControl
     tmp=MyString(radius[0])+"-"+MyString(radius[1])
     thefile=direct+Path_Sep()+fprefix+"_"+tmp+"_DPCx.png"
     AutoSaveCurrent, thefile, contrastmode="minmax"
     thefile=direct+Path_Sep()+fprefix+"_"+tmp+"_DPCx.h5"
     dummy=Write_hdf5(GetCurrentP(),thefile)
  END 
  IF SetCurrentP(retp[1]) THEN BEGIN
     Update_XTabControl
     tmp=MyString(radius[0])+"-"+MyString(radius[1])
     thefile=direct+Path_Sep()+fprefix+"_"+tmp+"_DPCy.png"
     AutoSaveCurrent, thefile, contrastmode="minmax"
     thefile=direct+Path_Sep()+fprefix+"_"+tmp+"_DPCy.h5"
     dummy=Write_hdf5(GetCurrentP(),thefile)
  END
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;
END

PRO MacroPickDiff
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  o.PickDiff, /CUMULATIVEMASK, /AUTOFILESAVE, /SCALEBAR, /ROTEMPAD
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
  ;;  
END
  
PRO MacroPeakSearch, NOLOAD=noload, FILE=file, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, UPSCALE=upscale, FPREFIX=fprefix, BIN=bin, THRESHOLD=threshold, MASKROWS=maskrows, CLOVERWRITE=cloverwrite
  IF keyword_set(file) THEN BEGIN
     SplitPath, file, direct, filename
     ;; load EMPAD data
     IF NOT(keyword_set(noload)) THEN BEGIN
        ReadEMPADData, file, SWAPENDIAN=swapend, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, CLOVERWRITE=cloverwrite
        PrintToCon, "% MacroPeakSearch: Loading data"
        IF cloverwrite THEN PrintToCon, "  camera length overwrite: "+Mystring(cloverwrite)+ " m"
        IF keyword_set(magnificationoverwrite) THEN PrintToCon, " magnification overwrite: "+Mystring(magnificationoverwrite)
     END
  END
  IF NOT(keyword_set(fprefix)) THEN fprefix=""
  p=GetCurrentP()
  porig=p
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  ;; assuming frames are aligned, though not necessary
  ;; correct margin rows for beam deflector issue
  if keyword_set(maskrows) THEN BEGIN
     o.MaskRows, maskrows, /TOP, /BOTTOM
     o.MaskCols, maskrows, /LEFT, /RIGHT
  END
  ;; spatially bin data
  SpatialBinning, o, bin
  ;; threshold new data
  p=GetCurrentP()
  IF NOT(PTR_VALID(p)) THEN return
  d=(*p).datap
  IF NOT(PTR_VALID(p)) THEN return
  o=(*d).extra
  IF NOT(OBJ_VALID(o)) THEN return
  IF (keyword_set(upscale) AND (upscale GT 1)) then BEGIN
     o.UpScale, upscale, upscale, P=p, /INTERP
     d=(*p).datap
     IF (PTR_VALID(d)) THEN BEGIN
        fact=(*d).binning
        CreateWindow, BIN=fact
        TVDisplay
        Update_XTabControlInfoField
     END
  END
  IF keyword_set(threshold) then o.threshold, threshold*bin*bin
  ;; search peaks with current presets
  framelist=1
  ;; set contrastmode to diff
  SetStackContrastMode, /AUTO, /SDEV ;; default is sdev
      ;; auto contrast, mode sdev
  ;; SetContrast, d, FRAME=frm, MODE='diff'
  ;;
  f=MyClusterAnalysis3D(INTERACTIVE=0,/LIVE,/REFINE,FRAMELIST=framelist)
  ;;
  mytable=TableImport2DArray(f.ToArray(),TITLE="Peak Data",COLUMNTITLES=["index","refined x","refined y","z","mass","gyration","x","y","refined distance x","refined distance y"])
  thefile=direct+Path_Sep()+fprefix+"_peakdata.dat"
  mytable->Export, /ALL, FILE=thefile, NONINTERACTIVE=noninteractive
  ;; create a subset with the frames that have peaks
  B=framelist.ToARRAY(TYPE=3)
  o.Subset, B
  p=GetCurrentP()
  osub=GetExtraObj()
  ;; save the subset
  printtocon, "% FileExport: export to HDF5"
  thefile=direct+Path_Sep()+fprefix+"_peakdata_subset.h5"
  dummy=Write_hdf5(GetCurrentP(), thefile,/NONINTERACTIVE)
  ;; create a BF image, it is zero where no peak was detected
  osub.BF, RADIUS=5
  thefile=direct+Path_Sep()+fprefix+"_peakdata_subset_BF_5pix.png"
  AutoSaveCurrent, thefile, contrastmode='minmax'
  ;; March throuhg COM datasets
  ;; Focus to original p
  dummy=SetCurrentP(porig)
  Update_XTabControl
END



PRO ProcessMe
  basepath="/Volumes/VerbatimDsk/EMREQ-006-EA-Coccoliths/"
  ;; basepath="/Volumes/VerbatimDsk/210105-SK-MAPI-FPEA-diff-SlotC"
  ;; j0=17 & j1=41 ;; Series2
  j0=44 & j1=67 ;; Series3
  ;; j0=3 & j1=14  ;; Series1
  ;; j0=44 & j1=45 ;; testing
  files=STRARR(j1-j0+1)
  thefileprefix=STRARR(j1-j0+1)
  for j=j0,j1 DO BEGIN
     files[j-j0]="acquisition_"+MyString(j)+"/acquisition_"+MyString(j)+".xml"
     thefileprefix[j-j0]="acquisition_"+MyString(j)+"_Series3_012-"
  END
  ;; spacing=[15,17] ;;; 014
  spacing=[10,13] ;;; 012
  lowerbound=3500
  For i=1,N_Elements(files) DO BEGIN
     thefile=basepath+files[i-1]
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; Get a fluctuation map
     ;; Macro_ReflectionMap, spacing, FILE=thefile, /NONINTERACTIV, $
     ;; MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1]
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; create diffraction subset based on the fluctuation in a
     ;; ;; range of scattering angles
     ;;
     ;;  Macro_DistilledDiffractionMap, spacing, FILE=thefile, $
     ;;  /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;;  FPREFIX=thefileprefix[i-1], LOWERBOUND=lowerbound
     ;;
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; create angular diffraction subset based on the fluctuation in a
     ;; ;; range of scattering angles with refinement of azimuth
     ;; ;; angles
     ;; ;; create a map of azimuth to use with an indexed colortable
     ;;
     ;; Macro_DistilledAngularDiffractionMap, spacing, FILE=thefile, $
     ;; /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;; FPREFIX=thefileprefix[i-1], LOWERBOUND=lowerbound
     ;; threshold=15000
     ;; medianwidth=3
     ;; AngularMap_To_Index, threshold,  FILE=thefile, $
     ;; FPREFIX=thefileprefix[i-1], MEDIANWIDTH=5
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; track and trace a feature in diffraction, this requires
     ;; ;; interaction, at least autoloading and saving takes place
     ;;
     ;; Macro_ROI_DIFF, FPREFIX=thefileprefix[i-1], FILE=thefile, $
     ;; /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000.
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; Create BF images and ADF images
     ;;
     ;; Macro_BFADF, FILE=thefile, /NONINTERACTIV, $
     ;; MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1]
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; produce Friedel pair maps
     ;; ;; autosave two-fold symmetry maps
     ;;
     ;;angwidth=2
     ;;Macro_FriedelPairMap, spacing, angwidth, FILE=thefile, $
     ;;                      /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;;                      FPREFIX=thefileprefix[i-1], MAXSYM=2, THRESHOLD=0.2, MEDIANWIDTH=2, SPATAVG=1
     ;; 
                   ;;
                           ;; ;; -----------------------------------------------------------------------------------
     ;; ;; produce DPC images
     ;; ;; autosave
     radius=[10,40]
     MacroDPC, FILE=thefile, /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1], RADIUS=radius
     ;; LARMOR=larmor, CLOCKWISE=clockwise, TYPE=type
     ;; ------------
     ;; ; delete all stuff and continue
     PurgeAll
     
  END
END

PRO ProcessMe2
  basepath="/Volumes/VerbatimDsk/210131-AuC-AberrationTest/"
  mag=5100000
  j0=1 & j1=10 ;; Series 2 & 3
  files=STRARR(j1-j0+1)
  thefileprefix=STRARR(j1-j0+1)
  for j=j0,j1 DO BEGIN
     files[j-j0]="acquisition_"+MyString(j)+"/acquisition_"+MyString(j)+".xml"
     thefileprefix[j-j0]=""
  END
  For i=1,N_Elements(files) DO BEGIN
     thefile=basepath+files[i-1]
      ;; Macro_QuadDodec, FILE=thefile, /NONINTERACTIVE, /AUTO, $
      ;;                MAGNIFICATIONOVERWRITE=mag, FPREFIX=thefileprefix[i-1], /SAVE
     ;;Macro_QuadOctet, FILE=thefile, /NONINTERACTIVE, /AUTO,
     ;;MAGNIFICATIONOVERWRITE=mag, FPREFIX=thefileprefix[i-1], /SAVE
     Macro_QuadOctetDodec, FILE=thefile, /AUTO, MAGNIFICATIONOVERWRITE=mag, FPREFIX=thefileprefix[i-1], /SAVE
     PurgeAll
  END

END




PRO ProcessMe3
  basepath="/Users/lothar/Desktop/210103-AberattionMeas-Trial/"
  mag=3600000
  j0=1 & j1=23 ;; Series 2 & 3
  files=STRARR(j1-j0+1)
  thefileprefix=STRARR(j1-j0+1)
  for j=j0,j1 DO BEGIN
     files[j-j0]="acquisition_"+MyString(j)+"/acquisition_"+MyString(j)+".xml"
     thefileprefix[j-j0]=""
  END
  For i=1,N_Elements(files) DO BEGIN
     thefile=basepath+files[i-1]
      ;; Macro_QuadDodec, FILE=thefile, /NONINTERACTIVE, /AUTO, $
      ;;                MAGNIFICATIONOVERWRITE=mag, FPREFIX=thefileprefix[i-1], /SAVE
     ;;Macro_QuadOctet, FILE=thefile, /NONINTERACTIVE, /AUTO,
     ;;MAGNIFICATIONOVERWRITE=mag, FPREFIX=thefileprefix[i-1], /SAVE
     Macro_QuadOctetDodec, FILE=thefile, /AUTO, MAGNIFICATIONOVERWRITE=mag, FPREFIX=thefileprefix[i-1], /SAVE
     PurgeAll
  END

END


PRO ProcessMe4
  
  MagOv=FLTARR(15)
  MagOv[0]=80000.
  for j=1,9 DO MagOv[j]=57000.
  ;; spacing=[15,17] ;;; 014
  For i=1,N_Elements(files) DO BEGIN
     thefile=basepath+files[i-1]
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; Get a fluctuation map
     ;; Macro_ReflectionMap, spacing, FILE=thefile, /NONINTERACTIV, $
     ;; MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1]
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; create diffraction subset based on the fluctuation in a
     ;; ;; range of scattering angles
     ;;
     ;;  Macro_DistilledDiffractionMap, spacing, FILE=thefile, $
     ;;  /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;;  FPREFIX=thefileprefix[i-1], LOWERBOUND=lowerbound
     ;;
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; create angular diffraction subset based on the fluctuation in a
     ;; ;; range of scattering angles with refinement of azimuth
     ;; ;; angles
     ;; ;; create a map of azimuth to use with an indexed colortable
     ;;
     ;; Macro_DistilledAngularDiffractionMap, spacing, FILE=thefile, $
     ;; /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;; FPREFIX=thefileprefix[i-1], LOWERBOUND=lowerbound
     ;; threshold=15000
     ;; medianwidth=3
     ;; AngularMap_To_Index, threshold,  FILE=thefile, $
     ;; FPREFIX=thefileprefix[i-1], MEDIANWIDTH=5
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; track and trace a feature in diffraction, this requires
     ;; ;; interaction, at least autoloading and saving takes place
     ;;
     ;; Macro_ROI_DIFF, FPREFIX=thefileprefix[i-1], FILE=thefile, $
     ;; /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000.
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; Create BF images and ADF images
     ;;
     Macro_BFADF, FILE=thefile, /NONINTERACTIV, $
                  MAGNIFICATIONOVERWRITE=MagOv[i-1], FPREFIX=thefileprefix[i-1]
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; produce Friedel pair maps
     ;; ;; autosave two-fold symmetry maps
     ;;
     ;; angwidth=1
     ;; Macro_FriedelPairMap, spacing, angwidth, FILE=thefile, $
     ;;                      /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=MagOv[i-1], $
     ;;                      FPREFIX=thefileprefix[i-1], MAXSYM=2, THRESHOLD=0.15, MEDIANWIDTH=3
     ;; SPATAVG=2
     ;; 
                           ;;
                           ;; ;; -----------------------------------------------------------------------------------
     ;; ;; produce DPC images
     ;; ;; autosave
     ;;radius=[10,40]
     ;;MacroDPC, FILE=thefile, /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1], RADIUS=radius
     ;; LARMOR=larmor, CLOCKWISE=clockwise, TYPE=type
     ;; ------------
     ;; ; delete all stuff and continue
     PurgeAll
     
  END
END


PRO MacroPeakDetection, PURGE=purge
  noninteractive=1
   l=GetFileListForBatchProc() 
   help=["Automated Peak Detection Parameters",$
               "", $ 
               "Parameters:", $
               "", $
         "Threshold: Lower threshold for binnig 1. Dark counts that are set to zero before processing.", $
         "           .", $
         "Binning: Spatial binning before processing. Reduces computation time significantlty. ", $
         "Upscaling: Upscaling before processing. Improves large spacing detection.. ", $
         "Row and Column Correction: Top-bottom row and left-right column correction and for deflector artefacts. ", $
         ""]
         s=list()
         s.Add, {value:1000,label:"Threshold",newrow:0B}
         s.Add, {value:2,label:"Binning",newrow:0B}
         s.Add, {value:1.,label:"Upscale",newrow:0B}
         s.Add, {value:2,label:"Row and Column Correction",newrow:0B}
         c=list()
         IF NOT(XMDataChoiceField(s, c, TITLE="Peak Detection", HELP=help) EQ 1) THEN return
  ;; overwrite for magnification tag
  For i=1,l.Count() DO BEGIN
     thefile=(l[i-1])[0]+Path_Sep()+(l[i-1])[1]
     thefileprefix=(l[i-1])[1]+"_m"
     mag=Float((l[i-1])[2])*1000.
     cl=Float((l[i-1])[3])/1000.
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; Get a fluctuation map
     ;; Macro_ReflectionMap, spacing, FILE=thefile, /NONINTERACTIV, $
     ;; MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1]
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; create diffraction subset based on the fluctuation in a
     ;; ;; range of scattering angles
     ;;
     ;;  Macro_DistilledDiffractionMap, spacing, FILE=thefile, $
     ;;  /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;;  FPREFIX=thefileprefix[i-1], LOWERBOUND=lowerbound
     ;;
     ;; ;; ------------------------------------------------------------------------------
     ;; ;; create angular diffraction subset based on the fluctuation in a
     ;; ;; range of scattering angles with refinement of azimuth
     ;; ;; angles
     ;; ;; create a map of azimuth to use with an indexed colortable
     ;;
     ;; Macro_DistilledAngularDiffractionMap, spacing, FILE=thefile, $
     ;; /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., $
     ;; FPREFIX=thefileprefix[i-1], LOWERBOUND=lowerbound
     ;; threshold=15000
     ;; medianwidth=3
     ;; AngularMap_To_Index, threshold,  FILE=thefile, $
     ;; FPREFIX=thefileprefix[i-1], MEDIANWIDTH=5
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; track and trace a feature in diffraction, this requires
     ;; ;; interaction, at least autoloading and saving takes place
     ;;
     ;; Macro_ROI_DIFF, FPREFIX=thefileprefix[i-1], FILE=thefile, $
     ;; /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000.
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; Create BF images and ADF images
     ;;
     ;; Macro_BFADF, FILE=thefile, /NONINTERACTIV, FPREFIX=thefileprefix
     ;;
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; produce Friedel pair maps
     ;; ;; autosave two-fold symmetry maps
     ;;
     ;; angwidth=1
     ;; Macro_FriedelPairMap, spacing, angwidth, FILE=thefile, $
     ;;                      /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=MagOv[i-1], $
     ;;                      FPREFIX=thefileprefix[i-1], MAXSYM=2, THRESHOLD=0.15, MEDIANWIDTH=3
     ;; SPATAVG=2
     ;; 
                           ;;
                           ;; ;; -----------------------------------------------------------------------------------
     ;; ;; produce DPC images
     ;; ;; autosave
     ;;radius=[10,40]
     ;;MacroDPC, FILE=thefile, /NONINTERACTIVE, MAGNIFICATIONOVERWRITE=20000., FPREFIX=thefileprefix[i-1], RADIUS=radius
     ;; LARMOR=larmor, CLOCKWISE=clockwise, TYPE=type

     ;; MacroPeakSearch, FILE=thefile, NONINTERACTIVE=noninteractive,
     ;; MAGNIFICATIONOVERWRITE=MagOv[i-1], FPREFIX=thefileprefix[i-1],
     ;; Bin=4
     ;; MacroPeakSearch, /NOLOAD, FILE=thefile, NONINTERACTIVE=noninteractive, FPREFIX=thefileprefix, Bin=s[1].value, THRESHOLD=s[0].value, MASKROWS=s[2].value
     MacroPeakSearch, FILE=thefile, NONINTERACTIVE=noninteractive, FPREFIX=thefileprefix, Bin=s[1].value, UPSCALE=s[2].value, THRESHOLD=s[0].value, MASKROWS=s[3].value, MAGNIFICATIONOVERWRITE=mag, CLOVERWRITE=cl
     ;; ------------
     ;; ; delete all stuff and continue
    IF keyword_set(purge) THEN PurgeAll 
     
  END
END

PRO MacroADFBF, PURGE=purge
  noninteractive=1
  ll=GetFileListForBatchProc() 
  ;; overwrite for magnification tag
  For i=1,ll.Count() DO BEGIN
     thefile=(ll[i-1])[0]+Path_Sep()+(ll[i-1])[1]
     thefileprefix=(ll[i-1])[1]+"_m"
     mag=Float((ll[i-1])[2])*1000.
     cl=Float((ll[i-1])[3])/1000.
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; Create BF images and ADF images
     ;;
     mag=(ll[i-1])[2]*1000. & if (mag eq 0) THEN mag=Fix(0)
     cl=(ll[i-1])[3]/1000. & if (cl eq 0) THEN cl=Fix(0)
     Macro_BFADF, FILE=thefile, /NONINTERACTIV, FPREFIX=thefileprefix, MAGNIFICATIONOVERWRITE=mag, CLOVERWRITE=cl
     IF keyword_set(purge) THEN PurgeAll 
  END
END


PRO MacroADFBF_PickDIFF, PURGE=purge, BASEPATH=basepath
  noninteractive=1
  ;; load file Mag CL data table
  ll=GetFileListForBatchProc() 
  ;; ll=MagCorrDataTable(l)
  ;; overwrite for magnification tag
  For i=1,ll.Count() DO BEGIN
     thefile=(ll[i-1])[0]+Path_Sep()+(ll[i-1])[1]
     thefileprefix=(ll[i-1])[1]+"_m"
     mag=Float((ll[i-1])[2])*1000.
     cl=Float((ll[i-1])[3])/1000.
     ;; ;; -----------------------------------------------------------------------------------
     ;; ;; Create BF images and ADF images
     ;;
     mag=(ll[i-1])[2]*1000. & if (mag eq 0) THEN mag=Fix(0)
     cl=(ll[i-1])[3]/1000. & if (cl eq 0) THEN cl=Fix(0)
     Macro_BFADF, FILE=thefile, /NONINTERACTIV, FPREFIX=thefileprefix, MAGNIFICATIONOVERWRITE=mag, CLOVERWRITE=cl
     MacroPickDiff ;; does not load data, cl should be ok
     IF keyword_set(purge) THEN PurgeAll 
  END
END

Pro LoadMultipleFiles, FMT=fmt
  fmt="*.h5"
  l=ProcessingFileList()
  For i=1,l.Count() DO BEGIN
     thefile=(l[i-1])[0]+Path_Sep()+(l[i-1])[1]
     Read_hdf5, thefile, /VERBOSE
  END   
END


PRO MergeData
  l=DataStackList()
  s=XListChoice(LIST=l,/Multiple,TITLE=MergeData)
  ;; chosen datastacks are in l[s]
  stacks=l[s]
  ;; get the number of framse in these stacks
  Nframes=LONARR(N_Elements(stacks))
  For i=1,N_Elements(stacks) DO BEGIN
     p=DataList_SearchName(GetRootP(),stacks[i-1])
     d=(*p).datap
     e=(*d).data 
     N=SIZE(*e)
     IF (N[0] EQ 3) THEN Nframes[i-1]=N[3]
     dimx=N[1] & dimy=N[2]
  END
  A=FLTARR(dimx,dimy,Total(Nframes))
  frame0=0
  For i=1,N_Elements(stacks) DO BEGIN
     p=DataList_SearchName(GetRootP(),stacks[i-1])
     d=(*p).datap
     e=(*d).data
     frame1=Nframes[i-1]+frame0
     A[*,*,frame0:(frame1-1)]=(*e)
     frame0=frame1
  END
  ThrowStack, PTR_NEW(A), "MergedStack" 
END

 
