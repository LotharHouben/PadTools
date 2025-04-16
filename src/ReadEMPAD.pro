FUNCTION ReadEMPADFile, filename, sX, sY, VERBOSE=verbose, SWAPENDIAN=swapendian
;;
;; EMPAD data is stored in a linear sequence of diffraction frames
;;
;; DimX, DimY: dimensions of a single diffraction pattern
;; DimX * DimY * 4: Size of a diffraction pattern in bytes
;;
  error = 1
  IF (GetDebugFlag() NE 1) THEN BEGIN
     CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% ReadEMPAD:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    return, 0
 END
END
  XConsole_PushState
XConsole_Busy
openr, LUN, filename, ERROR = o_err, /GET_LUN
if (o_err NE 0 ) then begin
   printtocon, "error while trying to open file " + filename+ ":" + !ERR_STRING
   return, 0
endif 
IF keyword_set(swapendian) THEN swapendian = 1 ELSE swapendian = 0
  DimX=128L & DimY=130L
  DimZ=LONG(sx)*LONG(sy) 
  offset=0
  ;; LinLen=ULONG64(DimX*DimY*4) ;; length of one diffraction pattern, 128 x 130 x 4byte
  
  APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
  i_data = ASSOC(LUN, FLTARR(DimX,DimY,DimZ),offset)
  if (swapendian EQ 1) then BEGIN
     (*(APtr)) = SWAP_ENDIAN(i_data(0))
  ENDIF ELSE BEGIN
     (*(APtr)) = i_data(0)
  END
  close, LUN
  free_lun, LUN
  XConsole_PopState
  return, APtr
END



FUNCTION XRawDialogEMPAD, sx, sy
XConsole_PushState
XConsole_WaitingForInput
result=0

base = Widget_Base(TITLE='EMPAD subframe selection', /COLUMN) ;; ,DISPLAY_NAME=GetDisplayName()) 

   input1 =  WIDGET_BASE(base, /ROW)
   x_label = CW_Field(input1, TITLE='  scan x,y', XSize=5, Frame=0, Value=MySTRING(sx))
   y_label = CW_Field(input1, TITLE='', XSize=5, Frame=0, Value=MySTRING(sy))
  
   b = CW_BGroup(base, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['accept','cancel'])

  
 WIDGET_CONTROL, base, /REALIZE
 
   do_it_again = 0
   REPEAT BEGIN
      ev = WIDGET_Event(base, /NoWait)
      
      IF (ev.id EQ b) THEN BEGIN
        CASE ev.value OF
            'cancel': BEGIN
               ;; print, ev.id
               do_it_again = NOT(do_it_again)
            END
            'accept': BEGIN
               TMP=''
               WIDGET_Control, x_label, GET_VALUE=TMP
               TMP=TMP(0)
               IF (TMP NE '') THEN BEGIN
                  sx=FIX(FLOAT(TMP))
                  IF (sx LT 0) THEN sx=0
               ENDIF
               TMP=''
               WIDGET_Control, y_label, GET_VALUE=TMP
               TMP=TMP(0)
               IF (TMP NE '') THEN BEGIN
                   sy=FIX(FLOAT(TMP))
                   IF (sy LT 0) THEN sy=0
                ENDIF
               do_it_again = NOT(do_it_again)
               result=1
            END 
            endcase
         END 
     ENDREP UNTIL do_it_again
      Widget_Control, base, /DESTROY
      ;;print, "imccord=", imcoord
      XConsole_PopState
      return, result ;; 1 if accepted, 0 if cancelled
   END 

PRO ReadEMPADData, fname, SWAPENDIAN=swapendian, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, CLOVERWRITE=cloverwrite
 If NOT(GetDebugFlag()) THEN BEGIN
  CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadEMPADData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END
END

 SplitPath, fname, path, filename
 suffix=STRSPLIT(filename, '.',/EXTRACT)
 len=SIZE(suffix,/N_ELEMENTS)
 suffix=suffix[len-1]
 sx=256 & sy= 256
 instrumentname="Titan Titan Cubed 3589"
 voltage=200000.
 cameralength=0.2863
 qsamplingx=1.0
 qsamplingy=1.0
 samplingx=1.0
 samplingy=1.0
 unit='nm'
 qunit='1/nm'
 magnification=57000.
 fraction=0.4
 detx=128 & dety=130
 IF (strupcase(suffix) EQ 'MRC') THEN BEGIN
    PrintToCon, "% ReadEMPADData:  Reading mrc file."
    Printtocon, "%         Expecting 128 x 130 frames."
 END
 IF (strupcase(suffix) EQ 'XML') THEN BEGIN
    ;;IF LMGR(/VM) THEN BEGIN
    ;;   void=DIALOG_MESSAGE('You need an IDL license to access the xml functionality, choose the raw data file for import.', /ERROR)
    ;;   return
    ;; END 
 ;; get parameters and filename from xml file
    h=ReadXMLToHash(fname) ;; testing
    printtocon, "% Reading EMPAD data from "+fname
    printtocon, "% Software Version   : " + h["software_version"]
    printtocon, "% Timestamp          : " + (h["timestamp"])["isoformat"]
    printtocon, "% Raw Data File      : " + (h["raw_file"])["filename"]
    if NOT(h.haskey("type")) THEN h["type"]="scan"
    Case h["type"] of
       "series": BEGIN
          printtocon, "% Type               : Point and Shoot "
          h["pix_x"]=h["series_count"]
          printtocon, "% Series count       : " + MyString(h["pix_x"])
          ;; initialize missing values
          if NOT(h.haskey("source.get_voltage")) then h["source.get_voltage"]='0'
          if NOT(h.haskey("optics.get_cameralength")) then h["optics.get_cameralength"]='0.'
          if NOT(h.haskey("optics.get_magnicication")) then h["optics.get_magnicication"]='0.'
          if NOT(h.haskey("calibrated_diffraction_angle")) then h["calibrated_diffraction_angle"]='0.'
          if NOT(h.haskey("scan_size")) THEN h["scan_size"]='1.0'
          if NOT(h.haskey("optics.get_full_scan_field_of_view")) THEN h["optics.get_full_scan_field_of_view"]='[0.,0.]'
          if NOT(h.haskey("pix_y")) THEN h["pix_y"]='1.'
          ;; if NOT(h.haskey("")) THEN h[""]=0.
       END
       ELSE: BEGIN
          printtocon, "% Type               : Scan "
          printtocon, "% Scan size          : " + MyString(h["pix_x"]) +"x"+MyString(h["pix_y"])
          printtocon, "% Scan Frame Size    : "+ h["optics.get_full_scan_field_of_view"]
          printtocon, "% Frame Fraction     : " + h["scan_size"]
          
       END
    END
    ;; convert to useful data
    sx=LONG(h["pix_x"]) & sy=LONG(h["pix_y"])
    printtocon, "% Voltage            : " +h["source.get_voltage"]
    printtocon, "% Camera Length      : " +h["optics.get_cameralength"] +" m"
    printtocon, "% Magnification      : " +h["optics.get_magnicication"]
    printtocon, "% Exposure time      : " + h["exposure_time"]
    printtocon, "% Post exposure time : " + h["post_exposure_time"]
    rawfilename=""
    origpath=path
    SplitPath, (h["raw_file"])["filename"], origpath, rawfilename
    rawfilename=path+Path_Sep()+rawfilename
    IF valid_num(h["source.get_voltage"],val) THEN voltage=val ELSE printtocon, "% Error reading voltage."  
    IF valid_num(h["optics.get_cameralength"],val) THEN cameralength=val ELSE printtocon, "% Error reading camera length."
    IF keyword_set(cloverwrite) THEN BEGIN
       PrintToCon, "% ReadEMPADData: Camera length overwrite ("+MyString(cloverwrite)+")" 
       cameralength=cloverwrite
    END
    IF valid_num(h["calibrated_diffraction_angle"],val) THEN radperpix=val ELSE printtocon, "% Error reading calibrated diffraction angle."
    IF valid_num(h["optics.get_magnicication"],val) THEN magnification=val ELSE printtocon, "% Error reading magnification."
    IF keyword_set(magnificationoverwrite) THEN BEGIN
       PrintToCon, "% ReadEMPADData: Magnification overwrite ("+MyString(magnificationoverwrite)+")"
       magnification=magnificationoverwrite
    END
    IF valid_num(h["scan_size"],val) THEN fraction=val ELSE printtocon, "% Error reading frame fraction."  
    qsamplingx=radperpix/electron_wavelength(voltage)
    qsamplingy=qsamplingx
    ;; IF valid_num(h["calibrated_pixelsize",val]) THEN
    ;; samplingx=val*1E9 ELSE printtocon, "% Error reading pixel
    ;; size."
    ;;
    ;; get the sampling from the field of view
    ;;
    Case h["type"] of
       "series": BEGIN
          samp=[0.,0.]
          samplingx=h["exposure_time"] ;; *1E-3
          samplingy=0.
          unit='ms'
       END
       ELSE: BEGIN
          samp = JSON_PARSE(h["optics.get_full_scan_field_of_view"]) 
          IF valid_num(samp[0],val) THEN samplingx=val*fraction/sx*1.E9
          IF valid_num(samp[1],val) THEN samplingy=val*fraction/sy*1.E9
          ;; get the samplingy from samplingx, there is only one fraction
          ;; parameter ...
          ;;
          samplingy=samplingx
          unit='nm'
       END
    END
    qunit='1/nm'    
    fname=rawfilename
 END
 ;; search for xml file
 
  ;; create empad object

  
 s=list()
 s.Add, {value:voltage,label:"Voltage ",newrow:0B}
 s.Add, {value:cameralength,label:"Camera length (m)",newrow:1B}
 s.Add, {value:magnification,label:"Magnification ",newrow:1B} 
 s.Add, {value:sx,label:"Scan size x, y",newrow:1B}
 s.Add, {value:sy,label:"",newrow:0B}
 s.Add, {value:fraction,label:"Fraction",newrow:0B}
 s.Add, {value:samplingx,label:"Scan sampling rate x, y",newrow:1B}
 s.Add, {value:samplingy,label:"",newrow:0B}
 s.Add, {value:unit,label:"Unit",newrow:0B}
 s.Add, {value:qsamplingx,label:"Detector sampling rate qx, qy",newrow:1B}
 s.Add, {value:qsamplingy,label:"",newrow:0B}
 s.Add, {value:qunit,label:"Unit",newrow:0B}
 c=list()
 c.Add, {value:1B,label:"Use PadTools magnification calibration",newrow:0B}
 c.Add, {value:1B,label:"Use PadTools camera length calibration",newrow:1B}
 IF NOT(keyword_set(noninteractive)) THEN BEGIN
    ;; inetractive setting
    IF NOT(XMDataChoiceField(s, c, TITLE="Edit Properties") EQ 1) THEN BEGIN 
       printtocon, "% ReadEMPADData: Aborting."
       return
    END
 END ELSE BEGIN
    ;; non-interactive setting
    IF keyword_set(magnificationoverwrite) THEN BEGIN
       ;; overwrite magnification with supplied value
       c[0]={value:1B,label:"Use PadTools magnification calibration",newrow:0B} ;; set value to 1B
       s[2]={value:magnificationoverwrite,label:"Magnification ",newrow:1B} 
    END ELSE BEGIN
       ;; use field of view setting
       c[0] =  {value:0B,label:"Use PadTools magnification calibration",newrow:0B}
    END
 END
 voltage=s[0].value
 sx=s[3].value
 sy=s[4].value
 samplingx=s[6].value
 samplingy=s[7].value
 unit=s[8].value
 qsamplingx=s[9].value
 qsamplingy=s[10].value
 qunit=s[11].value
 cameralength=s[1].value
 magnification=s[2].value
 fraction=s[5].value
 ;;
 ;; Reading calibration data
 ;;
 if ((c[0].value EQ 1) OR (c[1].value EQ 1)) THEN Begin
    printtocon, "% ReadEMPADData: Accessing active calibration table."
    mycal=GetCalibrationObj()
    IF OBJ_VALID(mycal) THEN BEGIN
       if (c[0].value EQ 1) THEN BEGIN
;;          calsampx=mycallist->Query(INSTRUMENT=instrumentname,DEVICE="EMPAD",MODE="STEM",CATEGORY="Magnification",SECTION=voltage,LABEL=magnification,BIN=fraction,DIMX=sx)
          fov=mycal->QuerySelectedCalibration(VALUE=float(magnification))
         
          If (fov NE !NULL) THEN BEGIN
             samplingx=fraction*fov/sx
             samplingy=fraction*fov/sx ;; should be square
             printtocon, "% ReadEMPADData: FOV=" +MyString(fov)+", Sampling="+Mystring(samplingx) 
          END  ELSE BEGIN
             printtocon, "% ReadEMPADData: Error retrieving magnification calibration."
             samplingx=1.
             samplingy=1.
          END
       END
       ;;
       if (c[1].value EQ 1) THEN BEGIN
          label=STRCOMPRESS(STRING(cameralength*1000),/REMOVE_ALL)
          ;; calqsampx=mycallist->Query(INSTRUMENT=instrumentname,DEVICE="EMPAD",MODE="STEM",CATEGORY="Cameralength",SECTION=voltage,LABEL=cameralength*1000,BIN=1.,DIMX=detx)
          fov=mycal->QuerySelectedCalibration(VALUE=float(label), /DIFFRACTION)
          IF (fov NE !NULL) THEN BEGIN
             qsamplingx=fov/detx
             qsamplingy=fov/detx
             printtocon, "% ReadEMPADData: Diffraction FOV="+STRING(fov)+", Sampling:"+STRING(qsamplingx)

          END  ELSE BEGIN
             printtocon, "% ReadEMPADData: Error retrieving camera length calibration."
             qsamplingx=1.
             qsamplingy=1.
          END
       END
       ;; STOP
    END ELSE BEGIN
       printtocon, "% ReadEMPADData: Calibration object not initialized."  
    END
 END
    
 ;;
 printtocon, "% "
 printtocon, "% ReadEMPADData: Reading " + fname + "."

 ;; creating EMPAD Object
  empaddata=obj_new('EMPADObj',fname)
  empaddata.SetScanSize, sx, sy
  empaddata.SetFrameSize, 128L, 130L
  empaddata.SetScanSampling, samplingx, samplingy
  empaddata.SetDetectorSampling, qsamplingx, qsamplingy
  ;; reading data array
  IF (strupcase(suffix) EQ 'MRC') THEN BEGIN
     empaddata.Read, fname
  END ELSE BEGIN
     empaddata.Read, fname
  END 
  ;;
  ;; link data to pointer list, so that it can be displayed
  ;;
  ptr=GetRootP() ;; datalistpointer
  names=DataList_GetNames(ptr)
  Selected=WHERE(names EQ fname)
  IF (Selected EQ -1) THEN BEGIN
     e=DataList_CreateElement(ptr, fname)
  END ELSE BEGIN
     i=2
     REPEAT BEGIN
       altname=fname+ "" + MyString(i)
       Selected=WHERE(names EQ altname)
       i=i+1
     END UNTIL (Selected EQ -1)
     e=DataList_CreateElement(ptr, altname)
  END
  IF PTR_VALID(e) THEN BEGIN
     (*e).data=empaddata.GetDataPointer()
     IF (PTR_VALID((*e).data)) THEN BEGIN
        bin=0.5
        (*e).id=empaddata.GetSetName()
        (*e).SzX=empaddata.GetFrameSize(/X)
        (*e).SzY=empaddata.GetFrameSize(/Y)
        (*e).SzZ=empaddata.GetScanSize(/X)*empaddata.GetScanSize(/Y)
        (*e).xsamp=qsamplingx
        (*e).ysamp=qsamplingy
        (*e).xunit=qunit
        (*e).yunit=qunit
        (*e).voltage=voltage
        (*e).id=fname
        (*e).type=4
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        (*e).extra=empaddata
        (*e).contrastmode="auto"
        (*e).contrastsubmode="minmax" 
        (*e).contrastroi="diff"
        (*e).binning=bin
        (*e).binningx=bin
        (*e).binningy=bin
        CreateWindow, BIN=bin
        TVDisplay
        Update_XTabControl
     End
  END 
END



FUNCTION ReadEMPADDataFromMRC, fname, sX, sY, VERBOSE=verbose, SWAPENDIAN=swapendian
;;
;; EMPAD data is stored in a linear sequence of diffraction frames
;;
;; DimX, DimY: dimensions of a single diffraction pattern
;; DimX * DimY * 4: Size of a diffraction pattern in bytes
;;
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadEMPADDataFromMRC:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return, PTR_NEW()
END
 ;;
 ;; open file to read header
 ;;
openr, LUN, fname, ERROR = o_err, /GET_LUN
if (o_err NE 0 ) then begin
   printtocon, ["% ReadEMPADDataFromMRC: Error while trying to open file " + fname,!ERR_STRING]
   ErrMsg, "% ReadEMPADDataFromMRC: Error while trying to open file " + fname+ ":" + !ERR_STRING
   return, PTR_NEW()
endif 
printtocon, "% "
printtocon, "% ReadEMPADDataFromMRC: Reading " + fname + "."
x=0 & y=0 & z=0
swap=0                           ;; need to swap the bytes?
offset=1024                      ;; starting byte for image data, regular, non-extended header
KnownDataType=[1,2,3,4,6,9]      ;; data type id's known
;; 9 is a fake unsigned byte data type
header=LONARR(56)  ;; read 224 bytes ....
readu, LUN, header
;; check for id and autodetect endian type
;; (you can ignore byte 0 for this and use id byte 1)
;; 0x197=407
IF (NOT(IN(header[3],KnownDataType))) THEN BEGIN
   ;; could it be that we have to swap bytes?
   header=SWAP_ENDIAN(header)
   IF ((IN(header[3],KnownDataType))) THEN BEGIN
      printtocon, "% ReadMRCData: swapping bytes"
      swap=1
   END ELSE BEGIN
      printtocon, "% ReadMRCData: Error - wrong file format or data type"
      close, LUN
      free_lun, LUN
      return, PTR_NEW()
   END
END     
x=FIX(header[0]) & y= FIX(header[1]) & z=FIX(header[2])
CASE header[3] OF
   9: BEGIN
      ;; unsigned byte
      type=1
      printtocon, "% ReadMRCData: warning - reading signed byte" 
   END
   1: BEGIN
      ;; signed integer 16 bit
      type=2
      ;;
   END
   2: BEGIN
      ;; floating point 32 bit
      type=4
      ;;
   END
   3: BEGIN
      ;; complex 16 bit integer
      printtocon, "% ReadMRCData: error - cannot read 16 bit complex integers"
      ErrMsg,  "% ReadMRCData: error - cannot read 16 bit complex integers"
      return, PTR_NEW()
      ;;
   END
   4: BEGIN
      ;; complex floating point 32 bit 
      type=6
      ;;
   END
   6: BEGIN
      ;; 16 bit unsigned integer
      type=12
      ;;
   END
   ELSE: BEGIN
      printtocon, "% ReadEMPADDataFromMRC: error - unknown data type "+MyString(header[3])
      ErrMsg, "% ReadEMPADDataFromMRC: error - unknown data type "+MyString(header[3])
      return, PTR_NEW()
   END
ENDCASE
extendedheadersize=0
extendedheadersize=header[23]
printtocon, "% ReadEMPADDataFromMRC: DimX=" + MyString(header[0]) + " DimY="+MyString(header[1])+ " DimZ="+MyString(header[2]) + " type="+MyString(type)
printtocon, "%              first data byte at " + MyString((offset+extendedheadersize))
;; check labels
labels=BYTARR(800) ;; 
readu, LUN, labels     
nlabelsused=header[55]
printtocon, "% "
printtocon, "% ReadEMPADDataFromMRC: Header Labels."
IF (nlabelsused GE 1) THEN BEGIN
   for k=1, nlabelsused DO BEGIN
      printtocon, "%   "+STRING(labels((k-1)*80:k*80-1))
   END
END
IF keyword_set(fei) THEN BEGIN
   ;; check for FEI copyright string
   ;; 'Fei Company (C)' should appear at byte E0
   copyrightstr=STRING(labels(0:14))
   if NOT(copyrightstr EQ "Fei Company (C)") THEN BEGIN
      ;; check that the header size matches 128 Bytes times 1024
      ;; IF (extendedheadersize NE 131072) THEN BEGIN
      printtocon, "% ReadMRCData: FEI copyright string not found, no FEI header?" 
   END ELSE BEGIN
      feiheader=FLTARR(32,1024)
      readu, LUN, feiheader
      printtocon, "% "
      printtocon, "% ReadMRCData: FEI header information."
      
      IF (z LT 1024) THEN BEGIN
         tmp=(feiheader(13,0:z-1)) 
         tmp=tmp(UNIQ(tmp))/1000
         PRINTTOCON, "%   High Tension (kV)       : "+ MyString(tmp)
         tmp=(feiheader(12,0:z-1)) 
         tmp=tmp(UNIQ(tmp))
         PRINTTOCON, "%   Magnification           : "+ MyString(tmp)
         tmp=(feiheader(14,0:z-1)) 
         tmp=tmp(UNIQ(tmp))
         PRINTTOCON, "%   Binning                 : "+ MyString(tmp)
         tmp=(feiheader(11,0:z-1)) 
         tmp=tmp(UNIQ(tmp))*1E9
         PRINTTOCON, "%   Sampling (nm/pix)       : "+ MyString(tmp)
         tmp=(feiheader(10,0:z-1)) 
         tmp=tmp(UNIQ(tmp))
         PRINTTOCON, "%   Tilt axis rotation (deg): "+ MyString(tmp)
         printtocon, "% " 
         PRINTTOCON, "% ReadEMPADDataFromMRC FEI single image information."
         printtocon, "%   image alpha(deg) beta(deg) defocus(nm) exptime(s) stage x(um) y(um) z(um) image shift x(um) y(um)"
         FMT='(F10.4,:," ")'
         ;; units should be m
         for k=0,(z-1) DO BEGIN
            s=STRING(FORMAT='(I4,:," ")',k)+ $
              STRING(FORMAT=FMT, feiheader[0,k])+ $
              STRING(FORMAT=FMT, feiheader[1,k])+ $
              STRING(FORMAT=FMT, feiheader[7,k]*1E9)+ $
              STRING(FORMAT=FMT, feiheader[8,k])+ $
              STRING(FORMAT=FMT, feiheader[2,k]*1E6)+ $
              STRING(FORMAT=FMT, feiheader[3,k]*1E6)+ $
              STRING(FORMAT=FMT, feiheader[4,k]*1E6)+ $
              STRING(FORMAT=FMT, feiheader[5,k]*1E6)+ $
              STRING(FORMAT=FMT, feiheader[6,k]*1E6)
            PRINTTOCON, s 
         END
         ;;
      END
   END
END  ; fei  
close, LUN
free_lun, LUN
IF NOT (x EQ 128) THEN BEGIN
   PRINTTOCON, "% ReadEMPADDataFromMRC: Size mismatch in x-dimension, x is "+MyString(y)+ " not 128."
   return, PTR_NEW()
END
IF NOT (y EQ 130) THEN BEGIN
   PRINTTOCON, "% ReadEMPADDataFromMRC: Size mismatch in y-dimension, y is "+MyString(y)+ " not 130."
   return, PTR_NEW()
END
IF NOT (y EQ 130) THEN BEGIN
   PRINTTOCON, "% ReadEMPADDataFromMRC: Size mismatch in y-dimension, z is "+MyString(z)+ " not "+MyString(sx)+" x "+MyString(sy)+"."
   return, PTR_NEW()
END
DimX=128L & DimY=130L
DimZ=LONG(sx)*LONG(sy) 
data=Read3DData(fname, (offset+extendedheadersize), DimX, DimY, DimZ, type, SWAPENDIAN=swap)
IF keyword_set(swapendian) THEN swapendian = 1 ELSE swapendian = 0
return, PTR_NEW(FLTARR(data))
END 
