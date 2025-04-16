
PRO ReadRaw3DData, fname, swap
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadRaw3DData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END
  x=128L & y=128L & z=128L & type=2 & offset=8192 & imcoord=0
  result=XRawDialog3D(x, y, z,type, offset, imcoord)
  IF (result EQ 0) then return
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
     (*e).data=Read3DData(fname, offset, x, y, z, type, SWAPENDIAN=swap)
     IF (PTR_VALID((*e).data)) THEN BEGIN
        (*e).SzX=x & (*e).SzY=y & (*e).SzZ=z
        (*e).id=fname
        (*e).type=type
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        CreateWindow
        TVDisplay
        Update_XTabControl
     End
  END 
END  


PRO SaveBinData, fname, FORMAT=format, AUTOSAVE=autosave, OVERWRITE=overwrite, NOPROGRESSBAR=noprogressbar
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% SaveBinData:  Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
 ;; 
 IF NOT(keyword_set(noprogressbar)) THEN InitProgressBar, MINV=1, MAXV=2, TEXT="Saving Data"
  IF (FileIsWriteable(fname, OVERWRITE=overwrite) NE 1) THEN BEGIN
     printtocon, "% SaveBinData: file "+ fname + " is not writeable"
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
     ErrMsg, "% SaveBinData: file "+ fname + " is not writeable"
     return
  END
  ptr=GetRootP() ;; datalistpointer
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% SaveBinData: Root pointer is invalid"
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
     ErrMsg, "% SaveBinData: Root pointer is invalid"
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% SaveBinData: Current stack pointer is invalid"
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
     ErrMsg, "% SaveBinData: Current stack pointer is invalid"
     return
  END
  e=(*c).datap
  IF PTR_VALID(e) THEN BEGIN
     ;;
     ;; open file to write header
     ;;
     openw, LUN, fname, ERROR = o_err, /GET_LUN
     if (o_err NE 0 ) then begin
	printtocon, "% SaveBinData: error while trying to open file " + fname+ ":" + !ERR_STRING
        IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
        ErrMsg,  "% SaveBinData: error while trying to open file " + fname+ ":" + !ERR_STRING
	return
     endif 
     x=(*e).SzX & y=(*e).SzY & z=(*e).SzZ
     CASE format OF 
        "mrc": BEGIN
                header=LONARR(256)
                header[0]=LONG(x) & header[1]=LONG(y) & header[2]=LONG(z)
                header[23]=0 ;; no extended header
                CASE (*e).type OF
                   1: BEGIN
                      ;; IDL unsigned byte
                      ;; has to be converted into signed integer, mrc
                      ;; type 1
                      header[3]=LONG(9)
                      printtocon, "% SaveBinData(MRC): saving unsigned byte data, not conform with mrc data types"
                      A=(*(*e).data)
                   END
                   2: BEGIN
                      ;; IDL signed integer
                      header[3]=LONG(1)
                      A=(*(*e).data)
                   END
                   3: BEGIN
                      ;; IDL Long Integer
                      ;; has to be converted into float, mrc
                      ;; type 2
                      header[3]=LONG(2)
                      printtocon, "% SaveBinData(MRC): warning - converting 32 but integer 32 bit float to conform with mrc data types"
                      A=FLOAT((*(*e).data))
                   END
                   4: BEGIN
                      ;; IDL 32 bit float
                      header[3]=LONG(2)
                      A=(*(*e).data)
                   END
                   5: BEGIN
                      ;; IDL 32 bit float
                      header[3]=LONG(2)
                      printtocon, "% SaveBinData(MRC): warning - converting 64 bit float to 32 bit float to conform with mrc data types"
                      A=FLOAT(*(*e).data)
                   END
                   6: BEGIN
                      ;; IDL 64 bit complex float
                      header[3]=LONG(4)
                      A=(*(*e).data)
                   END
                   9: BEGIN
                      ;; IDL 128 bit complex float
                      header[3]=LONG(4)
                      printtocon, "% SaveBinData(MRC): warning - converting 128 bit complex float to 64 bit complex float to conform with mrc data types"
                      A=COMPLEX(*(*e).data)
                   END
                   12: BEGIN
                      ;; IDL unsigned integer
                      header[3]=LONG(6)
                      A=(*(*e).data)
                   END
                   13: BEGIN
                      ;; IDL unsigned long integer
                      header[3]=LONG(2)
                      printtocon, "% SaveBinData(MRC): warning - converting 64 bit unsigned long integer to 32 bit float to conform with mrc data types"
                      A=FLOAT(*(*e).data)
                   END
                   ELSE: BEGIN
                      printtocon, "% SaveBinData: error - unknown IDL data type"
                      close, LUN
                      free_lun, LUN
                      IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
                      ErrMsg, "% SaveBinData: error - unknown IDL data type"
                      return
                   END
                END
                writeu, LUN, header 
                writeu, LUN, A
        END
        "feimrc": BEGIN
                header=LONARR(56)
                header[0]=LONG(x) & header[1]=LONG(y) & header[2]=LONG(z)
                header[23]=1024L+128*1024L;; offset for the first byte
                header[55]=1L ;; one label
                ;; label is 
                labels=BYTARR(800)  ;; 
                labels(*)=0
                labels(0:29)=[70,101,105,32,67,111,109,112,97,110,121,32,40,67,41,32,67,111,112,121,114,105,103,104,116,32,50,48,48,51]
                ;; means "Fei Company (C) Copyright 2003"
                CASE (*e).type OF
                   1: BEGIN
                      ;; IDL unsigned byte
                      ;; has to be converted into signed integer, mrc
                      ;; type 1
                      header[3]=LONG(1)
                      printtocon, "% SaveBinData(MRC): saving signed byte data, not conform with mrc data types"
                      A=FIX((*(*e).data),TYPE=2)
                   END
                   2: BEGIN
                      ;; IDL signed integer
                      header[3]=LONG(1)
                      A=(*(*e).data)
                   END
                   3: BEGIN
                      ;; IDL Long Integer
                      ;; has to be converted into float, mrc
                      ;; type 2
                      header[3]=LONG(2)
                      printtocon, "% SaveBinData(MRC): warning - converting 32 but integer 32 bit float to conform with mrc data types"
                      A=FLOAT((*(*e).data))
                   END
                   4: BEGIN
                      ;; IDL 32 bit float
                      header[3]=LONG(2)
                      A=(*(*e).data)
                   END
                   5: BEGIN
                      ;; IDL 32 bit float
                      header[3]=LONG(2)
                      printtocon, "% SaveBinData(MRC): warning - converting 64 bit float to 32 bit float to conform with mrc data types"
                      A=FLOAT(*(*e).data)
                   END
                   6: BEGIN
                      ;; IDL 64 bit complex float
                      header[3]=LONG(4)
                      A=(*(*e).data)
                   END
                   9: BEGIN
                      ;; IDL 128 bit complex float
                      header[3]=LONG(4)
                      printtocon, "% SaveBinData(MRC): warning - converting 128 bit complex float to 64 bit complex float to conform with mrc data types"
                      A=COMPLEX(*(*e).data)
                   END
                   12: BEGIN
                      ;; IDL unsigned integer
                      header[3]=LONG(6)
                      A=(*(*e).data)
                   END
                   13: BEGIN
                      ;; IDL unsigned long integer
                      header[3]=LONG(2)
                      printtocon, "% SaveBinData(MRC): warning - converting 64 bit unsigned long integer to 32 bit float to conform with mrc data types"
                      A=FLOAT(*(*e).data)
                   END
                   ELSE: BEGIN
                      printtocon, "% SaveBinData: error - unknown IDL data type"
                      close, LUN
                      free_lun, LUN
                      IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
                      ErrMsg, "% SaveBinData: error - unknown IDL data type"
                      return
                   END
                END
                writeu, LUN, header
                writeu, LUN, labels
                ;; now the extended header
                ;; 
                imageinfo=FLTARR(32)
                FOR m=1,1024 DO BEGIN
                   writeu, LUN, imageinfo
                END
                writeu, LUN, A
        END
        "raw": BEGIN
                writeu, LUN, (*(*e).data)
        END
        ELSE:  BEGIN
           printtocon, "% SaveBinData: error - unknown save format"
           close, LUN
           free_lun, LUN
           IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
           ErrMsg, "% SaveBinData: error - unknown save format"
           return
        END
     END 
     close, LUN
     free_lun, LUN
     printtocon, "% SaveBinData: exported data"
     printtocon, "%              format  ="+format
     printtocon, "%              filename="+fname
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
  END 
  IF NOT(keyword_set(autosave)) THEN BEGIN
     ;; Save command has been successful 
     ;; change data names and update display
     (*c).name=fname
     (*e).id=fname
  END
END  




PRO ReadMRCData, fname, FEI=fei, ERROR=error
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadMRCData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END
  error=1
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
     ;;
     ;; open file to read header
     ;;
     openr, LUN, fname, ERROR = o_err, /GET_LUN
     if (o_err NE 0 ) then begin
	printtocon, ["% ReadMRCData: Error while trying to open file " + fname,!ERR_STRING]
        ErrMsg, "% ReadMRCData: Error while trying to open file " + fname+ ":" + !ERR_STRING
	return
     endif 
     printtocon, "% "
     printtocon, "% ReadMRCData: Reading " + fname + "."
     x=0L & y=0L & z=0L
     swap=0   ;; need to swap the bytes?
     offset=1024       ;; starting byte for image data, regular, non-extended header
     KnownDataType=[1,2,3,4,6,9] ;; data type id's known
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
           return
        END
     END     
     x=LONG(header[0]) & y= LONG(header[1]) & z=LONG(header[2])
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
           return
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
           printtocon, "% ReadMRCData: error - unknown data type "+MyString(header[3])
           ErrMsg, "% ReadMRCData: error - unknown data type "+MyString(header[3])
           return
        END
     ENDCASE
     extendedheadersize=0
     extendedheadersize=header[23]
     printtocon, "% ReadMRCData: DimX=" + MyString(header[0]) + " DimY="+MyString(header[1])+ " DimZ="+MyString(header[2]) + " type="+MyString(type)
     printtocon, "%              first data byte at " + MyString((offset+extendedheadersize))
     ;; check labels
     labels=BYTARR(800) ;; 
     readu, LUN, labels     
     nlabelsused=header[55]
     printtocon, "% "
     printtocon, "% ReadMRCData: Header Labels."
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
              PRINTTOCON, "% ReadMRC: FEI single image information."
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
     END  
     close, LUN
     free_lun, LUN
     (*e).data=Read3DData(fname, (offset+extendedheadersize), x, y, z, type, SWAPENDIAN=swap)
     IF (PTR_VALID((*e).data)) THEN BEGIN
        (*e).SzX=x & (*e).SzY=y & (*e).SzZ=z
        (*e).id=fname
        (*e).type=type
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        CreateWindow
        TVDisplay
        Update_XTabControl
        error=0
     End
  END 
END 
