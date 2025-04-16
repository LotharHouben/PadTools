
PRO SaveMTiffData, fname, AUTOSAVE=autosave, OVERWRITE=overwrite, NOPROGRESSBAR=noprogressbar
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% SaveMTiffData:  Fatal error "
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
     printtocon, "% SaveMTiffData: file "+ fname + " is not writeable"
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
     ErrMsg, "% SaveMTiffData: file "+ fname + " is not writeable"
     return
  END
  ptr=GetRootP() ;; datalistpointer
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% SaveMTiffData: Root pointer is invalid"
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
     ErrMsg, "% SaveMTiffData: Root pointer is invalid"
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% SaveMTiffData: Current stack pointer is invalid"
     IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
     ErrMsg, "% SaveMTiffData: Current stack pointer is invalid"
     return
  END
  e=(*c).datap
  IF PTR_VALID(e) THEN BEGIN
     x=(*e).SzX & y=(*e).SzY & z=(*e).SzZ     
     CASE (*e).type OF
        1: BEGIN
           ;; IDL unsigned byte
           printtocon, "% SaveMTiffData: saving Byte data" 
           Write_Tiff, fname, REFORM((*(*e).data)[0,*,*]), COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM((*(*e).data)[j,*,*]), COMPRESSION=1, /APPEND
           END
        END
        2: BEGIN
           ;; IDL signed integer
           printtocon, "% SaveMTiffData: saving signed 16bit integer data" 
           Write_Tiff, fname, REFORM((*(*e).data)[0,*,*]), /SHORT, /SIGNED, COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM((*(*e).data)[j,*,*]), /SHORT, /SIGNED, COMPRESSION=1, /APPEND
           END
        END
        3: BEGIN
           ;; IDL Long Integer
           printtocon, "% SaveMTiffData: saving signed 32bit long integer data" 
           Write_Tiff, fname, REFORM((*(*e).data)[0,*,*]), /LONG, /SIGNED, COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM((*(*e).data)[j,*,*]), /LONG, /SIGNED, COMPRESSION=1, /APPEND
           END
        END
        4: BEGIN
           ;; IDL 32 bit float
           printtocon, "% SaveMTiffData: saving 32bit floating point data" 
           Write_Tiff, fname, REFORM((*(*e).data)[0,*,*]), /FLOAT, COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM((*(*e).data)[j,*,*]), /FLOAT, COMPRESSION=1, /APPEND
           END
        END
        5: BEGIN
           ;; IDL 64 bit float
           printtocon, "% SaveMTiffData: converting 64bit to 32bit floating point data" 
           Write_Tiff, fname, REFORM(FLOAT((*(*e).data)[0,*,*])), /FLOAT, COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM(FLOAT((*(*e).data)[j,*,*])), /FLOAT, COMPRESSION=1, /APPEND
           END
        END
        6: BEGIN
           ;; IDL 64 bit complex float
           printtocon, "% SaveMTiffData: converting 2x32bit complex data to 32bit floating point amplitude data" 
           Write_Tiff, fname, REFORM(FLOAT(Amp((*(*e).data)[0,*,*]))), /FLOAT, COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM(FLOAT(Amp((*(*e).data)[j,*,*]))), /FLOAT, COMPRESSION=1, /APPEND
           END
        END
        9: BEGIN
           ;; IDL 128 bit complex float
           printtocon, "% SaveMTiffData: converting 2x64bit complex data to 32bit floating point amplitude data" 
           Write_Tiff, fname, REFORM(FLOAT(Amp((*(*e).data)[0,*,*]))), /FLOAT, COMPRESSION=1
           For j=1,(x-1) Do Begin
              Write_Tiff, fname, REFORM(FLOAT(Amp((*(*e).data)[j,*,*]))), /FLOAT, COMPRESSION=1, /APPEND
           END
        END
        ELSE: BEGIN
           printtocon, "% SaveMTiffData: error - unknown IDL data type"
           IF NOT(keyword_set(noprogressbar)) THEN DestroyProgressBar
           ErrMsg, "% SaveMTiffData: error - unknown IDL data type"
           return
        END 
     END   
     printtocon, "% SaveMTiffData: exported data"
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




PRO ReadTiffData, fname
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadTiffData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END

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
     s=0
     ok = QUERY_TIFF(fname,s)  
     IF NOT(ok) THEN BEGIN 
        printtocon, "% ReadTiffData: error while trying to open file " + fname+ ":" + !ERR_STRING 
        return
     ENDIF
     printtocon, "% ReadTiffData:  TIFF header information for " + fname
     DimZ=s.NUM_IMAGES
     printtocon, "%    Number of images : " + MyString(DimZ)
     DimX=s.Dimensions[0]
     DimY=s.Dimensions[1]
     printtocon, "%    Dimensions       : " + MyString(DimX)+ " x " + MyString(DimY)
     type=s.PIXEL_TYPE
     printtocon, "%    Data type        : " + MyString(s.PIXEL_TYPE)
     printtocon, "%    Date             : " + s.DATE_TIME
     printtocon, "%    Name             : " + s.DOCUMENT_NAME
     printtocon, "%    Description      : " + s.DESCRIPTION
     printtocon, "% ReadTiffData: Opening " + fname
     
     CASE type OF
        0: BEGIN
           ;; unsigned byte
           type=1
           printtocon, "% ReadTiffData: warning - reading signed byte into usigned IDL data type" 
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
           printtocon, "% ReadTiffData: error - cannot read 16 bit complex integers"
           ErrMsg,  "% ReadTiffData: error - cannot read 16 bit complex integers"
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
           printtocon, "% ReadTiffData: error - unknown data type "+MyString(type)
           ErrMsg, "% ReadTiffData: error - unknown data type "+MyString(type)
           return
        END
     ENDCASE
     ;; create a new data stack
     case type of
	1 : BEGIN
             APtr=PTR_NEW(BYTARR(DimX,DimY,DimZ))
            END
	2 : BEGIN
             APtr=PTR_NEW(INTARR(DimX,DimY,DimZ))
            END
	3 : BEGIN
             APtr=PTR_NEW(LONARR(DimX,DimY,DimZ))
            END
	4 : BEGIN
             APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
            END
	5 : BEGIN
             APtr=PTR_NEW(DBLARR(DimX,DimY,DimZ))
            END
	6 : BEGIN
             APtr=PTR_NEW(COMPLEXARR(DimX,DimY,DimZ))
            END
	9 : BEGIN
             APtr=PTR_NEW(DCOMPLEXARR(DimX,DimY,DimZ))
            END
	12 : BEGIN
             APtr=PTR_NEW(UINTARR(DimX,DimY,DimZ))
            END
	else: BEGIN
                print, "% ReadMTiff: invalid data type " + MyString(type)
                return
              END
     endcase
     (*e).data=APtr
     IF (PTR_VALID((*e).data)) THEN BEGIN
        (*e).SzX=DimX & (*e).SzY=DimY & (*e).SzZ=DimZ
        (*e).id=fname
        (*e).type=type
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        ;; read data from tiff file
        FOR i=0,DimZ-1 DO BEGIN
           (*(*e).data)[*,*,i]=READ_TIFF(fname,IMAGE_INDEX=i)
        END
        CreateWindow
        TVDisplay
        Update_XTabControl
     End
  END 
END 

