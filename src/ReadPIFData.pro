;; READ PIF Data
;;
;; DEPENDENCIES: CleanString, Printtocon, ProgressBar


FUNCTION PIFIdentify, lun
;; returns -1 if identification fails
;; returns swap information otherwise, 0=now byte swap, 1=byteswap
little_endian = (BYTE(1, 0, 1))[0] ;; your machines endian
POINT_LUN, lun, 28
ALong=0L
READU, lun, along
IF ((along EQ 0) XOR (little_endian EQ 1)) THEN BEGIN
   swapendian=1 
   Printtocon, "% PIFIdentfy: Endian information hints to byte swap" 
END ELSE swapendian=0
;; now check data type
KnownDataType=[0,1,2,3,4,5,6,7,8,9,10,20,21,22,31,32,88,97] ;; data type id's known
POINT_LUN, lun, 80
ALong=0L
READU, lun, along
IF (NOT(IN(along,KnownDataType))) THEN BEGIN
   ;; could it be that we have to swap bytes?
   along=SWAP_ENDIAN(along)
   IF ((IN(along,KnownDataType))) THEN BEGIN
      print, "% PIFIdentify: Data type identifier hints to byte swap."
      swap=1
   END ELSE BEGIN
      printtocon, "% PIFIdentify: Wrong file format or data type."
      return, -1
   END
END ELSE BEGIN
   swap=0
END
printtocon, "% PIFIdentify: PIF file recognized."
return, swap
END       



FUNCTION ReadPIFHeader, lun
  ;; Read the 512 byte header
  res=0
  ;;
  KnownDataType=[0L,1L,2L,9L,10L] ;; data type id's known
  ;; 
  s=Create_STRUCT( $
  'numImages', 0L, $
  'nx', 0L, $
  'ny', 0L, $
  'nz', 0L, $
  'mode', 0L, $
  'htype', 0L, $
  'RealScaleFactor', 0., $
  'creator', STRING('', FORMAT = '(A32)') $  
;;  'xlength', 0., $  
;;  'ylength', 0., $  
;;  'zlength', 0., $  
;;  'alpha', 0., $  
;;  'beta', 0., $  
;;  'gamma', 0., $ 
;;  'timestamp', BYTARR(32), $
;;  'pixelsize', 0L $
  )
  along=0L
  ;;
  POINT_LUN, lun, 9
  READF, lun, FORMAT = '(A16)', w
  s.RealScaleFactor=w
  ;;
 POINT_LUN, lun, 24
 readu, LUN, along
 s.numImages=along
 POINT_LUN, lun, 32
 cs=STRING('', FORMAT = '(A32)')
 READF, lun, FORMAT = '(A32)', cs
 s.creator=cs
 POINT_LUN, lun, 64
 readu, LUN, along
 s.htype=along
 readu, LUN, along
 s.nx=along
 readu, LUN, along
 s.ny=along
 readu, LUN, along
 s.nz=along
 readu, LUN, along
 s.mode=along
 ;; EM data type e.g.
 ;; 0=byte
 ;; 1=Int
 ;; 2="floatInt"*4 (what the heck is that??)= LONG?
 ;; 3=complex int*2 (what??)
 ;; 4=complex floatint*4 (what the heck is that??)= LONG?
 ;; 5=structure factors
 ;; 6=boxed data
 ;; 7=floatint*2 (???)
 ;; 8=complex floatint*2
 ;; 9=float*4
 ;; 10=complex float*4
 ;; 20=MAP floatint*2
 ;; ...
 ;; check mode
 IF (NOT(IN(s.mode,KnownDataType))) THEN BEGIN
    printtocon, "% ReadPFDataHeader: Unsupported data type, data type is "+CleanString(s.mode)+"."
    return, 0 ;; return failure
 END
 return, PTR_NEW(s)
END


FUNCTION ReadPIFDataHeader, lun, pos
  ;; Read the 512 byte data block header
  res=0
  ;;
  KnownDataType=[0,1,2,9,10] ;; data type id's known
  ;; 
  s=Create_STRUCT( $
  'nx', 0L, $
  'ny', 0L, $
  'nz', 0L, $
  'mode', 0L $
;;  'xlength', 0., $  
;;  'ylength', 0., $  
;;  'zlength', 0., $  
;;  'alpha', 0., $  
;;  'beta', 0., $  
;;  'gamma', 0., $ 
;;  'timestamp', BYTARR(32), $
;;  'pixelsize', 0L $
  )
 h=LONARR(12)   ;
 POINT_LUN, lun, pos
 readu, LUN, h
 s.nx=h[0] & s.ny=h[1] & s.nz=h[2]
 s.mode=h[3]
 ;; EM data type e.g.
 ;; 0=byte
 ;; 1=Int
 ;; 2="floatInt"*4 (what the heck is that??)= LONG?
 ;; 3=complex int*2 (what??)
 ;; 4=complex floatint*4 (what the heck is that??)= LONG?
 ;; 5=structure factors
 ;; 6=boxed data
 ;; 7=floatint*2 (???)
 ;; 8=complex floatint*2
 ;; 9=float*4
 ;; 10=complex float*4
 ;; 20=MAP floatint*2
 ;; ...
 ;; check mode
 IF (NOT(IN(s.mode,KnownDataType))) THEN BEGIN
    printtocon, "% ReadPFDataHeader: Unsupported data type, data type is "+CleanString(s.mode)+"."
    return, 0 ;; return failure
 END
 return, PTR_NEW(s)
END



FUNCTION ReadPIFImages, lun, h, fname
;; h=header data
     CASE (*h).mode OF
        0: BEGIN
           ;; unsigned byte
           type=1
           print, "% ReadPIFImages: Data type is byte." 
           bytesz=1
        END
        1: BEGIN
           ;; signed integer 16 bit
           type=2
           print, "% ReadPIFImages: Data type is integer." 
           bytesz=2
           ;;
        END
        2: BEGIN
           ;; signed integer 32 bit
           type=3
           print, "% ReadPIFImages: Data type is long integer." 
           print, "%     Data type needs scaling to float?" 
           bytesz=4
           ;;
        END
        9: BEGIN
           ;; floating point 32 bit
           type=4
           print, "% ReadPIFImages: Data type is floating point." 
           bytesz=4
           ;;
        END
        10: BEGIN
           ;; complex floating point 32 bit 
           type=6
           print, "% ReadPIFImages: Data type is complex floating point." 
           bytesz=8
           ;;
        END
        ELSE: BEGIN
           printtocon, "% ReadPIFImages: Error - unknown data type "+CleanString((*h).mode)
           return, 0
        END
     ENDCASE
     IF ((*h).htype EQ 1) THEN BEGIN
        Printtocon, "% ReadPIFImages: Data file contains a 3D array. "
        DimX=(*h).nx & DimY=(*h).ny 
        ;; create the Array
        ptr=GetRootP() ;; get the root pointer of the list of all data stacks
        IF (NOT(PTR_VALID(ptr))) THEN BEGIN 
           ;; there is no root 
           print, "% ReadPIFData: Root pointer is invalid" 
           return, 0
        END
        ;; Create a new container for the data stack, the pointer to this ;;
        ;; container - pp - will be appended to the existing list and
        ;;             appear ;; automatically in the 'Data Inspector' 
        pp=DataList_CreateElement(ptr, fname) ;; set the dimensions of the data array
        (*pp).SzX=DimX & (*pp).SzY=DimY & (*pp).SzZ=(*h).numImages 
        ;; set the data type
        (*pp).type=type
        ;; allocate memory and get pointer ppd to the array
        ppd=(Data_GetEmptyArrayP(pp)) 
        IF NOT(PTR_VALID(ppd)) THEN BEGIN
           print, "% ReadPIFImages: Failed to create data array"
           return, 0
        END
     ;; store array pointer in data container
     (*pp).data=ppd

        IF PTR_VALID(ppd) THEN BEGIN
           imsz=(*h).nx*(*h).ny*bytesz
           InitProgressBar, MINV=[0], MAXV=[(*h).numImages], TEXT=["Reading image nr."]
           tmpArr=REFORM((*ppd)[*,*,0])
           for I=0, ((*h).numImages)-1 do begin
              ProgressBarSet, 0, I, TEXT=CleanString(I)
;;                 s=fname +":"+ STRING(I)
              offset=512L+(I+1L)*512L+LONG(I)*imsz
              ;; print, CleanString(I) +":"+CleanString(offset)
              point_lun, lun, offset
              READU, lun, tmpArr
              (*ppd)[*,*,I]=tmpArr
              ;; STOP
              ;; AutoContrast, q
           endfor
           DestroyProgressBar
           (*pp).zcoord=2
           (*pp).slice=FIX((*pp).SzZ/2)
           (*pp).contrastmode="auto"
           CreateWindow
           TVDisplay
           Update_XTabControl
           XConsole_Idle
           return, 1
        END 
     END  ELSE BEGIN
;; read sequence of single images
        Printtocon, "% ReadPIFImages: Data file contains a sequence of single images. "
        Printtocon, "%    Reading a sequence if images is supported yet, returning. "
        return, 0
     END
  END    


;;FUNCTION ReadPIFData, APtr, fname, x, y, z, mtype, VERBOSE=verbose, XSAMP=xsamp, YSAMP=ysamp, ZSAMP=zsamp
FUNCTION ReadPIFData, fname
IF NOT(GetDebugFlag()) THEN BEGIN
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% ReadPIFData:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     APtr=0
     XConsole_PopState  
     CATCH, /Cancel
     return, 0
  END
END
 XConsole_PushState
 XConsole_Busy
 res=0
 PrintToCon, "% ReadPIFData: Opening "+fname+"." 
 IF (fname NE '') THEN BEGIN
     ;;
     ;; open file to read header
     ;;
     openr, LUN, fname, /GET_LUN
     swapres=PIFIdentify(lun)
     IF (swapres GE 0) THEN BEGIN
        IF (swapres GE 1) THEN BEGIN
           close, LUN
           openr, LUN, fname, /GET_LUN, /SWAP_ENDIAN
        END
        h=ReadPifHeader(lun)
        IF PTR_VALID(h) THEN BEGIN
           res=ReadPIFImages(lun,h, fname)
           PTR_FREE, h
        END
     END
     close, LUN
     free_lun, LUN
     return, res
  End  
 XConsole_PopState  
END    


PRO PIFTest
;; get file name for export
swapend=0
crgrpd=1
type=0
filter="*.pif"
ftypes=PTR_NEW(["PIF"])
files = rmd_pickfile(filter_in = filter, $
        path=GetWorkingDir(), $
        get_path = out_path,          $
        cancelled = cancelled,        $
        swapen = swapend,             $
        type = type,             $
        ftypes = ftypes,     $
        crgrp = crgrpd,             $
        /multiple, $
        /open)
if (cancelled EQ 1) then begin
   printtocon, "% PIFTest::ExportData: Data export cancelled."
   return
END
IF NOT(n_elements(files) GE 1) THEN BEGIN
   return
END
  fname=files[0]
  p=ReadPIFData(fname)
END
