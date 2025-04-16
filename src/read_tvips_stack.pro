; $ 
;
; Copyright (C) IHP-Microelectronics, Frankfurt(Oder), Germany
;
; NAME:
;     read_tvips
;
; PURPOSE:
;     Read image in EM (TIETZ) format from disk and return
;     returns error status: 0 = o.k., 1=error 
; 
; CATEGORY:
;     I/O routine
;
; CALLING SEQUENCE:
;     verbose=1 & swapendian=1       
;     print, read_tvips(p, VERBOSE=verbose, SWAPENDIAN=swapendian)
;
; INPUTS:
;     p: a pointer to a data structure
;
; KEYWORD PARAMETERS:
;     VERBOSE: routine prints head and comment of the image when the keyword set
;     SWAPENDIAN: if set to 1, a big endian - little endian conversion is performed       
;     Type: format 1=new 0=old, the header information depends on this
;
; OUTPUTS:
;     head: array of 4 integers - see COMMENTS section
;     comment: array of 80 characters - see COMMENTS section
;     fdata: array of 40 floats - see COMMENTS section
;
; EXAMPLE:
;     no example
;
; ALGORITHM:
;     straightforward, no description
;
; SIDE EFFECTS:
;     none (as far as I know)
;
; RESTRICTIONS:
;     none
;
; COMMON BLOCKS:
;     none
;
; EXTERNAL COMPONENTS:
;     none
;
; COMMENTS:
;     File Format(EM-Data):
;             unknown: byte
;             format: byte     ; 1 => new Tietz format, 0 => old format
;             type:  int       ; 1=byte, 2=int, 4=long 5=float 8=compl
;             SzX: long
;             SzY: long
;             inum:  long      ; number of images in the file
;             comm:  char(80)  ; zero terminated comment string
;             (96 bytes up to here)
;             fdata: float(40) ; float data for old Tietz format, 4 bytes each
;             fdata: float(104) ; float data for old Tietz format, 4 bytes each
;             idata: type      ; image data (XSIZE*YSIZE pixels)
;
; REFERENCE:
;     ? TVIPS manual ?
;
; MODIFICATION HISTORY:
;     written by:   W.-D. Rau, 19.08.1998
;     modified :    P.Formanek, 28.11.2000
;                   added output parameters, added LUN request
;                   L. Houben, 16.02.2001
;                   modification to conform with ImageProcessing Routines        
;                   L. Houben, 12.04.2002
;                   reading new and old Tietz format automatically
;                   automatic endian detection
; $ ----------------------------------------------------------------------------

FUNCTION read_tvips_stack, e, fname, x, y, z, dtype, SWAPENDIAN=swapendian, VERBOSE=verbose
;assign structure to variables


  nfiles = N_Elements(fname)

  unknown = 0B & format = 0B & type = 0 & SzX = 0L & SzY = 0L & inum = 0L
  comm = BYTARR(80)
  
  ipos=0
  swapendian=0
  

  For k=0,(nfiles-1) DO BEGIN 
                                ;get LUN and open file
     openr, LUN, fname[k], ERROR = o_err, /GET_LUN
     if (o_err NE 0 ) then begin
        print, "error while trying to open file " + fname + ":" + !ERR_STRING
        return, 0
     endif ELSE BEGIN
        
        ;; get header data from first file
        if (k EQ 0) THEN BEGIN 
                                ;read header data from file
           readu, LUN, unknown
           readu, LUN, format
           readu, LUN, type
           readu, LUN, SzX		
           readu, LUN, SzY
           readu, LUN, inum				
           readu, LUN, comm     ; 80 chars comment
           if (format EQ 1) THEN BEGIN
              fdat = LONARR(104) 
              ipos=512
           ENDIF ELSE BEGIN 
              fdat=FLTARR(40)
              ipos=256
           ENDELSE
           readu, LUN, fdat     ; 40 or 104 float descriptors
           
           if (type GT 256) THEN swapendian=1
           
           if (swapendian EQ 1) then BEGIN
              if KEYWORD_SET(verbose) then Print, "swapping endian"
              unknown=SWAP_ENDIAN(unknown)
              type=SWAP_ENDIAN(type)
              SzX=SWAP_ENDIAN(SzX)
              SzY=SWAP_ENDIAN(SzY)
              inum=SWAP_ENDIAN(inum)
              comm=SWAP_ENDIAN(COMM) 
              fdat=SWAP_ENDIAN(fdat)
           ENDIF
           
                                ;print information about image
           if KEYWORD_SET(verbose) then begin
              print
              print, 'Filename: ' + fname
              print, 'Header(type, format, sizeX, sizeY, image number): ' + String(type)+ ", " + String(format) +", " + String(SzX)+ ", " + String(SzY) + ", " + String(inum)
              print, 'Comment: '+String(comm)
;	print, 'Float data: '
              print, "Magnification:       " + String(fdat(3))
              print, "Post col. mag.:      "+ String(fdat(4))
              print, "exposure time (ms):  "+ String(fdat(5))
              print, "defocus (0.1nm):     "+ String(fdat(10))
              print, "CCD Pix size [um]:   "+ String(fdat(23))
              print, "Image pix size [nm]: "+ String(1000*(fdat(23)/(fdat(3)*fdat(4))))
              print
           endif
           
           x=SzX & y=SzY & z=nfiles
           IF (K EQ 0) THEN BEGIN
              case type of
                 1 : BEGIN
                    dtype=1
                    (*e).data=PTR_NEW(BYTARR(SzX,SzY,nfiles))
                 END
                 2 : BEGIN
                    dtype=2
                    (*e).data=PTR_NEW(INTARR(SzX,SzY,nfiles))
                 END
                 4 : BEGIN
                    dtype=3
                    (*e).data=PTR_NEW(LONARR(SzX,SzY,nfiles))
                 END
                 5 : BEGIN
                    dtype=4
                    (*e).data=PTR_NEW(FLTARR(SzX,SzY,nfiles))
                 END
                 8 : BEGIN
                    dtype=6
                    (*e).data=PTR_NEW(COMPLEXARR(SzX,SzY,nfiles))
                 END
                 else: BEGIN
                    print, "error: invalid data type " + String(type)
                    close, LUN
                    free_lun, LUN
                    return, 0 
                 END
              endcase
           END
           
        endif   
        
        case type of
           1 : i_data = ASSOC(LUN, BYTARR(SzX, SzY), ipos)	
           2 : i_data = ASSOC(LUN, INTARR(SzX, SzY), ipos)	
           4 : i_data = ASSOC(LUN, LONARR(SzX, SzY), ipos)	
           5 : i_data = ASSOC(LUN, FLTARR(SzX, SzY), ipos)	
           8 : i_data = ASSOC(LUN, COMPLEXARR(SzX, SzY), ipos)	
           else: BEGIN
              print, "error: invalid data type " + String(type)
              close, LUN
              free_lun, LUN
              return, 0 
           END
        endcase
        
        print, "% read_tvips_stack: loading image nr. "+String(k)
;transfer data from file to memory
        if (swapendian EQ 1) then BEGIN
           (*(*e).data)[*,*,k] = SWAP_ENDIAN(i_data(0))
        ENDIF ELSE BEGIN
           (*(*e).data)[*,*,k] = i_data(0)
        ENDELSE
        
;close file
        close, LUN
        free_lun, LUN
     ENDELSE
  END ;; FOR  
  return, 1
END








