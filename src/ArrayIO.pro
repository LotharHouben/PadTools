;; TODO
;; - arrays need to be processed by pointer dereference!

Function ArrayIOObj::init, fname
  self.fname=fname
  self.parray=PTR_NEW()
  self.NDim=0B
  self.PDim=PTR_NEW()
  self.Type=0
  self.magic=FIX(3308)
  ;; initialize paramter match hash
  self.lun=-1L
  return, 1
END

Pro ArrayIOObj__define
  ;; fname: filename
  ;; lun: logical unit number
  ;;         the structure contains the data tags and data
  ;; magic: file identifier = CECEhex
  void={ArrayIOObj, fname:'', lun:-1L, swap: 0B, magic: 3308, NDim:0B, PDim:PTR_NEW(), Type:0B, parray:PTR_NEW()}
END

PRO ArrayIOObj::set_filename, filename
 self.fname=filename
END

PRO ArrayIOObj::set_arrayp, arrayp
;; analyse array info and set internal variables
  N=SIZE(*(arrayp))
  self.NDim=Byte(N[0])
  IF NOT ((self.NDim GT 0) AND (self.NDim LE 8)) THEN Begin
     printtocon, "% ArrayIOObj::set_arrayp: Number of array dimensions is less than or equal to 0 or larger than 8."
     return
  END
  IF PTR_VALID(self.PDim) THEN PTR_FREE, self.PDim
  self.PDim=PTR_NEW(LONARR(self.NDim))     
  self.parray=arrayp
  ;; write the number of Dimensions, 1 BYTE
  FOR i=1, (self.NDim) DO BEGIN
     (*(self.PDim))[i-1]=N[i]
  END 
  ;; write the data type, 1 BYTE
  self.type=Byte(N[self.NDim+1])
END

Function ArrayIOObj::read_file
  ;; reads the structure *(self.pstruc) from the file self.fname
  IF (FILE_TEST(self.fname ,/READ) NE 1) THEN BEGIN
     printtocon, "% ArrayIOObj::read_file: The file "+ self.fname + " is not readable."
     ErrMsg, "% ArrayIOObj::read_file: The file "+ self.fname + " is not readable."
     return, 0
  END
  GET_LUN, tmp
  self.lun=tmp
  openr, self.lun, self.fname, ERROR = o_err
  if (o_err NE 0 ) then begin
     printtocon, "% ArrayIOObj::read_file: Error while trying to open file " + self.fname+ ":" + !ERR_STRING
     ErrMsg,  "% ArrayIOObj::read_file: Error while trying to open file " + self.fname+ ":" + !ERR_STRING
     return, 0
  endif 
  ;; try to read header
  anint=FIX(0)
  READU, self.lun, anint
  ;; check magic integer
  IF NOT(anint EQ self.magic) THEN BEGIN
     ;; need to swap bytes?
     IF (SWAP_ENDIAN(anint) EQ self.magic) THEN BEGIN
        ;; need byte swapping
        self.swap=1B
        close, self.lun
        free_lun, self.lun
        ;; reopen with swap flag
        GET_LUN, tmp
        self.lun=tmp
        openr, self.lun, self.fname, ERROR = o_err, /SWAP_ENDIAN
        READU, self.lun, anint
     END
     ;; 
     IF NOT(anint EQ self.magic) THEN BEGIN
       close, self.lun
       free_lun, self.lun
       self.lun = -1
       printtocon, "% ArrayIOObj::read_file: Wrong file format. Failed to read magic bytes."
       return, 0
     END
  END
  ;; now everything should be set ... 
  ;; read the header data 
  ;; first read the dimension
  abyte=0B
  READU, self.lun, abyte
  self.NDim=abyte
  IF ((self.NDim GT 0) AND (self.NDim LE 8)) THEN Begin
     self.PDim=PTR_NEW(LONARR(self.NDim))     
  END ELSE BEGIN
     close, self.lun
     free_lun, self.lun
     self.lun = -1
     printtocon, "% ArrayIOObj::read_file: Number of array dimensions is less than or equal to 0 or larger than 8."
     return, 0
  END
  totelem=1L ;; total number of elements
  along=0L
  FOR i=0, (self.NDim-1) DO BEGIN
     READU, self.lun, along     
     (*(self.PDim))[i]=along
     totelem=totelem*along
  END 
  ;; then the data type
  READU, self.lun, abyte
  self.type=abyte
  ;; prepare array, read a one-dimensional array and reform it later 
  ipos=(self.NDim+1)*4L
  CASE self.type OF
     	1 : BEGIN
             self.parray=PTR_NEW(BYTARR(totelem))
             i_data = ASSOC(self.lun, BYTARR(totelem), ipos)	
            END
	2 : BEGIN
             self.parray=PTR_NEW(INTARR(totelem))
             i_data = ASSOC(self.lun, INTARR(totelem), ipos)	
            END
	3 : BEGIN
             self.parray=PTR_NEW(LONARR(totelem))
             i_data = ASSOC(self.lun, LONARR(totelem), ipos)	
            END
	4 : BEGIN
             self.parray=PTR_NEW(FLTARR(totelem))
             i_data = ASSOC(self.lun, FLTARR(totelem), ipos)	
            END
	5 : BEGIN
             self.parray=PTR_NEW(DBLARR(totelem))
             i_data = ASSOC(self.lun, DBLARR(totelem), ipos)
          END
	6 : BEGIN
             self.parray=PTR_NEW(COMPLEXARR(totelem))
             i_data = ASSOC(self.lun, COMPLEXARR(totelem), ipos)	
            END
	9 : BEGIN
             self.parray=PTR_NEW(DCOMPLEXARR(totelem))
             i_data = ASSOC(self.lun, DCOMPLEXARR(totelem), ipos)	
            END
	12 : BEGIN
             self.parray=PTR_NEW(UINTARR(totelem))
             i_data = ASSOC(self.lun, UINTARR(totelem), ipos)	
            END
	13 : BEGIN
             self.parray=PTR_NEW(ULONARR(totelem))
             i_data = ASSOC(self.lun, ULONARR(totelem), ipos)	
            END
	else: BEGIN
                printtocon, "% ArrayIOObj::read_file: invalid data type " + MyString(type)
                close, self.lun
                free_lun, self.lun
                self.lun=-1
                return, 1 
              END
  ENDCASE
  ;;
  ;; read data into array
  *(self.parray) = i_data(0) 
  ;; Now reform array      
  CASE self.NDim OF
     2: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], /OVERWRITE)
     END
     3: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], (*(self.PDim))[2], /OVERWRITE)
     END
     4: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], (*(self.PDim))[2], (*(self.PDim))[3], /OVERWRITE)
     END
     5: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], (*(self.PDim))[2], (*(self.PDim))[3],(*(self.PDim))[4], /OVERWRITE)
     END
     6: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], (*(self.PDim))[2], (*(self.PDim))[3],(*(self.PDim))[4], (*(self.PDim))[5], /OVERWRITE)
     END
     7: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], (*(self.PDim))[2], (*(self.PDim))[3],(*(self.PDim))[4], (*(self.PDim))[5], (*(self.PDim))[6], /OVERWRITE)
     END
     8: BEGIN
        *(self.parray)=REFORM((*(self.parray)),(*(self.PDim))[0],(*(self.PDim))[1], (*(self.PDim))[2], (*(self.PDim))[3],(*(self.PDim))[4], (*(self.PDim))[5], (*(self.PDim))[6], (*(self.PDim))[7], /OVERWRITE)
     END
    ELSE: 
 END
  IF (self.lun GT -1) THEN BEGIN
     close, self.lun
     free_lun, self.lun
     self.lun=-1
  END
  ;; STOP
  return, self.parray
END 


Function ArrayIOObj::save_file
  ;; saves the structure  *(self.pstruc) to the file self.fname
  ;; scalar values can be transferred directly 
  ;; arrays should be transferred as pointers
  IF (FileIsWriteable(self.fname) NE 1) THEN BEGIN
     printtocon, "% ArrayIOObj::save_file: The file "+ self.fname + " is not writeable."
     ErrMsg, "% ArrayIOObj::save_file: The file "+ self.fname + " is not writeable."
     return, 0
  END
  IF NOT(PTR_VALID(self.parray)) THEN BEGIN
     printtocon, "% ArrayIOObj::save_file: There is no array defined."
     ErrMsg, "% ArrayIOObj::save_file: There is no array defined."
     return, 0
  END
  GET_LUN, tmp
  self.lun=tmp
  openw, self.lun, self.fname, ERROR = o_err
  if (o_err NE 0 ) then begin
     printtocon, "% ArrayIOObj::save_file: Error while trying to open file " + self.fname+ ":" + !ERR_STRING
     ErrMsg,  "% ArrayIOObj::save_file: Error while trying to open file " + self.fname+ ":" + !ERR_STRING
     free_lun, self.lun
     return, 0
  endif 
  ;; write magic number, 1 INT
  writeu, self.lun, FIX(self.magic)
  ;; write array info
  N=SIZE(*(self.parray))
  self.NDim=Byte(N[0])
  IF ((self.NDim GT 0) AND (self.NDim LE 8)) THEN Begin
     IF PTR_VALID(self.PDim) THEN PTR_FREE, self.PDim
     self.PDim=PTR_NEW(LONARR(self.NDim))     
  END ELSE BEGIN
     close, self.lun
     free_lun, self.lun
     self.lun = -1
     printtocon, "% ArrayIOObj::save_file: Number of array dimensions is less than or equal to 0 or larger than 8."
     return, 0
  END
  ;; write the number of Dimensions, 1 BYTE
  writeu, self.lun, self.NDim
  ;; write the Dimensions, each dimension as a LONG integer
  FOR i=1, (self.NDim) DO BEGIN
     Writeu, self.lun, LONG(N[i])     
     (*(self.PDim))[i-1]=N[i]
  END 
  ;; write the data type, 1 BYTE
  self.type=Byte(N[self.NDim+1])
  Writeu, self.lun, self.type
  ;; now the array
  WriteU, self.lun, *(self.parray)
  ;;
  IF (self.lun GT -1) THEN BEGIN
     close, self.lun
     free_lun, self.lun
     self.lun=-1
  END
  return, 1
END



Function ArrayIOObj::cleanup
 IF (self.lun GT -1) THEN BEGIN
    ;; check whether file is open, if so then close it 
     close, self.lun
     free_lun, self.lun
 END
END


PRO TestArrayIO
  ;; theta data
  filename='/Users/lothar/Desktop/test.dat'
  f=obj_new('ArrayIOObj', filename)
  wp=PTR_NEW(RandomU(seed,10,5,4))
  print, *wp
  f->set_arrayp, wp 
  res=f->save_file()
  rp=f->read_file()
  IF PTR_VALID(rp) THEN print, *rp
  obj_destroy, f
  IF PTR_VALID(wp) THEN PTR_FREE, wp
  IF PTR_VALID(rp) THEN PTR_FREE, rp
END
