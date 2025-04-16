; $Id: //depot/idl/IDL_70/idldir/lib/modifyct.pro#1 $
;
; Copyright (c) 1982-2007, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

PRO MyMODIFYCT, ITAB, NAME, R, G, B, FILE=file, NEWFILE=newfile	;MODIFY COLOR TABLE IN FILE
;+
; NAME:
;	MODIFYCT
;
; PURPOSE:
;	Update the distribution color table file "colors1.tbl" or the
;	user-supplied file with a new table.
;
; CATEGORY:
;	Z4 - Image processing, color table manipulation.
;
; CALLING SEQUENCE:
;	MODIFYCT, Itab, Name, R, G, B
;
; INPUTS:
;	Itab:	The table to be updated, numbered from 0 to 255.  If the
;		table entry is greater than the next available location
;		in the table, then the entry will be added to the table
;		in the available location rather than the index specified
;		by Itab.  On return, Itab will contain the index for the
;		location that was modified or extended.  The table
;		can be loaded with the IDL command:  LOADCT, Itab.
;
;	Name:	A string up to 32 characters long that contains the name for
;		the new color table.
;
;	R:	A 256-element vector that contains the values for the red
;		color gun.
;
;	G:	A 256-element vector that contains the values for the green
;		color gun.
;
;	B:	A 256-element vector that contains the values for the blue
;		color gun.
;
; KEYWORD PARAMETERS:
;	FILE:	If this keyword is set, the file by the given name is used
;		instead of the file colors1.tbl in the IDL directory.  This
;		allows multiple IDL users to have their own color table file.
;		The file specified must be a copy of the colors1.tbl file.
;		The file must exist. Use NEWFILE if that is not the case.
;	NEWFILE: If this keyword is set, the file FILE is created
; OUTPUTS:
;	Itab:	The index of the entry which was updated, 0 to 255.  This
;		may be different from the input value of Itab if the
;		input value was greater than the next available location
;		in the table.  If this was the case the entry was added to
;		the table in the next available location instead of leaving
;		a gap in the table.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The distribution file "colors.tbl1" or the user-supplied file is
;	modified with the new table.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	Aug, 1982, DMS, Written.
;	Unix version, 1987, DMS.
;	ACY, 9/92, Update for new color table structure, Add FILE keyword.
;		   Allow extending table.
;	WSO, 1/95, Updated for new directory structure
;
;-
COMPILE_OPT strictarr
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% MyModifyCt:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
END

  IF keyword_set(newfile) THEN BEGIN 
     printtocon, "% MyModifyCT: creating empty color table file "+file
     ntables=0b
     OPENW, outunit, file, /GET_LUN 
     WRITEU, outunit, ntables
     FREE_LUN, outunit
  END
     
  IF (ITAB LT 0) OR (ITAB GT 255) THEN message, $
		'Color table number out of range.'
  
  IF (N_ELEMENTS(file) GT 0) THEN filename = file $
  ELSE BEGIN
     ;; filename = FILEPATH('colors1.tbl', subdir=['resource',
     ;; 'colors'])
     ErrMsg, "Cannot edit IDL color tables"
  END
  
  GET_LUN, IUNIT                 ;GET A LOGICAL UNIT
  OPENU,IUNIT, filename, /BLOCK ;OPEN FILE
  ntables = 0b
  readu, IUNIT, ntables
  
  if (ITAB LT ntables) then begin ; Update an existing record
     AA=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1)	;UPDATE NAME RECORD.
     a = aa[0]
     a[*,ITAB] = 32B			;blank out old name
     a[0:strlen(name)-1,ITAB] = byte(name)
     aa[0]=a				;Write names out

     AA=ASSOC(IUNIT,BYTARR(256),1)	;UPDATE VECTORS. SKIP PAST COUNT
     AA[ITAB*3]   = BYTE(R)		;PUT IN RED. GUARANTEE BYTE
     AA[ITAB*3+1] = BYTE(G)		;GREEN IN 2ND BLOCK
     AA[ITAB*3+2] = BYTE(B)		;BLUE IN 3RD BLOCK

  endif else begin			; Add a new record at the end of table
     ITAB = ntables
     If (ntables GT 0) THEN BEGIN 
                                ; Add new vectors.  First, read names, then insert vectors
        AA=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1) ;UPDATE NAME RECORD.
        a = aa[0]
     ; Skip past old vectors
        AA=ASSOC(IUNIT,BYTARR(256),ntables*768L+1) ;UPDATE VECTORS
        AA[0] = BYTE(R)                            ;PUT IN RED. GUARANTEE BYTE
        AA[1] = BYTE(G)                            ;GREEN IN 2ND BLOCK
        AA[2] = BYTE(B)                            ;BLUE IN 3RD BLOCK
        
                                ; Skip past new vector to put in names
        AA=ASSOC(IUNIT,BYTARR(32,ntables+1), (ntables+1)*768L+1)
                                ; Add new name to end
        temp=bytarr(32)+32B
        temp[0:strlen(name)-1]=byte(name)
        allnames=bytarr(32,ntables+1)
        allnames[*,0:ntables-1] = a
        allnames[*,ntables]=temp
        AA[0] = allnames        ; write the names out
        
                                ; Update count
        AA=ASSOC(IUNIT,BYTARR(1))
        AA[0] = [ntables+1B]
     end else begin
                                ; Skip past old vectors
        AA=ASSOC(IUNIT,BYTARR(256),1) ;UPDATE VECTORS
        AA[0] = BYTE(R)               ;PUT IN RED. GUARANTEE BYTE
        AA[1] = BYTE(G)               ;GREEN IN 2ND BLOCK
        AA[2] = BYTE(B)               ;BLUE IN 3RD BLOCK
        
           AA=ASSOC(IUNIT,BYTARR(32,1), 768L+1)
           temp=bytarr(32)+32B
           temp[0:strlen(name)-1]=byte(name)
           allnames=bytarr(32,1)
           allnames[*,0]=temp
           AA[0] = allnames     ; write the names out
        
                                ; Update count
           AA=ASSOC(IUNIT,BYTARR(1))
           AA[0] = [ntables+1B]
        end
  endelse

  GOTO, close_file

bad:
  MESSAGE, /CONTINUE, 'Error writing file: ' + file + ', ' + !ERROR_STATE.msg

close_file:
  FREE_LUN,IUNIT
  RETURN
END



PRO MyDeleteCT, index, file
;+
; NAME:
;	MyDeleteCT
;
; PURPOSE:
;	Delete a table entry in a color table file "colors1.tbl" or the
;	

;
; CALLING SEQUENCE:
;	MyDeleteCT, index, name
;
; INPUTS:
;	index:	The table to be deleted, numbered from 0 to 255.  If the
;		table entry is greater than the next available location
;		in the table, then the last entry will be deleted.
;
;	Name:	table file name
;
; OUTPUTS:
;	index:	The next index of the entry which was deleted, 0 to 254.  This
;		may be different from the input value of index if the
;		table deteled was at the end.
;		a gap in the table.
;
;
; Organisation of color table files:
;
; NTABLES= number of color tables
;
; each colortable has 768 BYTE values, the r g b vectors of
; type BYTE(256)
; all table rgb values are written at the binning og he raw data file
; table names are 32 BYTE strings, they are written at the end of the
; file starting from byte ntables*768L+1
;
COMPILE_OPT strictarr
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% MyDeleteCt:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
END

IF (index LT 1) OR (index GT 255) THEN BEGIN
   ErrMsg, 'Color table number out of range. '
   return
END

filename = FILEPATH('colors1.tbl', subdir=['resource','colors'])

IF (file EQ filename) THEN BEGIN
     ;; 
   ErrMsg, "Cannot edit IDL color tables"
   return
END

printtocon, "% MyDeleteCT: Accessing  "+ file+"."
  GET_LUN, IUNIT                 ;GET A LOGICAL UNIT
  OPENU,IUNIT, file, /BLOCK      ;OPEN FILE
  ;;GET_LUN, CPIUNIT                 ;GET A LOGICAL UNIT
  ;; OPENW,CPIUNIT, file+".cp", /BLOCK ;OPEN FILE
  ntables = 0b
  readu, IUNIT, ntables
  
  if (index LT ntables) then begin ; Update an existing record
     f_data=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1)	;UPDATE NAME RECORD.
     names = f_data[0]
     ;; print, names[*,index]
     ;; aa[0]=a				;Write names out

     f_data=ASSOC(IUNIT,BYTARR(256,3,ntables),1) ; rgb-vectors
     ;; r[table_index]=rgbs[*,0,table_index]
     ;; g[table_index]=rgbs[*,1,table_index]
     ;; b[table_index]=rgbs[*,2,table_index]
     rgbs=f_data[0]
     ;;
     
     printtocon, "% MyDeleteCT: Deleting "+ STRING(names[*,index])+"."
     For j=index+1, (ntables-1) DO BEGIN
        names[0:31,j-1] = names[0:31,j]
        rgbs[*,*,j-1]=rgbs[*,*,j]
     END
     ;; now the last table ntable-1 may not be written out
     nnewtables=ntables-1
     POINT_LUN, IUNIT, 0
     WRITEU, IUNIT, Byte(nnewtables)
     WRITEU, IUNIT, rgbs[*,*,0:(nnewtables-1)]
     WRITEU, IUNIT, names[*,0:(nnewtables-1)]
     ;;
     ;; update index
     if (index EQ (ntables-1)) THEN index=nnewtables-1 
     ;;
  end 
  ;; FREE_LUN,CPIUNIT
  FREE_LUN,IUNIT
  RETURN
END


PRO MyRenameCT, index, file, newname
;+
; NAME:
;	MyRenameCT
;
; PURPOSE:
;	Rename a table entry in a color table file "colors1.tbl" or the
;	

;
; CALLING SEQUENCE:
;	MyRenameCT, index, file, newname
;
; INPUTS:
;	index:	The table to be deleted, numbered from 0 to 255.  If the
;		table entry is greater than the next available location
;		in the table, then the last entry will be deleted.
;
;       file:	table file name
;       newname:   new table name
;
;
;
; Organisation of color table files:
;
; NTABLES= number of color tables
;
; each colortable has 768 BYTE values, the r g b vectors of
; type BYTE(256)
; all table rgb values are written at the binning og he raw data file
; table names are 32 BYTE strings, they are written at the end of the
; file starting from byte ntables*768L+1
;
COMPILE_OPT strictarr
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% MyRenameCT:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
END

IF (index LT 0) OR (index GT 255) THEN BEGIN
   ErrMsg, 'Color table number out of range. '
   return
END

filename = FILEPATH('colors1.tbl', subdir=['resource','colors'])

IF (file EQ filename) THEN BEGIN
     ;; 
   ErrMsg, "Cannot edit IDL color tables"
   return
END

printtocon, "% MyRenameCT: Accessing  "+ file+"."
  GET_LUN, IUNIT                 ;GET A LOGICAL UNIT
  OPENU,IUNIT, file, /BLOCK      ;OPEN FILE
  ;;GET_LUN, CPIUNIT                 ;GET A LOGICAL UNIT
  ;; OPENW,CPIUNIT, file+".cp", /BLOCK ;OPEN FILE
  ntables = 0b
  readu, IUNIT, ntables
  
  if (index LT ntables) then begin ; Update an existing record
     f_data=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1)	;UPDATE NAME RECORD.
     names = f_data[0]
     ;;
     f_data=ASSOC(IUNIT,BYTARR(256,3,ntables),1) ; rgb-vectors
     ;; r[table_index]=rgbs[*,0,table_index]
     ;; g[table_index]=rgbs[*,1,table_index]
     ;; b[table_index]=rgbs[*,2,table_index]
     rgbs=f_data[0]
     ;;
     if (strlen(newname) GT 32) THEN newname=STRMID(newname, 0, 32)
     printtocon, "% MyRenameCT: Renaming "+ STRING(names[*,index])+"."
     printtocon, "     New name - "+ newname+"."
     temp=bytarr(32)+32B
     temp[0:strlen(newname)-1]=byte(newname)
     names[0:31,index] = temp
        
     ;; now write tables
     POINT_LUN, IUNIT, 0
     WRITEU, IUNIT, Byte(ntables)
     WRITEU, IUNIT, rgbs[*,*,0:(ntables-1)]
     WRITEU, IUNIT, names[*,0:(ntables-1)]
     ;;
  end 
  ;; FREE_LUN,CPIUNIT
  FREE_LUN,IUNIT
  RETURN
END


PRO MyInsertCT, index, file, newname, r, g, b, APPEND=append
;+
; NAME:
;	MyInsertCT
;
; PURPOSE:
;	Insert a table entry in a color table file "colors1.tbl" or the
;	

;
; CALLING SEQUENCE:
;	MyDeleteCT, index, name
;
; INPUTS:
;	index:	The table position to insert the new table, numbered from 0 to 255. 
;
;	file:	table file name
;	newname:	table  name
;
; OUTPUTS:
;	index:	The next index of the entry which was deleted, 0 to 254.  This
;		may be different from the input value of index if the
;		table deteled was at the end.
;		a gap in the table.
;
;
; Organisation of color table files:
;
; NTABLES= number of color tables
;
; each colortable has 768 BYTE values, the r g b vectors of
; type BYTE(256)
; all table rgb values are written at the binning og he raw data file
; table names are 32 BYTE strings, they are written at the end of the
; file starting from byte ntables*768L+1
;
COMPILE_OPT strictarr
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% MyInsertCT:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
END

IF (index LT 0) OR (index GT 255) THEN BEGIN
   ErrMsg, 'Color table number out of range. '
   return
END

filename = FILEPATH('colors1.tbl', subdir=['resource','colors'])

IF (file EQ filename) THEN BEGIN
     ;; 
   ErrMsg, "Cannot edit IDL color tables"
   return
END

printtocon, "% MyInsertCT: Accessing  "+ file+"."
  GET_LUN, IUNIT                 ;GET A LOGICAL UNIT
  OPENU,IUNIT, file, /BLOCK      ;OPEN FILE
  ;;GET_LUN, CPIUNIT                 ;GET A LOGICAL UNIT
  ;; OPENW,CPIUNIT, file+".cp", /BLOCK ;OPEN FILE
  ntables = 0b
  readu, IUNIT, ntables

  if (ntables GE 255) THEN BEGIN
     ErrMsg, "Color table file already contains 255 entries. Create a new file or delete table from this file."
     FREE_LUN,IUNIT
     return
  END
  if (index LT ntables) then begin ; Update an existing record
     f_data=ASSOC(IUNIT,BYTARR(32,ntables), ntables*768L+1)	;UPDATE NAME RECORD.
     names = f_data[0]
     ;; print, names[*,index]
     ;; aa[0]=a				;Write names out

     f_data=ASSOC(IUNIT,BYTARR(256,3,ntables),1) ; rgb-vectors
     ;; r[table_index]=rgbs[*,0,table_index]
     ;; g[table_index]=rgbs[*,1,table_index]
     ;; b[table_index]=rgbs[*,2,table_index]
     rgbs=f_data[0]
     ;;
     if (strlen(newname) GT 32) THEN newname=STRMID(newname, 0, 32)
     temp=bytarr(32)+32B
     temp[0:strlen(newname)-1]=byte(newname)
     printtocon, "% MyInsertCT: Adding "+ newname+"."

     IF keyword_set(append) THEN BEGIN
        ;; The easy case
        nnewtables=ntables+1
        POINT_LUN, IUNIT, 0
        WRITEU, IUNIT, Byte(nnewtables)
        WRITEU, IUNIT, rgbs[*,*,0:(ntables-1)]
        WRITEU, IUNIT, r
        WRITEU, IUNIT, g
        WRITEU, IUNIT, b
        WRITEU, IUNIT, names[*,0:(ntables-1)]
        WRITEU, IUNIT, temp
        index=nnewtables-1
     END ELSE BEGIN
        nnewtables=ntables+1
        POINT_LUN, IUNIT, 0
        WRITEU, IUNIT, Byte(nnewtables)
        ;; slightly more difficult: insert at index
        ;; First part
        For j=0,index-1 DO WRITEU, IUNIT, rgbs[*,*,j]
        ;; new table
        WRITEU, IUNIT, r
        WRITEU, IUNIT, g
        WRITEU, IUNIT, b
        ;; rest of the tables
        For j=index,ntables-1 DO WRITEU, IUNIT, rgbs[*,*,j]
        ;;
        ;; now the names
        For j=0,index-1 DO WRITEU, IUNIT, names[*,j]
        WRITEU, IUNIT, temp
        For j=index,ntables-1 DO WRITEU, IUNIT, names[*,j]
        ;; 
     END
  end 
  ;; FREE_LUN,CPIUNIT
  FREE_LUN,IUNIT
  RETURN
END
