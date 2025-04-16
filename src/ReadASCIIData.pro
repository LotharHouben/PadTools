pro remchar,st,char	
;Remove character
;+
; NAME:
;	REMCHAR
; PURPOSE:
;	Remove all appearances of character (char) from string (st)	
;	
; CALLING SEQUENCE:	
;	REMCHAR, ST, CHAR	
;	
; INPUT-OUTPUT:	
;	ST  - String from which character will be removed, scalar or vector  	
; INPUT:	
;	CHAR- Single character to be removed from string or all elements of a	
;		string array 	
;	
; EXAMPLE:	
;	If a = 'a,b,c,d,e,f,g' then 	
;	
;	IDL> remchar,a, ','	
;	
;      will give a = 'abcdefg'	
;	
; REVISIONS HISTORY	
;	Written D. Lindler October 1986	
;	Test if empty string needs to be returned   W. Landsman  Feb 1991	
;	Work on string arrays    W. Landsman   August 1997	
;	Converted to IDL V5.0   W. Landsman   September 1997	
;-	
                                 ;Convert string to byte	
 if N_params() LT 2 then begin	
     print,'Syntax - REMCHAR, string, character'	
     return	
 endif	
	
 bchar = byte(char) & bchar = bchar[0]          ;Convert character to byte	
	
 for i = 0,N_elements(st)-1 do  begin	
	
 bst = byte(st[i])	
 good = where( bst NE bchar, Ngood)	
 if Ngood GT 0 then st[i] = string(bst[good]) else st[i] = ''	
	
 endfor	
 return	
 end	

;-------------------------------------------------------------	
;+	
; NAME:	
;       REPCHR	
; PURPOSE:	
;       Replace all occurrences of one character with another in a text string.	
; CATEGORY:	
; CALLING SEQUENCE:	
;       new = repchr(old, c1, [c2])	
; INPUTS:	
;       old = original text string.          in	
;       c1 = character to replace.           in	
;       c2 = character to replace it with.   in	
;            default is space.	
; KEYWORD PARAMETERS:	
; OUTPUTS:	
;       new = edited string.                 out	
; COMMON BLOCKS:	
; NOTES:	
; MODIFICATION HISTORY:	
;       R. Sterner.  28 Oct, 1986.	
;       Johns Hopkins Applied Physics Lab.	
;       RES 1 Sep, 1989 --- converted to SUN.	
;       R. Sterner, 27 Jan, 1993 --- dropped reference to array.	
;	
; Copyright (C) 1986, Johns Hopkins University/Applied Physics Laboratory	
; This software may be used, copied, or redistributed as long as it is not	
; sold and this copyright notice is reproduced on each copy made.  This	
; routine is provided as is without any express or implied warranties	
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.	
;	Converted to IDL V5.0   W. Landsman   September 1997	
;-	
;-------------------------------------------------------------	
 	
	FUNCTION REPCHR, OLD, C1, C2, help=hlp	
 	
	if (n_params(0) lt 2) or keyword_set(help) then begin	
	  print,' Replace all occurrences of one character with another '+$	
	    'in a text string.'	
	  print,' new = repchr(old, c1, [c2])'	
	  print,'   old = original text string.          in'	
	  print,'   c1 = character to replace.           in'	
	  print,'   c2 = character to replace it with.   in'	
	  print,'        default is space.'	
	  print,'   new = edited string.                 out'	
	  return, -1	
	endif	
 	
	B = BYTE(OLD)			   ; convert string to a byte array.	
	CB1 = BYTE(C1)			   ; convert char 1 to byte.	
	W = WHERE(B EQ CB1[0])		   ; find occurrences of char 1.	
	IF W[0] EQ -1 THEN RETURN, OLD	   ; if none, return old string.	
	IF N_PARAMS(0) LT 3 THEN C2 = ' '  ; default char 2 is space.	
	CB2 = BYTE(C2)			   ; convert char 2 to byte.	
	B[W] = CB2[0]			   ; replace char 1 by char 2.	
	RETURN, STRING(B)		   ; return new string.	
	END	


function numlines,file	
;+	
; NAME:	
;     NUMLINES() 	
; PURPOSE:	
;     Return the number of lines in a file	
;	
;     This procedures became mostly obsolete in V5.6 with the introduction of	
;     the FILE_LINES() procedure	
; CALLING SEQUENCE:	
;     nl = NUMLINES( filename )	
; INPUT:	
;     filename = name of file, scalar string	
; OUTPUT:	
;     nl = number of lines in the file, scalar longword	
;          Set to -1 if the number of lines could not be determined	
; METHOD:	
;     If Unix then spawn to wc; otherwise read 1 line at a time and count	
;     Call FILE_LINES() if V5.6 or later	
;	
; PROCEDURE CALLS:	
;    ; MODIFICATION HISTORY:	
;     W. Landsman                              February 1996	
;     Use /bin/sh shell with wc under Unix     March 1997	
;     Converted to IDL V5.0   W. Landsman   September 1997	
;     Call intrinsic FILE_LINES() if V5.6 or later   December 2002	
;     Always return a scalar even if 1 element array is input  March 2004	
;-	
 On_error,2	
	
 if N_params() EQ 0 then begin	
        print,'Syntax - nl = NUMLINES( file)'	
        return,-1	
 endif	
	
 if !VERSION.RELEASE GE '5.6' then return,file_lines(file[0])	
  nl = -1L	
 openr,lun,file,/get_lun, ERROR = err	
 if err NE 0 then begin	
         message,'ERROR - Unable to open file '+ file,/CON	
        return,-1	
 endif	
	
               ;=====>> Loop through file counting lines  	
        On_ioerror,NOASCII	
        nl = 0l	
        tmp = ' '	
         while not eof(lun) do begin	
           readf,lun,tmp	
           nl = nl + 1	
         endwhile	
         free_lun,lun	
         return,nl	
	
	
NOASCII:	
  message,'Error reading file ' + string(file),/CON	
  return,-1	
 end	



function gettok,st,char, exact=exact	
;+	
; NAME:	
;	GETTOK                                    	
; PURPOSE:	
;	Retrieve the first part of a (vector) string up to a specified character	
; EXPLANATION:	
;	GET TOKen - Retrieve first part of string until the character char 	
;	is encountered.   	
;	
; CALLING SEQUENCE:	
;	token = gettok( st, char, [ /EXACT ] )	
;	
; INPUT:	
;	char - character separating tokens, scalar string	
;	
; INPUT-OUTPUT:	
;	st - string to get token from (on output token is removed),	
;            scalar or vector	
;	
; OUTPUT:	
;	token - extracted string value is returned, same dimensions as st	
; OPTIONAL INPUT KEYWORD:	
;       /EXACT -  The default behaviour of GETTOK is to remove any leading 	
;              blanks and (if the token is a blank) convert tabs to blanks.    	
;              Set the /EXACT keyword to skip these steps and leave the 	
;              input string unchanged before searching for the  character 	
;              tokens. 	
;	
; EXAMPLE:	
;	If ST is ['abc=999','x=3.4234'] then gettok(ST,'=') would return	
;	['abc','x'] and ST would be left as ['999','3.4234'] 	
;	
; PROCEDURE CALLS:	
;       REPCHR()	
; HISTORY	
;	version 1  by D. Lindler APR,86	
;	Remove leading blanks    W. Landsman (from JKF)    Aug. 1991	
;	Converted to IDL V5.0   W. Landsman   September 1997	
;       V5.3 version, accept vector input   W. Landsman February 2000	
;       Slightly faster implementation  W. Landsman   February 2001	
;       Added EXACT keyword  W. Landsman March 2004	
;       Use COMPLEMENT keyword to WHERE W. Landsman   March 2004	
;-	
;----------------------------------------------------------------------	
  On_error,2                           ;Return to caller	
	
  if N_params() LT 2 then begin	
      print,'Syntax - token = gettok( st, char, [ /EXACT ] )'	
      return,-1	
  endif 	
	
; if char is a blank treat tabs as blanks	
	
 if not keyword_set(exact) then begin	
    st = strtrim(st,1)              ;Remove leading blanks and tabs	
    if char EQ ' ' then begin 	
       tab = string(9b)                 	
       if max(strpos(st,tab)) GE 0 then st = repchr(st,tab,' ')	
    endif	
  endif	
  token = st	
	
; find character in string	
	
  pos = strpos(st,char)	
  test = pos EQ -1	
  bad = where(test, Nbad)	
  if Nbad GT 0 then st[bad] = ''	
 	
; extract token	
 good = where(1b-test, Ngood)	
 if Ngood GT 0 then begin	
    stg = st[good]	
    pos = reform( pos[good], 1, Ngood )	
    token[good] = strmid(stg,0,pos)	
    st[good] = strmid(stg,pos+1)	
 endif	
	
;  Return the result.	
	
 return,token	
 end	


function strnumber, st, val, hex = hexflg	
;+	
; NAME:	
;      STRNUMBER	
; PURPOSE:	
;      Function to determine if a string is a valid numeric value.	
;	
; CALLING SEQUENCE:	
;      result = strnumber( st, [val, /HEX] )	
;	
; INPUTS:	
;      st - any IDL scalar string	
;	
; OUTPUTS:	
;      1 is returned as the function value if the string st has a	
;      valid numeric value, otherwise, 0 is returned.	
;	
; OPTIONAL OUTPUT:	
;      val - (optional) value of the string.  real*8	
;	
; OPTIONAL INPUT KEYWORD:	
;       /HEX - If present and nonzero, the string is treated as a hexadecimal	
;             longword integer.	
;	
; EXAMPLES:	
;      IDL> res = strnumber(' ',val)	
;           returns res=0 (not a number) and val is undefined	
;	
;      IDL> res = strnumber('0.2d', val)	
;           returns res=1 (a valid number), and val = 0.2000d	
;              	
; NOTES:	
;      (1) STRNUMBER was modified in February 1993 to include a special test for 	
;      empty or null strings, which now returns a 0 (not a number).    Without	
;      this special test, it was found that a empty string (' ') could corrupt	
;      the stack.	
;	
;       (2) STRNUMBER will return a string such as '23.45uyrg' as a valid 	
;      number (=23.45) since this is how IDL performs the type conversion.  If	
;      you want a stricter definition of valid number then use the VALID_NUM	
;      function.	
; HISTORY:	
;      version 1  By D. Lindler Aug. 1987	
;      test for empty string, W. Landsman          February, 1993	
;      Converted to IDL V5.0   W. Landsman   September 1997	
;      Hex keyword added.  MRG, RITSS, 15 March 2000.	
;-	
 if N_params() EQ 0 then begin	
      print,'Syntax - result = strnumber( st, [val, /HEX] )'	
      return, 0	
 endif	
	
 newstr = strtrim( st )	
	
 if ( newstr EQ '' ) then return, 0    ;Empty string is a special case	
	
 On_IOerror, L1                 ;Go to L1 if conversion error occurs	
	
  If (NOT keyword_set(hexflg)) Then Begin	
   val = double( newstr )	
 EndIf Else Begin	
   val = 0L	
   reads, newstr, val, Format="(Z)"	
 EndElse	
	
 return, 1                      ;No conversion error	
	
 L1: return, 0                  ;Conversion error occured	
	
 end	



pro readcol,name,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15, $
            v16,v17,v18,v19,v20,v21,v22,v23,v24,v25, COMMENT = comment, $
            FORMAT = fmt, DEBUG=debug, SILENT=silent, SKIPLINE = skipline, $
            NUMLINE = numline, DELIMITER = delimiter
;+
; NAME:
;       READCOL
; PURPOSE:
;       Read a free-format ASCII file with columns of data into IDL vectors 
; EXPLANATION:
;       Lines of data not meeting the specified format (e.g. comments) are 
;       ignored.  Columns may be separated by commas or spaces.
;
;       Use READFMT to read a fixed-format ASCII file.   Use RDFLOAT for
;       much faster I/O (but less flexibility).    Use FORPRINT to write 
;       columns of data (inverse of READCOL).    
;
; CALLING SEQUENCE:
;       READCOL, name, v1, [ v2, v3, v4, v5, ...  v25 , COMMENT=
;           DELIMITER= ,FORMAT = , /DEBUG ,  /SILENT , SKIPLINE = , NUMLINE = ]
;
; INPUTS:
;       NAME - Name of ASCII data file, scalar string.  
;
; OPTIONAL INPUT KEYWORDS:
;       FORMAT - scalar string containing a letter specifying an IDL type
;               for each column of data to be read.  Allowed letters are 
;               A - string data, B - byte, D - double precision, F- floating 
;               point, I - integer, L - longword, Z - longword hexadecimal, 
;               and X - skip a column.
;
;               Columns without a specified format are assumed to be floating 
;               point.  Examples of valid values of FMT are
;
;       'A,B,I'        ;First column to read as a character string, then 
;                       1 column of byte data, 1 column integer data
;       'L,L,L,L'       ;Four columns will be read as longword arrays.
;       ' '             ;All columns are floating point
;
;       If a FORMAT keyword string is not supplied, then all columns are 
;       assumed to be floating point.
;
;       /SILENT - Normally, READCOL will display each line that it skips over.
;               If SILENT is set and non-zero then these messages will be 
;               suppressed.
;       /DEBUG - If this keyword is non-zero, then additional information is
;                printed as READCOL attempts to read and interpret the file.
;       COMMENT -  character array specifying comment signals.   Any line 
;                beginning with one of these characters will be skipped.   Default is
;                no comment lines.
;       DELIMITER - single character specifying delimiter used to separate 
;                columns.   Default is either a comma or a blank.
;       SKIPLINE - Scalar specifying number of lines to skip at the top of file
;               before reading.   Default is to start at the first line.
;       NUMLINE - Scalar specifying number of lines in the file to read.  
;               Default is to read the entire file
;
; OUTPUTS:
;       V1,V2,V3,...V25 - IDL vectors to contain columns of data.
;               Up to 25 columns may be read.  The type of the output vectors
;               are as specified by FORMAT.
;
; EXAMPLES:
;       Each row in a file position.dat contains a star name and 6 columns
;       of data giving an RA and Dec in sexigesimal format.   Read into IDL 
;       variables.   (NOTE: The star names must not include the delimiter 
;       as a part of the name, no spaces or commas as default.)
;
;       IDL> FMT = 'A,I,I,F,I,I,F'
;       IDL> READCOL,'position.dat',F=FMT,name,hr,min,sec,deg,dmin,dsec  
;
;       The HR,MIN,DEG, and DMIN variables will be integer vectors.
;
;       Alternatively, all except the first column could be specified as
;       floating point.
;
;       IDL> READCOL,'position.dat',F='A',name,hr,min,sec,deg,dmin,dsec 
;
;       To read just the variables HR,MIN,SEC
;       IDL> READCOL,'position.dat',F='X,I,I,F',HR,MIN,SEC
;
; RESTRICTIONS:
;       This procedure is designed for generality and not for speed.
;       If a large ASCII file is to be read repeatedly, it may be worth
;       writing a specialized reader.
;
;       Columns to be read as strings must not contain the delimiter character
;       (i.e. commas or spaces by default).   Either change the default 
;       delimiter with the DELIMITER keyword, or use READFMT to read such files.
;
;       Numeric values are converted to specified format.  For example,
;       the value 0.13 read with an 'I' format will be converted to 0.
;
; PROCEDURES CALLED
;       GETTOK(), NUMLINES(), STRNUMBER()
;
; MINIMUM IDL VERSION:
;       V5.3 (Uses STRSPLIT)
; REVISION HISTORY:
;       Written         W. Landsman                 November, 1988
;       Modified             J. Bloch                   June, 1991
;       (Fixed problem with over allocation of logical units.)
;       Added SKIPLINE and NUMLINE keywords  W. Landsman    March 92
;       Read a maximum of 25 cols.  Joan Isensee, Hughes STX Corp., 15-SEP-93.
;       Call NUMLINES() function W. Landsman          Feb. 1996
;       Added DELIMITER keyword  W. Landsman          Nov. 1999
;       Fix indexing typos (i for k) that mysteriously appeared W. L. Mar. 2000
;       Hexadecimal support added.  MRG, RITSS, 15 March 2000.
;       Default is comma or space delimiters as advertised   W.L. July 2001
;       Faster algorithm, use STRSPLIT if V5.3 or later  W.L.  May 2002
;       Accept null strings separated by delimiter ,e.g. ',,,'
;       Use SCOPE_VARFETCH instead of EXECUTE() for >V6.1  W.L. Jun 2005
;       Added compile_opt idl2   W. L.  July 2005
;-
  On_error,2                           ;Return to caller
  compile_opt idl2

  if N_params() lt 2 then begin
     print,'Syntax - READCOL, name, v1, [ v2, v3,...v25, '
     print,'        FORMAT= ,/SILENT  ,SKIPLINE =, NUMLINE = , /DEBUG]'
     return
  endif

  no_exec = !VERSION.RELEASE GE '6.1'
; Get number of lines in file

   nlines = NUMLINES( name )
   if nlines LT 0 then return

   if keyword_set(DEBUG) then $
      message,'File ' + name+' contains ' + strtrim(nlines,2) + ' lines',/INF

   if not keyword_set( SKIPLINE ) then skipline = 0
   nlines = nlines - skipline
   if keyword_set( NUMLINE) then nlines = numline < nlines

  ncol = N_params() - 1           ;Number of columns of data expected
  vv = 'v' + strtrim( indgen(ncol)+1, 2)
  nskip = 0

  if N_elements(fmt) GT 0 then begin    ;FORMAT string supplied?

    if size(fmt,/tname) NE 'STRING' then $
       message,'ERROR - Supplied FORMAT keyword must be a scalar string'
;   Remove blanks from format string
    frmt = strupcase(strcompress(fmt,/REMOVE))   
    remchar, frmt, '('                  ;Remove parenthesis from format
    remchar, frmt, ')'           

;   Determine number of columns to skip ('X' format)
    pos = strpos(frmt, 'X', 0)

    while pos NE -1 do begin
        pos = strpos( frmt, 'X', pos+1)
        nskip = nskip + 1
    endwhile

  endif else begin                     ;Read everything as floating point

    frmt = 'F'
    if ncol GT 1 then for i = 1,ncol-1 do frmt = frmt + ',F'
    if not keyword_set( SILENT ) then message, $
      'Format keyword not supplied - All columns assumed floating point',/INF

  endelse

  nfmt = ncol + nskip
  idltype = intarr(nfmt)

; Create output arrays according to specified formats

   k = 0L                                     ;Loop over output columns
   hex = bytarr(nfmt)
   for i = 0L, nfmt-1 do begin

       fmt1 = gettok( frmt, ',' )
       if fmt1 EQ '' then fmt1 = 'F'         ;Default is F format
       case strmid(fmt1,0,1) of 
          'A':  idltype[i] = 7          
          'D':  idltype[i] = 5
          'F':  idltype[i] = 4
          'I':  idltype[i] = 2
          'B':  idltype[i] = 1
          'L':  idltype[i] = 3
          'Z':  begin 
                idltype[i] = 3               ;Hexadecimal
                hex[i] = 1b
                end
          'X':  idltype[i] = 0               ;IDL type of 0 ==> to skip column
         ELSE:  message,'Illegal format ' + fmt1 + ' in field ' + strtrim(i,2)
      endcase

; Define output arrays

      if idltype[i] GT 0 then begin
          if no_exec then (SCOPE_VARFETCH(vv[k], LEVEL=0))= $
	        make_array(nlines,TYPE = idltype[i]) else $
          tst = execute(vv[k] + '= make_array(nlines,TYPE = idltype[i] )' ) 
           k = k+1
      endif

   endfor
   goodcol = where(idltype)
   idltype = idltype[goodcol]
   check_numeric = (idltype NE 7)
   openr, lun, name, /get_lun
   ngood = 0L

   temp = ' '
   if !VERSION.RELEASE GE '5.6' then skip_lun,lun,skipline, /lines else $
   if skipline GT 0 then $
       for i = 0, skipline-1 do readf, lun, temp        ;Skip any lines

   if not keyword_set(delimiter) then delimiter = ' ,'
 
   for j = 0L, nlines-1 do begin

      readf, lun, temp
      if strlen(temp) LT ncol then begin    ;Need at least 1 chr per output line
          ngood = ngood-1
          if not keyword_set(SILENT) then $
                       message,'Skipping Line ' + strtrim(skipline+j+1,2),/INF
          goto, BADLINE 
       endif
    k = 0
    temp = strtrim(temp,1)                  ;Remove leading spaces
    if keyword_set(comment) then BEGIN
        ii=0
        WHILE (ii LT N_ELEMENTS(comment)) DO  BEGIN
            if strmid(temp,0,1) EQ comment[ii] then begin
                ngood = ngood-1
                if keyword_set(DEBUG) then $
                  message,'Skipping Comment Line ' + strtrim(skipline+j+1,2),/INF
                goto, BADLINE 
            endif
            ii=ii+1
        END  
    END 

    var = strsplit(strcompress(temp),delimiter,/extract, /preserve_null) 
    if N_elements(var) LT nfmt then begin 
                 if not keyword_set(SILENT) then $ 
                      message,'Skipping Line ' + strtrim(skipline+j+1,2),/INF 
                 ngood = ngood-1            
                 goto, BADLINE         ;Enough columns?
    endif
    var = var[goodcol]

    for i = 0L,ncol-1 do begin
 
           if check_numeric[i] then begin    ;Check for valid numeric data
             tst = strnumber(var[i],val,hex=hex[i])          ;Valid number?
             if tst EQ 0 then begin            ;If not, skip this line
                 if not keyword_set(SILENT) then $ 
                      message,'Skipping Line ' + strtrim(skipline+j+1,2),/INF 
                 ngood = ngood-1
                 goto, BADLINE 
             endif
          if no_exec then $
	      (SCOPE_VARFETCH(vv[k], LEVEL=0))[ngood] = val else $
	       tst = execute(vv[k] + '[ngood] = val')

         endif else $
         if no_exec then $
	 (SCOPE_VARFETCH(vv[k], LEVEL=0))[ngood] = var[i] else $
           tst = execute(vv[k] + '[ngood] = var[i]')

      k = k+1

  endfor

BADLINE:  ngood = ngood+1

   endfor

  free_lun,lun
  if ngood EQ 0 then begin 
     message,'ERROR - No valid lines found for specified format',/INFORM
     return
  endif

  if not keyword_set(SILENT) then $
        message,strtrim(ngood,2) + ' valid lines read', /INFORM  

; Compress arrays to match actual number of valid lines
  if no_exec then begin
  for i=0,ncol-1 do $
       (SCOPE_VARFETCH(vv[i], LEVEL=0)) = $
            (SCOPE_VARFETCH(vv[i], LEVEL=0))[0:ngood-1]
 endif else begin
  for i = 0,ncol-1 do $
      tst = execute(vv[i] + '='+ vv[i]+ '[0:ngood-1]')
 endelse 

  return
  end


Pro ReadListFromFile, Mode=mode, Name=name, DATAP=datap
;; reads ascii block data from a file and returns a float array in the
;; 2D data list, address it via 
;;
;; ptr2D=GetRootP2D()
;; p=PList_GetCurrentDataP(ptr2D)
;; A=(*p).data ;; A is a pointer to the data array
;;
;; if you are looking for a data array with a certain name, say
;; 'myarray' then use the following commands:
;;
;; ptr2D=GetRootP2D()
;; p=DataList_SearchName(ptr2D,'myarray')
;; IF PTR_VALID(p) THEN ...
;;   
;; KEYWORDS
;;
;; Mode:: 
;; PixXY -> 2 column integer data will be read into 2xn float array
;; DataXY -> 2 column float data will be read into 2xn float array    
;; DataIX -> 2 column data (integer, float) data will be read into 2xn float array      
;; DataIXY -> 3 column data (integer, float, float) data will be read into 3xn float array       
;; DataXYZ -> 3 column float data will be read into 3xn float array       
;; XY1Y2Y3 -> 4 column float data will be read into 4xn float array
;;
;; Name:
;; the title name of the file dialog
;;
;; DATAP:
;; by default the routine stores the data in the data pointer list
;; if DATAP is set then the array pointer will not be stored in the data list
;; but returned in datap
;; 
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% ReadListFromFile: Error while attempting to read data "
    PrintToCon, "%                " + !ERR_STRING
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    return
 END
p=0
tlist=PTR_NEW(["X Y1 Y2 ... Block Data"])
readdatatype=0
result=rmd_pickfile(  $
       filter_in=datafilter, $
       filter_out=datafilter, $
       path = GetWorkingDir(),          $
       get_path = out_path,          $
       title = "Read Ascii Data File",          $
;;       crgrp = savealldata,          $
       cancelled = cancelled,        $
       type = readdatatype,        $
       ftypes=tlist, $
       /open)

ftype=(*tlist)[readdatatype]
PTR_FREE, tlist
ap=PTR_NEW()
IF NOT(cancelled) THEN BEGIN
   SetWorkingDir, out_path
    fn=result[0]
    IF Not(keyword_set(name)) THEN name=fn
    si=0 ;;  vector for integer data
    sx=0 ;; vector for x-data
    sy=0 ;; vector for y-data
    sz=0 ;; vector for z-data
    sy1=0 ;; vector for x-data
    sy2=0 ;; vector for y-data
    sy3=0 ;; vector for z-data
    CASE ftype OF
        'XmGrace readable': BEGIN
            comm=['@','&','#']
        END
        ELSE: comm=['#']
    END
    IF NOT(KEYWORD_SET(mode)) THEN mode='DataXY'
    CASE mode OF
        'PixXY': BEGIN
            s="Coordinate List read from "+fn
            format='I,I'
            readcol, fn, sx, sy, F=format, COMMENT=comm
            NPoints=N_Elements(sx)
            s="Coordinate List read from "+fn
            ap=PTR_NEW(FLTARR(2,NPoints))
            (*ap)[0,*]=sx & (*ap)[1,*]=sy
         END
        'DataXY': BEGIN
            s="XYData read from "+fn
            format='F,F'
            readcol, fn, sx, sy, F=format, COMMENT=comm
            NPoints=N_Elements(sx)
            ap=PTR_NEW(FLTARR(2,NPoints))
            (*ap)[0,*]=sx & (*ap)[1,*]=sy
        END
        'DataIXY': BEGIN
            s="IXYData read from "+fn
            format='I,F,F'
            si=0
            readcol, fn, si, sx, sy, F=format, COMMENT=comm
            NPoints=N_Elements(sx)
            ap=PTR_NEW(FLTARR(3,NPoints))
            (*ap)[0,*]=FLOAT(si) & (*ap)[1,*]=sx & (*ap)[2,*]=sy 
        END
        'DataIX': BEGIN
            s="IXData read from "+fn
            format='I,F'
            si=0
            readcol, fn, si, sx, F=format, COMMENT=comm
            NPoints=N_Elements(sx)
            ap=PTR_NEW(FLTARR(2,NPoints))
            (*ap)[0,*]=FLOAT(si) & (*ap)[1,*]=sx 
        END
        'DataXYZ': BEGIN
            s="XYZData read from "+fn
            format='F,F,F'
            readcol, fn, sx, sy, sz, F=format, COMMENT=comm
            NPoints=N_Elements(sx)
            ap=PTR_NEW(FLTARR(3,NPoints))
            (*ap)[0,*]=sx & (*ap)[1,*]=sy & (*ap)[2,*]=sz 
        END
        'XY1Y2Y3': BEGIN
            s="X Y1 Y2 Y3 Data read from "+fn
            format='F,F,F,F'
            readcol, fn, sx, sy1, sy2, sy3, F=format, COMMENT=comm
            NPoints=N_Elements(sx)
            ap=PTR_NEW(FLTARR(4,NPoints))
            (*ap)[0,*]=sx & (*ap)[1,*]=sy1 & (*ap)[2,*]=sy2 & (*ap)[3,*]=sy2   
        END
        ELSE: printtocon, "% ReadListFromFile: Invalid data format."
     ENDCASE
    IF NOT(PTR_VALID(ap)) THEN BEGIN
       ErrMsg, "% ReadListFromFile: Attempt to read data from file failed."
       return
    END
    PrintToCon, "%  ReadListFromFile: Data list read from "+fn
    IF NOT(keyword_set(datap)) THEN BEGIN
       ptr2D=GetRootP2D() ;; datalistpointer
       ;; does it already exist?
       REPEAT BEGIN
          q=DataList_SearchName(ptr2D,name)
          IF PTR_VALID(q) THEN BEGIN 
             ErrMsg, "A data array with the name "+name+" will be overwritten."
             ;; delete q
             dummy=DataList_DeleteCurrentElement(ptr2D)
          END
       ENDREP until (NOT(PTR_VALID(q)))
       e=DataList_CreateElement(ptr2D, name)
       ;; STOP
       IF PTR_VALID(e) THEN BEGIN
          (*e).data=ap
       END 
    END ELSE BEGIN
       datap=ap
    END
 END
 IF keyword_set(datap) THEN datap=ap
END    


PRO OutPutData, lun, XYZData, HEADER=header, TAIL=tail, FORMAT=format, SEPARATOR=separator, LINEFEED=linefeed
;;
;; LINEFEED
;;    CR = string("15b)
;;    LF = string("12b)
;;    DOS: CR+LF
;;
  if keyword_set(linefeed) THEN BEGIN
     CASE linefeed OF
        "msdos": BEGIN
           CR = string("15b)
           LF = string("12b)
           linefeed=CR+LF
        END
        ELSE:
     END
  END
  ;; PrintToCon, "% OutPutData: writing data set"
  IF Not(keyword_set(separator)) then separator=" " 
  A=REFORM(XYZData)
  NA=SIZE(A)
  PrintToCon, "% OutPutData: exporting data set" 
  IF KEYWORD_SET(header) THEN BEGIN
      FOR i=0,(N_ELEMENTS(header)-1) DO  BEGIN
          IF (lun GE 0) THEN BEGIN
             IF keyword_set(linefeed) THEN BEGIN 
                writeu, lun, header(i) + linefeed ;; supress linefeed otherwise
             END ELSE BEGIN
                printf, lun, header(i)
             END
          END ELSE BEGIN
             PrintToCon,  header(i)
          END
          PrintToCon, "            "+header(i) 
      END
  END ELSE BEGIN
      FOR i=0,(N_ELEMENTS(comm)-1) DO  BEGIN
          PrintToCon, "%             "+comm(i)
          IF (lun GE 0) THEN printf, lun, comm(i) ELSE PrintToCon, comm(i)
      END
  END 
  IF (NA(0) GT 1) THEN BEGIN
     FOR i=0,(NA(2)-1) DO BEGIN
        IF NOT(keyword_set(format)) THEN BEGIN
           s=STRING(A(0,i), /PRINT)
           For k=1,(NA(1)-1) DO BEGIN
              s=s+separator+STRING(A(k,i), /PRINT)
           END
        END ELSE BEGIN
           s=STRING(A(0,i), FORMAT=format)
           For k=1,(NA(1)-1) DO BEGIN
              s=s+separator+STRING(A(k,i), FORMAT=format)
           END
        END
        IF (lun GE 0) THEN BEGIN
           IF keyword_set(linefeed) THEN BEGIN 
              writeu, lun, s + linefeed ;; supress linefeed otherwise
           END ELSE BEGIN
              printf, lun, s
           END
        END ELSE BEGIN
           PrintToCon,  s
        END
     END
  END ELSE BEGIN
     FOR i=0,(NA(1)-1) DO BEGIN
        IF NOT(keyword_set(format)) THEN BEGIN
           s=STRING(A(i), /PRINT)
        END ELSE BEGIN
           s=STRING(A(i), FORMAT=format)
        END
        IF (lun GE 0) THEN BEGIN
           IF keyword_set(linefeed) THEN BEGIN 
              writeu, lun, s + linefeed ;; supress linefeed otherwise
           END ELSE BEGIN
              printf, lun, s
           END
        END ELSE BEGIN
           PrintToCon,  s
        END   
     END
  END
  IF KEYWORD_SET(tail) THEN BEGIN
      FOR i=0,(N_ELEMENTS(tail)-1) DO  Begin
          IF (lun GE 0) THEN BEGIN
             IF keyword_set(linefeed) then BEGIN
                writeu, lun, tail(i)+linefeed  
             END ELSE BEGIN
                printf, lun, tail(i)  
             END  
          END ELSE PrintToCon,  tail(i)
          PrintToCon, "            "+tail(i) 
       END 
   END 
END  

PRO DataExport, DataP
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% DataExport: Error while attempting to read data "
    PrintToCon, "%                " + !ERR_STRING
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    return
 END
END
p=0
tlist=PTR_NEW(["X Y1 Y2 ... Block Data"])
readdatatype=0
result=rmd_pickfile(  $
       filter_in=datafilter, $
       filter_out=datafilter, $
       path = GetWorkingDir(),          $
       get_path = out_path,          $
       title = "Save Ascii Data File",          $
;;       crgrp = savealldata,          $
       cancelled = cancelled,        $
       type = readdatatype,        $
       ftypes=tlist, $
       /save)
header=["# Data file written by tomato (c) L. Houben"]
tail  =[" "]
ftype=(*tlist)[readdatatype]
PTR_FREE, tlist
ap=PTR_NEW()
IF NOT(cancelled) THEN BEGIN
   SetWorkingDir, out_path
    fn=result[0]
    lun=-1
    IF (fn EQ "STDOUT") THEN BEGIN
       lun=-1
       PrintToCon, "% DataExport: data export to STDOUT" 
    END ELSE BEGIN
       if (LMGR(/DEMO)) then begin
          MESSAGE, 'Feature disabled for demo mode.'
          return
       END
       PrintToCon, "% DataExport: data export to "+fn 
       GET_LUN, lun
       OPENW, lun, fn
    END   
    ;; 
    OutPutData, lun, DataP, HEADER=header, TAIL=tail, FORMAT='(1E15.5)', SEPARATOR=' '
    IF (lun GE 0) THEN BEGIN
       FREE_LUN, lun
    END
 END 
END  

Pro ReadTest
  name='Load Tilt Axis Data'
  ReadListFromFile, Mode='DataXY', Name=name
  ptr2D=GetRootP2D()
  p=DataList_SearchName(ptr2D,name)
  IF PTR_VALID(p) THEN BEGIN
     IF PTR_VALID((*p).datap) THEN BEGIN
        IF PTR_VALID((*(*p).datap).data) THEN BEGIN
           A=((*(*p).datap).data) ;; A is a pointer to the data array
        END
     END
  END
END 
