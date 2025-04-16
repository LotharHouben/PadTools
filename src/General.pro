PRO About
  ;; License=GetLicenseData()
 licensestring="Full License"
 Message_Text=["PadTools"," ","(c) L. Houben","",GetVersion() ," ", $
               "Compiled on IDL Version "+  !version.release +" (IntelMac)", $ 
               " ", $
               "Running on IDL Version "+!version.release, $ 
                "License Model: "+licensestring,$
               ;;"MAC addresses: "+GetMacAddresses(/TEXT),$
              ;;  "License Nr.  : "+ License(0),$
              ;;  "License Code : "+ License(1),$
               " ",$
               "mailto: lothar.houben@weizmann.ac.il  ", $
               ""]
   result=DIALOG_MESSAGE( Message_text , /INFORMATION, TITLE="About")
END 

 PRO InitPlotColors
 COMMON PLOTCOLORS, backgr, foregr
 Device, Get_decomposed=decomposed
 maxrgb=!D.TABLE_SIZE
 black=0
 IF (decomposed EQ 1) THEN BEGIN
     ;; 24 bit RGB in three 8bit values
    white=((maxrgb-1)+(maxrgb-1)*maxrgb+(maxrgb-1)*maxrgb*maxrgb)
 END ELSE BEGIN
    ;; 8 bit RGB
    white=(maxrgb-1)
 END
    backgr=white & foregr=black
    !P.BACKGROUND=backgr & !P.COLOR=foregr
 END

FUNCTION IN, a, set
 result=0
 FOR i=0,(N_Elements(set)-1) DO BEGIN
  IF (a EQ set(i)) THEN result=1
 END
 return, result
END

FUNCTION MySTRING, x
  IF N_ELEMENTS(x) GT 0 THEN BEGIN
     return, STRCOMPRESS(STRING(x, /PRINT), /REMOVE_ALL)
  END ELSE BEGIN
     return, ''
  END
END

Pro SetRootP,r
COMMON RootPointer, p
p=r
END

FUNCTION GetRootP
COMMON RootPointer, p
return, p
END


FUNCTION GetCurrentP
COMMON RootPointer, p
IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% GetCurrentP: Root pointer is invalid" 
     return, 0
  END
  c=(*p).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% Warning: Current stack pointer is invalid." 
     return, 0
  END
  return, c
END 

FUNCTION SetCurrentP, p
  ;; returns 1 on success
  r=GetRootP()
  IF (NOT(PTR_VALID(r))) THEN BEGIN
     printtocon, "% SetCurrentP: Root pointer is invalid" 
     return, 0
  END
  curr=(*r).first
  WHILE (PTR_VALID(curr) AND (p NE curr)) DO curr=(*curr).next
  IF (curr EQ p) THEN BEGIN
     (*r).current=curr
     return, 1
  END
  return, 0
END 

FUNCTION GetCurrentPData
p=GetCurrentP()
IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% GetCurrentP: Current list pointer is invalid" 
     return, 0
  END
  c=(*p).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% Warning: Current stack pointer is invalid." 
     return, 0
  END
  d=(*c).data
  IF (NOT(PTR_VALID(d))) THEN BEGIN
     printtocon, "% Warning: Current data array pointer is invalid." 
     return, 0
  END
  return, d
END 

Pro SetRootP2D,r
COMMON RootPointer2D, p
p=r
END

FUNCTION GetRootP2D
COMMON RootPointer2D, p
return, p
END

Pro SetRootPXYGraph,r
COMMON RootPointerXYGraph, p
p=r
END

FUNCTION GetRootPXYGraph
COMMON RootPointerXYGraph, p
return, p
END

Pro SetRootPTable,r
COMMON RootPointerTable, p
p=r
END

FUNCTION GetRootPTable
COMMON RootPointerTable, p
return, p
END


FUNCTION IN, a, set
 result=0
 FOR i=0,(N_Elements(set)-1) DO BEGIN
  IF (a EQ set(i)) THEN result=1
 END
 return, result
END

FUNCTION type_spec, i
;; 
;; returns an identifier string for data type i
;;
;; i = Integer
;;
   s = ''
   CASE i of   
      1: s = 'Byte'	
      2: s = 'Integer'	
      3: s = 'Longword Integer'
      4: s = 'Floating Point'	
      5: s = 'Double-precision Floating Point'	
      6: s = 'Complex Floating Point'	
      7: s = 'String'	
      8: s = 'Structure'	
      9: s = 'Double-precision Complex Floating Point'	
      10: s = 'Pointer'	
      11: s = 'Object Reference'	
      12: s = 'Unsigned Integer'
      13: s = 'Unsigned Longword Integer'	
      ELSE: s = 'Undefined'
   ENDCASE
   return, s
END

PRO PrintToCon, s
print, s
END

PRO XConsole_PopState
END


FUNCTION GetWaitDelay
return, 0.05
END


PRO SetStackValues, axis_, num_, STACKB=stackb
COMMON StackVALUES, axis, num, axisB, numB
;; print, "SetStackValues: Setting stack values"
IF NOT(Keyword_set(stackb)) THEN BEGIN
   axis=axis_
   num=num_
END ELSE BEGIN
   axisB=axis_
   numB=num_
END
END

PRO GetStackValues, axis_, num_, STACKB=stackb
COMMON StackVALUES, axis, num, axisB, numB
;; print, "GetStackValues: Getting stack values"
IF NOT(Keyword_set(stackb)) THEN BEGIN
   axis_=axis
   num_=num
END ELSE BEGIN
   axis_=axisB
   num_=numB
END
END


PRO ClearMouseButton, buttonid, DELAY=delay
;; read the Cursor Pos
 maxi=5000
 i=0
 print, "clear mouse event"
 While ((!mouse.button EQ buttonid) AND (i LT maxi)) DO BEGIN
     !mouse.button=-buttonid ;; avoid infinit loop
     ;; do nothing
     ;; PrintToCon, "ignoring buffered mouse button event"
     ;; read mouse state again
     cursor, ix, iy, 1, /dev, /NOWAIT
     i=i+1
 END
 IF (i GE maxi) THEN BEGIN
     PrintToCon, "% ClearMouseButton: buffer event overflow for mouse button! Increase loop limit!"
  END
 IF keyword_set(delay) THEN WAIT, delay
END

PRO SetHomeDir
COMMON HOMEDIR, home
home=programrootdir()
END

FUNCTION GetHomeDir
COMMON HOMEDIR, home
return, home
END

PRO SetDebugFlag, value
COMMON DEBUGFLAG, debugflagvalue
debugflagvalue=value
END


FUNCTION GetDebugFlag
COMMON DEBUGFLAG, debugflagvalue
return, debugflagvalue
END

PRO SetCalibrationObj, value
COMMON calib, calibvalue
 calibvalue=value
END

FUNCTION GetCalibrationObj
COMMON calib, calibvalue
return, calibvalue
END


PRO MakeBinary
  print, "% MakeBinary: creating PadTools.sav in the directory release ..."
  RESOLVE_ALL, SKIP_ROUTINES="F", /CONTINUE_ON_ERROR       
  ;; RESOLVE_EVENMORE
  SAVE, /ROUTINES, FILENAME="release/PadTools.sav"
  print, "% MakeBinary: ... finished"
END

FUNCTION ArrayDistance, a, b
 IF (N_Elements(a) NE N_Elements(b)) THEN return, -1
 return, TOTAL(ABS(a-b))
END


function IntPow2, x
;; check whether x is an integral power of 2 
i=1
while (i LT x) DO i=i*2
return, (i EQ x)
end

function FloorIntPow2, x
;; returns largest integer 2^n that is smaller or equal to x
i=1
while ((i*2) LT x) DO i=i*2
return, i
end

function CeilIntPow2, x
;; returns smallest integer 2^n that is larger or equal to x
i=1
while (i LT x) DO i=i*2
return, i
end

function GetRandomSeed
;; return a seed value for random number generation from the current
;; time 
return, LONG(Systime(/SECONDS))
END


Function RGBColor,r,g,b
 return, r + 256*g + (256L*256L)*b 
END


Function WidgetID, id, GETGROUPLEADERTOP=getgroupleadertop, SETGROUPLEADERTOP=setgroupleadertop
  COMMON WIDGETIDS, GroupLeadertop
  result=-1
  IF keyword_set(getgroupleadertop) then result=GroupLeadertop
  IF keyword_set(setgroupleadertop) then GroupLeadertop=id
  return, result
END

FUNCTION DeleteElement, array, index
  first = 0
   last = N_Elements(array)-1
   CASE index OF
      first: array = array[1:*]
      last: array = array[first:last-1]
      ELSE: array = [ array[first:index-1], array[index+1:last] ]
   ENDCASE
   return, array
END

Pro ToggleIDLBackingStore, ON=on, OFF=off
 IF keyword_set(on) THEN DEVICE, retain=2
 IF keyword_set(off) THEN DEVICE, retain=1
END

FUNCTION ggT, m, n
a=m & b=n
WHILE 1 DO BEGIN
      r = a MOD b
      IF (r EQ 0) THEN BEGIN
          return, b
      END ELSE BEGIN
          a=b & b=r
      END
  END
  
END


FUNCTION IDLVersionToDouble, version, DIGITS=digits
  IF NOT(keyword_set(digits)) THEN digits=2
  fact=1.D &  For i=1,digits DO fact=fact/10.
  nums=Double(STRSPLIT(version,'.',/Extract))
  res=0 & i=N_Elements(nums)-1
  While (i GT 0) DO BEGIN
     res=(res+nums[i])*fact
     i=i-1
  END
  res+=nums[0]
  return, res
END



PRO SplitPath, f, path, filename
  path="" & filename=""
  s=STRSPLIT(f, Path_Sep(), /EXTRACT)
  N=N_ELEMENTS(s)
  if (N GT 0) THEN BEGIN
      filename=s(N-1)
      if (N GT 1) THEN BEGIN 
          if (!version.os_family EQ 'unix') THEN path=Path_Sep()+s[0] $
             ELSE path=s[0]
          for i=1,(N_ELEMENTS(s)-2) DO path=path + Path_Sep() + s[i]  
      END
  END
END

function datatype,var, flag0, descriptor=desc, help=hlp, Tname = tname
 ;+
; NAME: 
;      DATATYPE()
;
; PURPOSE: 
;      Returns the data type of a variable.
;
; EXPLANATION: 
;      This routine returns the data type of a variable in a format specified
;      by the optional flag parameter.    Can also be used to emulate, in 
;      earlier versions of IDL, the SIZE(/TNAME) option introduced in V5.1.
;
;      This routine was originally derived from the JHUAPL library ***but has 
;      diverged from the JHUAPL library for the newer data types.***  For this
;      reason DATATYPE is no longer used in any other procedure in the IDL 
;      Astronomy Library.
; CALLING SEQUENCE         : 
;      Result = DATATYPE( VAR  [, FLAG , /TNAME, /DESC ] )
;
; INPUTS: 
;     VAR     = Variable to examine, no restrictions
;
; OPTIONAL INPUT PARAMETERS: 
;     FLAG  = Integer between 0 and 3 giving the output format flag as 
;             explained below.  The default is 0.
;     /DESC = If set, then return a descriptor for the given variable.  If the
;             variable is a scalar the value is returned as a string.  If it is
;             an array a description is returned just like the HELP command 
;             gives.  Ex:'
;             IDL> print, datatype(fltarr(2,3,5),/desc) gives the string
;                           'FLTARR(2,3,5)'
;     /TNAME - If set, then returns a identical result to the use of the /TNAME
;                keyword to the SIZE() function in IDL V5.2 and later.   
;               Overrides the value of FLAG.
;     /HELP    = If set, then a short explanation is printed out.
;
; OUTPUT PARAMETERS: 
;       The result of the function is the either a string or integer giving the
;       data type of VAR.  Depending on the value of FLAG or /TNAME, the result
;       will be one of the values from the following table:
;
;      FLAG = 0       FLAG = 1       FLAG = 2    FLAG = 3      /TNAME
;
;      UND            Undefined        0          UND          UNDEFINED  
;      BYT            Byte             1          BYT          BYTE
;      INT            Integer          2          INT          INT
;      LON            Long             3          LON          LONG
;      FLO            Float            4          FLT          FLOAT
;      DOU            Double           5          DBL          DOUBLE
;      COM            Complex          6          COMPLEX      COMPLEX
;      STR            String           7          STR          STRING
;      STC            Structure        8          STC          STRUCT
;      DCO            DComplex         9          DCOMPLEX     DCOMPLEX
;      PTR            Pointer         10          PTR          POINTER
;      OBJ            Object          11          OBJ          OBJREF
;      UIN            UInt            12          UINT         UINT
;      ULN            ULong           13          ULON         ULONG
;      L64            Long64          14          LON64        LONG64
;      U64            ULong64         15          ULON64       ULONG64
;
;
; REVISION HISTORY: 
;       Original Version: R. Sterner, JHU/APL, 24 October 1985.
;       Major rewrite, add /TNAME keyword, unsigned and 64 bit datatypes
;       W. Landsman   August 1999
;       Zarro (SM&A/GSFC) - November 2001, replace error stops by continues
;-
;-------------------------------------------------------------
 
 
        if (N_params() lt 1) or keyword_set(hlp) then begin
          print,' Datatype of variable as a string (3 char or spelled out).'
          print,' typ = datatype(var, [flag])'
          print,'   var = variable to examine.         in'
          print,'   flag = output format flag (def=0). in'
          print,'   typ = datatype string or number.   out'
          print,'      flag=0    flag=1      flag=2  flag=3      /TNAME'
          print,'      UND       Undefined   0       UND         UNDEFINE'
          print,'      BYT       Byte        1       BYT         BYTE'
          print,'      INT       Integer     2       INT         INT'
          print,'      LON       Long        3       LON         LONG'
          print,'      FLO       Float       4       FLT         FLOAT'
          print,'      DOU       Double      5       DBL         DOUBLE'
          print,'      COM       Complex     6       COMPLEX     COMPLEX'
          print,'      STR       String      7       STR         STRING'
          print,'      STC       Structure   8       STC         STRUCT'
          print,'      DCO       DComplex    9       DCOMPLEX    DCOMPLEX'
          print,'      PTR       Pointer    10       PTR         POINTER'
          print,'      OBJ       Object     11       OBJ         OBJREF'
          print,'      UIN       UInt       12       UINT        UINT'
          print,'      ULO       ULong      13       ULON        ULONG'
          print,'      L64       Long64     14       LON64       LONG64'
          print,'      U64       ULong64    15       ULON64      ULONG64'
          print,' Keywords:'                                     
          print,'  /TNAME - Identical output to SIZE(/TNAME) '
          print,'  /DESCRIPTOR returns a descriptor for the given variable.'
          print,'     If the variable is a scalar the value is returned as'
          print,'     a string.  If it is an array a description is return'
          print,'     just like the HELP command gives.  Ex:'
          print,'     datatype(fltarr(2,3,5),/desc) gives'
          print,'       FLTARR(2,3,5)  (flag always defaults to 3 for /DESC).'
          return, -1
        endif 
 
 s_tname = ['UNDEFINE', 'BYTE','INT','LONG','FLOAT','DOUBLE','COMPLEX',$
            'STRING','STRUCT','DCOMPLEX','POINTER','OBJREF','UINT','ULONG', $
            'LONG64','ULONG64']

 s_flag0 =  ['UND','BYT','INT','LON','FLO','DOU','COM','STR','STC','DCO','PTR',$
             'OBJ','UIN','ULO','L64','U64']

 s_flag1 = ['Undefined','Byte','Integer','Long','Float','Double','Complex', $
            'String','Structure','DComplex','Pointer','Object','UInt','ULong',$
            'Long64','ULong64']

 s_flag3 = [ 'UND','BYT','INT','LON','FLT','DBL','COMPLEX','STR','STC', $
            'DCOMPLEX','PTR','OBJ','UINT','ULON','LON64','ULON64']
 
       s = size(var)
       stype =  s[s[0]+1]
        if stype GT N_elements(s_tname) then begin
         message,'ERROR - Unrecognized IDL datatype',/cont
         stype=0
        endif

        if keyword_set(TNAME) then return, s_tname[stype]          

        if N_params() lt 2 then flag0 = 0      ; Default flag.
        if keyword_set(desc) then flag0 = 3
  
       case flag0 of 
 
  0: return, s_flag0[stype]
  1: return, s_flag1[stype]
  2: return, stype
  3: typ = s_flag3[stype]
  else: message,'ERROR - Flag parameter must be between 0 and 3'
  endcase 
  
 if keyword_set(desc) then begin
          if stype EQ 0 then begin
           message,'ERROR - Input variable is undefined',/cont
           return,'Undefined'
          endif
          if s[0] eq 0 then return,strtrim(var,2)       ; Return scalar desc.
          aa = typ+'ARR('
          for i = 1, s[0] do begin                      
            aa = aa + strtrim(s[i],2)                 
            if i lt s[0] then aa = aa + ','          
            endfor                                     
          aa = aa+')'                                   
          return, aa
        endif else return,typ
 
   end

FUNCTION SquareROIIndices, rowdim, coldim, width, POS=pos, NONCENTERED=noncentered
  ;; calculate an index patch in a 2D array
  ;; in case /NONCENTERED is not set: a (2*width+1)*(2*width+1) patch
  ;; of indices is returned,
  ;;
  ;; - example: width=1, noncentered=0
  ;;            returns a 3x3 patch
  ;;
  ;;                    ooo
  ;;                    oco
  ;;                    ooo
  ;;
  ;; in case /NONCENTERED is set: a width x width patch is returned
  ;;
  ;; - example: width=1, noncentered=1
  ;;            returns a 2x2 patch
  ;;
  ;;                    oo
  ;;                    co
  ;;
  ;; column: index DIV rowdim
  ;; row: index MOD rowdim
  ;;
  IF keyword_set(noncentered) THEN BEGIN
     filter=LONARR(width*width)
     count=0
     For i=0,width-1 DO BEGIN
        For j=0,width-1 DO BEGIN
           filter[count]=(LONG(j)*rowdim)+i
           count++
        END
     END
  END ELSE BEGIN
     filter=LONARR((2*width+1)*(2*width+1))
     count=0
     For i=-width,width DO BEGIN
        For j=-width,width DO BEGIN
           filter[count]=(LONG(j)*rowdim)+i
           count++
        END
     END
  END    
  IF keyword_set(pos) THEN BEGIN
     filter+=(LONG(pos[1])*rowdim)+pos[0]
  END
  return, filter
END


FUNCTION GetExtraObj
  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: No 3D Data found."
     return, 0
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: Current 3D Data invalid."
     return, 0
  END
  ptr=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  Menu: Current 3D Data holds no data."
     return, 0
  END
  return, (*ptr).extra
END

FUNCTION LeadingZero, s, l
 s=STRTRIM(STRING(s),2)
 WHILE (STRLEN(s) LT l) DO BEGIN 
     s='0' + STRING(s)
 END
 return, s    
END


