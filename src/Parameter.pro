;+
; NAME: Parameter
;
;
;
; PURPOSE: Object Class for handling parameters
;          The object handles parameter sets in hashes
;          A parameter structure can be stored and written to or can
;          be read from a file
;          The object has methods 
;          - to store a pointer to a structure 
;            under a tagname
;          - to retrieve the pointer using this tagname
;          - to write the structure data for all tagnames to a file
;          - to retrieve the tagnames and structure data from a file
;
; DEPENDENCIES: requires ReadEntry.pro, FileCheck.pro,
; CleanString.pro, XHashDialog
;
; MODIFICATION HISTORY:
;
;-
;; TODO
;; - arrays need to be processed by pointer dereference!

Function ParameterIOObj::init, fname
  self.fname=fname
  ;; initialize paramter match hash
  self.phash=hash()
  return, 1
END

Pro ParameterIOObj__define
  ;; fname: filename
  ;; lun: logical unit number
  ;; readable: flag - indicates whether fname is a readable file
  ;; writeable: flag - indicates whether fname is writeable file
  ;;                   phash: pointer to a hash that contains pinters to structures with 
  ;;                   parameter data
  void={ParameterIOObj, fname:'', lun:-1L, readable:0B, writeable:0B, phash:hash()}
END



FUNCTION ParameterIOObj::isset, tagname
  ;; reads whether the data field of the structure self.pstruc with tag name
  ;; is defined
  res=0
  IF (self.phash).HasKey(tagname) THEN BEGIN
     res=1
  END
  return, res
END

PRO ParameterIOObj::create_field, tagname
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; add a new hash tag
     (self.phash)[tagname]=PTR_NEW() ;; empty structure pointer 
  END ELSE BEGIN
     ;; delete old structure data
    print, "ParameterIOObj::create_field: (Warning) Field "+tagname+" already exists." 
  END
  return
END

PRO ParameterIOObj::add_token, tagname, tokenname, tokenvalue
;; add a token to a field
;; tagname=name of the field
;; tokenname=name of the token
;; tokenvalue=intitial value
;; example: pobj->add_token, 'DIALOG::COLORS', 'R', 255
;;          pobj->add_token, 'DIALOG::COLORS', 'G', 255
;;          pobj->add_token, 'DIALOG::COLORS', 'B', 255
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; add a new hash tag
     print, "ParameterIOObj::add_token: Field "+tagname+" does not exists."
  END ELSE BEGIN
     pstruct=(self.phash)[tagname] 
     IF PTR_VALID(pstruct) THEN BEGIN
        ;; Structure exists, append token
        ;; first check if it already exists
        tmpstruct=*pstruct
        N=N_TAGS(tmpstruct)
        IF (N GE 1) THEN BEGIN
           tagnames=TAG_NAMES(tmpstruct)
           ;; For i=1, N_TAGS(tmpstruct) DO BEGIN
           ;; END
           tagexists=WHERE(tagnames EQ tokenname)
           IF (tagexists GE 0) THEN BEGIN
              print, "ParameterIOObj::add_token: Field "+tagname+" already has a field " + tokenname +"."
           END ELSE BEGIN
              ;; tokenname does not exist, add it to the structure
              (self.phash)[tagname]=PTR_NEW(CREATE_STRUCT(Tokenname, tokenvalue, tmpstruct))
              ;; release old pointer
              PTR_FREE, pstruct
           END
        END ELSE BEGIN
           ;; Structure is empty, create a new one
           (self.phash)[tagname]=PTR_NEW(CREATE_STRUCT(Tokenname, tokenvalue))
           PTR_FREE, pstruct
        END  
     END ELSE BEGIN
        ;; Structure does not exist, create a new one
        (self.phash)[tagname]=PTR_NEW(CREATE_STRUCT(Tokenname, tokenvalue))
     END
  END
END 

FUNCTION ParameterIOObj::get_token_value, tagname, tokenname, ERRSTATE=errstate
  ;; returns the token value
  ;; errstate is 0 if successful, 1 otherwise
  ;; example: red=pobj->get_token_value('DIALOG::COLORS','R', ERR=err)
  errstate=1
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; add a new hash tag
     print, "ParameterIOObj::get_token_value: Field "+tagname+" does not exists."
     return, 0
  END ELSE BEGIN
     pstruct=(self.phash)[tagname] 
     IF PTR_VALID(pstruct) THEN BEGIN
        ;; Structure exists, append token
        ;; first check if it already exists
        tmpstruct=*pstruct
        N=N_TAGS(tmpstruct)
        IF (N GE 1) THEN BEGIN
           tagnames=TAG_NAMES(tmpstruct)
           tagexists=WHERE(tagnames EQ tokenname)
           IF (tagexists GE 0) THEN BEGIN
              ;; tokenname exists, get the value
              errstate=0
              return, (*pstruct).(tagexists)
           END ELSE BEGIN
              ;; tokenname does not exist
              print, "ParameterIOObj::get_token_value: Field "+tagname+" does not have a token "  + tokenname +"."
              return, 0
           END
        END ELSE BEGIN
           ;; Structure is empty
           print, "ParameterIOObj::get_token_value: Field "+tagname+" does not have a valid token."
           return, 0
        END  
     END ELSE BEGIN
        ;; Structure does not exist, create a new one
        print, "ParameterIOObj::get_token_value: Field "+tagname+" does not have any tokens."
        return, 0
     END
  END
  return, 0
END 

PRO ParameterIOObj::set_token_value, tagname, tokenname, tokenvalue, ERRSTATE=errstate
  ;; sets the token value
  ;; old values will be overwritten with the new content
  ;; pointers will be overwritten as well, if tokenvalue is a new
  ;; pointer the the old pointer will be released
  ;; 
  ;; errstate is 0 if successful, 1 otherwise
  ;; example: pobj->set_token_value('DIALOG::COLORS','R', 128, ERR=err)
  errstate=1
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; add a new hash tag
     print, "ParameterIOObj::set_token_value: Field "+tagname+" does not exists."
     return
  END ELSE BEGIN
     pstruct=(self.phash)[tagname] 
     IF PTR_VALID(pstruct) THEN BEGIN
        ;; Structure exists, append token
        ;; first check if it already exists
        tmpstruct=*pstruct
        N=N_TAGS(tmpstruct)
        IF (N GE 1) THEN BEGIN
           tagnames=TAG_NAMES(tmpstruct)
           tagexists=WHERE(tagnames EQ tokenname)
           IF (tagexists GE 0) THEN BEGIN
              ;; tokenname exists, set the value
              val=(*pstruct).(tagexists)
              ;; compare data types
              IF (SIZE(val, /TYPE) NE SIZE(tokenvalue, /TYPE)) THEN BEGIN
                 print, "ParameterIOObj::set_token_value: Type mismatch for token "+tokenname+" in field "+tagname+"."
                 return
              END ELSE BEGIN
                 (*pstruct).(tagexists)=tokenvalue
                 IF (SIZE(val, /TYPE) EQ 10) THEN BEGIN
                    IF (tokenvalue NE val) THEN PTR_FREE, val
                 END
              END
              ;;
              errstate=0
              return
           END ELSE BEGIN
              ;; tokenname does not exist
              print, "ParameterIOObj::get_token_value: Field "+tagname+" does not have a token "  + tokenname +"."
              return
           END
        END ELSE BEGIN
           ;; Structure is empty
           print, "ParameterIOObj::get_token_value: Field "+tagname+" does not have a valid token."
           return
        END  
     END ELSE BEGIN
        ;; Structure does not exist, create a new one
        print, "ParameterIOObj::get_token_value: Field "+tagname+" does not have any tokens."
        return
     END
  END
  return
END

FUNCTION ParameterIOObj::set_field, tagname, pstruct
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; add a new hash tag
     (self.phash)[tagname]=pstruct
  END ELSE BEGIN
     ;; delete old structure data
     ptmp=(self.phash)[tagname]
     IF PTR_VALID(ptmp) THEN PTR_FREE, ptmp
     (self.phash).Remove, tagname
     (self.phash)[tagname]=pstruct
  END
  return, 1
END

FUNCTION ParameterIOObj::get_field, tagname
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; tag does not exist
     return, 0
  END
  ;; add the field to the hash behind tag, if it does not exist yet
  return, (self.phash)[tagname]
END

FUNCTION ParameterIOObj::edit_field_tokens, tagname, EXCLUDE=exclude, HELP=help
  IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; tag does not exist
     return, 0
  END
  res=1
  ;; add the field to the hash behind tag, if it does not exist yet
  pstruct=(self.phash)[tagname]
  IF PTR_VALID(pstruct) THEN BEGIN
     ;; Structure exists
     N=N_TAGS(*pstruct)
     IF (N GE 1) THEN BEGIN
        edittoken=BYTARR(N) XOR 1
        tokennames=TAG_NAMES(*pstruct)
        ;; check whether elements should be exluded from editing
        if keyword_set(exclude) THEN BEGIN
           for i=0,N_Elements(exclude)-1 DO BEGIN
              hit=WHERE(tokennames EQ exclude[i])
              if ((hit GE 0) AND (hit LT N)) THEN BEGIN
                 edittoken[hit]=0
              END
           END
        END
        ;; pointer elements should be exluded from editing
        for i=0,N-1 DO BEGIN
           IF (SIZE((*pstruct).(i), /TYPE) EQ 10) THEN edittoken[i]=0
        END
        ;; open the dialog to edit the token values
        mydialog=obj_new("ListDialog")
        FOR i=0,N-1 DO BEGIN
           IF (edittoken[i] EQ 1) THEN mydialog->AddOption, tokennames[i], (*pstruct).(i)
        END
        mydialog->SetTitle, tagname
        IF keyword_set(help) THEN mydialog->SetHelp, help
        ;; launch the dialog window and get the return value
        ;; if the return value is 0 then the user pressed the cancel
        ;; button
        IF (mydialog->UserDialog() GT 0) THEN BEGIN
           ;; read parameter values from the Dialog object
           ;; notes:
           ;; - make sure that the parameter name matches exactly the
           ;;   definition in the mydialog->AddOption call 
           FOR i=0,N-1 DO BEGIN
              IF (edittoken[i] EQ 1) THEN BEGIN
                 tempvar = SIZE(TEMPORARY(x)) ;; undefine x
                 IF (mydialog->Value(tokennames[i],x)) THEN BEGIN
                    (*pstruct).(i)=x
                 END ELSE print, "Invalid option/parameter for "+tokennames[i]
              END
           END
           print, mydialog->report()
        END  ELSE res=0
;; destroy the Dialog object to free memory
        obj_destroy, mydialog
     END
  END
  return, res
END 



PRO ParameterIOObj::report, KEY=key
 l=(self.phash).Keys()
 FOREACH e, l DO BEGIN
    ;; print, "Key: ", e
    ptmp=(self.phash)[e]
    IF PTR_VALID(ptmp) THEN BEGIN
       HELP, *ptmp, OUTPUT=s
       IF Keyword_set(key) THEN BEGIN
          IF (STRUPCASE(key) EQ e) THEN BEGIN
             PrintToCon, s
          END
       END ELSE BEGIN
          PrintToCon, s
       END
    END
 END
END   



Function ParameterIOObj::save_file, FILENAME=filename
  ;; saves the structure  *(self.pstruc) to the file self.fname
  ;; scalar values can be transferred directly 
  ;; arrays should be transferred as pointers
;;  IF (FileIsWriteable(self.fname) NE 1) THEN BEGIN
;;     print, "% ParameterIOObj::save_file: The file "+ self.fname + " is not writeable."
;;     ;; ErrMsg, "% ParameterIOObj::save_file: The file "+ self.fname + " is not writeable."
;;     return, 0
;;  END
  IF LMGR(/Demo) THEN BEGIN
     Print, '% ParameterIOObj::save_file: IDL is running in DEMO mode, no file IO'
     return, 0
 END
  GET_LUN, tmp
  self.lun=tmp
  IF NOT(keyword_set(filename)) THEN filename=self.fname
  openw, self.lun, filename, ERROR = o_err
  if (o_err NE 0 ) then begin
     print, "% ParameterIOObj::save_file: Error while trying to open file " + filename+ ":" + !ERR_STRING
     return, 0
  endif 
  print, "% ParameterIOObj::save_file: Writing keys to " + filename+ "."
  l=(self.phash).Keys()
  
  FOREACH e, l DO BEGIN
     print, "Exporting key: ", e
     ptmp=(self.phash)[e]
     IF PTR_VALID(ptmp) THEN BEGIN
        tmpstruct=*ptmp
        N=N_TAGS(tmpstruct)
        IF (N GE 1) THEN BEGIN
           
           tagnames=TAG_NAMES(tmpstruct)
           For i=1, N_TAGS(tmpstruct) DO BEGIN
              ;; ---- print tagname
              printf, self.lun, "["+e+"::"+tagnames(i-1)+"]" 
              type=SIZE(tmpstruct.(i-1),/TNAME)
              ;; ---- print datatype
              printf, self.lun, type
              ;; dimensions of the field
              ;; 0: scalar, treat pointer differently!
              ;; 1: one dimensional array
              dim=SIZE(tmpstruct.(i-1),/N_DIMENSION)
              ;; ---- print, dimension
              printf, self.lun, CleanString(dim)
              IF (dim EQ 1) THEN BEGIN
                 ;; it is an array
                 nelem=SIZE(tmpstruct.(i-1),/N_ELEMENTS)
                 printf, self.lun, CleanString(nelem)
                 For j=1,nelem DO BEGIN
                    printf, self.lun, MySTRING((tmpstruct.(i-1))[j-1])
                 END
              END
              IF (dim EQ 0) THEN BEGIN
                 ;; is it a pointer?
                 if (type EQ 'POINTER') THEN BEGIN 
                    pp=tmpstruct.(i-1)
                    IF PTR_VALID(pp) THEN BEGIN
                       ;; dereference pp and check if it is an array
                       type=SIZE((*pp),/TNAME)
                       dim=SIZE((*pp),/N_DIMENSION)
                       nelem=SIZE((*pp),/N_ELEMENTS)
                       printf, self.lun, CleanString(type)
                       printf, self.lun, CleanString(dim)
                       ;; 
                       if (dim EQ 0) THEN BEGIN
                          ;; it is a pointer to a scalar
                          printf, self.lun, CleanString((*pp))
                       END
                       if (dim GT 0) THEN BEGIN
                          printf, self.lun, CleanString(nelem)
                          For j=1,nelem DO BEGIN
                             printf, self.lun, CleanString((*pp)[j-1])
                          END
                       END 
                    END ELSE BEGIN
                       ;; it is a NIL pointer
                       printf, self.lun, "NIL"
                    END
                 END ELSE BEGIN
                    ;; is it a pointer?
                    ;; seems to be a scalar
                    printf, self.lun, CleanString(tmpstruct.(i-1))
                 END
              END
              printf, self.lun, ""
           END
        END
     END 
  END   
     IF (self.lun GT -1) THEN BEGIN
        close, self.lun
        free_lun, self.lun
     self.lun=-1
  END
  return, 1
END 



Function ParameterIOObj::read_file, FILENAME=filename
  ;; reads the structure *(self.pstruc) from the file self.fname
  tag="init"
;;  IF NOT(GetDebugFlag()) THEN
  BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      ErrMsg, "ParameterIOObj::read_file: Error reading tag "+tag+" - "+ !ERROR_STATE.MSG
      CATCH, /Cancel
      return, 0
   END
  END 
  IF NOT(keyword_set(filename)) THEN filename=self.fname
  IF (FILE_TEST(filename ,/READ) NE 1) THEN BEGIN
     print, "% ParameterIOObj::read_file: The file "+ filename + " is not readable."
     return, 0
  END
  GET_LUN, tmp
  self.lun=tmp
  openr, self.lun, filename, ERROR = o_err
  if (o_err NE 0 ) then begin
     print, "% ParameterIOObj::read_file: Error while trying to open file " + filename + ":" + !ERR_STRING
     return, 0
  endif 
  print, "% ParameterIOObj::read_file: Reading keys from " + filename + "."
  l=(self.phash).Keys()
 FOREACH e, l DO BEGIN
    ;; IF GetDebugFlag() THEN print, "% ParameterIO::read_file: Searching key ", e
    pstruc=(self.phash)[e]
    IF PTR_VALID(pstruc) THEN BEGIN
       N=N_TAGS(*pstruc)
       IF (N GE 1) THEN BEGIN
          tagnames=TAG_NAMES(*pstruc)
          For i=1,N DO BEGIN
             ;; read entry
             tag="["+e+"::"+tagnames(i-1)+"]"
             ;; IF GetDebugFlag() THEN print, "% ParameterIO::read_file: Scanning for tag "+tag
             s=ReadEntry(tag, self.lun)
             ;; IF GetDebugFlag() THEN print, "% Search result: ", s
             ;; parse s
             ;; regular case: 
             ;; s[0] is the type
             ;; s[1] is the dimension
             ;; if dimension is 0 (scalar) then s[2]=scalar value
             ;; if dimension is 1 (array) then s[2] is the number of elements 
             ;;                            and s[3...(3+s[2]-1)] are the elements
             ;; pointer case:
             ;; s[0]='POINTER'
             ;; s[1]=0 
             ;; s[2=2+0] is the type
             ;; s[3=2+1] is the dimension
             ;; if dimension is 0 (scalar) then s[4=2+2]=scalar value
             ;; if dimension is 1 (array) then s[4=2+2] is the number of elements 
             ;;                            and s[5=2+3...(5+s[4]-1=2+3 +s[4=2+2]-1)] are the elements
             IF (SIZE(s,/DIMENSIONS) GT 0) THEN BEGIN
                ;; we found a matching entry in the file!
                ;; IF GetDebugFlag() THEN print, "% ParameterIOObj::read_file: Found entry for tag "+tagnames(i-1)+"."
                ;; (self.pmatch)[tagnames(i-1)]=1B
                ;; check for pointer type first
                ispointer=0
                IF (CleanString(s[0]) EQ 'POINTER') THEN BEGIN
                   ispointer=1
                   ;; reduce index by 2
                   tmp=s[2:(N_Elements(s)-1)]
                   s=tmp
                END
                IF (CleanString(s[0]) EQ 'NIL') THEN BEGIN
                   ;; it is a nil pointer
                   IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW()
                END
                ;; type is BYTE
                IF s[0] EQ 'BYTE' THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(BYTE(Fix(s[2]))) ELSE (*pstruc).(i-1)=BYTE(Fix(s[2]))
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(BYTARR(nelem)) ELSE (*pstruc).(i-1)=BYTARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=BYTE(Fix((s[2+j]))) ELSE $
                               (*pstruc).(i-1)[j-1]=BYTE(Fix((s[2+j])))
                         END
                      END
                   END
                END 
                ;; type is FLOAT
                IF s[0] EQ 'FLOAT' THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(Float(s[2])) ELSE (*pstruc).(i-1)=Float(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(FLTARR(nelem)) ELSE (*pstruc).(i-1)=FLTARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                      IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=Float((s[2+j])) ELSE $
                         (*pstruc).(i-1)[j-1]=Float((s[2+j]))
                   END
                      END
                   END
                END 
                ;; type is Double
                IF (CleanString(s[0]) EQ 'DOUBLE') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(DOUBLE(s[2])) ELSE (*pstruc).(i-1)=DOUBLE(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(DBLARR(nelem)) ELSE (*pstruc).(i-1)=DBLARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=DOUBLE((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=DOUBLE((s[2+j]))
                         END
                      END
                   END
                END 
                ;; type is STRING
                IF (CleanString(s[0]) EQ 'STRING') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN
                      ;; is a scalar
                      IF (N_Elements(s) GT 2) THEN BEGIN ;; fix a bug when string is empty
                         IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(STRING(s[2])) ELSE (*pstruc).(i-1)=STRING(s[2])
                      END
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(STRARR(nelem)) ELSE (*pstruc).(i-1)=STRARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=STRING((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=STRING((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'INT') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(Fix(s[2])) ELSE (*pstruc).(i-1)=Fix(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(INTARR(nelem)) ELSE (*pstruc).(i-1)=INTARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=Fix((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=Fix((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'LONG') THEN  BEGIN
              IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                 IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(LONG(s[2])) ELSE (*pstruc).(i-1)=LONG(s[2])
              END 
              IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                 nelem=FIX(s[2])
                 IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(LONARR(nelem)) ELSE (*pstruc).(i-1)=LONARR(nelem) 
                 FOR j=1,nelem DO BEGIN
                    IF (N_Elements(s) GT (2+j)) THEN BEGIN
                       IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=LONG((s[2+j])) ELSE $
                          (*pstruc).(i-1)[j-1]=LONG((s[2+j]))
                    END
                 END
              END
           END 
                IF (CleanString(s[0]) EQ 'UINT') THEN  BEGIN
              IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                 IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(UINT(s[2])) ELSE (*pstruc).(i-1)=UINT(s[2])
              END 
              IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                 nelem=FIX(s[2])
                 IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(UINTARR(nelem)) ELSE (*pstruc).(i-1)=UINTARR(nelem) 
                 FOR j=1,nelem DO BEGIN
                    IF (N_Elements(s) GT (2+j)) THEN BEGIN
                       IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=UINT((s[2+j])) ELSE $
                          (*pstruc).(i-1)[j-1]=UINT((s[2+j]))
                    END
                 END
              END
           END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONG(s[2])) ELSE (*pstruc).(i-1)=ULONG(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULONARR(nelem)) ELSE (*pstruc).(i-1)=ULONARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=ULONG((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=ULONG((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'LONG64') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(L64(s[2])) ELSE (*pstruc).(i-1)=L64(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(LON64ARR(nelem)) ELSE (*pstruc).(i-1)=LON64ARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=L64((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=L64((s[2+j]))
                         END
                      END
                   END
                END 
                IF (CleanString(s[0]) EQ 'ULONG64') THEN  BEGIN
                   IF (FIX(s[1]) EQ 0) THEN BEGIN ;; it is a scalar
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(UL64(s[2])) ELSE (*pstruc).(i-1)=UL64(s[2])
                   END 
                   IF (FIX(s[1]) EQ 1) THEN BEGIN ;; it is an array
                      nelem=FIX(s[2])
                      IF (ispointer EQ 1) THEN (*pstruc).(i-1)=PTR_NEW(ULON64ARR(nelem)) ELSE (*pstruc).(i-1)=ULON64ARR(nelem) 
                      FOR j=1,nelem DO BEGIN
                         IF (N_Elements(s) GT (2+j)) THEN BEGIN
                            IF (ispointer EQ 1) THEN (*(*pstruc).(i-1))[j-1]=UL64((s[2+j])) ELSE $
                               (*pstruc).(i-1)[j-1]=UL64((s[2+j]))
                   END
                      END
                   END
                END 
;;           STOP
  
        END
     END 
  END
    END
 END  


  IF (self.lun GT -1) THEN BEGIN
     close, self.lun
     free_lun, self.lun
     self.lun=-1
  END
  ;; STOP
  ;; self->report
  return, 1
END  



Function ParameterIOObj::remove, tagname
 IF NOT((self.phash).HasKey(tagname)) THEN BEGIN
     ;; tag does not exist
     return, 0
  END
    ptmp=(self.phash)[tagname]
    IF PTR_VALID(ptmp) THEN BEGIN
       tmpstruct=(*ptmp)
       tagnames=TAG_NAMES(tmpstruct)
       FOR i=1,N_TAGS(tmpstruct) DO BEGIN
          p=tmpstruct.(i-1)
          IF PTR_VALID(p) THEN PTR_FREE, p
       END
       PTR_FREE, ptmp
    END
    ;; remove tag
    (self.phash).Remove, tagname
    return, 1
 END 


Function ParameterIOObj::cleanup
 l=(self.phash).Keys()
 FOREACH e, l DO BEGIN
    ptmp=(self.phash)[e]
    IF PTR_VALID(ptmp) THEN BEGIN
       tmpstruct=(*ptmp)
       tagnames=TAG_NAMES(tmpstruct)
       FOR i=1,N_TAGS(tmpstruct) DO BEGIN
          p=tmpstruct(i-1)
          IF PTR_VALID(p) THEN PTR_FREE, p
       END
       PTR_FREE, ptmp
    END
   (self.phash).Remove, e
 END
 IF (self.lun GT -1) THEN BEGIN
    ;; check whether file is open, if so then close it 
     close, self.lun
     free_lun, self.lun
 END
END 


PRO TestParameterIO
  ;; Add tomography data parameters
  thetaaxis='x'
  theta=FINDGEN(10)
  filename='/Users/lothar/Desktop/test.prm'
  numslices=9U
  tiltaxis=1B
  ;; define a structure
  partags=['ThetaAxis','Theta','Numslices','tiltaxis']  
  struc=CREATE_STRUCT(partags[0],PTR_NEW(thetaaxis),partags[1],PTR_NEW(theta),partags[2],numslices,partags[3],tiltaxis)
  pstruc=PTR_NEW(struc)
  f=obj_new('ParameterIOObj', filename)
  res=f->set_field('Tomography::TiltSetup',pstruc)
  res=f->get_field('Tomography::TiltSetup')
  print, f->isset('Tomography::TiltSetup')  
  f->report
  res=f->save_file()
  ;; Add FileIO parameters  
  partags=['mrcslicedim','mrcslice','mrcextended']
  struc=CREATE_STRUCT(partags[0],2B,partags[1],0B,partags[2],0B)
  pstruc=PTR_NEW(struc)
  res=f->set_field('FileIO::MRC',pstruc)
  res=f->get_field('FileIO::MRC')
  print, f->isset('FileIO::MRC')  
  f->report
  res=f->save_file()
  print, f->remove('Tomography::TiltSetup')  
  print, f->isset('Tomography::TiltSetup')
  obj_destroy, f
END


PRO TestParameterIO_2
  homedir=GETENV('HOME')
  userf=homedir+PATH_Sep()+"test.dlg"
  f=obj_new('ParameterIOObj', userf) 
  ;; initialize parameter data
  ;; scalar parameter
  f->create_field, 'DIALOG::COLORS'
  f->add_token, 'DIALOG::COLORS', 'R', 255B
  f->add_token, 'DIALOG::COLORS', 'G', 128B
  f->add_token, 'DIALOG::COLORS', 'B', 12B
  ;; how to store a one-dimensional array
  angles=FINDGEN(10)
  f->create_field, 'DIALOG::ROTATION'
  f->add_token, 'DIALOG::ROTATION', 'ANGLES', PTR_NEW(angles)
  ;; report data
  print, "+-" 
  print, "TestParameterIO-DEMO: Initialized Parameter Data:" 
  print, "+-" 
  f->report
  ;;
  IF NOT(f->read_file()) THEN print, "ParameterTest: Failed to write data to "+userf+"."
  ;;
  print, "+-" 
  print, "TestParameterIO-DEMO: Updated Parameter Data from File:" 
  f->report
  print, "+-" 
  ;;
  f->set_token_value, 'DIALOG::COLORS', 'G', 25B, ERRSTATE=err
  print, "+-" 
  print, "TestParameterIO-DEMO: Modified Parameter Data from File:" 
  print, "+-" 
  f->report
  ;; save data
  IF NOT(f->save_file()) THEN print, "ParameterTest: Failed to write data to "+userf+"."
  obj_destroy, f
END

