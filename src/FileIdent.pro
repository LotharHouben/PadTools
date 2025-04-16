FUNCTION filetypes, READ=read, PTRARRAY=ptrarray, MATCHEXT=matchext, FULL=full
;; (0) return a hash with key and values:  r=filetypes(/READ) 
;; (1) Return a pointer to a string array with read data types
;; r=filetypes(/READ,/PTRARRAY)   
;; (2) search for a filetype when the extension is given
;; x=Filetypes(/READ,MATCHEXT="tif")
;; print, x.type  ;; shows TIFF
;; print, x.index ;; shows 15
IF keyword_set(read) THEN BEGIN
   s=OrderedHash('A',{type:"PadTools Data",ext:["ptd","json"],unique:""},$
   'B',{type:"PadTools HDF5",ext:["h5","hdf5"],unique:""},$
                    'C',{type:"EMPAD",ext:["xml"],unique:""},$
                    'D',{type:"DECTRIS",ext:["h5"],unique:"master.h5"},$
                    'E',{type:"MRC", ext:"mrc",unique:""}, $
                    'G',{type:"Raw Binary 3D-Data", ext:"raw",unique:""}, $
                    'L',{type:"Digital Micrograph Image", ext:["dm4","dm3"],unique:""}, $
                    ;;'I',{type:"Multiple Image TIFF", ext:["tif","tiff"],unique:""}, $
                    ;;'K',{type:"PIF", ext:["pif"],unique:""}, $
                    /EXTRACT, /FOLD_CASE, /NO_COPY)
      IF keyword_set(ptrarray) THEN BEGIN
         ;; extracttypes
         v=s.VALUES()
         res=PTR_NEW(STRARR(v.Count()))
         For i=0,v.Count()-1 Do BEGIN
            (*res)[i]=((v[i].VALUES())[0])
         END
         return, res
      END
      IF keyword_set(matchext) THEN BEGIN
         matchext=STRUPCASE(matchext)
         extmatching=list()
         uniquematching=list()
         res=""
         v=s.VALUES()
         For i=0,v.Count()-1 Do BEGIN
            IF (Total(STRMATCH(STRUPCASE(((v[i].VALUES())[1])),STRUPCASE(matchext))) GE 1) THEN BEGIN
               extmatching.Add, {type: ((v[i].VALUES())[0]), index: i}
            END
            IF (Total(STRMATCH(STRUPCASE(((v[i].VALUES())[2])),STRUPCASE(full))) GE 1) THEN BEGIN
               matching.Add, {type: ((v[i].VALUES())[0]), index: i}
            END
         END
         if (extmatching.Count() EQ 0) THEN BEGIN
            return, {type: "", index: -1}
         END
         if (extmatching.Count() EQ 1) THEN BEGIN
            return, extmatching[0]
         END
         if (extmatching.Count() GT 1) THEN BEGIN
            if keyword_set(full) THEN BEGIN
               matchstr=STRUPCASE(full)
               For i=1,extmatching.Count() DO BEGIN
                  h=extmatching[i-1]
                  ind=h.index
                  uns=STRUPCASE(((v[ind].VALUES())[2]))
                  IF (uns NE "" AND matchstr.contains(uns)) THEN BEGIN ;; found unique string in full name
                     return, extmatching[i-1]
                  END
               END
            END
            return, extmatching[0] ;; default result
         END 
      END
     return, s 
END
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

PRO SplitFileName, fname, base, ext
  base=fname & ext=""
  IF (STRLEN(fname) GT 0) THEN BEGIN
     subnm = StrSplit(fname, ".", /Extract)
     numsubnm = N_Elements(subnm)
     CASE numsubnm OF
        1: base = subnm[0]
        2: BEGIN
           base = subnm[0]
           ext = subnm[1]
        END
        ELSE: BEGIN
           base = StrJoin(subnm[0:numsubnm-2],'.')
           ext = subnm[numsubnm-1]
        END
     ENDCASE
  END
  return
END 



FUNCTION AutoType, filename
  fn="" & path="" & SplitPath, filename, path, fn
  base="" & ext=""
  SplitFileName, fn, base, ext
  IF (ext EQ "") THEN return, "" ;; no extension 
  
  
END
