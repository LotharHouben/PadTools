FUNCTION read_json, file
    ;+#read_json
    ;+ Reads a JSON file that can have YAML like comments
    ;+***
    ;+##Arguments
    ;+    **file**: JSON file
    ;+
    ;+##Return Value 
    ;+Structure containg JSON values
    ;+
    ;+##Example Usage
    ;+```idl
    ;+IDL> json_struct = read_json("./file.json")
    ;+```

    openr,lun,file,/GET_LUN
    json=''
    tmp=''
    while not EOF(lun) do begin
        readf,lun,tmp
        json+=tmp
        json+=string(10b)
    endwhile
    free_lun,lun
    return, json
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

FUNCTION GuessFileName, stackp
  result=''
  s=''
  IF (SIZE(STRSPLIT((*stackp).name,'('),/N_ELEMENTS) GT 1) THEN BEGIN
     s= STRSPLIT((*stackp).name,'(',/EXTRACT)
     s='_'+s[0]
  END  
xmlfiles = FILE_SEARCH('*.xml', /FOLD_CASE, COUNT=count)
IF (count GE 1) THEN BEGIN
   ;; found an EMPAD descriptor
   ;; strip off suffix
   if (count GT 1) THEN xmlfiles=xmlfiles[0]
   Result = FILE_BASENAME(xmlfiles, '.xml', /FOLD_CASE)
   return, result+s 
END
dectrismaster= FILE_SEARCH('_master.h5', /FOLD_CASE, COUNT=count)
IF (count GE 1) THEN BEGIN
   ;; found an EMPAD descriptor
   ;; strip off suffix
   if (count GT 1) THEN dectrismaster=dectrismaster[0]
   Result = FILE_BASENAME(xmlfilesdectrismaster, '_master.h5', /FOLD_CASE)
   return,  result+s
END
return, result+s
END

