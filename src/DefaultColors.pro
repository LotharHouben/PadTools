PRO set_default_colormode, decomposed
  pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     printtocon, "% Set_default_colortable: Parameter object is not defined. Send bug report."
     return
  END
  pobj->set_token_value, 'COLORMAP', 'DECOMPOSED', decomposed
END

PRO set_default_colortable, FILE=file, INDEX=index
  sysfile=Filepath(SubDir=['resource','colors'], 'colors1.tbl')
  pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     printtocon, "% Set_default_colortable: Parameter object is not defined. Send bug report."
     return
  END
  IF keyword_set(file) THEN pobj->set_token_value, 'COLORMAP', 'DEFAULT', file ELSE pobj->set_token_value, 'COLORMAP', 'DEFAULT', sysfile
  IF keyword_set(index) THEN pobj->set_token_value, 'COLORMAP', 'INDEX', index ELSE pobj->set_token_value, 'COLORMAP', 'INDEX', 0
END

PRO load_default_colortable, FILE=file, INDEX=index
;; overwrite with file from parameter object if available
  sysfile=Filepath(SubDir=['resource','colors'], 'colors1.tbl')
  IF NOT(keyword_set(file)) THEN file=sysfile
  IF NOT(keyword_set(index)) THEN index=0
  pobj=GetDLGParObject()
  
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     printtocon, "% Set_default_colortable: Parameter object is not defined. Send bug report."
     return
  END
  decomposed=pobj->get_token_value('COLORMAP','DECOMPOSED')
  IF (decomposed EQ 1)  THEN BEGIN
  printtocon, "% Set_default_colortable: Default mode is True Color mode. Switch to Pseudocolor mode before."
      DEVICE, decomposed=1
     return
 END ELSE BEGIN
   tryfile=pobj->get_token_value('COLORMAP','DEFAULT')
   IF ((tryfile NE "") AND FILE_TEST(tryfile)) THEN BEGIN
      file=tryfile
      index=pobj->get_token_value('COLORMAP','INDEX')
   END ELSE BEGIN
      printtocon, "% Set_default_colortable: Could not load color map file " +tryfile +"."
      printtocon, "    Using system default tables instead."
   END 
END
;; Load CT
  ;; get names to check whether index is in the range of the number of
  ;; tables
  LOADCT, GET_NAMES=names, file=tryfile
  IF (N_ELEMENTS(names) LT 1) THEN BEGIN
     file=sysfile
     printtocon, "% Set_default_colortable: Color map file " +tryfile +" has no entry."
      printtocon, "    Using system default tables instead."
  END
  IF NOT(index LT N_ELEMENTS(names)) THEN BEGIN
     printtocon, "% Set_default_colortable: Invalid table index, taking first table in file."
     index=0
  END
  ;; now make sure that decomposed mode is not set
  DEVICE, decomposed=0
  LOADCT, index, FILE=file
  printtocon, "% Set_default_colortable:"
  printtocon, "     Color map file - " +file +"."
  printtocon, "     Color map      - " +names[index] +"."
END 

FUNCTION get_default_colortable, INDEX=index
;; overwrite with file from parameter object if available
  sysfile=Filepath(SubDir=['resource','colors'], 'colors1.tbl')
  ind=0
  pobj=GetDLGParObject()
  
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     printtocon, "% Get_default_colortable: Parameter object is not defined. Send bug report."
     file=sysfile
  END ELSE BEGIN
     file=pobj->get_token_value('COLORMAP','DEFAULT')
     ind=pobj->get_token_value('COLORMAP','DEFAULT')
  END
  if (keyword_set(index)) THEN index=ind
  return, file
END 
