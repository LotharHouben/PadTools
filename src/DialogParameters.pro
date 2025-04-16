FUNCTION GetDlgParObject
;; returns an object pointer to a parameter object 
COMMON DLGPAR, f
IF (N_Elements(f) GT 0) THEN BEGIN
   ;; the parameter object variable is defined
   ;; return object pointer if valid
   IF (OBJ_VALID(f)) THEN return, f
END 
;; +++++++++++++++++++++++++++++++++++++++++++++++++
;; shift analysis parameters 
;; +++++++++++++++++++++++++++++++++++++++++++++++++
homedir=GETENV('HOME')
userf=homedir+PATH_Sep()+"empadTools.dlg"
f=obj_new('ParameterIOObj', userf) 
;;
;; GSAlign Parameters
f->create_field, 'COLORMAP'
f->add_token, 'COLORMAP', 'DEFAULT', ""
f->add_token, 'COLORMAP', 'INDEX', 0
f->add_token, 'COLORMAP', 'DECOMPOSED', 1
;; cluster search
f->create_field, 'CLUSTER'
f->add_token, 'CLUSTER','DIAMETER', 2.5
f->add_token, 'CLUSTER','NOISELEN', 1.0
f->add_token, 'CLUSTER','SEPARATION', 3.
f->add_token, 'CLUSTER','MINMASS', 100.
f->add_token, 'CLUSTER','PEAKMIN', 10.
f->add_token, 'CLUSTER','MARGIN',4
f->add_token, 'CLUSTER','MEDWID',5
f->add_token, 'CLUSTER','ITERATE', 0
;; WINDOWS::
f->create_field, 'WINDOW'
f->add_token, 'WINDOW', 'EDITORWINHEIGHT', 15
f->add_token, 'WINDOW', 'EDITORWINWIDTH', 700
;; read data from file
res=f->read_file()
;;
;; f->report
;; return, f
END  


PRO DestroyDlgParObject
  f=GetDLGParObject()
  res=f->save_file()
  obj_destroy, f
END
