PRO Initialize 
 COMMON MYPATH, CurrPath
 CurrPath='.'
END



Pro PadTools
  ;; create the structure list for the 3D Data
  ptr=DataList_Init('3D-Data')
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     print, "% tomato: fatal error"
     print, "          could not create 3D data structure"
     return
  END
  SetRootP, ptr
  ;; create the structure list for the 2D Data (e.g. tilt angle values)
  ptr2D=DataList_Init('2D-Data')
  IF NOT(PTR_VALID(ptr2D)) THEN BEGIN
     print, "% tomato: fatal error"
     print, "          could not create 2D data structure"
     return
  END
  SetRootP2D, ptr2D
  ;; ;; create the structure list for the 2D Graphs (e.g. SIRT RMS plots)
  ptrXYGraph=XYGraphList_Init('XY-Graphs')
  IF NOT(PTR_VALID(ptrXYGraph)) THEN BEGIN
     print, "% tomato: fatal error"
     print, "          could not create XY graph list structure"
     return
  END
  SetRootPXYGraph, ptrXYGraph
  ;; ;; create the structure list for the 2D Graphs (e.g. SIRT RMS plots)
  ptrTable=XYGraphList_Init('Tables')
  IF NOT(PTR_VALID(ptrTable)) THEN BEGIN
     print, "% tomato: fatal error"
     print, "          could not create table list structure"
     return
  END
  SetRootPTable, ptrTable
  ;; set preferences
  UpdateMonitorInfo
  ;; SetCorrelPrefs
  SetBinFmtPrefs3D
  SetBinFmtPrefs2D
  InitPlotColors
  InitMousePrefs
  SetProgressBarPrefs
  SetDebugFlag, 0
  SetContrastPrefs
  SetContrastInspectorPrefs
  ;;
  TestPreferences ;; <- read preferences from file, also [License] field
  SetHomeDir
  SetBaseDir
  ;;
  ;; read calibrations
  ;; mycallist=GetCalibrationListObject()
  calfile=GetEnv('HOME')+Path_Sep()+"MicroscopeCalibrationDB.json"
  print, "% Loading calibration file ", calfile
  SetCalibrationObj, OBJ_NEW('CalibrationObj',calfile)
  ;; launch menu
  ;;
  ;; License Check
  f=GetDLGParObject()
  ;;
  ;; get color model
  load_default_colortable
  ;; 
  lic=CheckLicense() ;; calculate license type, default is 0, meaning
  if (lic GT 100) then begin
     print, "% Starting developer GUI."
     XMenu, DEVELOPER=1
  END ELSE BEGIN
     print, "% Starting user GUI."
     XMenu, DEVELOPER=0
  END

  XTabControl
  XConsole
  XTabContrastDialog
  ;; XContrast
  ;;

  s=["% ", $
     "% Padtools   - IDL PAD Processing Tools", $
     "%              "+GetVersion(), $
     "%              (c) L. Houben", $
;;     "%              2006-2015 FZ Juelich GmbH", $
;;     "%              2015-     Weizmann Institute of Science", $
;;     "%              ", $
     "%              E-mail: lothar.houben@weizmann.ac.il", $
     "% ", $
     "% Disclaimer - PadTools is provided by the copyright", $
     "%              holders as is, and any express or implied", $
     "%              warranties including the fitness for a ", $
     "%              particular purpose are disclaimed.", $
     "%              In no event shall the copyright owner or ", $
     "%              contributor be liable for any direct,", $
     "%              or indirect damage including but not ", $
     "%              limited to data loss or business interruption.", $
     "% "]
  printtocon, s
  lic=CheckLicense()
END




