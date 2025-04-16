;; Modification for other programs:
;; (1) enter application string in the Function GetApplicationName
;; (2) enter application code in the Function GetApplicationCode
;; 
;; usage:
;; 
;; ;;
;; TestPreferences ;; <- read preferences from file, also [License] field
;; ;; Check License
;; lic=CheckLicense() ;; calculate license type, default is 0, meaning
;; ;; no license
;; if (lic GT 0) then begin
;;    ;; licensed ops
;; END ELSE BEGIN
;;    ;; no valid license
;;    DisplayLicenseMessage
;; END
;;
;; ;; Integration of the license dialog
;; 
;; base = WIDGET_BASE(TITLE=title, /COLUMN, MBAR=bar)
;; menu_1 = WIDGET_BUTTON(bar, VALUE='Preferences', /MENU)
;; licensebutton = WIDGET_BUTTON(menu_1, VALUE='License') 
;; ...
;; REPEAT BEGIN
;;    ev = WIDGET_Event(base, /NoWait)
;;    uv=""
;;    CASE ev.id OF
;;       licensebutton: BEGIN
;;          lic=LicenseDialog()
;;       END
;; ...

PRO SetBaseDir
COMMON BASEDIR, basedir
CD, CURRENT = basedir   ;; get current directory
END

FUNCTION GetBaseDir
COMMON BASEDIR, basedir
return, basedir
END




FUNCTION GetPreferencesFileName, basename, READ=read, WRITE=write
;; empty file is default
f=''
name=basename+'.pref'
IF NOT(keyword_set(dialogtext)) THEN dialogtext="preferences file"
erruser=1 ;; 1 means error while accessing user preferences,
          ;; it is the default
IF keyword_set(read) then writeable=0
IF keyword_set(write) then writeable=1
errcode=0 & errstr=""
;; check whether pref file in home directory is existent
IF keyword_set(read) then writeable=0
IF keyword_set(write) then writeable=1
homedir=GETENV('HOME')
userf=homedir+PATH_Sep()+name
;;
IF (Checkfile(userf, errcode, errstr, WRITEABLE=writeable) EQ 0) THEN BEGIN
   ;; problem with user pref file
   dummy= Dialog_Message(["An error occured while trying to access your ", $
                       "personal "+dialogtext+" in your home directory.", $
                       "", $
                       "Error message: ", $
                       errstr,$
                       "", $
                       "Trying to use the system-wide "+dialogtext+"."], /INFO)
   ;; check system preferences
   errcode=0 & errstr=""
    systemf=programrootdir()+GetApplicationName()+".pref"
    IF (Checkfile(systemf, errcode, errstr, WRITEABLE=writeable) EQ 0) THEN BEGIN
       ;; problem with user pref file
       dummy= Dialog_MESSAGE(["An error occured while trying to access your ",$
                           "system wide "+ dialogtext+ ".",$
                           "Error message: ", $
                           errstr], /INFORMATION)
    END ELSE BEGIN
       f=systemf
    END
END ELSE BEGIN
   f=userf
END
return, f
END


PRO SetWorkingDir, dir
COMMON WDIRECTORIES, dirlist
COMMON WDIR, workingdir
  Nlist=N_Elements(dirlist)
  I=Nlist-1
  WHILE (i GE 0) do BEGIN
      IF (dirlist[i] EQ dir) THEN BEGIN
          workingdir=dir
;;          printtocon, "% SetWorkingDir: directory already in list"
          return
      END
      i=i-1
  END
  ;; directory not in list
  ;; place as first element in dirlist
  i=Nlist-1
  While (i GE 1) do Begin
      dirlist[i]=dirlist[i-1]
      i=i-1
  END
  dirlist[0]=dir
  workingdir=dir
  return
END


FUNCTION GetAllWorkingDirs
COMMON WDIRECTORIES, dirlist
return, dirlist
END

FUNCTION GetWorkingDir
COMMON WDIR, workingdir
return, workingdir
END

PRO SetDesktopDir, dir
  COMMON DDIR, desktopdir
  desktopdir=dir
END

FUNCTION GetDesktopDir
COMMON DDIR, desktopdir
return, desktopdir
END


PRO SetAllWorkingDirs, x
COMMON WDIR, workingdir
COMMON WDIRECTORIES, dirlist
dirlist=x
workingdir=dirlist(0)
END

PRO SetWorkingDirData
COMMON WDIRECTORIES, dirlist
COMMON WDIR, workingdir
CD, CURRENT=workingdir ;; current directory
dirlist=[workingdir,".",".",".",".",".",".",".",".","."]
END

PRO SetAutosave, autoyesno
COMMON AutoSave, autos
CASE STRCOMPRESS(autoyesno) OF
   'no': autos=0
   'No': autos=0
   'NO': autos=0
   ELSE: autos=1
END
END

Function GetAutosave
  COMMON AutoSave, autos
  return, autos
END

Function GetFont
  COMMON Font, f
  return, f
END

PRO SetFont, fontsel
  COMMON Font, f
  f=fontsel
END

PRO SetLicenseData, lic
COMMON LICENSEDATA, licname, liccode
licname=lic[0] & liccode=lic[1]
END

FUNCTION GetLicenseData
COMMON LICENSEDATA, licname, liccode
return, [licname,liccode]
END


Pro InitPrefs
COMMON WDIR, workingdir
COMMON LICENSEDATA, licname, liccode
workingdir=programrootdir()
licname='00000000FF'
liccode='000000AA'
SetAutoSave, 'yes'
SetWorkingDirData
END

FUNCTION GetPrefDataStruct
basedir=programrootdir()
tmp=CREATE_STRUCT(name='SPreferences',['desktopdir','workingdir','license','autosave','idletime','calibrations','font'],basedir,[basedir,".",".",".",".",".",".",".",".","."],['0000000000','9999999999'],'yes',100.,PTR_NEW(),'helvetica*bold')
return, tmp
END




Function ReadPrefFromFile
;; read preferences from minor field in preferences structure
;; define
errcode=1 ;; error is default
looplimit=30
tmp=GetPrefDataStruct() ;; get an instance of the preferences data structure
basename=GetApplicationName()
FileName=GetPreferencesFileName(basename, /READ)
IF (FileName EQ '') THEN BEGIN
;;   dummy= Dialog_Info(["Warning: Unable to access preferences file.","Check error messages during program startup!"], /INFO)
   return, 1
END
Preflun=-1
;;IF LMGR(/Demo) THEN BEGIN
;;    Print, '% ReadPrefFromFile: IDL is running in DEMO mode, no file IO'
;;END Else
BEGIN
    ;; install an error handler in case the file io fails
   ;;if NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
    IF (Error_status NE 0) THEN BEGIN
        Print, "% ReadPrefFromFile: Error while accessing preferences file "+ FileName
        Print, "%                  " + !ERR_STRING
        CATCH, /Cancel
        IF ((Preflun GE 100) and (PrefLUN LE 128)) THEN FREE_LUN, Preflun
        return, 1
     END
    ;; END
    OPENR, Preflun, FileName, /GET_LUN
    print, '% ReadPrefFromFile: reading preferences from file ' + FileName
;; the License
    entry="[License]"
    s=ReadEntry(entry,Preflun)
;;    for i=0,N_Elements(s)-1 DO BEGIN
;;        print, s(i)
;;    END
    maxind=N_Elements(s)-1
;;    print, maxind
    if (maxind GT 1) THEN maxind=1
    if (maxind NE 0) THEN BEGIN
        for i=0,maxind DO BEGIN
           tmp.license(i)=s(i)
        END
    END
    ;; the Working directory
    entry="[Working directory]"
    s=ReadEntry(entry,Preflun)
    maxind=N_Elements(s)-1
    NDir=N_Elements(tmp.workingdir)-1
;;    print, NDir
    if (NDir LT maxind) THEN maxind=NDir
    if (maxind NE 0) THEN BEGIN
        for i=0,maxind DO BEGIN
           tmp.workingdir(i)=s(i)
        END
     END
    if (maxind EQ 0) THEN  tmp.desktopdir=programrootdir()
    SplitPath, tmp.workingdir(0), direct, filename
    dirwrite=FILE_TEST(direct, /DIRECTORY, /WRITE)
    IF (dirwrite EQ 0) THEN BEGIN
        print, "Error: Working directory "+direct+" does not exist or is not writeable!"
     END
 ;; the Desktop directory
    entry="[Desktop directory]"
    s=ReadEntry(entry,Preflun)
;;    for i=0,N_Elements(s)-1 DO BEGIN
;;        print, s(i)
;;    END
    maxind=N_Elements(s)
;;    print, maxind
    if (maxind NE 0) THEN tmp.desktopdir=s[0] ELSE  tmp.desktopdir=programrootdir()
    ;; SplitPath, tmp.desktopdir, direct, filename
    dirread=FILE_TEST(tmp.desktopdir, /DIRECTORY, /READ)
    IF (dirread EQ 0) THEN BEGIN
       print, "Error: Desktop directory "+direct+" does not exist or is not readable!"
    END
    ;; the autosave option
    entry="[Autosave]"
    s=ReadEntry(entry,Preflun)
    maxind=N_Elements(s)
    if (maxind NE 0) THEN tmp.autosave=s[0] ELSE  tmp.autosave='yes'
    ;; the autosave option
    entry="[Idletime]"
    s=ReadEntry(entry,Preflun)
    maxind=N_Elements(s)
    if (maxind NE 0) THEN tmp.idletime=FLOAT(s[0]) ELSE tmp.idletime=0.
;; the font option
    entry="[Font]"
    s=ReadEntry(entry,Preflun)
    maxind=N_Elements(s)
    if (maxind NE 0) THEN tmp.font=s[0] ELSE  tmp.font='helvetica*bold'
    ;; the autosave option
    FREE_LUN, Preflun
 END
;; set license data
SetLicenseData, tmp.license
;; set working directories
SetWorkingDir, tmp.workingdir(0)
SetAutosave, tmp.workingdir(0)
SetAllWorkingDirs, tmp.workingdir
;; Set Desktop directory
SetDesktopDir, tmp.desktopdir
;; set idletime for repeat loops in user interface
SetWaitTime, tmp.idletime*0.001
SetFont, tmp.font
return, 0
END


Pro SavePrefs
;;
tmp=GetPrefDataStruct() ;; get an instance of the preferences data structure
;;
;; update preferences data
;;
tmp.workingdir=GetAllWorkingDirs()
tmp.desktopdir=GetDesktopDir()
tmp.license=GetLicenseData()
tmp.idletime=GetWaitTime()*1000
IF (GetAutosave() EQ 1) then tmp.autosave='yes' ELSE tmp.autosave='no'
;;
basename=GetApplicationName()
;; FileName=programrootdir()+"ImageConverter.pref"
FileName=GetPreferencesFileName(basename, /WRITE)
IF (FileName EQ '') THEN BEGIN
   dummy= Dialog_Message(["Warning: Unable to access preferences file.",$
                       "Create an empty file called '"+GetApplicationName()+".pref'",$
                       "either in your home directory or in the",$
                       GetApplicationName()+" program directory! Make sure that",$
                       "the file has read/write permissions!"], /INFORMATION)
   return
END
print, '% SavePrefs: saving preferences to file ' + FileName
print, '% SavePrefs: saving to file ' + FileName
IF LMGR(/Demo) THEN BEGIN
    Print, '% SavePrefs: IDL is running in DEMO mode, no file IO'
END Else BEGIN

OPENW, Preflun, FileName, /GET_LUN
;; first write the License
PRINTF, Preflun, "[License]"
for i=0,(N_Elements(tmp.license)-1) DO BEGIN
    PRINTF, Preflun, STRING(tmp.license[i])
END
PrintF, Preflun, " "
PRINTF, Preflun, "[Working directory]"
for i=0,(N_Elements(tmp.workingdir)-1) DO BEGIN
    PRINTF, Preflun, STRING(tmp.workingdir[i])
 END
PrintF, Preflun, " "
PRINTF, Preflun, "[Desktop directory]"
PRINTF, Preflun, STRING(tmp.desktopdir)
PrintF, Preflun, " "
PRINTF, Preflun, "[Autosave]"
PRINTF, Preflun, STRING(tmp.autosave)
PrintF, Preflun, " "
PRINTF, Preflun, "[Idletime]"
PRINTF, Preflun, CleanSTRING(tmp.idletime)
PrintF, Preflun, " "
PRINTF, Preflun, "[Font]"
PRINTF, Preflun, CleanSTRING(tmp.font)
PrintF, Preflun, " "
FREE_LUN, Preflun
END
END

PRO TestPreferences
  InitPrefs
  basename=GetApplicationName()
  readsuccess=ReadPrefFromFile()
  if (readsuccess) THEN BEGIN
     license=GetLicenseData()
     print, "License Nr:", license(0)
     print, "License Code:", license(1)
  END ELSE BEGIN
     print, "% TestPreferences: Preferences file is not accessible"
  END
END

;; PRO TestShowEntries
;; FileName=programrootdir()+"ImageConverter.pref"
;; OPENR, Preflun, FileName, /GET_LUN
;; s=""
;; print, ShowEntries(s, preflun)
;; CLOSE, Preflun
;; END





FUNCTION GetMACAddresses, TEXT=text
  ;;get MACs
  result = LMGR(LMHOSTID = myId)
  ;;print, myID
  ;; split to array
  MACs=STRSPLIT(myId, " ",ESCAPE='"',/EXTRACT)
  N=Size(MACs)
  FOR i=1,N(1) DO BEGIN
     MACs[i-1]=STRTRIM(MACs[i-1])
  END

  IF Keyword_set(text) THEN BEGIN
     b=""
     FOR i=1,N(1) DO BEGIN
        b=b+" "
        j=0B
        l=strlen(MACs[i-1])
        WHILE ((j+2) LE l) DO BEGIN
           b=b+STRMID(MACs[i-1],j,2)
           ;; print, "j=", j, "b=",b
          IF ((j+2) LT l) then b=b+":"
           j=j+2
        END
     END
     b=STRTRIM(b)
     return, b
  END
  return, MACs
END

PRO DisplayLicenseMessage
  programname=GetApplicationName()
     s=[" ","          "+programname, " ",$
   "          You are running an unlicensed version of "+programname+".",$
   "          No functionality is provided. If you want to ",$
   "          activate all functions then send an E-mail to                  ",$
   "          lothar.houben@weizmann.ac.il with the subject '"+GetApplicationName(), $
   "          license request' and your MAC addresses in the body",$
   "          text.",$
   " ", $
   "          Your MAC addresses: "+GetMacAddresses(/TEXT),$
   " ", $
   "          If you have your license number and code you can ", $
   "          enter them by using the dialog that opens after clicking", $
   "          on the Preferences button. ", $
   "          Make sure to save the license data in the preferences", $
   "          file to make the settings permanent.", $
  ""]
     res=Dialog_Message(s, /INFO)
END

FUNCTION CheckLicense
  ;; application code for ImageConverter v1.00
  ;; bypass for now
  return, 100
  ;; 
  appcode=GetApplicationCode()
  Nappcode=0L
  ReadS, appcode, Nappcode, FORMAT='(Z)'
  ;;
  lic=3 ;; default is basic license
  ;; 16 code numbers
  LicenseData=GetLicenseData()

  NLicNr=0ULL
  ReadS, LicenseData(0), NLicNr, FORMAT='(Z)'

  NLicCode=0UL
  ReadS, LicenseData(1), NLicCode, FORMAT='(Z)'

  h=SWAP_ENDIAN(NLicCode)
  NLicCode=h XOR Nappcode

  seed=NLicCode

  IF (IDLVersionToDouble(!version.release) GT IDLVersionToDouble('8.2.1')) THEN MASK=RANDOMU(seed,200, /LONG, /RAN1) ELSE MASK=RANDOMU(seed,200, /LONG)


  X=NLicCode Mod 200

  Y=Mask(X)

  NMAC=0ULL

 ;; loop over Mac Adresses to find lic
  MAC=GetMACAddresses()
  MACstr=GetMacAddresses(/TEXT)
  i=1
  ;; print, "NLICCODE=", LicenseData(1), "=", NLicCode, " NLICNR=", NLicNr
  FOR i=1,N_Elements(MAC) DO BEGIN
     ReadS, MAC(i-1), NMAC, FORMAT='(Z)'
     ;; print, "NMAC=", NMAC
     ;; print, "Y=", Y
     code=(NMAC XOR Y) ;; your NLicNr
     ;; print, "code=", code
     if (code EQ NLicNr) THEN BEGIN
        lic=X
        IF (XREGISTERED('XConsole')) THEN Printtocon, "% CheckLicense: Valid license data for MAC "+String(MAC(i-1)) +"." ELSE Print, "% CheckLicense: Valid license data for MAC "+String(MAC(i-1)) +"."
     END ELSE BEGIN
        IF (XREGISTERED('XConsole')) THEN Printtocon, "% CheckLicense: No valid license data for MAC "+String(MAC(i-1)) +"." ELSE Print, "% CheckLicense: No valid license data for MAC "+String(MAC(i-1)) +"."
     END
  END

  IF (lic EQ 0) THEN BEGIN
     DisplayLicenseMessage
  END

  IF (lic GT 100) THEN BEGIN
     IF (XREGISTERED('XConsole')) THEN Printtocon, "% CheckLicense: Developer edition."
     
  END 

  return, lic
END




FUNCTION XSimpleDataField, X, TEXT=text, TITLE=t, ACCEPT=accept, CANCEL=cancel, HELP=hlp
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% XSimpleDataField:    Fatal error "
    Print, "%   Error status  - " + STRING(error_status)
    Print, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    ;;XConsole_PopState
    return, 0
END
Dim=SIZE(X, /DIMENSIONS)
IF (Dim(0) NE 2) THEN BEGIN
    Print, "% XSimpleDataField:    Fatal error "
    Print, "%    dimension of input array is wrong "
    return, 0
END
IF (SIZE(Dim, /N_ELEMENTS) EQ 1) THEN BEGIN 
   Dim=[Dim(0),1]
END ELSE BEGIN
IF (Dim(1) LT 1) THEN BEGIN
    ;; check whether it is one dimensional
        Print, "% XSimpleDataField:    Fatal error "
        Print, "%    dimension of input array is wrong "
        return, 0
END
END
;;XConsole_PushState
;;XConsole_WaitingForInput
result=1
if not(keyword_set(t)) then t="XSimpleDataField"
YOffs=5
base = Widget_Base(TITLE=t, /COLUMN, TAB_MODE = 1)
if (keyword_set(text)) THEN BEGIN
      ysz=N_ELEMENTS(text)
      YOffs=ysz*25
      label = Widget_TEXT(base, XSize=30, YSIZE=ysz, VALUE = text, SENSITIVE=1, TAB_MODE = 0)
   END
   input =  WIDGET_BASE(base, /COLUMN, XPAD=10)

   x_label=LONARR(Dim(1))
   FOR i=0,(Dim(1)-1) DO BEGIN
   x_label(i) = CW_Field(input, TITLE=X[0,i], XSize=20, Frame=0, Value=X[1,i], TAB_MODE = 1)
   END

  IF NOT(keyword_set(accept)) THEN accept="Apply"
  IF NOT(keyword_set(cancel)) THEN cancel="Cancel"

  IF NOT(keyword_set(hlp)) THEN BEGIN
      b = CW_BGroup(base, /ROW, [accept,cancel] , Button_UVALUE=['apply','cancel'], TAB_MODE = 1)
  END ELSE BEGIN
      b = CW_BGroup(base, /ROW, [accept,cancel,'  Help  '] , Button_UVALUE=['apply','cancel','help'], TAB_MODE = 1)
   END
  widpos=PlaceWidget(base, POSKEY="cc") 
  WIDGET_CONTROL, base,  XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
    do_it_again = 0
   REPEAT BEGIN
     ev = WIDGET_Event(base)
     IF (ev.id EQ b) THEN BEGIN                ;; Button-Menu
         CASE ev.value OF
             'cancel': BEGIN
                 do_it_again = NOT(do_it_again)
                 result=0
             END
             'help': BEGIN
                 msg = dialog_message(hlp, /Information)
             END
             'apply': BEGIN
                 For i=0,(Dim(1)-1) DO BEGIN 
                     TMP=''
                     WIDGET_Control, x_label(i), GET_VALUE=TMP
                     TMP=TMP(0)
                     X[1,i]=TMP
                 END   
                 do_it_again = NOT(do_it_again)      
                 result=1 
             END
         ENDCASE
     ENDIF 
 ENDREP UNTIL do_it_again
   Widget_Control, base, /DESTROY
   ;;XConsole_PopState
   return, result
END 

Function LicenseDialog
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% LicenseDialog:    Fatal error "
    Print, "%   Error status  - " + STRING(error_status)
    Print, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    ;;XConsole_PopState
    return, 0
 END
  a=STRARR(2,2)
  b=GetLicenseData()
  a[0,0]="License number:"
  a[1,0]=b(0)
  a[0,1]="License code  :"
  a[1,1]=b(1)
  helptxt=["License Dialog",$
           "",$
           "License number is a 10 digit hex code.",$
           "License code is a 8 digit hex code.",$
           " "]
  ;; now the call to he user dialog
  IF NOT(XSimpleDataField(a,Title="License dialog", HELP=helptxt)) THEN BEGIN
     ;; user decided to cancel
     print, "% GenerateLicName: user cancelled."
     return, 0
  END 
  tmp1=STRTRIM(a[1,0])
  tmp2=STRTRIM(a[1,1])    
  SetLicenseData, [tmp1,tmp2]
  return, CheckLicense()
END

Pro InitMousePrefs
COMMON MOUSEBINDINGS, mm_move, mm_resize, mm_break
  mm_move   = 1    ;; possible values: 1=left mouse button
                   ;;                  2=middle mouse button
                   ;;                  4=right mouse button
                   ;; anything else disables the function
  mm_resize = 2    ;; dto
  mm_break  = 4    ;; dto
END
