
FUNCTION GetConfigFileName, basename, READ=read, WRITE=write, DIALOGTEXT=dialogtext
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
