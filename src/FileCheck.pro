FUNCTION FileIsWriteable, fname, OVERWRITE=overwrite, NONINTERACTIVE=noninteractive, DETAILS=details
;;
;; returns 1 if the user is allowed and willing to write to the file fname
;; returns 0 otherwise
;;
filecheck=FILE_TEST(fname)

;; file exists 

IF (filecheck EQ 1) THEN BEGIN 
   IF Not(keyword_set(overwrite)) THEN BEGIN
      IF Not(keyword_set(noninteractive)) THEN BEGIN
         IF (Dialog_Message([" ","File already exists: "+fname, " ","Do you want to replace it with the newer version?",""], /QUESTION, TITLE="Warning", /DEFAULT_NO) EQ " No ") THEN BEGIN
            return, 0
         END          
      END ELSE BEGIN
         printtocon, "% FileIsWriteable: Warning - the file "+fname+" already exists! The content will be replaced!"
      END
   END 
   ;; is the file writeable?
   filewrite=FILE_TEST(fname, /WRITE)
   IF (filewrite EQ 0) THEN BEGIN
      ;; file exists but is not writeable
      IF Not(keyword_set(noninteractive)) THEN BEGIN
         dummy=Dialog_Message([" ", "Error: " + f + "is not writeable.",""], /INFORMATION, TITLE="Error")
      END ELSE BEGIN
         printtocon, "% FileIsWriteable: Error - the file "+fname+" is not writeable."
      END
      return, 0
   END
END ELSE BEGIN
    ;; file does not exist, but is the directory witeable?
    ;; split dir and filename first
    SplitPath, fname, direct, filename
    dirwrite=FILE_TEST(direct, /DIRECTORY, /WRITE)
    IF (dirwrite EQ 0) THEN BEGIN
       IF Not(keyword_set(noninteractive)) THEN BEGIN
          dummy=Dialog_Message([" ",  direct, " ","does not exist or is not writeable",""], /INFORMATION, TITLE="Error") 
       END ELSE BEGIN
          printtocon, "% FileIsWriteable: Error - the directory "+direct+" does not exist or is not writeable."
       END
        return, 0
    END
END
return, 1
END   

FUNCTION FileIsWriteable2, fname, DETAILS=details, WARNEXISTS=warnexists
;;
;; returns 1 if the user is allowed and willing to write to the file fname
;; returns 0 otherwise
;;

filecheck=FILE_TEST(fname)
;; file exists 

IF (filecheck EQ 1) THEN BEGIN 
    IF NOT(keyword_set(warnexists)) THEN BEGIN
       s=[fname +" already exists.","Do you want to overwrite it?"]
    END ELSE BEGIN
       s=[fname +" already exists.",warnexists]
    END
    IF (Dialog_Message(s, /QUESTION) EQ " No ") THEN BEGIN
        IF keyword_set(details) then details="cancelled"
        return, 0
    END ELSE BEGIN
        ;; is the file writeable?
        filewrite=FILE_TEST(fname, /WRITE)
        IF (filewrite EQ 0) THEN BEGIN
            ;; file exists but is not writeable
            dummy= Dialog_Message("Error: file "+fname+" is not writeable!", /INFO)
            IF keyword_set(details) then details="exists-isnotwriteable"
            return, 0
        END
    END
END ELSE BEGIN
    ;; file does not exist, but is the directory witeable?
    ;; split dir and filename first
    SplitPath, fname, direct, filename
    dirwrite=FILE_TEST(direct, /DIRECTORY, /WRITE)
    IF keyword_set(details) then details="existsnot-iswriteable"
    IF (dirwrite EQ 0) THEN BEGIN
       IF keyword_set(details) then details="existsnot-isnotwriteable"
        dummy= Dialog_Message("Error: directory "+direct+" does not exist or is not writeable!", /INFO)
        
        return, 0
    END
END
return, 1
END  
