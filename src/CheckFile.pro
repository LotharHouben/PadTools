FUNCTION Checkfile, fn, errcode, errstr, WRITEABLE=writeable
;; checks, whether the file fn exists
;; and whether it is writeable
;; returns
;; 0: an error occured 1: no error occurred
;; errcode:
;; 0: o.k.
;; >0: file does not exist
;; 1: directory does not exist
;; 2: directory is not readable
;; 3: directory is not writeable
;; 4: file does not exist
;; 5: file is not readable
;; 6: file is not writeable

;; meaning that a successful return value would be 1, errcode 0,
;; writeable would be 1
;;
err=0 ;; default is error
SplitPath, fn, direct, filename
mode=0
;; Check for directory
dirtest=FILE_TEST(direct, /DIRECTORY)
IF (dirtest EQ 0) THEN BEGIN
  ;; directory does not exist
  errcode=1
  errstr=errstr+"Directory "+direct+" does not exist. "
  return, err
END
;; directory exists
IF keyword_set(writeable) THEN BEGIN
   ;; check whether it is writeable
   dirtest=FILE_TEST(direct, /DIRECTORY,/WRITE)
   IF (dirtest EQ 0) THEN BEGIN
      ;; directory is not writeable
      errcode=3
      errstr=errstr+"Directory "+direct+" is not writeable. "
      return, err
   END
END ELSE BEGIN
;; check whether it is readable
   dirtest=FILE_TEST(direct, /DIRECTORY,/READ)
   IF (dirtest EQ 0) THEN BEGIN
      ;; directory does not exist
      errcode=2
      errstr=errstr+"Directory "+direct+" is not readable. "
      return, err
   END
END
;; directory exists and is readable or writeable
dirtest=FILE_TEST(fn,/REGULAR)
IF (dirtest EQ 0) THEN BEGIN
   ;; directory does not exist
      errcode=4
      errstr=errstr+fn+" is not a regular file. "
      return, err
   END
;; directory and file exist
IF keyword_set(writeable) THEN BEGIN
   ;; check whether file it is writeable
   dirtest=FILE_TEST(fn,/WRITE)
   IF (dirtest EQ 0) THEN BEGIN
      ;;file is not writeable
      errcode=6
      errstr=errstr+"The file "+fn+" is not writeable. "
      return, err
   END
END ELSE BEGIN
;; check whether it is readable
   dirtest=FILE_TEST(fn, /READ)
   IF (dirtest EQ 0) THEN BEGIN
      ;; file is not readable
      errcode=5
      errstr=errstr+"The file "+fn+" is not readable. "
      return, err
   END
END
;; everything is o.k.
return, 1
END 
