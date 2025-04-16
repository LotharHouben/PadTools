PRO ErrHandlerDemo
;;
;; Demo procedure showing the use of error handlers
;;
IF NOT(GetDebugFlag()) THEN BEGIN
;; you can set the debug flag in interactive mode 
;;    idl> SetDebugFlag, 1
;; (or from the "Developer" Menu)
;; then the error handler will be ignored and the program stops at the
;; error, here in the line x=(*p)
;;
;; install the error handler if the debug flag is not set
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      ;; on error print the error in the console ...
      Printtocon, "% ErrHandlerDemo:    Fatal error "
      Printtocon, "%   Error status  - " + STRING(error_status)
      Printtocon, "%   Error message - " + !ERROR_STATE.MSG
      ;; and display a dialog that the user has to acknowledge 
      ErrMsg, !ERROR_STATE.MSG
      ;; deinstall the error handler
      CATCH, /Cancel
      ;; and return immediately to the calling level
      return
   END
END
;; do your program stuff
;; create a nil pointer
p=PTR_NEW() 
;; dereference nil pointer, leads to an error
x=(*p) 
END 
