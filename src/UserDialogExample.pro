PRO UserDialogExample
;;
;; Example for a user dialog window
;;
;; create a new ListDialog object
mydialog=obj_new("ListDialog")
;; define variables that you want to use 
;; and 
;; add them to the dialog object: specify the identifier string, the preset value and
;; optionally the range of values
;; note that the preset value defines the data type!
;; the dialog will return exactly the same data type!
;; the dialog will do the range check for you if you define a range
floatpar=2.3432
intpar = 2
strpar = "test"
mydialog->AddOption, "Float Option", floatpar, RANGE=[0.,5.]
mydialog->AddOption, "Integer Option", intpar ;; no range restriction
mydialog->AddOption, "String Option", strpar
;; set the dialog window title
mydialog->SetTitle, "My Parameters"
help=["   User Parameter Dialog   ", " ", " You can add your help information here.", ""]
;; call the dialog function, this will display the input window
;; if the return value is greater than 0,then the user entered data
;; and clicked on accept
;; if the user pressed cancel the return value is 0
;; if RANGES is set the dialog will show the range limits
;; The dialog has a help button if a string array help is set 
IF (mydialog->UserDialog(/RANGES, HELP=help) GT 0) THEN BEGIN
   ;; user pressed accept, now get the user data
   ;; 
   IF (mydialog->Value("Float Option",x)) THEN floatpar=x ELSE print, "Float Option not found"
   IF (mydialog->Value("Integer Option",x)) THEN intpar=x ELSE print, "Integer Option not found"
   IF (mydialog->Value("String Option",x)) THEN strpar=x ELSE print, "String Option not found"
   ;; we don't need the ListDialog object anymore
   obj_destroy, mydialog
   ;; display parameter data in the console
   s=["% UserDialogExample ","Float Option: "+MyString(floatpar), "Integer Option: "+MyString(intpar),"String Option: "+strpar]
   printtocon, s
   ;; now you can do your program stuff
   ;; ...
   ;; ...
   ;; ...
END  ELSE BEGIN
   printtocon, "% UserDialogExamples: cancelled"
   obj_destroy, mydialog
END

END
