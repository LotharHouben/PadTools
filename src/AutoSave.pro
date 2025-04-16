
PRO AutoSaveStack, c, SAVENAME=savename
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% AutoSaveStack: Stack pointer is invalid." 
     return
  END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% AutoSaveStack: Root pointer is invalid." 
     return
  END
  (*ptr).current=c ;; set current stack to stack c
  Update_XTabControl  ;; highlight the stack in the stack list
  
     path = GetWorkingDir()
     IF NOT(keyword_set(savename)) THEN fname = path+Path_Sep()+"autosave.mrc" ELSE fname = path+Path_Sep()+savename
     printtocon, "% AutoSave: Saving data stack to "+fname+"."
     IF (LMGR(/DEMO) EQ 1) THEN BEGIN
        printtocon, "% AutoSaveStack: No file IO in demo mode."
        return
     END
     SaveBinData, fname, FORMAT="mrc", /AUTOSAVE, /OVERWRITE, /NOPROGRESSBAR
 
END 


PRO AutoSaveLog
IF GetAutosave() THEN BEGIN
   path = GetWorkingDir()
   fname = path+Path_Sep()+"autosave.log"
   printtocon, "% AutoSave: Saving log data to "+fname+"."
   IF (LMGR(/DEMO) EQ 1) THEN BEGIN
      printtocon, "% AutoSaveStack: No file IO in demo mode."
      return
   END
   XConsole_Save, fname, /AUTOSAVE, /OVERWRITE
END
END 
