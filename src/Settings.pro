PRO SelectDesktopDir
  cancelled=1
  files = RMD_PICKFILE(/DIREC, TITLE="Choose directory", CANCELLED = cancelled)
IF NOT(cancelled)  THEN SetDesktopDir, files
END
