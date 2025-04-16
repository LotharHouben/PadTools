PRO SetCommandLineMode
COMMON CLMode, clmodeon
clmodeon=1B
END

PRO UnsetCommandLineMode
COMMON CLMode, clmodeon
clmodeon=0B
END

FUNCTION GetCommandLineMode
COMMON CLMode, clmodeon
If (N_Elements(clmodeon) EQ 0) THEN clmodeon=0
;; defaults to interactive mode
return, clmodeon
END
