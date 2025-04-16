Pro ErrMsg, s, ERROR=error, FATAL=fatal, WARNING=warning
s0=""
IF keyword_set(error) THEN BEGIN
   dummy= Dialog_Message([' ','An error occured: ',s,' '], /ERROR)
END ELSE BEGIN
   IF keyword_set(fatal) THEN BEGIN 
      dummy= Dialog_Message([' ', "A fatal error occured: ",s,' '], /ERROR)
   END ELSE BEGIN
      IF keyword_set(warning) THEN BEGIN 
         dummy= Dialog_Message(['              ',s,'                '])
      END ELSE BEGIN
         dummy= Dialog_Message(['              ',s,'                '], /ERROR)
      END
   END 
END 
END 
