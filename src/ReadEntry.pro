Function ShowEntries, elist, lun
;;
 success=0
 list=STRARR(100)
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% ShowEntries: Error while reading file"
     Print, "%            " + !ERR_STRING
     CATCH, /Cancel
     return, 0
 END
 POINT_LUN, lun, 0
 line=""
 N=0
 WHILE (NOT(EOF(lun))) DO BEGIN
     READF, lun, line
     ;; STOP

     IF (STREGEX(line,'^\[') EQ 0) THEN BEGIN
         list[N]=line
         N=N+1
         Print, "% ShowEntries: found entry "+ line
     END
 END
 maxN=N-1
 IF (N GT 0) THEN BEGIN
     elist=list[0:maxN]
     success=1
 END ELSE elist=""
 return, success
END



Function ReadEntry, entry, lun
;;
 s=''
;; debug=0
;; IF debug THEN BEGIN
CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% ReadEntry: Error while reading file"
     Print, "%            " + !ERR_STRING
     CATCH, /Cancel
     return, s
  END
;;END
 POINT_LUN, lun, 0
 found=0
 line=""
 WHILE (NOT(EOF(lun)) AND (found EQ 0)) DO BEGIN
     READF, lun, line
     IF (STRCMP(line,entry, /FOLD_CASE)) THEN BEGIN
         ;; Print, "% ReadEntry: found entry "+entry
         found=1
     END
 END
 IF found THEN BEGIN
     data=STRARR(10000)
     i=-1
     READF, lun, line
     ;; print, "read: ", line
     WHILE NOT(EOF(lun) OR (STREGEX(line,'^\[') EQ 0)) DO BEGIN
         if (STRCOMPRESS(line) NE "") THEN  BEGIN
             i=i+1
             ;; print, "index: ", i
             data[i]=line
         END
         READF, lun, line
         ;; print, "read: ", line
     END
     ;; print, "returning ", data
     return, data[0:i]
 END
 return, s
END
