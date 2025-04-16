PRO TestWait, time, number
print, "starting test now"
print, "should take ", number*time, " seconds..."
For i=0,(number-1) do begin
WAIT, time
END
print, "finished test"
END


FUNCTION GetWaitTime
COMMON WAITIME, wt
IF (N_Elements(wt) EQ 0) THEN BEGIN
   wt=0.
END
return, wt
END

PRO SetWaitTime, time 
COMMON WAITIME, wt
print, "% SetWaitTime: Setting idle time to"+STRING(time)+" s"
wt=time
END

PRO MyWait, time
IF (time GT 0) THEN WAIT, time
END


PRO TestWait2, time, number
SetWaitTime, time
print, "starting test now"
print, "should take ", number*time, " seconds..."
i=1
REPEAT BEGIN
   WAIT, GetWaitTime()
   i=i+1
END UNTIL (i EQ Number)
print, "finished test"
END
