
FUNCTION Timestamp
time = Systime(UTC=Keyword_Set(utc))
day = Strmid(time, 0, 3)
date = String(StrMid(time, 8, 2), Format='(I2.2)') ; Required because UNIX and Windows differ in time format.
month = Strmid(time, 4, 3)
year = Strmid(time, 20, 4)
stamp = Strmid(time, 11, 8)
return, date+". " + month + " "+year + ' ' + stamp
END
