PRO Start_Profiler
COMMON PROFSTAT, t
Profiler, /SYSTEM & Profiler
t = SYSTIME(1)
END

PRO Stop_Profiler, DATA=data
COMMON PROFSTAT, t
dt=SYSTIME(1)-t
IF keyword_set(data) THEN Profiler, DATA=data ELSE Profiler, /REPORT
Profiler, /RESET
print, "Elapsed time (s): ", dt
END
