PRO GetMarkerBoxCoord, x1, x2, y1, y2, binx, biny
;; returns display coordinates
IF (binx NE 1) THEN BEGIN
   IF (binx GT 0) THEN BEGIN
     x1=FIX(FLOAT(x1)/binx)
     x2=FIX(FLOAT(x2)/binx)
   END ELSE BEGIN
     PrintToCon, "% GetMarkerBoxCoord: fatal error, x-binning <=0. " 
   END
END 
IF (biny NE 1) THEN BEGIN
   IF (biny GT 0) THEN BEGIN
     y1=FIX(FLOAT(y1)/biny)
     y2=FIX(FLOAT(y2)/biny)
   END ELSE BEGIN
     PrintToCon, "% GetMarkerBoxCoord: fatal error, y-binning <=0. " 
   END
END 
END

PRO MarkerBox, xi1, xi2, yi1, yi2, MaxX, MaxY, ABSOLUTE=absolute, BINX=binx, BINY=biny
;;
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% MarkerBox:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    return
END
                        ; return false by default
oplotmode=9
plcolor=100L

   ;;
   ;; is there a window
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       print, "% MarkerBox: invalid window id"
       return
    END
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   IF NOT(keyword_set(absolute)) THEN BEGIN
      ;;
      ;; read the window dimensions
      ;;
      
      ;;
      
      xfact=FLOAT(wsizex-1)/(MaxX-1)
      x1 = FLOOR(xi1*xfact)
      x2 = FLOOR(xi2*xfact)
      yfact=FLOAT(wsizey-1)/(MaxY-1)
      y1 = FLOOR(yi1*yfact)
      y2 = FLOOR(yi2*yfact)
      

   END 
   IF keyword_set(absolute) then begin
      x1=xi1 & x2=xi2 & y1=yi1 & y2=yi2
      IF NOT(keyword_set(binx)) THEN binx=1
      IF NOT(keyword_set(biny)) THEN biny=1
      GetMarkerBoxCoord, x1, x2, y1, y2, binx, biny
   END
   ;; check coordinates
   IF (x2 LT x1) THEN BEGIN
      tmp=x1 & x1=x2 & x2=tmp
   END
   IF (x1 LT 0) THEN x1=0
   IF (x2 GE wsizex) THEN x2=wsizex
   ;;
   IF (y2 LT y1) THEN BEGIN
      tmp=y1 & y1=y2 & y2=tmp
   END
   IF (y1 LT 0) THEN y1=0
   IF (y2 GE wsizey) THEN y2=wsizex
   device, get_graphics_function=gmode0  ; store the current graphics mode.
   device, set_graphics_function=oplotmode       ; XOR mode.
   plot_Box, x1, x2, y1, y2, COLOR=plcolor
   device, set_graphics_function=gmode0  
   ;;   
END
