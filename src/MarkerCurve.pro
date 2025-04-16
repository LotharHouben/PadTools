PRO plot_curve, x, y, COLOR=color
plcolor=25600L
IF KEYWORD_SET(color) THEN plcolor=color
   plots, x, y, /device, COLOR=plcolor
   return
END


PRO MarkerSin, a, b, c, d, MaxX, MaxY
;;
;; plots a curve f(x)=a*sin(bx-c)+d
;; in the current window
;; a= amplitude
;; b= radians per array element (in the TV image!)
;; c= phase offset
;; d= offset
;;
;;CATCH, Error_status
;;IF (Error_status NE 0) THEN BEGIN
;;    PrintToCon, "% MarkerSin:    Fatal error "
;;    PrintToCon, "%   Error status  - " + STRING(error_status)
;;    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
;;    CATCH, /Cancel
;;    return
;;END
print, "% MarkerSin:    entering routine "
;;   STOP                       ; return false by default
oplotmode=9
plcolor=100L

   ;;
   ;; is there a window
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       print, "% MarkerBox: invalid window id"
       return
   END
   ;;
   ;; read the window dimensions
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   x=FINDGEN(wsizex)
   ;;
   xfact=FLOAT(MaxX/FLOAT(wsizex))
   bwin=b*xfact
   SinCurve=(a*Sin(x*bwin-c)+d)*FLOAT(wsizey)/FLOAT(MaxY)
   tmp=WHERE(SinCurve GE wsizeY)
   IF (((Size(tmp))[0]) NE 0) THEN SinCurve(tmp)=wsizey
   tmp=WHERE(SinCurve LT 0)
   IF (((Size(tmp))[0]) NE 0) THEN SinCurve(tmp)=0
   device, get_graphics_function=gmode0  ; store the current graphics mode.
   device, set_graphics_function=oplotmode       ; XOR mode.
;;   STOP
   plot_curve, x, SinCurve, COLOR=plcolor
   device, set_graphics_function=gmode0  
   ;;   
END


PRO MarkerSin2, angles, a, c, d, BINX=binX, BINY=binY
;;
;; plots a curve f(x)=a*sin(angles-c)+d
;; in the current window
;; a= amplitude
;; angles=vector with angles in rad
;; c= phase offset
;; d= offset
;;
;;CATCH, Error_status
;;IF (Error_status NE 0) THEN BEGIN
;;    PrintToCon, "% MarkerSin:    Fatal error "
;;    PrintToCon, "%   Error status  - " + STRING(error_status)
;;    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
;;    CATCH, /Cancel
;;    return
;;END
print, "% MarkerSin:    entering routine "
;;   STOP                       ; return false by default
oplotmode=9
plcolor=100L

   ;;
   ;; is there a window
   ;;
   IF (!D.WINDOW LT 0) THEN BEGIN
       print, "% MarkerBox: invalid window id"
       return
   END
   ;;
   ;; read the window dimensions
   ;;
   wsizex=!D.X_VSIZE & wsizey=!D.Y_VSIZE
   x=FINDGEN(N_Elements(angles))
   ;;
   IF NOT(keyword_set(binX)) THEN binX=1.
   IF NOT(keyword_set(binY)) THEN binY=1.
   SinCurve=(a*Sin(angles-c)+d)/binY
   tmp=WHERE(SinCurve GE wsizeY)
   IF (((Size(tmp))[0]) NE 0) THEN SinCurve(tmp)=wsizey
   tmp=WHERE(SinCurve LT 0)
   IF (((Size(tmp))[0]) NE 0) THEN SinCurve(tmp)=0
   device, get_graphics_function=gmode0  ; store the current graphics mode.
   device, set_graphics_function=oplotmode       ; XOR mode.
;;   STOP
   dimX=FIX(N_Elements(angles)/binX)
   plot_curve, FINDGEN(dimX), CONGRID(SinCurve,dimX), COLOR=plcolor
   device, set_graphics_function=gmode0  
   ;;   
END


;; MarkerSin, 100, 0.034906585, 0.1, 200, 61, 600
