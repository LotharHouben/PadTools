;+
; NAME:
;
;
;
; PURPOSE:
;
;
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-

FUNCTION XGetLoopOptions, l, iter
   result=0 ; no loop by default
;; prepare XSimpleDataField dialog 
;;
hlp=[" ","Looping your animation: ", " ", $
        "You may add this NETSCAPE Application Extension to your animated gif.", $
        "The iteration count 0 means infinite looping. The maximum number", $
        "of iterations is 65535. The animated gif will only play once if you press", $
        "<No Loop>. "," "]                           
x=iter
IF (XDataField(x,"Number of iterations (0=infinite): ", TITLE="Optional Loop Parameters", ACCEPT="  Loop  ", CANCEL="  No Loop  ", HELP=hlp) GT 0) THEN BEGIN
    iter=UINT(x)
    l=1
    result=1
 END  
return, result
END 



PRO XAnimate
;;XConsole_PushState
;;XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% XAnimate:      Fatal error "
    Print, "%   Error status  - " + STRING(Error_status)
    Print, "%   Error message - " + !ERROR_STATE.MSG
;;    XConsole_PopState
    CATCH, /Cancel
    return
END
result=1
ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
   print, "% XAnimate: Root pointer is invalid"
   return
END
c=(*ptr).current
IF (NOT(PTR_VALID(c))) THEN BEGIN
   print, "% XAnimate: Root pointer is invalid"
   return
END
d=(*c).datap
maxslice=0L
CASE (*d).zcoord OF
   1: maxslice=(*d).SzX-1
   2: maxslice=(*d).SzY-1
   3: maxslice=(*d).SzZ-1
   ELSE: maxslice=0L
END
IF PTR_VALID(d) THEN BEGIN

        base = Widget_Base(TITLE="Animation Tool", /COLUMN)
        
                                ; row1
        row1 = WIDGET_BASE(base, /ROW) 
        no =  CW_Field(row1, TITLE="slice ", XSize=10, Frame=2, Value=STRING((*d).slice))
        
        delay=0.1
        fps =  CW_Field(row1, TITLE="delay in s ", XSize=10, Frame=2, Value=STRING(delay))
        
                                ; row 3
        b = CW_BGroup(base, /ROW, [' |<< ',' |< ', '   <   ','    o    ' ,'   >   ', ' >| ', '>>|'] , Button_UVALUE=['start','prev', 'bkw', 'stop', 'fwd', 'next', 'end'])
                                ; row 3.5
        ba = CW_BGroup(base, /ROW, ['Write Animated Gif ...'] , Button_UVALUE=['agif'])
                                ; row 4
        bb = CW_BGroup(base, /ROW, ['Help','Close'] , Button_UVALUE=['help','close'])
       WIDGET_CONTROL, base, /REALIZE

        do_it_again = 0
        REPEAT BEGIN
            ev = WIDGET_Event(base)
            IF (ev.id EQ ba) THEN BEGIN                ;; Button-Menu
                CASE ev.value OF
                    'agif':BEGIN
;;                        XConsole_Busy
                        Widget_Control, /HOURGLASS
;;                        printtocon, "writing animated gif"
	                gifdelay=100*delay
                        l=0 & iter=0
;;;; 	                AnimatedGif, g, DELAY=gifdelay
                        IF (XGetLoopOptions(l,iter) GT 0) THEN BEGIN
                            print, "% XAnimate; writing animated gif with loop option"
                            AnimatedGif_IDL, DELAY=gifdelay, LOOP=l, ITERATIONS=iter
                        END ELSE BEGIN
                            print, "% XAnimate; writing animated gif, no loop option"
                            AnimatedGif_IDL,  DELAY=gifdelay
                        END
;;                        XConsole_WaitingForInput
                    END 
                    ELSE:
                END 
            END 
            IF (ev.id EQ bb) THEN BEGIN                ;; Button-Menu
                CASE ev.value OF
                    'close': BEGIN
                        do_it_again = NOT(do_it_again)
                        result=0
                    END
                    'help': BEGIN
                        result=1 
                    END
                    ELSE:
                ENDCASE 
            ENDIF 
            IF (ev.id EQ b) THEN BEGIN                ;; Button-Menu
                CASE ev.value OF
                'start': BEGIN
                    (*d).slice=0L
                    TVDisplay
                    Update_XTabControl
                    ;; update entry in image number field
                    WIDGET_Control, no, SET_VALUE=STRING((*d).slice)
                END 
                'prev': BEGIN
                    if ((*d).slice GE 1) THEN BEGIN
                        (*d).slice=(*d).slice-1L
                        TVDisplay
                        Update_XTabControl
                        ;; update entry in image number field
                        WIDGET_Control, no, SET_VALUE=STRING((*d).slice)
                    END
                END 
                
                'bkw': BEGIN
                    print, 'animation: backward, stop by pressing on | o |'
                    ;; get delay settings
                    WIDGET_Control, fps, GET_VALUE=TMP
                    TMP=TMP(0)
                    IF (TMP NE '') THEN BEGIN
                        delay=FLOAT(TMP)
                    ENDIF
                    WIDGET_Control, fps, SET_VALUE=STRING(delay)
                    
                    finish = 0

                    REPEAT BEGIN
                        
                        IF ((*d).slice GT 0L) THEN (*d).slice=(*d).slice-1L ELSE (*d).slice=maxslice
                        
                        TVDisplay
                        WAIT, delay
                        
                        
                        ;; update entry in image number field
                        
                        WIDGET_Control, no, SET_VALUE=STRING((*d).slice)
                        
                        ev = WIDGET_Event(base, /NOWAIT)
                        IF ((ev.id EQ b)) THEN BEGIN 
                            IF (ev.value EQ 'stop') THEN finish = 1
                        END
                        ;;STOP
                    ENDREP UNTIL (finish EQ 1)
                    
                    ;;Update_XGroupListInfoField 
                    Update_XTabControl
                END 
                'stop': BEGIN
                    print, 'animation: stop/apply'
                END 
                'fwd': BEGIN
                    print, 'animation: forward, stop by pressing on | o |'
                    ;; get delay settings
                    WIDGET_Control, fps, GET_VALUE=TMP
                    TMP=TMP(0)
                    IF (TMP NE '') THEN BEGIN
                        delay=FLOAT(TMP)
                    ENDIF
                    WIDGET_Control, fps, SET_VALUE=STRING(delay)
                    
                    finish = 0
                    REPEAT BEGIN
                        (*d).slice=(*d).slice+1L
                        IF ((*d).slice GT maxslice) THEN (*d).slice=0L
                        TVDisplay
                        WAIT, delay
                        
                        
                        ;; update entry in image number field
                        
                        WIDGET_Control, no, SET_VALUE=STRING((*d).slice)
                        
                        ev = WIDGET_Event(base, /NOWAIT)
                        IF ((ev.id EQ b)) THEN BEGIN 
                            IF (ev.value EQ 'stop') THEN finish = 1
                        END
                        ;;STOP
                    ENDREP UNTIL (finish EQ 1)
                    
                    ;;Update_XGroupListInfoField 
                    Update_XTabControl
                END 
                'next': BEGIN
                        (*d).slice=(*d).slice+1L
                        IF ((*d).slice GT maxslice) THEN (*d).slice=0L
                        TVDisplay

                        Update_XTabControl
                        ;; update entry in image number field
                        WIDGET_Control, no, SET_VALUE=STRING((*d).slice)
                    
                END 
                
                'end': BEGIN
                   (*d).slice=maxslice
                    Update_XTabControl
                    ;; update entry in image number field
                    WIDGET_Control, no, SET_VALUE=STRING((*d).slice)
                END     
                ELSE:
            ENDCASE   
        ENDIF   
    ENDREP  UNTIL do_it_again
    Widget_Control, base, /DESTROY
END   
;;   XConsole_PopState
END 
