Pro UpdateMonitorInfo
COMMON DISPLAYSCOPE, offsetx, offsety, sizex, sizey, MonNum, MonNames, MonRects, MonPrim
 ;; oInfo = OBJ_NEW('IDLsysMonitorInfo')
;;   MonNum = oinfo->GetNumberOfMonitors()
  MonNum=1
  ;; oinfo->GetPrimaryMonitorIndex()
;;  MonNames = oinfo->GetMonitorNames()
  MonNames=["Monitor 1"]
;;   MonRects = oInfo->GetRectangles()
;;  MonPrim = oInfo->GetPrimaryMonitorIndex()
;;  OBJ_DESTROY, oInfo
  size=GET_SCREEN_SIZE()
  offsetx=0 & offsety=0
  sizex=size[0] & sizey=size[1]
;;  offsety=MonRects[1,MonPrim]
END


FUNCTION PlaceWidget, id, POSKEY=poskey
COMMON DISPLAYSCOPE, offsetx, offsety, sizex, sizey, MonNum, MonNames, MonRects, MonPrim
;; places a widget on the screen
;; use the keyword MAP=0 when creating the widget
;; if x,y, or poskey are not specified, the widget is placed at the
;; centre
;; poskey may be "ul","ll","ur", or "lr"
;;
place=[0,0]
IF (NOT WIDGET_INFO (id, /VALID)) THEN BEGIN
      print,  'Invalid widget ID: ' + MYSTRING (base_id)
       RETURN, place 
   ENDIF
                                
;   IF (NOT WIDGET_INFO (id, /REALIZED)) THEN $
;     WIDGET_CONTROL, id, /REALIZE
   
   
    ; Get widget size
    ;
    geom=WIDGET_INFO(id, /Geometry)
    xhalf= geom.Scr_XSize/2
    yhalf= geom.Scr_YSize/2
    ;; Compute center placement point

    centerh = (sizex/ 2) - xhalf + offsetx
    centerv= (sizey/ 2) - yhalf + offsety 

    ;; Compute left/right placement
    left=offsetx & right=sizex - xhalf*2 +offsetx
    ;; Compute upper/lower placement
    upper=offsety & lower=sizey- yhalf*2 +offsety

    x=centerv
    y=centerh


    IF KEYWORD_SET(poskey) THEN BEGIN
        CASE poskey OF
            "cc":BEGIN
                x=centerh & y=centerv
            END
            "cl":BEGIN
                x=left & y=centerv
            END
            "cr": BEGIN
                x=right & y=centerv
            END
            "uc": BEGIN
                x=centerh & y=upper
            END
            "ul": BEGIN
                x=left & y=upper
            END
            "ur": BEGIN
                x=right & y=upper
            END
            "lc": BEGIN
                x=centerh & y=lower
            END
            "ll": BEGIN
                x=left & y=lower
            END
            "lr": BEGIN
                x=right & y=lower
            END
            ELSE: BEGIN
                x=centerh & y=centerv
            END
        END
    END
    ;
    place=[x,y]
    return, place
END

