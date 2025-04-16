Pro SetBinFmtPrefs2D
COMMON BINFMT2D, BinType, BinSzX, BinSzY, BinSzZ, BinOffs, ImCounter
 BinType=2 & BinSzX=256 & BinSzY=256 & BinOffs=1024L & ImCounter=0
END


FUNCTION XRawDialog2D, x, y, type, offset, MRC=mrc
COMMON BINFMT2D, BinType, BinSzX, BinSzY, BinOffs, ImCounter
result=0
if not(keyword_set(mrc)) then mrc=0
if not(mrc EQ 1) THEN BEGIN
listitems = ['byte                  (  8bit)', $
             'integer               ( 16bit)', $
             'unsigned integer      ( 16bit)', $
             'long integer          ( 32bit)', $
             'unsigned long integer ( 32bit)', $
             'long integer          ( 64bit)', $
             'unsigned long integer ( 64bit)', $
             'float                 ( 32bit)', $ 
             'double                ( 64bit)', $
             'complex float         ( 64bit)', $
             'complex double        (128bit)']
END
if not(mrc EQ 1) THEN BEGIN
base = Widget_Base(TITLE='Image Format', /COLUMN);; ,DISPLAY_NAME=GetDisplayName()) 
END ELSE BEGIN
base = Widget_Base(TITLE='MRC image coordinates', /COLUMN);; DISPLAY_NAME=GetDisplayName()) 
END
   ;Create a row base to hold the input fields
if not(mrc EQ 1) THEN BEGIN
   input =  WIDGET_BASE(base, /ROW)
   x_label = CW_Field(input, TITLE='x-Dim', XSize=10, Frame=2, Value=MySTRING(x))
   y_label = CW_Field(input, TITLE='y-Dim', XSize=10, Frame=2, Value=MySTRING(y))
   offset_label = CW_Field(input, TITLE='offset', XSize=10, Frame=2, Value=MySTRING(offset))
   type_list=WIDGET_DROPLIST(base, VALUE = listitems, UVALUE = 'LIST')
END

   b = CW_BGroup(base, /ROW, ['   Apply   ','   Cancel   '] , Button_UVALUE=['accept','cancel'])
if not(mrc EQ 1) THEN BEGIN
   WIDGET_Control, type_list, SET_DROPLIST_SELECT=type
END
   widpos=PlaceWidget(base)
   WIDGET_CONTROL, base, /REALIZE
   do_it_again = 0
   REPEAT BEGIN
      ev = WIDGET_Event(base, /NoWait)
      if not(mrc EQ 1) THEN BEGIN
         IF (ev.id EQ type_list) THEN BEGIN
            type=ev.index
            print, type
         END
      END
      IF (ev.id EQ b) THEN BEGIN
        CASE ev.value OF
            'cancel': BEGIN
               ;; print, ev.id
               do_it_again = NOT(do_it_again)
            END
            'accept': BEGIN
               if not(mrc EQ 1) THEN BEGIN
                    TMP=''
                    WIDGET_Control, x_label, GET_VALUE=TMP
                    TMP=TMP(0)
                    IF (TMP NE '') THEN BEGIN
                   x=FIX(FLOAT(TMP))
                       IF (x LT 0) THEN x=0
                    ENDIF
                    TMP=''
                    WIDGET_Control, y_label, GET_VALUE=TMP
                    TMP=TMP(0)
                    IF (TMP NE '') THEN BEGIN
                   y=FIX(FLOAT(TMP))
                       IF (y LT 0) THEN y=0
                    ENDIF
                    TMP=''
                    WIDGET_Control, offset_label, GET_VALUE=TMP
                    TMP=TMP(0)
                    IF (TMP NE '') THEN BEGIN
                   offset=LONG(FLOAT(TMP))
                       IF (offset LT 0) THEN offset=0
                    ENDIF
                 END
                    do_it_again = NOT(do_it_again)
                    result=1
                  END
         endcase
        END
   ENDREP UNTIL do_it_again
   Widget_Control, base, /DESTROY
   ;; convert data type dtype
              ;; from 
              ;; 0 - Byte, 1 - integer, 3 - long, 4 - float, 5 - double, 
              ;; 6 - complex single precision, 7 - complex double precision
              ;; to
              ;; 1=byte, 2=int, 3=long, 4=float, 5=double,  6=compl, 9=double, 12=unsigned int,
              ;; 13=unsigned long
             mtype=1
              CASE type of
                 0: mtype=1
                 1: mtype=2
                 2: mtype=12
                 3: mtype=3
                 4: mtype=13
                 5: mtype=14
                 6: mtype=15
                 7: mtype=4
                 8: mtype=5
                 9: mtype=6
                 10: mtype=9
                 ELSE: BEGIN
                 END 
              ENDCASE
   BinSzX=x & BinSzY=y &  BinOffs=offset & type=mtype & BinType=mtype
   ;;print, "imccord=", imcoord
   ;; XConsole_PopState
   type=mtype
   return, result ;; 1 if accepted, 0 if cancelled
END









