FUNCTION XScalarOp, OP=op, SCALAR=scalar
XConsole_PushState
XConsole_WaitingForInput
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XScalarOp:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    XConsole_PopState
    return, 0
END
   result=0 
   ops=["+","-","*","/"] 
   opsel=0
   if not(keyword_set(op)) Then op="+"
   base = Widget_Base(TITLE="Scalar Operation", /COLUMN)
   row1= WIDGET_BASE(base, /ROW)
   x=1. & y=0.
   IF (keyword_set(scalar)) THEN BEGIN
       x=Real_Part(scalar) & y=Imaginary(scalar)
   END ELSE scalar=COMPLEX(1.,0)
   rlabel = CW_Field(row1, TITLE="", XSize=7, Value=MySTRING(x))
   ilabel = CW_Field(row1, TITLE="+ i*", XSize=7, Value=MySTRING(y))
   dlist=WIDGET_DROPLIST(row1, VALUE=ops, UVALUE = 'dlist')
   text=WIDGET_LABEL(row1, VALUE='Image Data')
 
   grid = WIDGET_BASE(base, COLUMN=2, /GRID_LAYOUT) 
   apply = WIDGET_BUTTON(grid, VALUE = ' Apply ', UVALUE = 'apply')
   quit = WIDGET_BUTTON(grid,   VALUE = ' Cancel ', UVALUE = 'cancel')

   widpos=PlaceWidget(base)
   Widget_Control, base, /REALIZE


   quit=0 & result = 0 
   REPEAT BEGIN
       
       
       uv=""
       ev = WIDGET_Event(base)
       WIDGET_CONTROL, ev.id, GET_UVALUE = uv
       CASE uv of
           'dlist': BEGIN
               opsel=ev.index 
           END
           'apply':  BEGIN
               TMP=''
               WIDGET_Control, rlabel, GET_VALUE=TMP
               TMP=TMP(0)
               IF (TMP NE '') THEN BEGIN
                   x=TMP
               ENDIF
               WIDGET_Control, ilabel, GET_VALUE=TMP
               TMP=TMP(0)
               IF (TMP NE '') THEN BEGIN
                   y=TMP
               ENDIF
               result=1
               quit=1
           END
           'cancel': BEGIN
               quit=1                 
               result=0
           END
           ELSE:
       ENDCASE
       
   ENDREP  until (quit EQ 1)
   IF (y EQ 0.) THEN scalar=FLOAT(x) ELSE scalar=COMPLEX(x,y)
   op=ops[opsel]
   Widget_Control, base, /DESTROY
   XConsole_PopState
   return, result
END


PRO ScalarTransform
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% ScalarTransform: Root pointer is invalid." 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ScalarTransform: Current stack pointer is invalid." 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ScalarTransform: Current data pointer is invalid." 
     return
  END
  x=1.0 & op="+"
  
  IF NOT(XScalarOp(OP=op, SCALAR=x) EQ 1) THEN BEGIN
          print, "% ScalarTransform: Cancelled." 
     return
  END
  IF (Imaginary(x) EQ 0.) THEN BEGIN
     type=4  ;; float
  END ELSE BEGIN
     type=6
  END 
  ;;Create a new stack
  name="ScalarTransform("+(*e).id+")"
  pp=DataList_CreateElement(ptr, name)
  (*pp).type=type
  ;;
  (*pp).SzX=(*e).SzX
  (*pp).SzY=(*e).SzY
  (*pp).SzZ=(*e).SzZ
  (*pp).xsamp=(*e).xsamp
  (*pp).ysamp=(*e).ysamp
  (*pp).zsamp=(*e).zsamp
  (*pp).zcoord=(*e).zcoord
  (*pp).slice=(*e).slice
  ;;  

  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% ScalarTransform: Failed to create data array for the new stack." 
     return
  END
  (*pp).data=ppd
  ;; process data
  Case op OF
     '+': (*ppd)=x+(*(*e).data)
     '-': (*ppd)=x-(*(*e).data)
     '*': (*ppd)=x*(*(*e).data)
     '/': (*ppd)=x/(*(*e).data)
     ELSE: PRINTtocon, "% ScalarTransform: undefined binary operation."
  ENDCASE
  ;; current stack is *pp, display the projections on the screen  
  (*pp).contrastmode="auto"
  IF NOT(GetCommandLineMode()) THEN BEGIN
     CreateWindow
     TVDisplay
     Update_XTabControl
  END
END 
