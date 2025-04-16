



PRO ProjectVolume, dim
  compile_opt idl2
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% ProjectVolume:    Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END 
  ptr=GetRootP()
  If (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% ProjectVolume: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ProjectVolume: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     print, "% ProjectVolume: current data pointer is invalid" 
     return
  END
  d=(*e).data
  IF (NOT(PTR_VALID(d))) THEN BEGIN
     print, "% ProjectVolume: current data pointer is invalid" 
     return
  END
  N=Size((*d))
  IF (N[0] EQ 3) THEN BEGIN
     Case dim OF
        3: BEGIN
           DimX=N[1] & DimY=N[2] & DimZ=N[3]
           norm=1./DimZ
           avg=DBLARR(DimX,DimY)
           For i=0L,DimZ-1 Do BEGIN
              avg+=norm*(*d)[*,*,i]
           END
           name="Average("+(*e).id+")"
           ThrowImage, avg, BIN=(*e).binning, TITLE=name, SAMP=[(*e).xsamp,(*e).ysamp,1.], UNIT=[(*e).xunit,(*e).yunit,(*e).zunit] ;; can only be 1/nm ... EMPADObject
        END
        ELSE: print, "% ProjectVolume: Averaging so far only along z-dimension." 
     END
  END
  TVDisplay
  Update_XTabControl
END  


PRO Formula, formulastr, p
;; to be continued
IF PTR_VALID(p) THEN BEGIN
END
END

PRO FormulaOperation, fx
;;
;; calculates OP (*(*p)).im
;;
  compile_opt strictarr
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% FormulaOperation:    Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
END
p=GetCurrentP()
IF Not(Ptr_Valid(p)) THEN BEGIN
   printtocon, "% FormulaOperation: invalid data set  pointer"
   return
END
d=(*p).datap
IF Not(Ptr_Valid(d)) THEN BEGIN
   printtocon, "% FormulaOperation: invalid data pointer"
   return
END
data=(*d).data
IF Not(Ptr_Valid(data)) THEN BEGIN
   printtocon, "% FormulaOperation: invalid data array pointer"
   return
END
N=SIZE(*data)
lam1 = Lambda('x: '+fx)
minv=[0]
maxv=[100]
pbarperc=0
pbarstep=5
mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
For loop=0,N[3]-1 DO Begin
    perc=LONG(loop*100. / M)
     if (FIX(perc) GT pbarperc) THEN BEGIN
        pbarperc += pbarstep
        mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
     END   
   frame=(*data)[*,*,loop]
   (*data)[*,*,loop]=frame.Map(lam1)
END
END   




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
   ;; base = Widget_Base(TITLE="Scalar Operation", /COLUMN, DISPLAY_NAME=GetDisplayName())
   base = Widget_Base(TITLE="Scalar Operation", /COLUMN)
   row1= WIDGET_BASE(base, /ROW)
   x=1. & y=0.
   IF (keyword_set(scalar)) THEN BEGIN
       x=Real_Part(scalar) & y=Imaginary(scalar)
   END ELSE scalar=COMPLEX(1.,0)
   rlabel = CW_Field(row1, TITLE="", XSize=7, Value=MySTRING(x))
   ilabel = CW_Field(row1, TITLE="+ i*", XSize=7, Value=MySTRING(y))
   dlist=WIDGET_DROPLIST(row1, VALUE=ops, UVALUE = 'dlist')
   text=WIDGET_LABEL(row1, VALUE='Frame Data')
 
   grid = WIDGET_BASE(base, COLUMN=2, /GRID_LAYOUT) 
   apply = WIDGET_BUTTON(grid, VALUE = ' Apply ', UVALUE = 'apply')
   quit = WIDGET_BUTTON(grid,   VALUE = ' Cancel ', UVALUE = 'cancel')

   widpos=PlaceWidget(base,  POSKEY=WidgetPos("DialogWin"))
   Widget_Control, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE


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

PRO ScalarFrameTransformation
;;
;; calculates OP (*(*p)).im
;;
  compile_opt strictarr
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% FormulaOperation:    Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
END
p=GetCurrentP()
IF Not(Ptr_Valid(p)) THEN BEGIN
   printtocon, "% FormulaOperation: invalid data set  pointer"
   return
END
d=(*p).datap
IF Not(Ptr_Valid(d)) THEN BEGIN
   printtocon, "% FormulaOperation: invalid data pointer"
   return
END
data=(*d).data
IF Not(Ptr_Valid(data)) THEN BEGIN
   printtocon, "% FormulaOperation: invalid data array pointer"
   return
END
N=SIZE(*data)
;; get operation
op='+'
scalar=32.
IF (XScalarOp(OP=op, SCALAR=scalar)) THEN BEGIN 
   minv=[0]
   maxv=[100]
   pbarperc=0
   pbarstep=5
   mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Processing frames ...", FRAMESTYLE=2)
   For loop=0,N[3]-1 DO Begin
      perc=LONG(loop*100. / N[3])
      if (FIX(perc) GT pbarperc) THEN BEGIN
         pbarperc += pbarstep
         mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
      END   
      frame=(*data)[*,*,loop]
      Case op OF
         '+': (*data)[*,*,loop]+=scalar
         '-': (*data)[*,*,loop]=scalar-(*data)[*,*,loop]
         '*': (*data)[*,*,loop]*=scalar
         '/': (*data)[*,*,loop]=scalar/(*data)[*,*,loop]
         ELSE: 
      END
   END
END 
END 



