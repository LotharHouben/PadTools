FUNCTION XListDialog, l, TITLE=title, ACCEPT=accept, CANCEL=cancel, HELP=hlp, XCONSOLE=xconsole, ROW=row
;; l is a list of options
;; the elements of l are anonymous structures of the type 
;; Create_Struct('tag', name, 'value', value, 'rangecheck', rangecheck, 'range', range)
;;
  IF NOT(GetDebugFlag()) THEN BEGIN
  CATCH, Error_status
  ;; Error_status=0
  IF (Error_status NE 0) THEN BEGIN
     Print, "% XListDialog:    Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     IF keyword_set(xconsole) THEN XConsole_PopState
     return, 0
  END
END
  IF keyword_set(xconsole) THEN BEGIN 
     XConsole_PushState
     XConsole_WaitingForInput
  END
  ;; 
  result=1
  if not(keyword_set(title)) then title="Input Dialog"
  base =  WIDGET_BASE(/Column, TITLE=title)
;;  IF keyword_set(row) THEN input =  WIDGET_BASE(base, /ROW, XPAD=10) ELSE input =  WIDGET_BASE(base, /COLUMN, XPAD=10)
  Nk=N_Elements(l)
;;  field_id=LONARR(Nk)
;;  YOffs=-10 & YOffs_incr=10
;;  FOR i=0,(Nk-1) Do BEGIN
;;     field_base=WIDGET_BASE(input, /ROW, YPAD=0)
;;     field_label=Widget_label(field_base, Value=STRTRIM(STRING(l[i].tag),2), UVALUE=STRTRIM(STRING(l[i].tag),2))
;;     field_id[i]=Widget_Text(field_base, XSize=10, Frame=0, Value=STRTRIM(STRING(*(l[i].pvalue)),2), TAB_MODE = 1, UVALUE=STRTRIM(STRING(l[i].tag),2))
;;  END 
  ;;
  ;; table widget
  data=STRARR(2,Nk)
  columnlabels = ['Value','Range Limits']
  columnwidths= [150,150]
  rowlabels=STRARR(Nk)
  For i=0,(Nk-1) Do BEGIN
     rowlabels[i]=STRTRIM(STRING(l[i].tag),2)
  END
  ;; preset values, first column
  FOR i=0,(Nk-1) Do BEGIN
     data(0,i)=STRTRIM(STRING(*(l[i].pvalue)),2)
  END
  FOR i=0,(Nk-1) Do BEGIN
     IF (l[i].rangecheck EQ 1) THEN BEGIN
        data(1,i)="["+STRTRIM(STRING(l[i].range[0]),2) +","+STRTRIM(STRING(l[i].range[1]),2)+"]"
     END ELSE data(1,i)=''
  END
;;  table = WIDGET_TABLE(base, VALUE=data, ROW_LABELS=rowlabels,
;;  COLUMN_LABELS=columnlabels, COLUMN_WIDTHS=columnwidths, / RESIZEABLE_COLUMNS,/ALL_EVENTS, /EDITABLE, /SENSITIVE, TAB_MODE=1) 
 table = WIDGET_TABLE(base, VALUE=data, ROW_LABELS=rowlabels,  COLUMN_LABELS=columnlabels, COLUMN_WIDTHS=columnwidths, /RESIZEABLE_COLUMNS,/ALL_EVENTS, /EDITABLE, /SENSITIVE, TAB_MODE=1, FRAME=0) 
  ;;
  IF NOT(keyword_set(accept)) THEN accept="Accept"
  IF NOT(keyword_set(cancel)) THEN cancel="Cancel"
  ;;
  YOffs=0
  IF NOT(keyword_set(hlp)) THEN BEGIN
      b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,cancel] , Button_UVALUE=['apply','cancel'], TAB_MODE = 1)
   END ELSE BEGIN
      b = CW_BGroup(base, /ROW, YOffset=YOffs+45, [accept,cancel,'Help'] , Button_UVALUE=['apply','cancel','help'], TAB_MODE = 1)
   END
;;  widpos=PlaceWidget(base, POSKEY="cc") 
;;  WIDGET_CONTROL, base,  XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
   WIDGET_CONTROL, base, /REALIZE
   do_it_again = 0
   REPEAT BEGIN
      ev = WIDGET_Event(base)

      IF (ev.id EQ b) THEN BEGIN                ;; Button-Menu
         print, ev.value
         CASE ev.value OF
            'cancel': BEGIN
               do_it_again = NOT(do_it_again)
               result=0
            END
            'help': BEGIN
               msg = dialog_message(hlp, /Information)
            END
            'apply': BEGIN
               WIDGET_CONTROL, table, SENSITIVE=0 ;; how do I update the content in the table from the GUI?
               WIDGET_CONTROL, table, GET_VALUE=table_value 
               rangeerr_bool=0
               rangeerr=""
                  FOR i=0,(Nk-1) DO BEGIN
                     tmp=table_value[0,i]
                     ;; determine data type 
                     type=SIZE(*(l[i].pvalue), /TYPE)
                     CASE type OF
                        1: tmp=Byte(tmp) ;; 'Byte'
                        2: tmp=FIX(tmp)  ;;'Integer'
                        3: tmp=LONG(tmp) ;;'Longword Integer'
                        4: tmp=FLOAT(tmp)     ;;'Floating Point'	
                        5: tmp=DOUBLE(tmp)     ;;s = 'Double-precision Floating Point'	
                        6: tmp=COMPLEX(tmp)     ;;s = 'Complex Floating Point'	
                        7:     ;;s = 'String'	
;;                        8: tmp=(tmp)     ;;s = 'Structure'	
                        9: tmp=DCOMPLEX(tmp)     ;;s = 'Double-precision Complex Floating Point'	
;;                        10: tmp=(tmp)     ;;s = 'Pointer'	
;;                        11: tmp=(tmp)     ;;s = 'Object Reference'	
                        12: tmp=UINT(tmp)     ;;s = 'Unsigned Integer'
                        13: tmp=ULONG(tmp)     ;;s = 'Unsigned Longword Integer'	
                        ELSE: BEGIN
                           print, "% XListDialog: conversion error, type not defined"
                        END
                     END 
                     ;; rangecheck
                     print, "tmp=", tmp
                     *(l[i].pvalue)=tmp
                     IF (l[i].rangecheck EQ 1) THEN BEGIN
                        IF ((tmp LT l[i].range[0]) OR (tmp GT l[i].range[1])) THEN BEGIN
                           rangeerr=[rangeerr,l[i].tag+": allowed range=["+STRTRIM(STRING(l[i].range[0]),2) +","+STRTRIM(STRING(l[i].range[1]),2)+ "], value="+STRTRIM(STRING(tmp),2)]
                           rangeerr_bool=1
                        END  
                     END
                  END  
                  IF (rangeerr_bool EQ 0) THEN BEGIN
                     do_it_again = NOT(do_it_again)
                     result=1
                  END ELSE BEGIN
                     rangeerr=["Please check your input:",rangeerr]
                     void=dialog_message(rangeerr, /ERROR)
                  END
               END
         ENDCASE 
      ENDIF  
   ENDREP UNTIL do_it_again
   Widget_Control, base, /DESTROY
   IF keyword_set(xconsole) THEN XConsole_PopState
   return, result
END    

;; ABSTRACT FUNCTION CONTAINER OBJECT 

function ListDialog::Init
  p=list()
  self.l=PTR_new(p)
  self.title='Parameter Dialog'
  self.xconsole=0
 return,1
end

pro ListDialog__define
 void={ListDialog,l:ptr_new(), title:'Parameter Dialog', xconsole:0}
 return 
end

Pro ListDialog::SetTitle, title
  self.title=title
END

Pro ListDialog::SetXConsole
  self.title=title
END

PRO ListDialog::AddOption, name, value, RANGE=range
  rangecheck=0
  IF keyword_set(range) then BEGIN
     
     IF (range(1) LT range(0)) THEN BEGIN
        tmp=range[1]
        range[1]=range[0]
        range[0]=range[1]
     END
     IF (range[1] GT range[0])  THEN BEGIN
        rangecheck=1
     END
     IF (rangecheck EQ 0) THEN print, "% ListDialog::AddOption - error when parsing range field for option"+STRTRIM(name,2)
  END
  IF (rangecheck EQ 0) then range=[0,0]
  s=Create_Struct('tag', name, 'pvalue', PTR_NEW(value), 'rangecheck', rangecheck, 'range', range)
  (*(self.l))->Add, s
END

FUNCTION ListDialog::UserDialog
  return, XListDialog((*(self.l)), TITLE=self.title, XConsole=self.xconsole)
END

FUNCTION ListDialog::Value, name, value
  ;; returns 0 upon error
  N=n_elements((*(self.l)))-1
  For i=0,N Do Begin
     s=(*(self.l))[i]
     if (s.tag EQ name) THEN BEGIN
        value=*(s.pvalue)
        return, 1
     END
  END
  return, 0
END


function ListDialog::cleanup
;-- free memory allocated to pointer when destroying object
  N=n_elements((*(self.l)))-1
  For i=0,N Do Begin
     ptr_free, s.pvalue
  END
 ptr_free, self.l
 return, 1
end 

PRO XListDialog_Test
;; create a new Dialog object
mydialog=obj_new("ListDialog")
;; add parameters and their initial values
;; the general syntax is  mydialog->AddOption, <Parametername>,
;; <Parametervalue>
;; notes:
;; - the return value will be of the same type as <Parametervalue> 
;; - for numerical values a valid paramter range can be defined by the
;;   optional keyword RANGE=[<min>,<max>]  
mydialog->AddOption, "Option 1", 2.3432, RANGE=[0.,5.]
mydialog->AddOption, "Option 2", 2
;; set the window title for the dialog window
mydialog->SetTitle, "My Parameters"
;; launch the dialog window and get the return value
;; if the return value is 0 then the user pressed the cancel button
IF (mydialog->UserDialog() GT 0) THEN BEGIN
   ;; read parameter values from the Dialog object
   ;; notes:
   ;; - make sure that the parameter name matches exactly the
   ;;   definition in the mydialog->AddOption call 
   IF (mydialog->Value("Option 1",x)) THEN print, "Option 1 = ", x ELSE print, "Option 1 not found"
   IF (mydialog->Value("Option 2",x)) THEN print, "Option 2 = ", x ELSE print, "Option 2 not found"
   ;; do whatever stuff you want to do
   ;; here comes your program code 
   ;; ...
   ;; ...
   ;; ...
END  ELSE print, "cancelled"
;; destroy the Dialog object to free memory
obj_destroy, mydialog
END
