PRO XNoteEditor_event, ev
  WIDGET_Control, ev.top, GET_UVALUE=pstate
  par=*pstate
  If (ev.id EQ par.bid) THEN BEGIN
    CASE ev.value OF
        'close': BEGIN
           ;; check whether there was a change
            PTR_FREE, par.help_text
            PTR_FREE, pstate
            Widget_Control, ev.top, /DESTROY 
        END
        'save': BEGIN
              IF OBJ_VALID(par.obj) THEN BEGIN
                 WIDGET_Control,  par.textid, GET_VALUE=s
                 ;; empty buffer
                 
                 par.obj->flush
                
                    i=0
                    For i=0, (N_Elements(s)-1) DO BEGIN
                      par.obj->Add, s[i], i+1
                    END
                 END ELSE BEGIN
                    PrintToCon, "% XNoteEditor: Failed to access textbuffer object. Nothing saved."
                 END
         
              END 
        'help': BEGIN
               msg = dialog_info(*(par.help_text), /HELP)
            END
        'timestamp': BEGIN
              IF OBJ_VALID(par.obj) THEN BEGIN
                 ;; add timestamp text
                 WIDGET_Control, par.textid, SET_VALUE=Timestamp(), /APPEND
              END
            END
        ELSE:
     ENDCASE 
 ENDIF 
END 


Pro XNoteEditor, textbufobj, TITLE=title,  help=hlp
  IF NOT(OBJ_VALID(textbufobj)) THEN BEGIN
     PrintToCon, "% XNoteEditor: Textbuffer Object is invalid."
     return
  END
pobj=GetDLGParObject()
winpar=pobj->get_field('WINDOW')

IF NOT(PTR_VALID(winpar)) THEN BEGIN
 ErrMsg, "% WINDOW parameters unavailable. Aborting."
 return
END
width=(*winpar).EDITORWINWIDTH
height=(*winpar).EDITORWINHEIGHT
   XW=FIX(width)
   NLines=20
   IF keyword_set(hlp) THEN BEGIN
      help_text=hlp
   END ELSE BEGIN
      help_text = ["", $
                   "Annotation Editor", $
                   "", $
                   "Here you can add group, image and groupimage annotations. These", $
                   "annotations are saved and can be retrieved as long as you are", $
                   "using the empadTools HDF5 format for storage.", $
                   "There is no limit for the number of annotation lines or the length",$
                   "of a single annotation line. Empty lines have to contain at least", $
                   "one space character, otherwise they will be deleted.", $
                   "The Time Stamp button will append a date string at the end of the ", $
                   "text input. ", $
                   "" $
                  ]   

   END
   IF NOT(keyword_set(title)) THEN title="Note Editor"
   mybase = Widget_BASE(TITLE=title, /COLUMN) ;;, DISPLAY_NAME=GetDisplayName(),
   row1 =  WIDGET_BASE(mybase, /ROW)
   x=textbufobj->ToStringArray()
   IF (x EQ !NULL) THEN x=""
   notetext = WIDGET_TEXT(row1,/EDITABLE,  /SCROLL, SCR_XSIZE=XW, Units=0, YSIZE=NLines, VALUE=x)
   row2 = WIDGET_BASE(mybase, /ROW)
   mkd_b = CW_BGroup(row2, /ROW, [' Update ',' Close ', ' Help ', 'Time Stamp'] , Button_UVALUE=['save','close','help','timestamp'])
   ;;
   ;; widpos=PlaceWidget(mybase, POSKEY=WidgetPos("NoteEditor"))
   ;; Widget_Control, mybase, XOFFSET=widpos[0], YOFFSET=widpos[1],
   ;; /REALIZE
   Widget_Control, mybase, /REALIZE
   par=CREATE_STRUCT('result',0B,'textid',notetext,'obj',textbufobj,'bid',mkd_b,'help_text',PTR_NEW(help_text))
   pstate=PTR_NEW(par)
   WIDGET_CONTROL, mybase, SET_UVALUE=pstate
   Xmanager, 'XNoteEditor', mybase, /NO_BLOCK
END


PRO PrintToNote, p, s, TIMESTAMP=timestamp
  
  IF OBJ_VALID((*p).note) THEN BEGIN
     i0=(*p).note->GetNumberOfLines()            
     IF (keyword_set(timestamp)) THEN (*p).note->Add, Timestamp(), i0+1          
     i0=(*p).note->GetNumberOfLines()
     For i=0, (N_Elements(s)-1) DO BEGIN
        (*p).note->Add, s[i], i0+i+1
     END
  END ELSE BEGIN
     PrintToCon, "% PrintToNote: Failed to access textbuffer object. Nothing saved."
  END
  
END  



