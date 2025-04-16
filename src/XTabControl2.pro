
FUNCTION XTabControl_Help
s= ["Use this window to manage an image stack",$
    "and its associated window.",$
    " ", $
    "General remarks: ", $
    "- The highlighted image stack in the selection box",$
    "  has the focus, all operations from the Command Menu are ", $
    "  performed on the focussed image in the <Image tab> list.",$
    "- Information about the image, stack is displayed in",$
    "  the text box in the lower part of each tab window.",$
    "- The selection lists are associated with context menues.",$
    "  Click the right mouse button over a list to display the menue.",$
    " ", $
    "Button functions:", $
    "[ Reread ] - updates the information in case it is not updated",$
    "            automatically",$
    "[  Help  ] - display this help page", $
    "[ Close  ] - close the control window", $
    " " $
]
return, s
END

PRO SHOWWIN, wdow_nr
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% ShowWin: Error while displaying image with index " + MyString(wdow_nr)
    Print, "%            " + !ERR_STRING
    Print, "%            Need to reopen window?"
;;     (*winp).wdow = -1
    ErrMsg, !ERR_STRING
    CATCH, /Cancel
    return
END
WSHOW, wdow_nr, 1
END


PRO Close_XTabControl
 COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
 WIDGET_CONTROL, base, /DESTROY
END





PRO Update_XTabControlInfoField
COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
   WIDGET_CONTROL, itext, SET_VALUE=ImageInfo()
END

PRO Update_XTabControlGraphInfoField
COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
   WIDGET_CONTROL, dtext, SET_VALUE=GraphInfo()
END

PRO Update_XTabControlTableInfoField
COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
   WIDGET_CONTROL, ttext, SET_VALUE=TableInfo()
END


PRO Zoom, fact
IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% Zoom:  Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg,  !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
 ptr=GetRootP()
 IF (NOT(PTR_VALID(ptr))) THEN BEGIN
    print, "% IListCtxtMenu: Root pointer invalid"
    return
 END
 p=(*ptr).current
 IF PTR_VALID(p) THEN BEGIN
       ;; change TV zoom
        ;; close window a recreate it
        d=(*p).datap
        IF (PTR_VALID(d)) THEN BEGIN
           CreateWindow, BIN=fact
           TVDisplay
           Update_XTabControlInfoField
        END ELSE BEGIN
           print, "% Zoom: Invalid data stack pointer"
        END
     END 
END 

PRO IListCtxtMenu, event
 COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id 
IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% IListCtxtMenu:  Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg,  !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END

 WIDGET_CONTROL, event.id, GET_UVALUE = uv
 ptr=GetRootP()
 IF (NOT(PTR_VALID(ptr))) THEN BEGIN
    print, "% IListCtxtMenu: Root pointer invalid"
    return
 END
 p=(*ptr).current
 IF PTR_VALID(p) THEN BEGIN
 CASE uv OF
    'save': BEGIN
       IF LMGR(/Demo) THEN BEGIN
          Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          FileExport
          Update_XTabControl
       END
    END
    'exportdisp': BEGIN
       IF LMGR(/Demo) THEN BEGIN
          Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          ExportBitmap, /DISPLAY
          Update_XTabControl
       END
    END
    'saveexportdisp': BEGIN
       IF LMGR(/Demo) THEN BEGIN
          Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          file="none"
          FileExport, GetFileName=file
          IF (file NE "none") THEN BEGIN
             SplitPath, file, path, filename
             filename=path+Path_Sep()+File_basename(filename,"h5")+"png"
             ExportDisplay, filename
          END
          Update_XTabControl
       END
    END 
    'save_nonint': BEGIN
       IF LMGR(/Demo) THEN BEGIN
          Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          FileExport_NonInteractive
          Update_XTabControl
       END
    END
     'redisplay':  BEGIN
         d=(*p).datap
         IF NOT(PTR_VALID(d)) THEN return
         WSET, (*d).window
        TVDisplay
     END 
     'link': BEGIN
        pold=p
        pparent=XListSelector(Title="Select a stack to link: ") ; 
        IF (NOT(PTR_VALID(pparent))) THEN BEGIN
           printtocon, "% IListCtxtMenu: No stack linked." 
           (*(*pold).datap).plink=PTR_NEW()
        END ELSE BEGIN
           printtocon, "% IListCtxtMenu: Linked stack = "+ MyString((*pparent).name)+" ("+MyString(pparent)+")"
           ;; set link pointer
           (*(*pold).datap).plink=pparent
        END
        p=pold
        Update_XTabControl
     END
     'delete':  BEGIN
        res=DataList_DeleteCurrentElement(ptr)
        Update_XTabControl
     END 
    'deleteother': BEGIN
         IF (Dialog_Info("Are you sure to delete all other images?", /QUESTION) EQ " Yes ") THEN BEGIN
             XConsole_PushState
             XConsole_Busy

             ;; WIDGET_CONTROL, /HOURGLASS
             keepp=(*ptr).current
             ;; 
             ;; print, "% Delete Other: Keeping pointer ", keepp

             f=(*ptr).first
             ;; print, "% Delete Other: first pointer ", keepp
             WHILE PTR_VALID(f) DO BEGIN
                (*ptr).current=f
                ;; print, "% Delete Other: current pointer ", keepp
                ;; print, "% Delete Other: next pointer ", (*f).next
                 If (f EQ keepp) THEN BEGIN
                    ;; print, "% Delete Other: jumping to next pointer ", (*f).next
                    f=(*f).next
                 END ELSE BEGIN
                    ;; print, "% Delete Other: deleting data under pointer ", (*ptr).current
                    res=DataList_DeleteCurrentElement(ptr)
                    f=(*ptr).current
                    ;; print, "% Delete Other: current pointer after deletion ", f
                 END
             END
             Update_XTabControl
             XConsole_PopState
          END 
      END
    'deleteall': BEGIN
         IF (Dialog_Info("Are you sure to delete all images?", /QUESTION) EQ " Yes ") THEN BEGIN
             XConsole_PushState
             XConsole_Busy

             ;; WIDGET_CONTROL, /HOURGLASS
             ;; 
             ;; print, "% Delete Other: Keeping pointer ", keepp

             f=(*ptr).first
             ;; print, "% Delete Other: first pointer ", keepp
             WHILE PTR_VALID(f) DO BEGIN
                (*ptr).current=f
                ;; print, "% Delete Other: current pointer ", keepp
                ;; print, "% Delete Other: next pointer ", (*f).next
                    ;; print, "% Delete Other: deleting data under pointer ", (*ptr).current
                    res=DataList_DeleteCurrentElement(ptr)
                    f=(*ptr).current
                    ;; print, "% Delete Other: current pointer after deletion ", f
             END
             Update_XTabControl
             XConsole_PopState
          END 
      END
     'duplicate':  BEGIN
        CopyStack ;; duplicates the current stack
        Update_XTabControl
     END
     'mergeframes':  BEGIN
        MergeFrames ;; duplicates the current stack
        TVDisplay
        Update_XTabControl
     END 
     'animate':  BEGIN
        XAnimate
         Update_XTabControl
     END 
     'extract':  BEGIN
        ExtractSlice
        Update_XTabControl
     END
     'topad': BEGIN
        ToPADObject
     END
     'flip':  BEGIN
        FlipAxis
        Update_XTabControl
     END 
     'X': BEGIN
        ;; change projection
        ;; close window a recreate it
        d=(*p).datap
        IF (PTR_VALID(d)) THEN BEGIN
           (*d).zcoord=1
           (*d).slice=LONG((*d).SzX/2)
           CreateWindow
           TVDisplay
           Update_XTabControlInfoField
           WIDGET_CONTROL, slider_id, GET_UVALUE=psliderstate
           Widget_CONTROL, avgslider_id, GET_VALUE=tmp
           (*psliderstate).width=tmp
           Slider_Control, psliderstate, /REFRESH
         END ELSE BEGIN
           print, "% IListCtxtMenu: Invalid data stack pointer"
        END
        ;; 
     END
     'Y': BEGIN
        ;; change projection
        ;; close window a recreate it
        d=(*p).datap
        IF (PTR_VALID(d)) THEN BEGIN
           (*d).zcoord=2
           (*d).slice=LONG((*d).SzY/2)
           CreateWindow
           TVDisplay
           Update_XTabControlInfoField
           WIDGET_CONTROL, slider_id, GET_UVALUE=psliderstate
            Widget_CONTROL, avgslider_id, GET_VALUE=tmp
           (*psliderstate).width=tmp
           Slider_Control, psliderstate, /REFRESH
         END ELSE BEGIN
           print, "% IListCtxtMenu: Invalid data stack pointer"
        END
        ;; 

     END
     'Z': BEGIN
        ;; change projection
        ;; close window a recreate it
        d=(*p).datap
        IF (PTR_VALID(d)) THEN BEGIN
           (*d).zcoord=3
           (*d).slice=LONG((*d).SzZ/2)
           CreateWindow
           TVDisplay
           Update_XTabControlInfoField
           WIDGET_CONTROL, slider_id, GET_UVALUE=psliderstate
            Widget_CONTROL, avgslider_id, GET_VALUE=tmp
           (*psliderstate).width=tmp
           Slider_Control, psliderstate, /REFRESH
        END ELSE BEGIN
           print, "% IListCtxtMenu: Invalid data stack pointer"
        END
        ;; 

     END
     'zoom2:1': Zoom,0.5
     'zoom4:1': Zoom,0.25
     'zoom8:1': Zoom,0.125
     'zoom3:1': Zoom,0.333333333333
     'zoom1:1': Zoom,1
     'zoom1:2': Zoom,2
     'zoom1:3': Zoom,3
     'zoom1:4': Zoom,4
     'zoom1:8': Zoom,8
     'zoom:other':    BEGIN
                            fact=1
                            hlp=[" ",$
                                 " Set the magnification for TV Display here.",$
                                 " Choose a value smaller than 1 to magnify ",$
                                 " and larger than 1 to demagnify. ",$
                                 " "]
 
                            IF (XDataField(fact, '1 : ', Title="Zoom factor", HELP=hlp) EQ 1) THEN Zoom, fact  
                         END
     'ScaleBar:None': BEGIN
        d=(*p).datap
        IF NOT(PTR_VALID(d)) THEN return
        IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
           (*d).DisplayScaleBar=0
           ;; get rid of object
           PTR_FREE, (*d).ScaleBar
           TVDisplay
        END 
     END 
     'ScaleBar:Max': BEGIN
        d=(*p).datap
        IF NOT(PTR_VALID(d)) THEN return
        IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
           (*(*d).ScaleBar).color = !D.N_COLORS-1
        END ELSE BEGIN
           (*d).DisplayScaleBar=1
           IF PTR_VALID((*d).ScaleBar) THEN PTR_FREE, (*d).ScaleBar 
           (*d).ScaleBar=PTR_NEW(GetScaleBar(((*d).SzX),((*d).xsamp), UNIT=((*d).xunit)))
        END
        TVDisplay
     END 
     'ScaleBar:Min': BEGIN
        d=(*p).datap
        IF NOT(PTR_VALID(d)) THEN return
        IF ((*d).DisplayScaleBar EQ 1) THEN BEGIN
           (*(*d).ScaleBar).color = 0
        END ELSE BEGIN
           (*d).DisplayScaleBar=1
           IF PTR_VALID((*d).ScaleBar) THEN PTR_FREE, (*d).ScaleBar 
           (*d).ScaleBar=PTR_NEW(GetScaleBar(((*d).SzX),((*d).xsamp), UNIT=((*d).xunit), /INVERT))
        END
        TVDisplay
     END
     'loadpadprop':BEGIN
        PropFileIO ;; will fail if not PAD object
        d=(*p).datap
        IF NOT(PTR_VALID(d)) THEN return
        IF (OBJ_VALID((*d).extra)) THEN BEGIN
           o=(*d).extra       
           (*d).xsamp=o->GetDetectorSampling(/X)
           (*d).ysamp=o->GetDetectorSampling(/Y)
           (*d).SzX=o->GetFrameSize(/X)
           (*d).SzY=o->GetFrameSize(/Y)
           (*d).SzZ=o->GetSetLength()
           Update_XTabControl
        END ELSE BEGIN
  
        END
     END 
     'savepadprop': BEGIN
        PropFileIO, /EXPORT
     END 
     'imeditprops': BEGIN
        d=(*p).datap
        IF NOT(PTR_VALID(d)) THEN return
        IF (OBJ_VALID((*d).extra)) THEN BEGIN
           padobj=(*d).extra
           EditPADObjectMetaData, padobj
           (*d).xsamp=padobj->GetDetectorSampling(/X)
           (*d).ysamp=padobj->GetDetectorSampling(/Y) 
        END ELSE BEGIN
           s=list()
           s.Add, {value:(*d).voltage,label:"Voltage ",newrow:0B}
           s.Add, {value:(*d).xsamp,label:"Sampling rate x, y, z",newrow:1B}
           s.Add, {value:(*d).ysamp,label:"",newrow:0B}
           s.Add, {value:(*d).zsamp,label:"",newrow:0B}
           s.Add, {value:(*d).xunit,label:"Sampling unit x, y, z",newrow:1B}
           s.Add, {value:(*d).yunit,label:"",newrow:0B}
           s.Add, {value:(*d).zunit,label:"",newrow:0B}
           c=list()
           if (XMDataChoiceField(s, c, TITLE="Edit Properties") EQ 1) THEN BEGIN
               (*d).voltage=s[0].value
               (*d).xsamp=s[1].value
               (*d).ysamp=s[2].value
               (*d).zsamp=s[3].value
               (*d).xunit=s[4].value
               (*d).yunit=s[5].value
               (*d).zunit=s[6].value
               ;; STOP
            END
        END
        Update_XTabControl
     END 
     'imeditnotes': BEGIN
        d=(*p).datap
        IF NOT(PTR_VALID(d)) THEN return
        IF OBJ_VALID((*d).note) THEN XNoteEditor, (*d).note, TITLE='Image Notes- '+(*d).id
     END
     ELSE:
END 
END  
END   

FUNCTION TableInfo
  ptr=GetRootPTable()
  s=['no description available']
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% TableInfo: Table root pointer is invalid"
     return, s
  END
  p=(*ptr).current
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print, "% TableInfo: current table pointer is invalid"
     return, s
  END
 IF (PTR_VALID(p)) THEN BEGIN
    datap=(*p).datap
     IF (PTR_VALID(datap)) THEN BEGIN
           s=["table name     : " + (*p).name]  
        mytable=(*datap).pobj
        IF OBJ_VALID(mytable) THEN BEGIN
           s=[s,$
              "Rows x Columns: "+STRTRIM(STRING(mytable->NRows())) + "x" +STRTRIM(STRING(mytable->NColumns()))]
           s=[s,*(mytable->GetPDescriptor())]
        END
     END
 END
 return, s
END


FUNCTION DataInfo, d
s=['no description available']
IF (PTR_VALID(d)) THEN BEGIN
END 
return, s
END

PRO DListCtxtMenu, event
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% DListCtxtMenu:        Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
  END
  WIDGET_CONTROL, event.id, GET_UVALUE = uv
  ptr=GetRootPXYGraph()
  d=(*ptr).current
  IF PTR_VALID(d) THEN BEGIN
     list_struct=(*d).datap ;; poiter to the structure element in the list
     IF NOT(PTR_VALID(list_struct)) THEN return
     mygraph=(*list_struct).pobj
     IF NOT(OBJ_VALID(mygraph)) THEN return
     CASE uv OF
        'deleteallgraphs': BEGIN
           IF (Dialog_Info("Are you sure to delete all graphs?", /QUESTION) EQ " Yes ") THEN BEGIN
              void=XYGraphList_DeleteOther(ptr)                  ;; purge: delete also the object
              void=XYGraphList_DeleteCurrentElement(ptr, /PURGE) ;; purge: delete also the object 
              Update_XTabControl
           END
        END
        'deleteothergraphs': BEGIN
        IF (Dialog_Info("Are you sure to delete all other graphs?", /QUESTION) EQ " Yes ") THEN BEGIN
           void=XYGraphList_DeleteOther(ptr) ;; purge: delete also the object 
           Update_XTabControl
        END
     END
     'plot': BEGIN
        mygraph->UpdateDisplay
     END
     'delete': BEGIN
;;        mygraph->TidyUp
;;        obj_destroy, mygraph
;;        (*list_struct).pobj=obj_new()
        void=XYGraphList_DeleteCurrentElement(ptr, /PURGE) ;; purge: delete also the object 
        Update_XTabControl
     END
     'export':  BEGIN 
        mygraph->ExportData
     END
     'exportgraph':  BEGIN 
        mygraph->ExportDisplay
     END
     'closewindow':  BEGIN 
            mygraph->CloseWindow
         END
     ELSE:
  END
END 
END 

PRO TListCtxtMenu, event
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% TListCtxtMenu:        Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
  END
  WIDGET_CONTROL, event.id, GET_UVALUE = uv
  ptr=GetRootPTable()
  
  IF (uv EQ 'load') THEN BEGIN
     header=[""]
     name="File data"
     ;; mytable=ReadTableFromBlockData(Header=header)
     mytable=ReadTableFromBlockData(Header=header, /NOARRAY)
     IF NOT(OBJ_VALID(mytable)) THEN return
     ;; link object
     newtable=GetNewTableContainer(mytable->GetTableName())
     IF NOT(PTR_VALID(newtable)) THEN BEGIN
        ;; an error occured, delete the object
        ErrMsg, "NewTable: Could not create table list item."
        obj_destroy, mytable
     END ELSE BEGIN
            (*newtable).pobj=mytable
         END
     mytable->WidgetEditor
     return
  END
  d=(*ptr).current
  IF PTR_VALID(d) THEN BEGIN
     list_struct=(*d).datap ;; poiter to the structure element in the list
     IF NOT(PTR_VALID(list_struct)) THEN return
     mytable=(*list_struct).pobj
     IF NOT(OBJ_VALID(mytable)) THEN return
     CASE uv OF
        'deletealltables': BEGIN
        IF (Dialog_Info("Are you sure to delete all tables?", /QUESTION) EQ " Yes ") THEN BEGIN
           void=TableList_DeleteOther(ptr) ;; purge: delete also the object
           void=TableList_DeleteCurrentElement(ptr, /PURGE) ;; purge: delete also the object 
           Update_XTabControl
        END
     END
        'deleteothertables': BEGIN
        IF (Dialog_Info("Are you sure to delete all other tables?", /QUESTION) EQ " Yes ") THEN BEGIN
           void=TableList_DeleteOther(ptr) ;; purge: delete also the object 
           Update_XTabControl
        END
     END
     'delete': BEGIN
        void=TableList_DeleteCurrentElement(ptr, /PURGE) ;; purge: delete also the object 
        Update_XTabControl
     END
     'edit':  BEGIN 
        mytable->WidgetEditor
     END
      'export':  BEGIN 
        mytable->Export, /ALL, /FILE
     End
     
     Else:
  END
END 
END 


PRO XTabControl_event, ev
COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
;; firstim is the pointer to the first image
ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
   print, "% XTabControl_event: Root pointer invalid"
   return
END
;;print, "event=", ev

IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_SLIDER') THEN BEGIN
   ;; print, "Slider event"
   WIDGET_CONTROL, slider_id, GET_UVALUE=psliderstate, GET_VALUE=sliderval
   sliderwidth=1L
   IF ((*psliderstate).avgid GT 1L)  THEN WIDGET_CONTROL, avgslider_id, GET_VALUE=sliderwidth
   ;; get new slider value
   Slider_Control, psliderstate, SET=sliderval, AVGSET=sliderwidth
   Update_XTabControl
   return
END

IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_TAB') THEN BEGIN
   ;; print, "Tab event"
   Update_XTabControl
END ELSE BEGIN 

                                ; First check for a right mouse button click!
   IF (TAG_NAMES(ev, /STRUCTURE_NAME) EQ 'WIDGET_CONTEXT') THEN BEGIN
       ;; process context menu event here
       ;; display context menu  
       IF (WIDGET_INFO(ev.id, FIND_BY_UNAME='ilistctxtmenu') GT 0) THEN BEGIN
           WIDGET_DISPLAYCONTEXTMENU, ev.id, ev.x,ev.y, WIDGET_INFO(ev.id, FIND_BY_UNAME='ilistctxtmenu')
       END 
       IF (WIDGET_INFO(ev.id, FIND_BY_UNAME='dlistctxtmenu') GT 0) THEN BEGIN
           WIDGET_DISPLAYCONTEXTMENU, ev.id, ev.x,ev.y, WIDGET_INFO(ev.id, FIND_BY_UNAME='dlistctxtmenu') 
        END
       IF (WIDGET_INFO(ev.id, FIND_BY_UNAME='tlistctxtmenu') GT 0) THEN BEGIN
          WIDGET_DISPLAYCONTEXTMENU, ev.id, ev.x,ev.y, WIDGET_INFO(ev.id, FIND_BY_UNAME='tlistctxtmenu') 
       END 
   END ELSE BEGIN
       
       WIDGET_CONTROL, ev.id, GET_UVALUE = uv
       
       ;; print, uv
       
         CASE uv OF
           'LIST': BEGIN
		  ; The list has been touched.
		  ; The index of the list item touched is held in
		  ; 'event.index'... how convenient!
               ptr=GetRootP()
               x=(*ptr).first
               i=0
               WHILE (i LT ev.index) DO BEGIN
                  x=(*x).next
                  i=i+1
               END
               IF PTR_VALID(x) THEN BEGIN
                   (*ptr).current=x
                   Widget_Control, /HOURGLASS
                                ; print, 'updating info field'
                   Update_XTabControlInfoField
                   WIDGET_CONTROL, slider_id, GET_UVALUE=psliderstate
                   Slider_Control, psliderstate, /REFRESH
               END ELSE printToCon, "% XTabControl_event: index not found in stack list, current stack pointer not changed"
           END
;;              'ICONTRAST': BEGIN
;;                  current=GetCurrentGroup()
;;                  IF PTR_VALID(current) THEN (*current).ic=1
;;              END
;;              'GCONTRAST': BEGIN
;;                  current=GetCurrentGroup()
;;                  IF PTR_VALID(current) THEN (*current).ic=0
;;              END
           'DLIST': BEGIN
		  ; The data list has been touched.
		  ; The index of the list item touched is held in
		  ; 'event.index'... how convenient!
              ptr=GetRootPXYGraph()
              x=(*ptr).first
              i=0
              WHILE (PTR_VALID(x) AND (i LT ev.index)) DO BEGIN
                 x=(*x).next
                 i=i+1
              END
              IF PTR_VALID(x) THEN BEGIN
                   (*ptr).current=x
                   Widget_Control, /HOURGLASS
                                ; print, 'updating info field'
                   Update_XTabControlGraphInfoField
                END ELSE printToCon, "% XTabControl_event: index not found in graph list, current stack pointer not changed"
              
           END
           'TLIST': BEGIN
		  ; The table list has been touched.
		  ; The index of the list item touched is held in
		  ; 'event.index'... how convenient!
              ptr=GetRootPTable()
              x=(*ptr).first
              i=0
              WHILE (PTR_VALID(x) AND (i LT ev.index)) DO BEGIN
                 x=(*x).next
                 i=i+1
              END
              IF PTR_VALID(x) THEN BEGIN
                   (*ptr).current=x
                   Widget_Control, /HOURGLASS
                                ; print, 'updating info field'
                   Update_XTabControlTableInfoField
                END ELSE printToCon, "% XTabControl_event: index not found in table list, current stack pointer not changed"
              
           END 
            'help': BEGIN
               msg = dialog_info(XTabControl_Help(), /Info)
            END
            'quit': BEGIN
                     WIDGET_CONTROL, ev.top, /DESTROY
                    END
            'reread': BEGIN
                       Widget_Control, /HOURGLASS
                       Update_XTabControl
                      END
            ELSE: printToCon, '% XTabControl_event: not yet functional'
        ENDCASE
    END
END 
END


PRO XTabControl
COMMON XTABCONTROLID, ilist, itext, glist, gilist, gtext, dlist, tlist, dtext, ttext, conbutton, coffbutton, slider_id, base, avgslider_id
; Create the top-level base for the widget:
XW=284
ListWidth=38
base = WIDGET_BASE(TITLE='Data Inspector', /COLUMN, XSIZE=XW, GROUP_LEADER=WidgetID(0,/GETGROUPLEADERTOP), TAB_MODE = 1)
;
wTab = WIDGET_TAB(base, LOCATION=0)
;
; row3
Tab1 = WIDGET_BASE(wTab, Title='  3D Data  ', /COLUMN)
row3 = WIDGET_BASE(Tab1, /ROW)
listitems=['empty'] ;; empty list
ilist = WIDGET_LIST(row3, XSIZE=ListWidth, VALUE = listitems, UVALUE = 'LIST', YSIZE = 9,/context_events)

; the image context menu

contextbase = widget_base(ilist, /context_menu, uname = 'ilistctxtmenu')
;; save = WIDGET_BUTTON(contextbase, VALUE='Save', UVALUE='save_nonint', EVENT_PRO='IListCtxtMenu')
save = WIDGET_BUTTON(contextbase, VALUE='Save', UVALUE='save', EVENT_PRO='IListCtxtMenu')
exportdisp = WIDGET_BUTTON(contextbase, VALUE='Export Display', UVALUE='exportdisp', EVENT_PRO='IListCtxtMenu')
saveexportdisp = WIDGET_BUTTON(contextbase, VALUE='Save and Export Display', UVALUE='saveexportdisp', EVENT_PRO='IListCtxtMenu')
ctxtrefresh = WIDGET_BUTTON(contextbase, VALUE = 'Refresh Display', UVALUE = 'redisplay', EVENT_PRO='',/SEPARATOR)
zoom = WIDGET_BUTTON(contextbase, VALUE='Zoom', /Menu)
zoom1 = WIDGET_BUTTON(zoom, VALUE='1:1', UVALUE='zoom1:1', EVENT_PRO='IListCtxtMenu')
zoom2 = WIDGET_BUTTON(zoom, VALUE='1:2', UVALUE='zoom1:2', EVENT_PRO='IListCtxtMenu')
;zoom3 = WIDGET_BUTTON(zoom, VALUE='1:3', UVALUE='zoom1:3', EVENT_PRO='IListCtxtMenu')
zoom4 = WIDGET_BUTTON(zoom, VALUE='1:4', UVALUE='zoom1:4', EVENT_PRO='IListCtxtMenu')
zoom4 = WIDGET_BUTTON(zoom, VALUE='1:8', UVALUE='zoom1:8', EVENT_PRO='IListCtxtMenu')
zoom5 = WIDGET_BUTTON(zoom, VALUE='2:1', UVALUE='zoom2:1', EVENT_PRO='IListCtxtMenu')
;zoom6 = WIDGET_BUTTON(zoom, VALUE='3:1', UVALUE='zoom3:1', EVENT_PRO='IListCtxtMenu')
zoom7 = WIDGET_BUTTON(zoom, VALUE='4:1', UVALUE='zoom4:1', EVENT_PRO='IListCtxtMenu')
zoom7 = WIDGET_BUTTON(zoom, VALUE='8:1', UVALUE='zoom8:1', EVENT_PRO='IListCtxtMenu')
zoom7 = WIDGET_BUTTON(zoom, VALUE='Other ...', UVALUE='zoom:other', EVENT_PRO='IListCtxtMenu')
sb = WIDGET_BUTTON(contextbase, VALUE = 'Display Scale Bar', /Menu)
sbnone=WIDGET_BUTTON(sb,VALUE='None',UVALUE = 'ScaleBar:None', EVENT_PRO='IListCtxtMenu')
sbmax=WIDGET_BUTTON(sb,VALUE='Max Color Index',UVALUE = 'ScaleBar:Max', EVENT_PRO='IListCtxtMenu')
sbmin=WIDGET_BUTTON(sb,VALUE='Min Color Index',UVALUE = 'ScaleBar:Min', EVENT_PRO='IListCtxtMenu')
ctxtinfoeditor = WIDGET_BUTTON(contextbase, VALUE = 'Edit Notes', UVALUE = 'imeditnotes', EVENT_PRO='IListCtxtMenu')
;; disp = WIDGET_BUTTON(contextbase, VALUE='Projection', /Menu)
;;dispX = WIDGET_BUTTON(disp, VALUE='X (YZ-plane)', UVALUE='X', EVENT_PRO='IListCtxtMenu')
;;dispY = WIDGET_BUTTON(disp, VALUE='Y (XZ-plane)', UVALUE='Y', EVENT_PRO='IListCtxtMenu')
;;dispZ = WIDGET_BUTTON(disp, VALUE='Z (XY-plane)', UVALUE='Z', EVENT_PRO='IListCtxtMenu')
ctxtanimate = WIDGET_BUTTON(contextbase, VALUE = 'Animate', UVALUE = 'animate', EVENT_PRO='IListCtxtMenu')
ctxtextract = WIDGET_BUTTON(contextbase, VALUE = 'Extract Current Slice', UVALUE = 'extract', EVENT_PRO='IListCtxtMenu')
ctxtextract = WIDGET_BUTTON(contextbase, VALUE = 'Convert To PAD', UVALUE = 'topad', EVENT_PRO='IListCtxtMenu')
PadPropdisp = WIDGET_BUTTON(contextbase, VALUE='PAD Meta Data', /Menu)
ctxtinfoeditor = WIDGET_BUTTON(PadPropdisp, VALUE = 'Edit PAD Meta Data', UVALUE = 'imeditprops', EVENT_PRO='IListCtxtMenu')
ctxtinfoeditor = WIDGET_BUTTON(PadPropdisp, VALUE = 'Load PAD Meta Data', UVALUE = 'loadpadprop', EVENT_PRO='IListCtxtMenu')
ctxtinfoeditor = WIDGET_BUTTON(PadPropdisp, VALUE = 'Save PAD Meta Data', UVALUE = 'savepadprop', EVENT_PRO='IListCtxtMenu')
;;tcoord = WIDGET_BUTTON(contextbase, VALUE='Tranpose Coordinates', /Menu)
;;ctxtflip = WIDGET_BUTTON(tcoord, VALUE = 'Align Coordinates for Tomato', UVALUE = 'tomatoflip', EVENT_PRO='IListCtxtMenu')
;;ctxtflip = WIDGET_BUTTON(tcoord, VALUE = 'Revert to Standard Coordinates', UVALUE = 'reverttomatoflip', EVENT_PRO='IListCtxtMenu')
;; ctxtflip = WIDGET_BUTTON(contextbase, VALUE = 'Flip Axes', UVALUE = 'flip', EVENT_PRO='IListCtxtMenu')
duplicate = WIDGET_BUTTON(contextbase, VALUE='Duplicate', UVALUE='duplicate', EVENT_PRO='IListCtxtMenu')
mergeframes = WIDGET_BUTTON(contextbase, VALUE='Merge Frames to Stack', UVALUE='mergeframes', EVENT_PRO='IListCtxtMenu')
;;save_par = WIDGET_BUTTON(contextbase, VALUE='Save Parameters', UVALUE='par_save', EVENT_PRO='IListCtxtMenu',/SEPARATOR)
;;restore_par = WIDGET_BUTTON(contextbase, VALUE='Load Parameters', UVALUE='par_restore', EVENT_PRO='IListCtxtMenu')
;;inspect_par = WIDGET_BUTTON(contextbase, VALUE='Set Parameters', UVALUE='par_inspect', EVENT_PRO='IListCtxtMenu')
;;inspect_par = WIDGET_BUTTON(contextbase, VALUE='Export Tilt Angle
;;List', UVALUE='ta_export', EVENT_PRO='IListCtxtMenu')

ctxtdel = WIDGET_BUTTON(contextbase, VALUE = 'Delete', UVALUE = 'delete', EVENT_PRO='IListCtxtMenu',/SEPARATOR)
ctxtdelother = WIDGET_BUTTON(contextbase, VALUE = 'Delete Other Stacks', UVALUE = 'deleteother', EVENT_PRO='IListCtxtMenu')
ctxtdelall = WIDGET_BUTTON(contextbase, VALUE = 'Delete All Stacks', UVALUE = 'deleteall', EVENT_PRO='IListCtxtMenu')
; row4
row4 = WIDGET_BASE(Tab1, /ROW)
s=[""]
itext = WIDGET_TEXT(row4, XSIZE=ListWidth, YSIZE=9, VALUE=s, /SCROLL)
;; add a slider
row5 = WIDGET_BASE(Tab1, /ROW)
s=[""]
psliderstate=PTR_NEW({name:'sliderstate',id:-1L,stackp:PTR_NEW(),active:0L,slice:0L,min:0L,max:1L,width:1L,avgid:-1L})
slider_id = WIDGET_SLIDER(row5, MINIMUM=(*psliderstate).min, XSIZE=FIX(XW)-30, MAXIMUM=(*psliderstate).max, Value=(*psliderstate).slice, TITLE="Slice  ", SENSITIVE=(*psliderstate).active, UVALUE=psliderstate)
(*psliderstate).id = slider_id
row51 = WIDGET_BASE(Tab1, /ROW)
s=[""]
avgslider_id = WIDGET_SLIDER(row51, MINIMUM=(*psliderstate).min, XSIZE=FIX(XW)-30, MAXIMUM=(*psliderstate).max, Value=(*psliderstate).width, TITLE="Average", SENSITIVE=(*psliderstate).active, UVALUE=psliderstate)
(*psliderstate).avgid = avgslider_id
Slider_Control, psliderstate 
;;
;; The Data Tab
;;

;; data list

Tab3 = WIDGET_BASE(wTab, Title=' XY-Graphs ', /COLUMN)
drow0 = WIDGET_BASE(Tab3, /ROW)
dlistitems=['empty'] ;; empty list
dlist = WIDGET_LIST(drow0, XSIZE=ListWidth, VALUE = listitems, UVALUE = 'DLIST', YSIZE = 11,/context_events)
;; context menu
;;
dcontextbase = widget_base(dlist, /context_menu, uname = 'dlistctxtmenu')
dctxtplot = WIDGET_BUTTON(dcontextbase, VALUE = 'Refresh Graph', UVALUE = 'plot', EVENT_PRO='DListCtxtMenu')
ctxtexport = WIDGET_BUTTON(dcontextbase, VALUE = 'Close Graph Window', UVALUE = 'closewindow', EVENT_PRO='DListCtxtMenu')
ctxtexport = WIDGET_BUTTON(dcontextbase, VALUE = 'Export Graph', UVALUE = 'exportgraph', EVENT_PRO='DListCtxtMenu')
ctxtexport = WIDGET_BUTTON(dcontextbase, VALUE = 'Export Graph Data', UVALUE = 'export', EVENT_PRO='DListCtxtMenu')
dctxtdelete = WIDGET_BUTTON(dcontextbase, VALUE = 'Delete Graph', UVALUE = 'delete', EVENT_PRO='DListCtxtMenu')
dctxtdeleteother = WIDGET_BUTTON(dcontextbase, VALUE = 'Delete Other Graphs', UVALUE = 'deleteothergraphs', EVENT_PRO='DListCtxtMenu')
dctxtdeleteother = WIDGET_BUTTON(dcontextbase, VALUE = 'Delete All Graphs', UVALUE = 'deleteallgraphs', EVENT_PRO='DListCtxtMenu')

drow1 = WIDGET_BASE(Tab3, /ROW)
s=[""]
dtext = WIDGET_TEXT(drow1, XSIZE=ListWidth, YSIZE=11, VALUE=s, /SCROLL)
;; data list

Tab4 = WIDGET_BASE(wTab, Title=' Tables ', /COLUMN)
trow0 = WIDGET_BASE(Tab4, /ROW)
tlistitems=['empty'] ;; empty list
tlist = WIDGET_LIST(trow0, XSIZE=ListWidth, VALUE = listitems, UVALUE = 'TLIST', YSIZE = 11,/context_events)
;; context menu
;;
tcontextbase = widget_base(tlist, /context_menu, uname = 'tlistctxtmenu')
ttxtedit = WIDGET_BUTTON(tcontextbase, VALUE = 'Edit Table', UVALUE = 'edit', EVENT_PRO='TListCtxtMenu')
tload = WIDGET_BUTTON(tcontextbase, VALUE = 'Load Table Data from File', UVALUE = 'load', EVENT_PRO='TListCtxtMenu')
texport = WIDGET_BUTTON(tcontextbase, VALUE = 'Export Table Data to File', UVALUE = 'export', EVENT_PRO='TListCtxtMenu')
tctxtdelete = WIDGET_BUTTON(tcontextbase, VALUE = 'Delete Table', UVALUE = 'delete', EVENT_PRO='TListCtxtMenu')
tctxtdeleteother = WIDGET_BUTTON(tcontextbase, VALUE = 'Delete Other Tables', UVALUE = 'deleteothertables', EVENT_PRO='TListCtxtMenu')
tctxtdeleteother = WIDGET_BUTTON(tcontextbase, VALUE = 'Delete All Tables', UVALUE = 'deletealltables', EVENT_PRO='TListCtxtMenu')


;;

trow1 = WIDGET_BASE(Tab4, /ROW)
s=[""]
ttext = WIDGET_TEXT(trow1, XSIZE=ListWidth, YSIZE=11, VALUE=s, /SCROLL)
;; general buttons
;;

grid = WIDGET_BASE(base, COLUMN=3, /GRID_LAYOUT) 
reread = WIDGET_BUTTON(grid, VALUE = '  Reread  ', UVALUE = 'reread', TOOLTIP="update lists and redisplay info text")
help = WIDGET_BUTTON(grid,   VALUE = '  Help  ', UVALUE = 'help')
quit = WIDGET_BUTTON(grid,   VALUE = '  Close  ', UVALUE = 'quit')


WIDGET_CONTROL, ilist, SET_LIST_SELECT=0
;; tmp=GetCurrentGroup()
;;IF PTR_VALID(tmp) THEN BEGIN
;;    IF ((*tmp).ic EQ 1) THEN WIDGET_CONTROL, conbutton, /SET_BUTTON ELSE WIDGE;;T_CONTROL, coffbutton, /SET_BUTTON
;:END
;;WIDGET_CONTROL, dlist, SET_LIST_SELECT=0
   widpos=PlaceWidget(base, POSKEY="ul")
   Widget_Control, base, XOFFSET=widpos[0], YOFFSET=widpos[1]+100, /REALIZE

XMANAGER, 'XTabControl', base, /NO_BLOCK
Update_XTabControl
END








