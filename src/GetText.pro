PRO GetText_Cleanup, baseid
  WIDGET_Control, baseid, GET_UVALUE=pstate
  WIDGET_Control, baseid, /DESTROY
END

PRO GetText_event, ev
  WIDGET_Control, ev.top, GET_UVALUE=pstate
  If (ev.id EQ (*pstate).bid) THEN BEGIN
     Case ev.value OF
        'Cancel': BEGIN
           (*pstate).success=0B
           (*pstate).name=''
        END
        'Accept': BEGIN
           (*pstate).success=1B
           WIDGET_CONTROL, (*pstate).labelid, GET_VALUE=tmp
           (*pstate).name=tmp
        END
        ELSE:
     END
     GetText_Cleanup, ev.top
  END
END

  
FUNCTION GetText, TITLE=title, GROUP_LEADER=group_leader
  result=''
  IF NOT(keyword_set(title)) THEN title="Get Input Text"
  IF N_Elements(group_leader) EQ 0 THEN BEGIN
      group_leader = Widget_Base(Map=0)
      Widget_Control, group_leader, /Realize
      destroy_groupleader = 1
   ENDIF ELSE destroy_groupleader = 0
  inpbase = Widget_Base(GROUP_LEADER=group_leader, TITLE=title, /COLUMN, /MODAL, /FLOATING)
  input =  WIDGET_BASE(inpbase, /ROW, XPAD=10)
  x_label = CW_Field(input, TITLE = "Name:", XSize=30, Frame=0)
  mkd_b = CW_BGroup(inpbase, /ROW, ['Accept','Cancel'] , Button_UVALUE=['Accept','Cancel'])
;; widpos=PlaceWidget(inpbase, POSKEY=WidgetPos("cc"))
;; WIDGET_CONTROL, inpbase, XOFFSET=widpos[0], YOFFSET=widpos[1],
;; /REALIZE
  WIDGET_CONTROL, inpbase, /REALIZE
  pstate=PTR_NEW({success:0B, name:'', bid:mkd_b, labelid:x_label })
  WIDGET_CONTROL, inpbase, SET_UVALUE=pstate
  xmanager, "GetText", inpbase, CLEANUP="GetText_Cleanup"
  result=(*pstate).name
  PTR_FREE, pstate
  IF destroy_groupleader THEN Widget_Control, group_leader, /Destroy
  return, result
END
