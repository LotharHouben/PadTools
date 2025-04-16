

;; Calibration Table object
;; Loads a JSON from a file
;;
;; 
;; Usage: initialize object with a db file
;;        activate a setting from the appliation, can be automatic upon file loading or via tree widget
;; TODO:  set from selection
;;        activatematch
;;        queryactive
;; 

FUNCTION NumericKeyString, val
  ;; converts argument to a minimal string
  ;; there are floating point values
  ;; or integers
  IF valid_num(val) THEN return, NumTrim(val) ELSE BEGIN
     print, "% NumericKeyString: input is not numeric, returning string."
     return, val
  END
  ;;  
END


FUNCTION CalibrationObj::Init, filename, UI=ui
  self.pjson=PTR_NEW(hash())
  self.pactivehash=PTR_NEW(hash())
  ;; check existence of file
  if not(file_test(filename)) THEN ui=1
  if keyword_set(ui) THEN BEGIN
     sFile = DIALOG_PICKFILE(PATH=GETENV('HOME'), TITLE='Select a calibration table file',  FILTER='*.json')
     IF (sFile EQ "") THEN BEGIN
        print, "CalibrationObj::LoadFromFile: No file selected."
        return, 0
     END ELSE filename=sFile
  END
  Result = FILE_TEST(filename, /READ)
  IF NOT(result) THEN BEGIN
     print, '% CalibrationObj::SetFileName: '+filename+ ' does not exist or is not accessible for reading.'
     self.dbfilename=""
  END
  self.dbfilename=filename
  self.pjson=PTR_NEW(hash())
  ;; parse the json string
  OPENR, lun, self.dbfilename, /GET_LUN
; Read one line at a time, saving the result into array
  str = ''
  lines=list()
  WHILE NOT EOF(lun) DO BEGIN 
     READF, lun, str
     lines.Add, str 
  ENDWHILE   
; Close the file and free the file unit
  FREE_LUN, lun
  IF lines.IsEmpty() THEN BEGIN
    print, "CalibrationListObj::LoadFromFile: File "+sFile+" does not contain calibration data."
    return, 0
 END ELSE BEGIN
    js=''
    For i=0,lines.Count()-1 DO js+=lines[i]
 END
 CATCH, Error_status
 
   ;This statement begins the error handler:
   IF Error_status NE 0 THEN BEGIN
      PRINT, 'JSON parse error: ', Error_status
      PRINT, 'Error message: ', !ERROR_STATE.MSG
      ; Handle the error by extending A:
      
      CATCH, /CANCEL
      return, 0
   ENDIF
   dbjson=JSON_PARSE(js)
   if (dbjson.HasKey("version") and (float(dbjson["version"]) GE 2.)) THEN BEGIN 
      self.pjson=PTR_NEW(dbjson)
      PRINT, 'JSON db loaded, version is ', float(dbjson["version"])
   END ELSE BEGIN
     PRINT, 'JSON db version error (> 2.0 required): ', float(dbjson["version"])
   END
      
    ;; oh=*(self.pjson) ;; ordered hash
    ;; oh["version"]    ;; db version
    
    ;; oh["instrumenttables"].keys()                         ;; list of instruments
    ;; (oh["instrumenttables"])["Titan Cubed 3589"] ;; instrument entry, ordered hash
    ;; ((oh["instrumenttables"])["Titan Cubed 3589"]).Keys() ;; list of devices
    ;; ((oh["instrumenttables"])["Titan Cubed 3589"])["ELA"] ;; list of modes
    ;; (((oh["instrumenttables"])["Titan Cubed 3589"])["ELA"])[0] ;; a mode ... ordered hash
 return, 1
END


PRO CalibrationObj::PrintDB
  IF PTR_VALID((self.pjson)) THEN BEGIN
     oh=*(self.pjson)
     Foreach instrument, oh["instrumenttables"].keys() DO BEGIN
        Print,"-",instrument
        value=(oh["instrumenttables"])[instrument]
        foreach detector, value.keys() Do BEGIN
           Print, "    |_", detector
           modelist=value[detector]
           Foreach mode, modelist DO BEGIN
              modestr=mode["mode"]+":"+mode["submode"]
              IF mode.HasKey("subsetting") THEN modestr+=":"+mode["subsetting"]
              Print, "        |_",modestr
              IF mode.HasKey("voltage") THEN BEGIN
                 voltages=mode["voltage"]
                 foreach voltage, voltages.keys() DO BEGIN
                    Print, "            |_", voltage
                    foreach calibcategory, (voltages[voltage]).keys() DO BEGIN
                       Print, "                |_",calibcategory
                       calibset=(voltages[voltage])[calibcategory] 
                       ;; Print, "                |_",calibset
                       Print, "                     |_ ", calibset["unit"]
                       Print, "                     |_ ", calibset["interpolation"]
                       Foreach val,calibset["values"].keys() DO Print, "                     |_", val, ":",(calibset["values"])[val]
                      
                    END 
                 END  
              END 
           END 
        END 
     end 
  END      
END 




FUNCTION CalibrationObj::ReportSelectedMode
  res=list()
  IF PTR_VALID((self.psel)) THEN BEGIN
     h=*(self.psel)
     res.Add, (h['mode'])['instrument']
     res.Add, (h['mode'])['detector']
     str=(h['mode'])['mode']+":"+(h['mode'])['submode']
     IF (h['mode']).haskey('subsetting') THEN str+=":"+(h['mode'])['subsetting']
     res.Add, str
     res.Add, (h['mode'])['voltage']+"V"
     res.Add, "diffraction - "+(h["interpolation"])("Cameralength")
     res.Add, "image       - "+(h["interpolation"])("Magnification")
  END
  return, res
END




FUNCTION CalibrationObj::MatchMode, h,  VERBOSE=verbose
  ;; set a mode with
  result=0
  IF (h.haskey("instrument") and h.haskey("detector") and h.haskey("mode") and h.haskey("submode") and h.haskey("voltage")) THEN BEGIN
     ;; set active mode
     oh=*(self.pjson)
     item=h['instrument']
     IF (oh["instrumenttables"]).haskey(item) THEN BEGIN
        value=(oh["instrumenttables"])[item]
        item=h['detector']
        IF value.haskey(item) THEN BEGIN
           modelist=value[item] ;; mode list is a list
           match=0
           ctr=0
           WHILE ((match NE 1) and (ctr LT modelist.Count())) DO BEGIN
              mode=modelist[ctr]
              IF ((mode['mode'] EQ h['mode']) and (mode['submode'] EQ h['submode'])) THEN BEGIN
                 IF mode['voltage'].haskey(h['voltage']) THEN BEGIN
                    ;; STOP
                    IF h.haskey('subsetting') THEN BEGIN
                       IF mode.haskey('subsetting') THEN IF (h['subsetting'] EQ mode['subsetting']) THEN match=1
                    END ELSE match=1
                 END 
              END
              ctr+=1
           END 
        END        
     END
     IF match THEN BEGIN
        ;; STOP
        d=hash()
        d["mode"]=h
        d["interpolation"]=hash()
        (d["interpolation"])['Cameralength']=(((mode['voltage'])[h['voltage']])['Cameralength'])['interpolation']
        (d["interpolation"])['Magnification']=(((mode['voltage'])[h['voltage']])['Magnification'])['interpolation']
        self.psel=PTR_NEW(d)
        ;; print, "Selection: ",  ((*(self.psel))['data'])['Cameralength']
        result=1
     END ELSE BEGIN
        self.psel=PTR_NEW()
     END
  END ELSE print, "% CalibrationObj::MatchMode: Missing keyword in mode hash."
     return, result
  END  



FUNCTION CalibrationObj::QuerySelectedCalibration, VALUE=value, DIFFRACTION=diffraction
  ;;
  ;; value: indicated magnification or cameralength value
  ;;
  ;; magnification calibration is returned by default
  ;; if keyword diffraction is set then the diffraction calibration is returned
  ;;
  ;; returns field of view, if no active calibration is present then it returns !NULL 
  ;;
  compile_opt idl2
  result=!NULL
  IF NOT(PTR_VALID(self.psel)) THEN BEGIN
     return, result
  END
  h=*(self.psel)
  ;; STOP
  IF NOT(h.haskey('interpolation')) THEN BEGIN
   return, result
  END ELSE BEGIN 

     key='Magnification' & if keyword_set(diffraction) THEN key='Cameralength'
     IF (h['interpolation']).haskey(key) THEN BEGIN
        equation=(h['interpolation'])[key]
        f=call_function('lambda',equation)
        result=f(value)
        return, result
     END 
  END 
  return, result
END    
 



pro CalibrationObj__define
 void={CalibrationObj,name:'CalibrationObj', $
       pjson: PTR_NEW(), $ ;; hash("name","","mode","","submode","")
       pactivehash: PTR_NEW(), $
       dbfilename: '', $
       wTLB:0L, $
       psel:PTR_NEW(hash()), $ ;; a hash with a selected mode
       break: 0}
 return
end


PRO CalibrationObj::Cleanup
  ;; CATCH, Error_status
  ;; IF (Error_status NE 0) THEN BEGIN
  ;;    Print, "% CalibrationObj::Cleanup: Fatal error message"
  ;;    Print, "%            " + !ERR_STRING
  ;;   CATCH, /Cancel‚
  ;;   return
  ;; END
  Print, "% CalibrationObj::Cleanup: Cleaning up CalibrationObj object."
  IF PTR_VALID(self.pactivehash) THEN BEGIN
     ;; delete
     PTR_FREE, self.pactivehash
  END
  IF PTR_VALID(self.pjson) THEN BEGIN
     ;; delete
     PTR_FREE, self.pjson
  END
  obj_destroy, self
  HELP, self
  return
END


; Event-handler routine for CalibrationUI widget
PRO CalibrationUI_event, ev

  COMPILE_OPT hidden

  ; We use widget user values to determine what action to take.
  ; First retrieve the user value (if any) from the widget that
                                ; generated the event.
  WIDGET_CONTROL, ev.top, GET_UVALUE=pobj  ;; get object pointer 

  ;; STOP
  WIDGET_CONTROL, ev.ID, GET_UVALUE=uName
  ; If the widget that generated the event has a user value, check
  ; its value and take the appropriate action.
  IF (N_ELEMENTS(uName) NE 0) THEN BEGIN
     IF PTR_VALID(uName) THEN BEGIN
    ;; IF (uName EQ 'LEAF') THEN BEGIN
      ; Make sure the value does not change when the leaf node
      ; is selected with a single click.
        ;;  IF (ev.CLICKS EQ 2) THEN TWE_ToggleValue, ev.ID
        WIDGET_CONTROL, ev.ID, /SET_TREE_SELECT
        ;; h= *(uName)
        IF NOT(pobj->MatchMode(*uName)) THEN print, "% CalibrationUI_event: Failed to match mode via calibration object."
    ENDIF ELSE BEGIN
       IF (uName EQ 'DONE') THEN WIDGET_CONTROL, ev.TOP, /DESTROY
    END
  ENDIF

END


PRO CalibrationUI_cleanup, wTLB
  ;; trick xmanager to call a cleanup procedure
END

; Routine to change the value of a leaf node's text.
PRO TWE_ToggleValue, widID

  COMPILE_OPT hidden

  ; Get the current value.
  WIDGET_CONTROL, widID, GET_VALUE=curVal

  ; Split the string at the colon character.
  full_string = STRSPLIT(curVal, ':', /EXTRACT)

  ; Check the value of the text after the colon, and toggle
  ; to the new value.
  full_string[1] = (full_string[1] EQ ' Off') ? ': On' : ': Off'

  ; Reset the value of the leaf node's text.
  WIDGET_CONTROL, widID, SET_VALUE=STRJOIN(full_string)

END



; Widget creation routine.
PRO CalibrationObj::UI

  ;; cal: calibration object
  IF NOT(PTR_VALID((self.pjson))) THEN BEGIN
     print, "% CalibrationObj::UI: No calibration data loaded."
     return
  END
  ; Start with a base widget.
  wTLB = WIDGET_BASE(/COLUMN, TITLE='Calibration Set', XSIZE=300)
  self.wTLB=wTLB
  ; The first tree widget has the top-level base as its parent.
  ; The visible tree widget branches and leaves will all be
  ; descendants of this tree widget.
  wTree = WIDGET_TREE(wTLB)

  ; Place a folder at the root level of the visible tree.
  wtRoot = WIDGET_TREE(wTree, VALUE='Calibrations', /FOLDER, /EXPANDED)

  ; Create leaves and branches. Note that we set the user value of
  ; every leaf node (tree widgets that do not have the FOLDER keyword
  ; set) equal to 'LEAF'. We use this in the event handler to determine
                                ; whether or not to change the text value.
  instlist=list()
  oh=*(self.pjson)
  moderecord=hash()
  Foreach instrument, oh["instrumenttables"].keys() DO BEGIN
     item=WIDGET_TREE(wtRoot, VALUE=instrument, /FOLDER, /EXPANDED)
     instlist.Add, item
     value=(oh["instrumenttables"])[instrument]
     moderecord["instrument"]=instrument
     foreach detector, value.keys() Do BEGIN
        itemdet = WIDGET_TREE(item, VALUE=detector, /FOLDER)
        moderecord["detector"]=detector
        modelist=value[detector]
        Foreach mode, modelist DO BEGIN
           modestr=mode["mode"]+":"+mode["submode"]
           moderecord["mode"]=mode["mode"]
           moderecord["submode"]=mode["submode"]
           
           IF mode.HasKey("subsetting") THEN BEGIN
              modestr+=":"+mode["subsetting"]
              moderecord["subsetting"]=mode["subsetting"]
           END ELSE BEGIN
              IF moderecord.haskey("subsetting") THEN moderecord.REMOVE, ["subsetting"]
           END
           itemmode = WIDGET_TREE(itemdet, VALUE=modestr, /FOLDER)
           IF mode.HasKey("voltage") THEN BEGIN
              voltages=mode["voltage"]
              foreach voltage, voltages.keys() DO BEGIN
                 moderecord["voltage"]=voltage
                 h=hash()
                 Foreach key,moderecord.keys() DO h[key]=moderecord[key]
                 ;; itemvoltage = WIDGET_TREE(itemmode, VALUE=voltage, UVALUE=PTR_NEW(h), /CHECKBOX, CHECKED=0)
                 itemvoltage = WIDGET_TREE(itemmode, VALUE=voltage, UVALUE=PTR_NEW(h))
                 ;; 
              END
           END
        END 
     END
  END
  ;; wtLeaf11 = WIDGET_TREE(wtRoot, VALUE='Setting 1-1: Off', $
  ;;   UVALUE='LEAF')
  ;; wtBranch12 = WIDGET_TREE(wtRoot, VALUE='Branch 1-2', $
  ;;   /FOLDER, /EXPANDED)x
  ;; wtLeaf121 = WIDGET_TREE(wtBranch12, VALUE='Setting 1-2-1: Off', $
  ;;   UVALUE='LEAF')
  ;; wtLeaf122 = WIDGET_TREE(wtBranch12, VALUE='Setting 1-2-2: Off', $
  ;;   UVALUE='LEAF')
  ;; wtLeaf13 = WIDGET_TREE(wtRoot, VALUE='Setting 1-3: Off', $
  ;;   UVALUE='LEAF')
  ;; wtLeaf14 = WIDGET_TREE(wtRoot, VALUE='Setting 1-4: Off', $
  ;;   UVALUE='LEAF')

  ; Create a 'Done' button, setting its user value for use in the
  ; event handler.
  wDone = WIDGET_BUTTON(wTLB, VALUE="Apply", UVALUE='DONE')
  ;; 
  
                                ; Realize the widgets and run XMANAGER to manage them.
   
  WIDGET_CONTROL, self.wTLB, /REALIZE
  WIDGET_CONTROL, wTLB, SET_UVALUE=self
  XMANAGER, 'CalibrationUI', self.wTLB, ClEANUP='CalibrationUI_cleanup', EVENT_HANDLER='CalibrationUI_event';; , /NO_BLOCK

END

FUNCTION CalibrationObj::HasSelection
  IF NOT(PTR_VALID(self.psel)) THEN BEGIN
     return, 0
  END
  return, 1
END


Pro Test_CalibrationObj
  mycal=OBJ_NEW('CalibrationObj',"", /UI)
  ;; INSTRUMENT="Titan Titan Cubed 3589", DEVICE='EMPAD',MODE='STEM',SUBMODE='Nanoprobe'
;;mycal->AddCategory, "Magnification"
;;print, mycal->MATCH(INSTRUMENT="Titan Titan Cubed 3589",DEVICE="EMPAD",MODE="STEM")
;;print, mycal->QueryItem(CATEGORY="Magnification",SECTION=200000,LABEL='630000',BIN=0.4,DIMX=256)
  mycal->PrintDB
  mycal->UI
  print, mycal->ReportSelectedMode()
  mag=87000
  val=mycal->QuerySelectedCalibration(VALUE=mag)
  print, "Magnification ", mag, ":, ",val
  obj_destroy, mycal
END
