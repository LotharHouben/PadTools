PRO CalibData
  If NOT(GetDebugFlag()) THEN BEGIN
  CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% CalibData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END
END
  mycal=GetCalibrationObj()
  IF NOT(OBJ_VALID(mycal)) THEN BEGIN
     printtocon, "% CalibData: Accessing internal calibration data base failed."
     ErrMsg, ["Accessing internal calibration table failed.","Make sure the database file is present in your home directory."]
     return
  END
  IF NOT(mycal->HasSelection()) THEN BEGIN
     printtocon, "% CalibData: Accessing internal calibration table failed, select a default calibration table first."
     ErrMsg, ["Accessing internal calibration table failed.","Please select one first."]
     return
  END
  origp=GetCurrentP()
  pd=(*origp).datap
  pdata=(*pd).data
  o=GetExtraObj()
  IF OBJ_VALID(o) THEN BEGIN ;; it is 4D
     s=list()
     cameralength=720.
     magnification=28500.
     fraction=1.
     spbin=1. & recbin=1.
         
     s.Add, {value:magnification,label:"Real space magnification ",newrow:1B}
     s.Add, {value:fraction,label:"Fraction",newrow:0B}
     s.Add, {value:cameralength,label:"Diffraction camera length (mm)",newrow:1B}
     c=list()
     c.Add, {value:1B,label:"Magnification calibration",newrow:1B}
     c.Add, {value:1B,label:"Diffraction calibration",newrow:1B}
     IF NOT(XMDataChoiceField(s, c, TITLE="Choose parameters and select calibrations") EQ 1) THEN BEGIN
        printtocon, "% CalibData: Aborting."
        return
     END  
     printtocon, "% Menu: Accessing internal calibration table."
    
        
        if (c[0].value EQ 1) THEN BEGIN
           fov=mycal->QuerySelectedCalibration(VALUE=float(s[0].value))
           IF (fov NE !NULL) THEN BEGIN
              ;;STOP
              samplingx=s[1].value*fov/o->GetScanSize(/X) 
              samplingy=samplingx
              o->SetScanSampling, samplingx, samplingy
              printtocon, "% CalibData: FOV="+STRING(fov)+", Sampling:"+STRING(samplingx)
           END
        END
        if (c[1].value EQ 1) THEN BEGIN
           fov=mycal->QuerySelectedCalibration(VALUE=float(s[2].value), /DIFFRACTION)
           qsamplingx=fov/o->GetFrameSize(/X)
           qsamplingy=qsamplingx
           o->SetDetectorSampling, qsamplingx, qsamplingy
           printtocon, "% CalibData: Diffraction FOV="+STRING(fov)+", Sampling:"+STRING(qsamplingx)
           (*pd).xsamp=qsamplingx
           (*pd).ysamp=qsamplingy
        END
        Update_XTabControl
     
  END ELSE BEGIN
     
     ;; it is a 3D dataset
     s=list()
     val=1000.
     fraction=1.
     spbin=1. & recbin=1.
         
     s.Add, {value:val,label:"Magnification or camera length",newrow:1B}
     s.Add, {value:fraction,label:"Fraction",newrow:0B}
     c=list()
     e=list()
     e.Add, {value:1B,label:"Magnification",newrow:1B}
     e.Add, {value:0B,label:"Diffraction ",newrow:0B}
     IF NOT(XMDataChoiceField(s, c, excl=e, TITLE="Choose nmagnification value and calibration type") EQ 1) THEN BEGIN
        printtocon, "% CalibData: Aborting."
        return
     END
     printtocon, "% Menu: Accessing internal calibration table."
    
        
        if (e[0].value EQ 1) THEN BEGIN
           fov=mycal->QuerySelectedCalibration(VALUE=float(s[0].value))
           IF (fov NE !NULL) THEN BEGIN
              ;;STOP
              samplingx=s[1].value*fov/o->GetScanSize(/X) 
              samplingy=samplingx
              (*pd).xsamp=samplingx
              (*pd).ysamp=samplingy
              ;; o->SetScanSampling, samplingx, samplingy
              printtocon, "% CalibData: FOV="+STRING(fov)+", Sampling:"+STRING(samplingx)
           END
        END
        if (e[1].value EQ 1) THEN BEGIN
           fov=mycal->QuerySelectedCalibration(VALUE=float(s[0].value), /DIFFRACTION)
           qsamplingx=fov/o->GetFrameSize(/X)
           qsamplingy=qsamplingx
           ;; o->SetDetectorSampling, qsamplingx, qsamplingy
           printtocon, "% CalibData: Diffraction FOV="+STRING(fov)+", Sampling:"+STRING(qsamplingx)
           (*pd).xsamp=qsamplingx
           (*pd).ysamp=qsamplingy
        END
        Update_XTabControl
  END
END  
