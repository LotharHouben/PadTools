;; 
;; The Main Menu widget
;;
;; depends on
;; - GenericImage.pro
;; - General.pro
;; - Debugging.pro
;; - XSimpleDialog.pro
;; - XPoint.pro



PRO MM_event, ev
;; COMMON DATALIST, dfirst, dcurrent
;; the COMMON block IMAGELIST declares first and p, the first and 
;; the current window pointer in the window list
;; the COMMON block GROUPLIST declares gfirst and gp, the first and 
;; the current group pointer in the group list
;; the COMMON block GROUPLIST declares the context, either 'image' or 'group'
 WIDGET_CONTROL, ev.id, GET_UVALUE = uv
 CASE uv OF ;; 
;;  File Menu Operations
;;
   'file:open': BEGIN
                 FileImport
              END
   'file:importlargeset': BEGIN
                 dummy=ImportLargeSet(/crop,/gui)
              END
   'file:save': BEGIN
       IF LMGR(/Demo) THEN BEGIN
           Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          FileExport
          Update_XTabControl
       END
    END
   'file:exportdisplay': BEGIN
       IF LMGR(/Demo) THEN BEGIN
           Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          ExportBitmap, /DISPLAY
          Update_XTabControl
       END
    END
   'file:saveexp': BEGIN
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
   'file:save_nonint': BEGIN
       IF LMGR(/Demo) THEN BEGIN
           Print, '% XMenu: IDL is running in DEMO mode, no file IO'
       END Else BEGIN
          FileExport_NonInteractive
          Update_XTabControl
       END
   END
   'File:About': BEGIN
      About
   END
   'File:News': BEGIN
      News
   END
   'File:License': BEGIN
      lic=LicenseDialog() ;; calculate license type, default is 0, meaning``
   END
   'File:Ddir': BEGIN
       SelectDesktopDir
    END
   'corr:beamstop': BEGIN
o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         help=["Beam Stop",$
               "", $ 
               "Parameters:", $
               "", $
               "Radius: Mask radius or full width at half maximum in the case of a gaussian mask.", $
               "Off-center shift: Center displacement in horizontal and vertical direction in pixel", $
               "Gaussian: Use gausian mask instead of a smoothed step function.", $
               "Show beamstop mask: Optionally display the beamstop mask.", $ 
         ""]
         s=list()
         s.Add, {value:2,label:"Radius: ",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix): ",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Gaussian mask",newrow:0B}
         c.Add, {value:0B,label:"Show beamstop mask",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Gaussian Beamstop Parameters", HELP=help) EQ 1) THEN o.BeamStop, Radius=s[0].value, DX=s[1].value, DY=s[2].value, MASK=c[1].value, GAUSSIAN=c[0].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
      'corr:subrotav': BEGIN
o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         help=["Subtract Rotational Average",$
               "", $ 
               "Parameters:", $
               "", $
               "Delta: annular width for averaging in pixel, default is 2", $
               "Sigma: sigma for robust mean value estimation, default is 3.", $
               "Off-center shift: Center displacement in horizontal and vertical direction in pixel", $
         ""]
         s=list()
         s.Add, {value:2,label:"Delta: ",newrow:0B}
         s.Add, {value:3.,label:"Sigma: ",newrow:1B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix): ",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Subtract Rotational Average", HELP=help) EQ 1) THEN o.RotationalBackgroundSubtraction, Delta=FIX(s[0].value), SIGMA=s[1].value, DX=s[2].value, DY=s[3].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
 'corr:threshold': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
          help=["Threshold",$
               "", $ 
               "Parameters:", $
               "", $
               "Lower Threshold: Lower threshold value below which pixel data is replaced with zero.", $
                "To one: set threshold values to 1 instead of 0.", $
                ""]
         s=list()
         s.Add, {value:10,label:"Lower threshold     ",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"To one",newrow:0B}
         ;; c.Add, {value:0B,label:"Centre on maximum (no
         ;; refinement)",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Threshold cut-off") EQ 1) THEN o.Threshold,  s[0].value, TOONE=c[0].value
         ;; print, s
         ;; print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END  
   'corr:ronchishift': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
          help=["Ronchigram Shift",$
               "", $ 
               "Parameters:", $
               "", $
               "Radius range: Search range for the Ronchigram radius.", $
               "Correct Mean Shift: Fit shift across multiple frames and compensate average displacement.", $
               "Difference Method: Check this option for low dose frames. Less accurate but more robust.", $ 
         ""]
         s=list()
         s.Add, {value:5,label:"Radius range (pix)    ",newrow:0B}
         s.Add, {value:63,label:"",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Correct Mean Shift",newrow:0B}
         c.Add, {value:1B,label:"Difference Method",newrow:0B}
         ;; c.Add, {value:0B,label:"Centre on maximum (no
         ;; refinement)",newrow:1B}
         dx=1 & dy=1 & radius=1
         IF (XMDataChoiceField(s, c, TITLE="Ronchigram Shift") EQ 1) THEN BEGIN
            if (c[1].value EQ 1) THEN BEGIN
               o.DiscCentre,  SEARCHRADII=[s[0].value,s[1].value], DX=dx, DY=dy, /SPARSE, RADIUS=radius, CORRECT=c[0].value, METHOD="filtereddiff", /DISPLAY
            END ELSE o.DiscCentre,  SEARCHRADII=[s[0].value,s[1].value], DX=dx, DY=dy, /SPARSE, RADIUS=radius, CORRECT=c[0].value, /DISPLAY
         END
         ;; print, s
         ;; print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'corr:hotpix': BEGIN
      origp=GetCurrentP()
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         help=["Hot Pixel Correction",$
               "", $ 
               "Parameters:", $
               "", $
               "Threshold: Cut-off value for the hot pixel, factor relative to the robust mean of the data.", $
               "Radius   : The value of the hot pixel will be replaced with the median of a region around  ", $
               "           the pixel, the radius the radius of that region.",$
               "Flatten  : Remove low frequency components for hot pixel detection.", $
               "Iterate  : Iterate, recalculates the mean value and treshold after each correction.", $
         ""]
         s=list()
         s.Add, {value:200,label:"Threshold",newrow:0B}
         s.Add, {value:2,label:"Radius",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Flatten",newrow:0B}
         c.Add, {value:0B,label:"Iterate",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Hot Pixel Correction", HELP=help) EQ 1) THEN o.FixHotPix, ITERATE=c[1].value, FLATTEN=c[0].value,RADIUS=s[1].value, THRESH=s[0].value
         ;;print, s
         ;;print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'corr:maskrows': BEGIN
      origp=GetCurrentP()
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         help=["Mask margin rows",$
               "", $
               "Set rows to 0 or 1. Useful to get rid of beam blanking artefacts in e.g. peak search.",$
               "",$
               "Parameters:", $
               "", $
               "Number: Number of rows.", $
               "Top   : Mask top rows.", $
               "Bottom: Mask Bottom rows.",$
               "To One: By default the frame is set to 0 values, you can choose 1 here.", $
         ""]
         s=list()
         s.Add, {value:2,label:"Number",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Top",newrow:0B}
         c.Add, {value:1B,label:"Bottom",newrow:1B}
         c.Add, {value:0B,label:"To One",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Mask margin rows", HELP=help) EQ 1) THEN o.MaskRows, s[0].value, TOP=c[0].value, BOTTOM=c[1].value, TOONE=c[2].value
         ;;print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'corr:maskcols': BEGIN
      origp=GetCurrentP()
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         help=["Mask margin columns",$
               "", $
               "Set columns to 0 or 1. Useful to get rid of beam blanking artefacts in e.g. peak search.",$
               "",$
               "Parameters:", $
               "", $
               "Number: Number of rows.", $
               "Left   : Mask left margin columns.", $
               "Right  : Mask right margin columns.",$
               "To One: By default the frame is set to 0 values, you can choose 1 here.", $
         ""]
         s=list()
         s.Add, {value:2,label:"Number",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Left",newrow:0B}
         c.Add, {value:1B,label:"Right",newrow:1B}
         c.Add, {value:0B,label:"To One",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Mask margin columns", HELP=help) EQ 1) THEN o.MaskCols, s[0].value, LEFT=c[0].value, RIGHT=c[1].value, TOONE=c[2].value
         ;;print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'corr:fixdectrispix': BEGIN
     DECTRIS_FixPix, GetCurrentP(),/AUTO
   END
   'corr:simplediffshift': BEGIN
      RecenterDiffractionFrames
   END
   'corr:diffshift': BEGIN
      origp=GetCurrentP()
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         help=["Frame Centre Correction",$
               "", $ 
               "Parameters:", $
               "", $
               "Center of mass refinement radius: Radius of the circular mask within which the center of mass ", $
               "                                  will be calculated.", $
               "Polynomial fit order:             Pixel displacements will be modelled by a two-dimensional polymomial ", $
               "                                  function to analyze the general trend. The trend can be used for descan",$
               "                                  correction.", $
               "Pixelwise correction:             Apply the correction pixel for pixel. Useful if you want to align diffraction", $
               "                                  data and do not care about the information coded in diffrection deflection.", $
               "Subpixel accuracy:                Align with subpixel accuracy. Beware of interpolation artefacts.", $
               "Subregion search:                 Search for strongest peak in a fix circular subregion that you mark.", $
               "Display 2D maps:                  Display 2D displacement maps for x and y shifts.", $
         ""]
         s=list()
         s.Add, {value:5,label:"Center of mass refinement radius (pix)",newrow:0B}
         s.Add, {value:2,label:"Polynomial fit order",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Pixelwise correction",newrow:0B}
         c.Add, {value:0B,label:"Subpixel accuracy",newrow:1B}
         c.Add, {value:0B,label:"Use upscaling for subpixel shift",newrow:1B}
         c.Add, {value:0B,label:"Subregion search",newrow:1B}
         c.Add, {value:0B,label:"Display 2D maps",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Alignment parameters", HELP=help) EQ 1) THEN o.CenterCorrection, SUBPIX=c[1].value, COMRADIUS=s[0].value, FITORDER=s[1].value, PIXWISE=c[0].value, UPSCALING=c[2].value, GRAPHS=0, REGION=c[3].value, ITERATE=1, MAPS=c[4].value
         ;print, s
         ;print, c
         IF SetCurrentP(origp) THEN Update_XTabControl
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Transf:reshape': BEGIN
       origp=GetCurrentP()
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         o.ReshapeScanSize
      END
   END  
    'Transf:radialprofile': BEGIN
       origp=GetCurrentP()
       pd=(*origp).datap
       pdata=(*pd).data
       o=GetExtraObj()
       empadframe=0
       ;; check dimensions
       N=Size(*pdata)
      help=["Radial Profile",$
               "", $ 
               "Parameters:", $
               "", $
               "DX: Center Shift in horizontal direction. ", $
               "DY: Center Shift in vertical direction. ", $
               "EPS: Elliptical distortion, 2a/(a+b)-1. a=long axis, b=short axis.", $
            "THETA: Elliptical distortion, azimuth angle of long axis a in radians.", $
            "ETA: Shear distortion, Shear matrix is (1,eta),(0,1) when shear angle is 0", $
               "RHO: shear angle, zero means horizontal axis is sheared.", $
               "BINWIDTH: Sampling interval for radius dimension. ", $
               "Center Refinement: Peak refinement of the central spot, subpixel accuracy. Recommended.", $
            "Pixel Count Normalization: Normalize to pixel count. Recommended.", $
            "Return All Profiles: Return profiles for each diffraction frame in addition to the total.", $ 
         ""]
         s=list()
         s.Add, {value:0,label:"dx",newrow:0B}
         s.Add, {value:0,label:"dy",newrow:0B}
         s.Add, {value:0.0226166,label:"eps",newrow:1B}
         s.Add, {value:0.0870776,label:"theta",newrow:1B}
         s.Add, {value:-0.0452447,label:"eta",newrow:1B}
         s.Add, {value:2.43196,label:"rho",newrow:1B}
         s.Add, {value:0.3,label:"binwidth",newrow:1B}
         c=list()
         c.Add, {value:0B,label:"Use Shear Correction instead of Elliptical Correction",newrow:0B}
         c.Add, {value:1B,label:"Center refinement",newrow:0B}
         c.Add, {value:1B,label:"Pixel Count Normalization",newrow:1B}
         IF (N[0] EQ 3) THEN c.Add, {value:1B,label:"Return All Profiles",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Radial Profile Parameters", HELP=help) EQ 1) THEN BEGIN
            IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
               ;; get stack pointer
               ;;
               empadframe=1
            END ELSE BEGIN
               
               
            END
            ;; check dimensions
            N=Size(*pdata)
            IF NOT(empadframe) THEN BEGIN
                  IF ((N[1] EQ 128) AND (N[2] EQ 130)) THEN empadframe=1
            END
            IF (N[0] EQ 3) THEN BEGIN
               IF (c[0].value EQ 0) THEN BEGIN
                  r=StackToRadialDistanceProfile(pdata, EMPADFRAME=empadframe, DX=s[0].value, DY=s[1].value, EPS=s[2].value, THETA=s[3].value, BINWIDTH=s[6].value, REFINECENTER=c[1].value, NORMALIZE=c[2].value, RETALL=c[3].value)
               END ELSE BEGIN
                  r=StackToRadialDistanceProfile(pdata, EMPADFRAME=empadframe, DX=s[0].value, DY=s[1].value,  ETA=s[4].value, $
                                                 RHO=s[5].value, BINWIDTH=s[6].value, REFINECENTER=c[1].value, NORMALIZE=c[2].value, RETALL=c[3].value)
               END
               name="RadialProfile("+(*origp).name+")"
               ThrowGraph, list(r.y), XVALUES=r.x*(*pd).xsamp, TITLE=name, XTITLE="g ("+(*(*origp).datap).XUNIT+")", YTITLE="Intensity"
               ;; arrange array
               IF c[2].value THEN BEGIN
                  ;; r.all contains the array of profiles for each
                  ;; image
                  ;; r.all[i,*] is the profile for image i, i=0,N[3]-1
                  name="RadialProfiles("+(*origp).name+")"
                  ThrowImage, r.all, TITLE=name, SAMP=[1.,o.GetDetectorSampling(/X),1.], UNIT=['nm','1/nm','nm']
               END 
               
            END ELSE BEGIN
               IF (N[0] EQ 2) THEN BEGIN
                  r=SingleFrameToRadialDistanceProfile((*pdata), EMPADFRAME=empadframe, DX=s[0].value, DY=s[1].value, Eps=s[2].value, THETA=s[3].value, ETA=s[4].value, $
                                                       RHO=s[5].value, BINWIDTH=s[6].value,REFINECENTER=c[1].value, NORMALIZE=c[2].value)
                  ThrowGraph, list(r.y), XVALUES=r.x*(*pd).xsamp
               End
            END
         END
      ;;
         Update_XTabControl
      END
    'Sim:SparseDiff': BEGIN
       ;;
       Wrapper_SparseEventModeDiffPattern
    END
    'ImageTF:SlidFFT': BEGIN
       help=["Sliding FFT - create an FFT 4D dataset of an image",$
               "", $ 
               "Parameters:", $
               "", $
               "Frame Size: Sliding window size in pixel. ", $
             "Step Size : Sliding window shift step in x and y. Defines the sampling density.", $
             "Zero Padd Size: Zero Padding of the sliding window, defines the padded window size.", $
             "FFT Power : Default output is the FFT magnitude. Use this swith to return the FFT power.", $
             "Hanning Window : Apply a hanning window to reduce image shape artefacts.", $
             ""]
       s=list()
       s.Add, {value:128,label:"Frame Size (pix)",newrow:0B}
       s.Add, {value:64,label:"Step Size (pix)",newrow:1B}
       s.Add, {value:256,label:"Zero Padd Size (pix)",newrow:1B}
       c=list()
       c.Add, {value:0B,label:"FFT Power",newrow:0B}
       c.Add, {value:0B,label:"Hanning Window",newrow:1B}
       IF (XMDataChoiceField(s, c,TITLE="Sliding FFT Parameters", HELP=help) EQ 1) THEN SlidingFFT, FIX(s[0].value), FIX(s[1].value), POW=c[0].value, HANNING=c[1].value, PADD=FIX(s[2].value)
         
      END
   'VD:bf': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:5,label:"Radius (pix)",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c,TITLE="Bright-field parameters") EQ 1) THEN o.BF, RADIUS=s[0].value, DX=s[1].value, DY=s[2].value
         print, s
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'VD:adf': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer collection radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Show detector mask",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Annular dark-field parameters") EQ 1) THEN o.ADF, RADIUS=[s[0].value,s[1].value], DX=s[2].value, DY=s[3].value, MASK=c[0].value
         print, s
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'VD:segdet': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer collection radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:4,label:"Number of segments and larmor offset (deg)",newrow:1B}
         s.Add, {value:0.,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Sharp mask",newrow:1B}
         c.Add, {value:1B,label:"Clockwise segment notation",newrow:0B}
         c.Add, {value:0B,label:"Show detector masks",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Annular dark-field parameters") EQ 1) THEN BEGIN
            o.SegmentDet, RADIUS=[s[0].value,s[1].value], SYMMETRY=s[2].value, LARMOR=s[3].value*!Pi/180., DX=s[4].value, DY=s[5].value, MASK=c[2].value, CLOCKWISE=c[1].value, TOPHAT=c[0].value
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Filter:segfilt': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer collection radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:4,label:"Number of segments and larmor offset (deg)",newrow:1B}
         s.Add, {value:0.,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         s.Add, {value:5,label:"Keep BF disk with radius",newrow:1B}
         c=list()
         c.Add, {value:1B,label:"Sharp mask",newrow:1B}
         c.Add, {value:1B,label:"Clockwise segment notation",newrow:0B}
         c.Add, {value:0B,label:"Show detector masks",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Annular filter parameters") EQ 1) THEN BEGIN
            o.FilterSegment, RADIUS=[s[0].value,s[1].value], SYMMETRY=s[2].value, LARMOR=s[3].value*!Pi/180., DX=s[4].value, DY=s[5].value, MASK=c[2].value, CLOCKWISE=c[1].value, TOPHAT=c[0].value, BF=s[6].value
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'VD:TwoSegHist': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Detector 1: Inner, outer collection radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"center offset dx, dy (pix)",newrow:0B}
         s.Add, {value:0,label:"",newrow:0B}
         s.Add, {value:10,label:"Detector 2: Inner, outer collection radius (pix)",newrow:1B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"center offset dx, dy (pix)",newrow:0B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Show detector mask",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Detector Parameters") EQ 1) THEN BEGIN
              radius1=[s[0].value,s[1].value]
              radius2=[s[4].value,s[5].value]
              o.MulipleDetectorSignal, RADIUS1=radius1, RADIUS2=radius2, DX1=s[2].value, DY1=s[3].value, DX2=s[6].value, DY2=s[7].value, MASK=c[0].value
           END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'VD:ThreeSegHist': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:5,label:"Detector 1: Inner, outer collection radius (pix)",newrow:0B} ;; 0
         s.Add, {value:20,label:"",newrow:0B} ;; 1
         s.Add, {value:0,label:"center offset dx, dy (pix)",newrow:0B} ;; 2
         s.Add, {value:0,label:"",newrow:0B} ;; 3
         s.Add, {value:25,label:"Detector 2: Inner, outer collection radius (pix)",newrow:1B} ;; 4 -7
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"center offset dx, dy (pix)",newrow:0B}
         s.Add, {value:0,label:"",newrow:0B}
         s.Add, {value:45,label:"Detector 3: Inner, outer collection radius (pix)",newrow:1B} ;; 8-11
         s.Add, {value:80,label:"",newrow:0B}
         s.Add, {value:0,label:"center offset dx, dy (pix)",newrow:0B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Show detector mask",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Detector Parameters") EQ 1) THEN BEGIN
              radius1=[s[0].value,s[1].value]
              radius2=[s[4].value,s[5].value]
              radius3=[s[8].value,s[9].value]
              o.MulipleDetectorSignal, RADIUS1=radius1, RADIUS2=radius2, RADIUS3=radius3, DX1=s[2].value, DY1=s[3].value, DX2=s[6].value, DY2=s[7].value,  DX3=s[10].value, DY3=s[11].value, MASK=c[0].value
           END
      END  ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'VD:AnnularSegments': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2,label:"Integration width (pix)",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Sharp mask",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Annular segment of all diameters") EQ 1) THEN o.FluctuationImages, DELTA=s[0].value, DX=s[1].value, DY=s[2].value, /TOTAL , TOPHAT=c[0].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'VD:VirtualAperture': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         ptr=GetRootP()
         IF NOT(PTR_VALID(ptr)) THEN BEGIN
            printtocon, "% Menu: No 3D Data found."
            return
         END
         ptr=(*ptr).current
         IF NOT(PTR_VALID(ptr)) THEN BEGIN
            printtocon, "% Menu: Current 3D Data invalid."
            return
         END
         ptr=(*ptr).datap
         IF NOT(PTR_VALID(ptr)) THEN BEGIN
            printtocon, "%  Menu: Current 3D Data holds no data."
            return
         END
         ;; get coordinate and radius
         x0=FIX(((*ptr).SzX/2)/(*ptr).binning) & x=x0
         y0=FIX(((*ptr).SzY/2)/(*ptr).binning) & y=y0
         r=FIX(((*ptr).SzX/10)/(*ptr).binning)
         IF (XCircle(x,y,r, bin=(*ptr).binning) GT 0) THEN BEGIN
            ;; create a BF image with the appropriate shift
             o.BF, RADIUS=r, DX=(x-(*ptr).SzX/2), DY=(y-(*ptr).SzX/2), TITLE="VirtObjAp"
         END
      END
      
   END
   'WD:templatemap':BEGIN
      TemplateCorrelateDiff
   END
   'WD:omap': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer collection radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Show detector mask",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Orientation map annular parameters") EQ 1) THEN o.OrientationMap, RADIUS=[s[0].value,s[1].value], DX=s[2].value, DY=s[3].value, MASK=c[0].value
         print, s
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'WD:omap2': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer ring radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         s.Add, {value:1.0,label:"Angular sampling (deg)",newrow:1B}
         s.Add, {value:3,label:"Median filter width ",newrow:1B}
         s.Add, {value:0.15,label:"Correlation threshold ",newrow:1B}
         c=list()    
         IF (XMDataChoiceField(s, c, TITLE="Orientation map annular parameters") EQ 1) THEN o.OrientMapAzimuthCorr, RADIUS=[s[0].value,s[1].value], DX=s[2].value, DY=s[3].value, ANGWIDTH=s[4].value, MEDIANWIDTH=FIX(s[5].value)
         print, s
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'PC:COM': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:5,label:"Center of mass refinement radius (pix)    ",newrow:0B}
         s.Add, {value:2,label:"Polynomial baseline fit, max. order (0..3)",newrow:1B}
         c=list()
         ;; c.Add, {value:0B,label:"Centre on maximum (no refinement)",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Alignment parameters") EQ 1) THEN o.COM,  COMRADIUS=s[0].value,  FITORDER=s[1].value
         print, s
         ;; print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'PC:DiscShift': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:5,label:"Radius range (pix)    ",newrow:0B}
         s.Add, {value:63,label:"",newrow:0B}
         c=list()
         ;; c.Add, {value:0B,label:"Centre on maximum (no refinement)",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Disc Shift") EQ 1) THEN o.DiscCentre,  RADIUS=[s[0].value,s[1].value]
         ;; print, s
         ;; print, c
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'PC:DPC': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer collection radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0.,label:"Larmor Offset (deg)",newrow:1B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"DPC-x=A-C, DPC-y=B-D instead of DPC-x=(A+D)-(C+B), DPC-y=(A+B)-(C+D)",newrow:0B}
         c.Add, {value:0B,label:"Clockwise Segment Notation",newrow:1B}
         c.Add, {value:1B,label:"Sharp detector masks",newrow:1B}
         c.Add, {value:1B,label:"Show detector masks",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="DPC parameters") EQ 1) THEN o.DPC, RADIUS=[s[0].value,s[1].value], LARMOR=s[2].value, DX=s[3].value, DY=s[4].value, MASK=c[3].value, CLOCKWISE=c[1].value, TYPE=c[0].value, TOPHAT=c[2].value
         print, s
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'TF:FlipXY': BEGIN
     o=GetExtraObj()
     IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
        o.FlipScanXY
        ;; printtocon, "% Menu: Flipping scan XY."
     END
  END

   'TF:PolarToRect': BEGIN
     o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner/Outer radius (pix)",newrow:1B}
         s.Add, {value:63,label:"",newrow:0B}
         s.Add, {value:1.0,label:"Angular increment (degree)",newrow:1B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:-2,label:"",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Fill missing data",newrow:0B}
         c.Add, {value:0B,label:"Project",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Polar to rectangular coordinate transformation") EQ 1) THEN o.PolarToRect, s[3].value, s[4].value, s[1].value, s[2].value, FILL=c[0].value, PROJ=c[1].value, INNER=s[0].value
      END ELSE BEGIN
         printtocon, "% Menu: Current 3D Data is not PAD data."
         printtocon, "%       Trying single image data."
         p=GetCurrentP()
         IF PTR_VALID(p) THEN BEGIN
            q=(*p).datap
            IF PTR_VALID(q) THEN BEGIN
               IF ((*q).SzZ EQ 1) THEN BEGIN
                  ;; Place Marker
                   x0=FIX(((*q).SzX/2)/(*q).binning)
                   y0=FIX(((*q).SzY/2)/(*q).binning)
                   r0=FIX(((*q).SzX/4)/(*q).binning)
                   IF (XCircle(x0,y0,r0) EQ 1) THEN BEGIN
                      dx=FLOOR((x0*1.0*(*q).binning))
                      dy=FLOOR((y0*1.0*(*q).binning))
                      radius=FLOOR(r0*1.0*(*q).binning)
                      angwidth=1.
                      refrectim=PTR_NEW(RotationalImageData(*refp, dx, dy, radius, delta, FILL=1, INNER=0))
                      ThrowImage, *refrectim, title="Pattern"
                  ;;    N=SIZE(rectim)
                  ;;    create a graph object
                  ;;    mygraph=obj_new('XYGraph', 'Radial Scattering Data', XTITLE='q (pix)', YTITLE='Intensity')
                  ;;    register graph in Inspector graph list!
                  ;;    newgraph=GetNewXYGraphContainer("Radial Scattering Data")
                  ;;    IF NOT(PTR_VALID(newgraph)) THEN BEGIN
                  ;;       an error occured, delete the object
                  ;;       ErrMsg, "Could not create graph list item"
                  ;;       mygraph->TidyUp
                  ;;       obj_destroy, mygraph
                  ;;    END ELSE BEGIN
                  ;;       store the object reference in the graph list item
                  ;;       (*newgraph).pobj=mygraph
                  ;;       create a data set, X and Y are arrays
                  ;;       mygraph->AddSet, 'Set 0', INDGEN(N[1]), Total(rectim,2), PSYM=4, SYMSIZE=1, /LINE, COLOR=RGBColor(200,0,0)
                  ;;       mygraph->UpdateDisplay
                  ;;    END
                  ;; END
                  
               END 
            END
         END
      END 
      END
   END 
   'uDiff:fluct': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:12,label:"Inner, outer ring radius (pix)",newrow:0B}
         s.Add, {value:17,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Sharp mask",newrow:1B}
         c.Add, {value:0B,label:"Show detector mask",newrow:0B}
         c.Add, {value:0B,label:"Calculate entropy",newrow:0B}
         IF (XMDataChoiceField(s, c, TITLE="Intensity fluctuation on diffraction ring") EQ 1) THEN BEGIN
            if c[2].value THEN method="entropy" ELSE method="sdev"
            o.FluctuationMap, RADIUS=[s[0].value,s[1].value], DX=s[2].value, DY=s[3].value, MASK=c[1].value, TOPHAT=c[0].value, METHOD=method
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END  
   'uDiff:fluctang': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2,label:"Integration width (degrees)",newrow:0B}
         s.Add, {value:10,label:"Inner, outer ring radius (pix)",newrow:1B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Angular diffraction intensity fluctuation") EQ 1) THEN o.AngularFluctuationImages, DELTA=s[0].value, DX=s[3].value, DY=s[4].value, RADIUS=[s[1].value,s[2].value]
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:fluctangmap': BEGIN
      s=list()
      s.Add, {value:5000,label:"Threshold value",newrow:0B}
      s.Add, {value:5,label:"Median width (pix)",newrow:1B}
      s.Add, {value:0,label:"",newrow:0B}
      c=list()
      IF (XMDataChoiceField(s, c, TITLE="Angular diffraction intensity fluctuation map") EQ 1) THEN AngularMap_To_Index, s[0].value, MEDIANWIDTH=FIX(s[1].value) 
   END
   'uDiff:fluctall': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2,label:"Integration width (pix)",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Sharp mask",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Intensity fluctuation on diffraction rings of all diameters") EQ 1) THEN o.FluctuationImages, DELTA=s[0].value, DX=s[1].value, DY=s[2].value, TOPHAT=c[0].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:acorr': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:10,label:"Inner, outer ring radius (pix)",newrow:0B}
         s.Add, {value:40,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         s.Add, {value:1.0,label:"Angular sampling (deg)",newrow:1B}
         s.Add, {value:3,label:"Median filter width ",newrow:1B}
         s.Add, {value:0.15,label:"Correlation threshold ",newrow:1B}
         c=list()
         c.Add, {value:1B,label:"Twofold only",newrow:1B}
         c.Add, {value:0B,label:"Show Angular Maps",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Azimuthal correlation - Friedel pair analysis") EQ 1) THEN BEGIN
            if (c[0].value EQ 1B) THEN maxsym=2 ELSE maxsym=0
            o.AzimuthCorr, RADIUS=[s[0].value,s[1].value], ANGWIDTH=s[4].value, MEDIANWIDTH=FIX(s[5].value), THRESHOLD=s[6].value, ARG=c[1].value, MAXSYM=maxsym
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:acorr:subset': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2,label:"Symmetry (2,3,..6)",newrow:0B}
         s.Add, {value:0.2,label:"Lower correlation bound",newrow:1B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Extract higher symmetry correlation data") EQ 1) THEN o.GetAzimuthCorrSubset, s[0].value, s[1].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:fluct:subset': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:1000,label:"Lower fluctuation bound",newrow:1B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Extract higher fluctuation data") EQ 1) THEN o.GetFluctuationMapSubset, s[0].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:distill:average': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
          o.Average
       END ELSE BEGIN
          printtocon, "% Menu: Current 3D Data is not PAD data."
          ;; Check if it is a 3D stack
          ProjectVolume, 3
       END
   END
   'uDiff:pickdiff': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2,label:"Inner, outer ring radius (pix)",newrow:0B}
         s.Add, {value:10,label:"",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Preview Image") EQ 1) THEN BEGIN
            o.PickDiff, /CUMULATIVEMASK, INNERR=s[0].value, OUTERR=s[1].value
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:picklinediff': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         o.PickLineDiff
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'uDiff:peaksearch': BEGIN
      
      p=GetCurrentP()
      b=[" Proceed "," Redo "," Cancel "]
      answer="Cancel"
      s=["","Do you want to proceed with the result", "or redo the peak search or cancel the calculation?"]
      REPEAT BEGIN
         framelist=1
         returncom=1
         returnstrongest=1
         f=MyClusterAnalysis3D(/INTERACTIVE,/LIVE,/DISPLAY,/REFINE,FRAMELIST=framelist,RETURNCOM=returncom, REFFILT=1, RETURNSTRONGEST=returnstrongest)
         IF (TYPENAME(f) NE 'LIST') THEN BEGIN
            printtocon, "% Menu: Peak search cancelled."
            return
         END
         if (f.Count() GT 100000) THEN BEGIN
            s[0]="Calculation result is a large number ("+MyString(f.Count())+")."
         END
         answer=XMultiButtonDialog(s,b)
      END UNTIL ((answer EQ " Cancel ") OR (answer EQ " Proceed "))
      
      IF (answer EQ " Proceed ") THEN BEGIN
         mytable=TableImport2DArray(f.ToArray(),TITLE="Peak Data",COLUMNTITLES=["index","refined x","refined y","z","mass","gyration","x","y","refined distance x","refined distance y"])  
         IF OBJ_VALID(mytable) THEN BEGIN
            name="Peak Table"
            newtable=GetNewTableContainer(Name)
            IF NOT(PTR_VALID(newtable)) THEN BEGIN
               ;; an error occured, delete the object
               ErrMsg, "NewTable: Could not create table list item."
               obj_destroy, mytable
            END ELSE BEGIN
               (*newtable).pobj=mytable
            END
         END 
         o=GetExtraObj()
         IF Obj_VALID(o) THEN BEGIN
            o.MapScalarFrameDataToImage, returncom[*,0], LABEL="Int"
            o.MapScalarFrameDataToImage, returncom[*,1], LABEL="ComX"
            o.MapScalarFrameDataToImage, returncom[*,2], LABEL="ComY"
            o.MapScalarFrameDataToImage, returncom[*,3], LABEL="MagCom"
            o.MapScalarFrameDataToImage, returncom[*,4], LABEL="ArgCom"
            o.MapScalarFrameDataToImage, returnstrongest[*,0], LABEL="StrongestPeakInt"
            o.MapScalarFrameDataToImage, returnstrongest[*,1], LABEL="StrongestPeakX"
            o.MapScalarFrameDataToImage, returnstrongest[*,2], LABEL="StrongestPeakY"
            o.MapScalarFrameDataToImage, returnstrongest[*,3], LABEL="StrongestPeakArg"
            B=framelist.ToARRAY(TYPE=3)
            o.Subset, B, /PREVIEW
            multiplicity=2
            Polar24bitImage, REFORM(returnstrongest[*,3],o.GetScanSize(/X),o.GetScanSize(/Y)), REFORM(returnstrongest[*,0],o.GetScanSize(/X),o.GetScanSize(/Y)), /SDEV, MULTIPLICITY=multiplicity
         END ELSE BEGIN
            res=Dialog_Message("Data is not a PAD data set. Convert to PAD data first.", /ERROR)
         END 
      END  
   END   
   'uDiff:peakanalysis:pseudopowderplot':BEGIN
      xsize=512 & ysize=512
      xsamp=1. & ysamp=1.
      thresh=10.
      binsize=0.5
      rotoffs=0.
      eps=0. & theta=0.
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         xsize=o.GetFrameSize(/X)
         ysize=o.GetFrameSize(/Y)
         xsamp=o.GetDetectorSampling(/X)
         ysamp=o.GetDetectorSampling(/Y)
      END
      s=list()
      s.Add, {value:xsize,label:"Detector dimensions x,y (pix): ",newrow:0B}
      s.Add, {value:ysize,label:"",newrow:0B}
      s.Add, {value:xsamp,label:"Detector sampling x,y: ",newrow:1B}
      s.Add, {value:ysamp,label:"",newrow:0B}
      s.Add, {value:thresh,label:"Intensity threshold: ",newrow:1B}
      s.Add, {value:binsize,label:"Binning (fraction of pixel): ",newrow:1B}
      s.Add, {value:rotoffs,label:"Rotation offset (degree): ",newrow:1B}
      s.Add, {value:eps,label:"Distortion eps, theta (degree): ",newrow:1B}
      s.Add, {value:theta,label:"",newrow:0B}
      c=list()
      c.Add, {value:0B,label:"Direct beam at frame center (otherwise: strongest peak)",newrow:1B}
      excl=list()
      excl.Add, {value:1B,pretext:"Distortion Correction:  ",label:"none",newrow:1B}
      excl.Add, {value:0B,label:"elliptical",newrow:0B}
      excl.Add, {value:0B,label:"shear",newrow:0B}
      hlp=["Powder Plot", "", $
           "Create a powder diffraction plot from a list of refined peaks.",$
           "You can run this routine after a Peak Search or after loading a Peak Search",$
           "data table to the Tables tab in the Data Inspector.", $
           "Make sure that the according table is the active table in the Tables tab. ", $
           "If available, select the original PAD data in the 3D Data tab as the active",$
           "data set, this will help identifying the shape of the diffraction frames and ", $
           "their sampling. You'll have to edit these data in case the PAD data is not available.", $
           "All scaling will be in pixel units in case the detector sampling is set to 1.", $
           "The intensity threshold can be used to discriminate weak features.",$
           "Binning refers to the sub-pixel binsize for the intensity histogram.",$
           "The rotation offset can be used to compensate for a rotation in the diffraction data with ", $
           "respect to the image frame. The rotation angle in degrees is positive when the diffraction", $
           "data is rotated counterclockwise with respect against the image.", $
           "",$
           "Three graphs will be produced:",$
           "'1') Event histogram, each peak is represented by one count irrespective of its intensity",$
           "'2') Powder plot, displaying the total diffracted amplitude in a distance bin.",$
           "'3') Polar plot of peaks, each peak is represented by one count. Useful for distortion analysis.",$
           "",$
           "Use the context menu in the XY-Graphs tab to export plots and data.",$
           "", $
           "By default the center of the diffraction frame is refined to the strongest peak. In cases  ", $
           "of strong diffraction it may be necessary to fix the frame center to the peak that is closest to ", $
           "the nominal frame center.", $
           "", $
           "A distortion correction can be applied using the distortion parameters eps and theta. ",$
           "For an elliptical correction an value of eps=0.1 ad theta=0 degrees will stretch the values on the", $
           "horizontal axis by 10 percent. For a shear distortion a percent shear along the horizontal axis", $
                 "will result, the lower left corner moving left, the upper right corner moving right.", $
                 "Shear distortion correction parameters eps=-0.06 and theta=45 deg will contract  the horizontal axis", $
                 "and dilate the vertical axis as a primary effect, similar to an elliptical distortion correction with", $
                 "eps=-0.03 and theta=0 deg. All distortion corrections conserve area and are applied before a rotation.", $
           ""]
      IF (XMDataChoiceField(s, c, EXCL=excl, TITLE="Powder Plot Parameters",HELP=hlp) EQ 1) THEN BEGIN
         ellip=0
         shear=0
         IF (excl[1].value EQ 1) THEN ellip=[s[7].value, !PI*s[8].value/180.] 
         IF (excl[2].value EQ 1) THEN shear=[s[7].value, !PI*s[8].value/180.] 
         ProcessClusterTable, /POWDERPLOT, XSIZE=FIX(s[0].value), YSIZE=FIX(s[1].value), THRESHOLD=s[4].value, HISTBINSIZE=s[5].value, DETSAMPX=s[2].value, DETSAMPY=s[3].value, ROTOFFS=s[6].value*!DPI/180., DYNAMICSCATT=c[0].value, ELLIP=ellip, SHEAR=shear
      END   
   END
   'uDiff:peakanalysis:dsplot':BEGIN
      xsize=512 & ysize=512
      xsamp=1. & ysamp=1.
      thresh=10.
      binsize=0.5
      rotoffs=0.
      eps=0. & theta=0.
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         xsize=o.GetFrameSize(/X)
         ysize=o.GetFrameSize(/Y)
         xsamp=o.GetDetectorSampling(/X)
         ysamp=o.GetDetectorSampling(/Y)
      END
      s=list()
      s.Add, {value:xsize,label:"Detector dimensions x,y (pix): ",newrow:0B}
      s.Add, {value:ysize,label:"",newrow:0B}
      s.Add, {value:xsamp,label:"Detector sampling x,y: ",newrow:1B}
      s.Add, {value:ysamp,label:"",newrow:0B}
      s.Add, {value:thresh,label:"Intensity threshold: ",newrow:1B}
      s.Add, {value:rotoffs,label:"Rotation offset (degree): ",newrow:1B}
      s.Add, {value:eps,label:"Distortion eps, theta (degree): ",newrow:1B}
      s.Add, {value:theta,label:"",newrow:0B}
      c=list()
      c.Add, {value:0B,label:"Direct beam at frame center (otherwise: strongest peak)",newrow:1B}
      excl=list()
      excl.Add, {value:1B,pretext:"Distortion Correction:  ",label:"none",newrow:1B}
      excl.Add, {value:0B,label:"elliptical",newrow:0B}
      excl.Add, {value:0B,label:"shear",newrow:0B}
            hlp=["Debye-Scherrer Plot", "", $
           "Create a Debye-Scherrer powder diffraction plot from a list of refined peaks.",$
           "You can run this routine after a Peak Search or after loading a Peak Search",$
           "data table to the Tables tab in the Data Inspector.", $
           "Make sure that the according table is the active table in the Tables tab. ", $
           "If available, select the original PAD data in the 3D Data tab as the active",$
           "data set, this will help identifying the shape of the diffraction frames and ", $
           "their sampling. You'll have to edit these data in case the PAD data is not available.", $
           "All scaling will be in pixel units in case the detector sampling is set to 1.", $
                 "The intensity threshold can be used to discriminate weak features.",$
                 "The rotation offset can be used to compensate for a rotation in the diffraction data with ", $
                 "respect to the image frame. The rotation angle in degrees is positive when the diffraction", $
                 "data is rotated counterclockwise with respect against the image.", $
                 " ", $
                 "By default the center of the diffraction frame is refined to the strongest peak. In cases  ", $
                 "of strong diffraction it may be necessary to fix the frame center to the peak that is closest to ", $
                 "the nominal frame center.", $
                            "", $
          "A distortion correction can be applied using the distortion parameters eps and theta. ",$
           "For an elliptical correction an value of eps=0.1 ad theta=0 degrees will stretch the values on the", $
           "horizontal axis by 10 percent. For a shear distortion a percent shear along the horizontal axis", $
                 "will result, the lower left corner moving left, the upper right corner moving right.", $
                 "Shear distortion correction parameters eps=-0.06 and theta=45 deg will contract  the horizontal axis", $
                 "and dilate the vertical axis as a primary effect, similar to an elliptical distortion correction with", $
                 "eps=-0.03 and theta=0 deg. All distortion corrections conserve area and are applied before a rotation.", $
           ""]
            IF (XMDataChoiceField(s, c, EXCL=excl, TITLE="Debye Scherrer Plot Parameters", HELP=hlp) EQ 1) THEN BEGIN
               ellip=0
               shear=0
               IF (excl[1].value EQ 1) THEN ellip=[s[6].value, !PI*s[7].value/180.] 
               IF (excl[2].value EQ 1) THEN shear=[s[6].value, !PI*s[7].value/180.] 
               ProcessClusterTable, /DEBYESCHERRER, XSIZE=FIX(s[0].value), YSIZE=FIX(s[1].value), THRESHOLD=s[4].value, DETSAMPX=s[2].value, DETSAMPY=s[3].value, ROTOFFS=s[5].value*!DPI/180., DYNAMICSCATT=c[0].value, ELLIP=ellip, SHEAR=shear
      END   
   END
   'uDiff:peakanalysis:exportjson':BEGIN
      xsize=512 & ysize=512
      xsamp=1. & ysamp=1.
      thresh=10.
      binsize=0.5
      rotoffs=0.
      eps=0.
      theta=0.
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         xsize=o.GetFrameSize(/X)
         ysize=o.GetFrameSize(/Y)
         xsamp=o.GetDetectorSampling(/X)
         ysamp=o.GetDetectorSampling(/Y)
      END
      s=list()
      s.Add, {value:xsize,label:"Detector dimensions x,y (pix): ",newrow:0B}
      s.Add, {value:ysize,label:"",newrow:0B}
      s.Add, {value:xsamp,label:"Detector sampling x,y: ",newrow:1B}
      s.Add, {value:ysamp,label:"",newrow:0B}
      s.Add, {value:thresh,label:"Intensity threshold: ",newrow:1B}
      s.Add, {value:rotoffs,label:"Rotation offset (degree): ",newrow:1B}
      s.Add, {value:eps,label:"Distortion eps, theta (degree): ",newrow:1B}
      s.Add, {value:theta,label:"",newrow:0B}
      c=list()
      c.Add, {value:0B,label:"Direct beam at frame center (otherwise: strongest peak)",newrow:1B}
      excl=list()
      excl.Add, {value:1B,pretext:"Distortion Correction:  ",label:"none",newrow:1B}
      excl.Add, {value:0B,label:"elliptical",newrow:0B}
      excl.Add, {value:0B,label:"shear",newrow:0B}
            hlp=["Peak Data Export ", "", $
           "Create a list of peak locations relative to the central spot from a list of refined peaks.",$
           "You can run this routine after a Peak Search or after loading a Peak Search",$
           "data table to the Tables tab in the Data Inspector.", $
           "Make sure that the according table is the active table in the Tables tab. ", $
           "If available, select the original PAD data in the 3D Data tab as the active",$
           "data set, this will help identifying the shape of the diffraction frames and ", $
           "their sampling. You'll have to edit these data in case the PAD data is not available.", $
           "All scaling will be in pixel units in case the detector sampling is set to 1.", $
                 "The intensity threshold can be used to discriminate weak features.",$
                 "The rotation offset can be used to compensate for a rotation in the diffraction data with ", $
                 "respect to the image frame. The rotation angle in degrees is positive when the diffraction", $
                 "data is rotated counterclockwise with respect against the image.", $
                 "Output format: a JSON hash. Each hash element key is the frame number, the value is a list  ", $
                 "of peak coordinates in the catesion diffraction space, two values for calibrated coordinated" ,$
                 "and two values for non-calibrated values in pixel units.", $
                 "", $
                 "By default the center of the diffraction frame is refined to the strongest peak. In cases  ", $
                 "of strong diffraction it may be necessary to fix the frame center to the peak that is closest to ", $
                 "the nominal frame center.", $
                                             "", $
           "A distortion correction can be applied using the distortion parameters eps and theta. ",$
           "For an elliptical correction an value of eps=0.1 ad theta=0 degrees will stretch the values on the", $
           "horizontal axis by 10 percent. For a shear distortion a percent shear along the horizontal axis", $
                 "will result, the lower left corner moving left, the upper right corner moving right.", $
                 "Shear distortion correction parameters eps=-0.06 and theta=45 deg will contract  the horizontal axis", $
                 "and dilate the vertical axis as a primary effect, similar to an elliptical distortion correction with",$
                 "eps=-0.03 and theta=0 deg. All distortion corrections conserve area and are applied before a rotation.", $
             ""]
            IF (XMDataChoiceField(s, c, EXCL=excl, TITLE="Debye ScherrerPeak Export Parameters", HELP=hlp) EQ 1) THEN BEGIN
               ellip=0
               shear=0
               IF (excl[1].value EQ 1) THEN ellip=[s[6].value, !PI*s[7].value/180.] 
               IF (excl[2].value EQ 1) THEN shear=[s[6].value, !PI*s[7].value/180.] 
         ProcessClusterTable, /BRAGGPEAKS, XSIZE=FIX(s[0].value), YSIZE=FIX(s[1].value), THRESHOLD=s[4].value, DETSAMPX=s[2].value, DETSAMPY=s[3].value, ROTOFFS=s[5].value*!DPI/180., DYNAMICSCATT=c[0].value, ELLIP=ellip, SHEAR=shear
      END   
   END
   'uDiff:peakanalysis:omap':BEGIN
      origp=GetCurrentP()
      IF PTR_VALID(origp) THEN name=(*origp).name ELSE name="table"
      xsize=512 & ysize=512
      scanxsize=128 & scanysize=128
      rangemin=10. & rangemax= 20.
      xsamp=1. & ysamp=1.
      rsxsamp=1. & rsysamp=1.
      thresh=10.
      rotoffs=0.
      binsize=0.5
      eps=0. & theta=0.
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         xsize=o.GetFrameSize(/X)
         ysize=o.GetFrameSize(/Y)
         scanxsize=o.GetScanSize(/X)
         scanysize=o.GetScanSize(/Y)
         xsamp=o.GetDetectorSampling(/X)
         ysamp=o.GetDetectorSampling(/Y)
         rsxsamp=o.GetScanSampling(/X)
         rsysamp=o.GetScanSampling(/Y)
      END
      s=list()
      s.Add, {value:xsize,label:"Detector dimensions x,y (pix): ",newrow:0B}
      s.Add, {value:ysize,label:"",newrow:0B}
      s.Add, {value:xsamp,label:"Detector sampling x,y: ",newrow:1B}
      s.Add, {value:ysamp,label:"",newrow:0B}
      s.Add, {value:thresh,label:"Intensity threshold: ",newrow:1B}
      s.Add, {value:rangemin,label:"Distance range in fractional pix (min, max): ",newrow:1B}
      s.Add, {value:rangemax,label:"",newrow:0B}
      s.Add, {value:scanxsize,label:"Scan size x, y (pix): ",newrow:1B}
      s.Add, {value:scanysize,label:"",newrow:0B}
      s.Add, {value:rotoffs,label:"Rotation offset (degree): ",newrow:1B}
      s.Add, {value:eps,label:"Distortion eps, theta (degree): ",newrow:1B}
      s.Add, {value:theta,label:"",newrow:0B}
      c=list()
      c.Add, {value:1B,label:"Angular values in radians",newrow:1B}
      c.Add, {value:0B,label:"Create subset with matching frames",newrow:1B}
      c.Add, {value:0B,label:"Direct beam at frame center (otherwise: strongest peak)",newrow:1B}
      excl=list()
      excl.Add, {value:1B,pretext:"Distortion Correction:  ",label:"none",newrow:1B}
      excl.Add, {value:0B,label:"elliptical",newrow:0B}
      excl.Add, {value:0B,label:"shear",newrow:0B}
            hlp=["Orientation Map", "", $
                 "Create an orientation map from a list of refined peaks.",$
                 "You can run this routine after a Peak Search or after loading a Peak Search",$
                 "data table to the Tables tab in the Data Inspector.", $
                 "Make sure that the according table is the active table in the Tables tab. ", $
                 "If available, select the original PAD data in the 3D Data tab as the active",$
                 "data set, this will help identifying the shape of the diffraction frames and ", $
                 "their sampling. You'll have to edit these data in case the PAD data is not available.", $
                 "All scaling will be in pixel units in case the detector sampling is set to 1.", $
                 "The intensity threshold can be used to discriminate weak features.",$
                 "The distance range defines the search range for the peak data (lattice spacing).", $
                 "Note that these values are defined in fractional pixel units.", $
                 "The rotation offset can be used to compensate for a rotation in the diffraction data with ", $
                 "respect to the image frame. The rotation angle in degrees is positive when the diffraction", $                         "data is rotated counterclockwise with respect against the image.", $
                 "The output of the orientation angle is by default in radians. Uncheck the checkbox ", $
                 "to return the result in degrees.", $
                 "If the original dataset is active then creating a subset with matching frames will help you",$
                 "locating the spatial origin of peaks.", $
                 "By default the center of the diffraction frame is refined to the strongest peak. In cases  ", $
                 "of strong diffraction it may be necessary to fix the frame center to the peak that is closest to ", $
                 "the nominal frame center.", $
          "A distortion correction can be applied using the distortion parameters eps and theta. ",$
           "For an elliptical correction an value of eps=0.1 ad theta=0 degrees will stretch the values on the", $
           "horizontal axis by 10 percent. For a shear distortion a percent shear along the horizontal axis", $
                 "will result, the lower left corner moving left, the upper right corner moving right.", $
                 "Shear distortion correction parameters eps=-0.06 and theta=45 deg will contract  the horizontal axis,", $
                 "and dilate the vertical axis as a primary effect, similar to an elliptical distortion correction with", $
                 "eps=-0.03 and theta=0 deg. All distortion corrections conserve area and are applied before a rotation.", $
            ""]
            IF (XMDataChoiceField(s, c, EXCL=excl, TITLE="Orientation Map Parameters", HELP=hlp) EQ 1) THEN BEGIN
               ellip=0
               shear=0
               IF (excl[1].value EQ 1) THEN ellip=[s[10].value, !PI*s[11].value/180.] 
               IF (excl[2].value EQ 1) THEN shear=[s[10].value, !PI*s[11].value/180.] 
              maporient=1
              selection=c[1].value
              ProcessClusterTable, MAPORIENT=maporient, XSIZE=FIX(s[0].value), YSIZE=FIX(s[1].value), THRESHOLD=s[4].value, DETSAMPX=s[2].value, DETSAMPY=s[3].value,  DISTRANGE=[s[5].value,s[6].value], ROTOFFS=s[9].value*!DPI/180., SUBSET=selection, DYNAMICSCATT=c[1].value, ELLIP=ellip, SHEAR=shear
              ;; process maporient
              img=FLTARR(s[7].value,s[8].value)
              indices=LONG(maporient[*,0])
              img[indices]=maporient[*,1]
              name="Spacings("+name+")"
              ThrowImage, img, TITLE=name, SAMP=[rsxsamp,rsysamp,1.], UNIT=['nm','nm','']
              img[indices]=maporient[*,3]
              name="Int("+name+")"
              ThrowImage, img, TITLE=name, SAMP=[rsxsamp,rsysamp,1.], UNIT=['nm','nm','']
              img[indices]=maporient[*,2]
              IF (c[0].value EQ 1B) THEN img=img*!DPI/180.
              name="Arg("+name+")"
              ThrowImage, img, TITLE=name, SAMP=[rsxsamp,rsysamp,1.], UNIT=['nm','nm','']
              IF (c[1].value EQ 1B) THEN BEGIN
                 IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
                    o->Subset, selection, /PREVIEW
                 END 
              END 
      END   
   END
   'Series:slice': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:0,label:"Range selection - first, last: ",newrow:0B}
         s.Add, {value:10,label:"",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Range selection") EQ 1) THEN o.CropSeries, [s[0].value,s[1].value]
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Strain:Map': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:4,label:" Upsampling ",newrow:0B}
         s.Add, {value:3,label:" SBR threshold (%)",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Strain Mapping parameters") EQ 1) THEN BEGIN
            upsampling=FIX(s[0].value)
            IF (upsampling LT 1) THEN BEGIN
               upsampling=1
               ErrMsg, "Upsampling: The value has to be larger or equal to 1."
               return
            END
            threshold=s[1].value
            IF ((threshold LT 0.1) OR (threshold GT 99)) THEN BEGIN
               ErrMsg, "Threshold: The value has to be larger than 0.1 % and smaller than 99 %"
               return  
            END
            StrainEvaluation, THRESHOLD=threshold/100., UPSAMPLING=upsampling
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Proc:CalibDB': BEGIN
      mycal=GetCalibrationObj()
      mycal->UI
      s=mycal->ReportSelectedMode()
      printtocon, "% Calibration DB, active set:"
      foreach subs,s DO printtocon, "% "+subs
   END

   'Proc:CalibData': BEGIN
      CalibData
   END  
   
   'Proc:SpatialAvg': BEGIN
      o=GetExtraObj()
      hlp=["Running spatial average or sum","",$
           "Calculate the spatial average over a square region of neighbouring points.",$
           "The side length of the square region has an odd number of pixels, 2*halfwidth+1,",$
           "e.g. in total 9 pixels will fall into the averaging frame for halfwidth=1.",$
           "Summing instead of averaging will avoid rounding errors in integer arithmetics,",$
           "but it will *not* preserve the total power.",$
          ""]
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:1,label:"Halfwidth (pix): ",newrow:0B}
         c=list()
         c.Add, {value:0B,label:"Summing instead of averaging",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Range selection",Help=hlp) EQ 1) THEN o.SpatialAverage, s[0].value, SUMUP=c[0].value
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Proc:ProcessFramesScalar': BEGIN
      ScalarFrameTransformation
   END 
   'Proc:ProcessFramesExpr': BEGIN
      hlp=["Expression-based frame transformation","",$
           "Apply a formula to each pixel in a frame of the data set.",$
           "Examples: f(x)= x*32. will multiply all diffraction intensities by 32.,",$
           "",$
           "",$
          ""]
         s=list()
         s.Add, {value:"x+1",label:"f(x) = ",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Apply formula to frames",Help=hlp) EQ 1) THEN FormulaOperation, s[0].value
      END 
   'Proc:SpatialBin': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2,label:"Binning width (pix): ",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c, TITLE="Binning Parameters") EQ 1) THEN BEGIN
            ;; SpatialBinning, o, s[0].value
            SpatialBinningSimplified, o, s[0].value
            ;; STOP
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Proc:HalfRotAdd': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
             o.HalfRotAdd
            ;; STOP
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Proc:FrameUpScale': BEGIN
      origp=GetCurrentP()
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         s=list()
         s.Add, {value:2.,label:"Scaling factor x, y: ",newrow:0B}
         s.Add, {value:2.,label:" ",newrow:0B}
         c=list()
         c.Add, {value:1B,label:"Use Cubic Convolution  Interpolation",newrow:1B}
         IF (XMDataChoiceField(s, c, TITLE="Scaling factors") EQ 1) THEN BEGIN
            o.UpScale, s[0].value, s[1].value, P=origp, INTERP=c[0].value
            d=(*origp).datap
            IF (PTR_VALID(d)) THEN BEGIN
               fact=(*d).binning
               CreateWindow, BIN=fact
               TVDisplay
               Update_XTabControlInfoField
            END
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Proc:SubAvg': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
      s=list()
      s.Add, {value:0,label:"Center Mask Radius (pix): ",newrow:0B}
      c=list()
      IF (XMDataChoiceField(s, c, TITLE="Binning Parameters") EQ 1) THEN BEGIN
               o.SubtractAvg, CENTERMASKRADIUS=s[0].value
      END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."
   END
   'Proc:TypeCast': BEGIN
      TypeCast
   END
   'Proc:MorphTF': BEGIN
      MorphologicalTransform
   END
   'Proc:FilterNAN': BEGIN
      FilterNAN
   END
 'Proc:Negative': BEGIN
      ContrastChange, /NEGATIVE
   END
   'Proc:Inverse': BEGIN
      ContrastChange, /INVERT
   END
   'Proc:Log': BEGIN
      ContrastChange, /LOG
   END
   'Proc:MaxToZero': BEGIN
      ContrastChange, /MAXTOZERO
   END
   'Proc:MinMax': BEGIN
      ContrastChange, /MINMAX
   END
   'Proc:ContrastReplace': BEGIN
      ContrastReplace, /MEANFORZERO
   END
   'Proc:SubtractMean': BEGIN
      NormalizeVol, MODE='subtractmean'
   END
   'Proc:DividebyMean': BEGIN
      NormalizeVol, MODE='dividebymean'
   END
   'Proc:DivideByStddev': BEGIN
      NormalizeVol, MODE='dividebystddev'
   END
   'Proc:ROINormalize': BEGIN
      ROINormalize
   END
;;   'Proc:NormIntToRef': BEGIN
;;      NormalizeIntToRef, /ALIGN
;;   END
   'Proc:Rotate': BEGIN
      RotateVol
   END
;   'Proc:QuickRotate': BEGIN
;      QuickRotateVol, 20
;   END
   'Proc:Translate': BEGIN
      TranslateVol
   END
   'Proc:Scale': BEGIN
      ScaleVol
   END
   'Proc:Binning': BEGIN
      BinVol, /MENU
   END
   'Proc:CutVolume': BEGIN
      CutVolume
   END
   'Proc:CutVolume2D': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         dimx=o->GetFrameSize(/X)
         dimy=o->GetFrameSize(/Y)
         x0=FLOOR(dimx/4) & x1=x0+FLOOR(dimx/2)
         y0=FLOOR(dimy/4) & y1=y0+FLOOR(dimy/2)
         ;; create a BF image for the cropping
         s=list()
         s.Add, {value:5,label:"Radius (pix)",newrow:0B}
         s.Add, {value:0,label:"Off-center shift dx, dy (pix)",newrow:1B}
         s.Add, {value:0,label:"",newrow:0B}
         c=list()
         IF (XMDataChoiceField(s, c,TITLE="Bright-field parameters") EQ 1) THEN o.BF, RADIUS=s[0].value, DX=s[1].value, DY=s[2].value
         p=GetCurrentP()
         pdata=(*p).datap
         result=XBox(x0, y0, x1, y1, bin=(*pdata).binning)
         IF (result NE 1) THEN BEGIN
            return  
         END
         roi=[x0,x1,y0,y1]
         ;; roi=FIX(roi*(*pdata).binning)
         o.CropToSubset, roi
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."     
   END
   'Proc:CropFrames': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         p=GetCurrentP()
         origp=p
         pdata=(*p).datap
         dimx=o->GetFrameSize(/X)
         dimy=o->GetFrameSize(/Y)
         x0=FLOOR(dimx/4) & x1=x0+FLOOR(dimx/2)
         y0=FLOOR(dimy/4) & y1=y0+FLOOR(dimy/2)
         result=XBox(x0, y0, x1, y1, bin=(*pdata).binning)
         IF (result NE 1) THEN BEGIN
            return  
         END
         ;; roi=FIX(roi*(*pdata).binning)
         o.CropFrames, x0, x1-x0+1, y0, y1-y0+1, P=origp
         d=(*origp).datap
         IF (PTR_VALID(d)) THEN BEGIN
            fact=(*d).binning
            CreateWindow, BIN=fact
            TVDisplay
            Update_XTabControlInfoField
         END
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."     
   END 
    'Proc:CutVolume2DMask': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         ;; choose mask image
         maskp=XListSelector(TITLE="Choose Mask Data")
         IF PTR_VALID(maskp) THEN maskp=(*maskp).datap
         IF PTR_VALID(maskp) THEN maskp=(*maskp).data
         IF PTR_VALID(maskp) THEN o.SubsetFromBinaryMask, maskp  ELSE printtocon, "% Menu: Invalid mask data pointer."
      END ELSE printtocon, "% Menu: Current 3D Data is not PAD data."     
   END
    'Proc:CreateBinaryMask': BEGIN
       CreateBinaryMask
    END
   'Proc:Debug': BEGIN
      SetDebugFlag, 1
      PrintToCon, "% Debugging is switched on." 
   END
   'Proc:NoDebug': BEGIN
      SetDebugFlag, 0
      PrintToCon, "% Debugging is switched off." 
   END
   'Proc:StartProfiler': BEGIN
      Start_Profiler
      PrintToCon, "% Profiler started." 
   END
   'Proc:StopProfiler': BEGIN
      Stop_Profiler
      PrintToCon, "% Profiler stopped." 
   END
   'Proc:IDLBackingstoreOn': BEGIN
      ToggleIDLBackingStore, /ON
      PrintToCon, "% IDL backingstore is switched on." 
   END
   'Proc:IDLBackingstoreOff': BEGIN
      ToggleIDLBackingStore, /OFF
      PrintToCon, "% IDL backingstore is switched off." 
   END
   'Proc:ROISelection': BEGIN
      ROISelection

   END
   'FixDiffractionRotationStack': BEGIN
      FixEMPADDiffRot
   END
   'FixDiffractionRotationFrame': BEGIN
      CORRECTDIFFORIENTATION, /EMPAD
   END
   'FixTestRows': BEGIN
      o=GetExtraObj()
      IF ((OBJ_VALID(o) AND (TypeName(o) EQ "EMPADOBJ"))) THEN BEGIN
         dimx=o->GetFrameSize(/X)
         dimy=o->GetFrameSize(/Y)
         IF ((dimx EQ 128) AND (dimy EQ 130)) THEN BEGIN
            nrows=2
            o.MaskFrameRows, nrows, /TOP
         END
      END ELSE BEGIN
         PrintToCon, '% Not an EMPAD dataset.'
      END
   END
   'Proc:MeasurePeaks':BEGIN
      MeasurePeaks, /PEAKTOPEAK
   END
   'Proc:MarkerCircle': BEGIN
      ptr=GetRootP()
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: No 3D Data found."
         return
      END
      ptr=(*ptr).current
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: Current 3D Data invalid."
         return
      END
      ptr=(*ptr).datap
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END
      print, XCircle(FIX(((*ptr).SzX/2)/(*ptr).binning), FIX(((*ptr).SzY/2)/(*ptr).binning), FIX(((*ptr).SzX/4)/(*ptr).binning), bin=(*ptr).binning)
   END
   'Proc:MarkerPoint': BEGIN
      ptr=GetRootP()
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: No 3D Data found."
         return
      END
      ptr=(*ptr).current
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: Current 3D Data invalid."
         return
      END
      ptr=(*ptr).datap
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END
      print, XPoint(FIX(((*ptr).SzX/2)/(*ptr).binning), FIX(((*ptr).SzY/2)/(*ptr).binning), bin=(*ptr).binning)
   END 
   'Proc:FilterGUI': BEGIN
      dummy=FilterGUI3D()
   END
   'Vis:OrientationMapColoring': BEGIN
      OrientationMapColoring
   END
   'Vis:ColorWheel': BEGIN
      s=list()
      s.Add, {value:1,label:"Multiplicity (1,2,3,..6,..)",newrow:0B}
      c=list()
      c.Add, {value:1B,label:"Add Circular Mask",newrow:1B}
      c.Add, {value:0B,label:"Use Current Colortable",newrow:1B}
      c.Add, {value:1B,label:"Apply Chroma Ramp",newrow:1B}
      IF (XMDataChoiceField(s, c, TITLE="Angular Map Parameters") EQ 1) THEN RGBColorCircle,  MULTIPLICITY=s[0].value, MASK=c[0].value, FROMCT=c[1].value, NOCHROMA=(c[2].value EQ 0B)
   END
   'Proc:MarkerPoint': BEGIN
      ptr=GetRootP()
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: No 3D Data found."
         return
      END
      ptr=(*ptr).current
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: Current 3D Data invalid."
         return
      END
      ptr=(*ptr).datap
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END
      print, XCircle(FIX(((*ptr).SzX/2)/(*ptr).binning), FIX(((*ptr).SzY/2)/(*ptr).binning), FIX(((*ptr).SzX/4)/(*ptr).binning), bin=(*ptr).binning)
   END
   'Vis:iImage': BEGIN
      ptr=GetRootP()
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: No 3D Data found."
         return
      END
      ptr=(*ptr).current
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "% Menu: Current 3D Data invalid."
         return
      END
      ptr=(*ptr).datap
      IF NOT(PTR_VALID(ptr)) THEN BEGIN
         printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END
      d=(*ptr).data
      IF NOT(PTR_VALID(d)) THEN BEGIN
         printtocon, "%  Menu: Current 3D Data holds no data."
         return
      END ELSE iImage, *d
   END
   'Macros:Purge': PurgeAll
   'Macros:ADFBF': MACROADFBF, /PURGE
   'Macros:PeakDetection': MacroPeakDetection, /PURGE
   'Macros:DirADFBFPickDiff': MACROADFBF_PickDiff, /PURGE
   'Macros:uDiffADF': MacroADFBF
   'Macros:uDiffROI': MACRO_ROI_DIFF
   'Macros:QuadDodec': MACRO_QUADDODEC, /AUTO
   'Macros:QuadOctet': MACRO_QUADOCTET, /AUTO
   'Macros:LoadMultiple': LoadMultipleFiles
   'Macros:MergeMultiple': MergeData
   'Macros:PrepMagCLTable': BEGIN
      l=ProcessingDirList()
      ll=MagCorrDataTable(l)
   END
   'Win:DataInspector': BEGIN
     IF NOT(XREGISTERED('XTabControl')) THEN XTabControl
   END
   'Win:ContrastInspector': BEGIN
     IF NOT(XREGISTERED('XTabContrastDialog')) THEN XTabContrastDialog
   END
   'Win:Console': BEGIN
     IF NOT(XREGISTERED('XConsole')) THEN XConsole
   END
;;   'Win:Contrast': BEGIN
;;     IF NOT(XREGISTERED('XContrast')) THEN XContrast
;;   END
;;   'File:WorkingDir': BEGIN
;;              dummy=XSetWorkingDir()
;;       END
;;   'File:Preferences': BEGIN
;;              XPreferences
;;       END
   'file:quit': BEGIN
          ;; IF (CloseDownProc() EQ 1) THEN BEGIN
           WIDGET_CONTROL, ev.top, /DESTROY
           ;; print, "% memory information: "
           ;; HELP, /MEMORY
           ;; print, "% CloseDown: heap variables still in use: "
           ;; HELP, /HEAP_VARIABLES
           ptr=GetRootP()
           IF (NOT(DataList_Destroy(ptr))) THEN BEGIN
              print, "% XMenu: error"
              print, "          some 3D data structures are not deleted"
           END
           ptr2D=GetRootP2D()
           IF (NOT(DataList_Destroy(ptr2D))) THEN BEGIN
              print, "% XMenu: error"
              print, "          some 2D data structures are not deleted"
           END
           ptrXYGraph=GetRootPXYGraph()
           IF Not(XYGraphList_Destroy(ptrXYGraph)) THEN BEGIN
              print, "% XMenu: error"
              print, "          some XY graph structures are not deleted"
           END 
           ptrTable=GetRootPTable()
           IF Not(XYGraphList_Destroy(ptrTable)) THEN BEGIN
              print, "% XMenu: error"
              print, "          some table structures are not deleted"
           END 
           SavePrefs
           DestroyDlgParObject
           obj_destroy, GetCalibrationObj()
           exit
        END
   'Util:Debug': BEGIN
      IF GetDebugFlag() THEN BEGIN
         SetDebugFlag, 0
         printtocon, "% Debug mode is off."
      END ELSE BEGIN
         SetDebugFlag, 1
         printtocon, "% Debug mode is on."
      END
   END
   'Util:GraphicsMode': BEGIN
      SetGraphicsMode
   END
   'Util:Color': BEGIN
       ctable='no'
       DEVICE, Get_Visual_Depth=vdepth
       if (vdepth EQ 8) then ctable='yes'
       if (vdepth GT 8) THEN BEGIN
           DEVICE, Get_Decomposed=decomp
           if (decomp GT 0) THEN BEGIN 
               IF (Dialog_Info("Use color table instead of decomposed rgb color indices?", /QUESTION) EQ " Yes ") THEN BEGIN
                   DEVICE, Decomposed=0
                   ctable='yes'
               END
           END ELSE ctable='yes'
       END
       if (ctable EQ 'yes') THEN XColors
   END
   'Util:Shell': BEGIN
          ;; 
          PRINT, "program is stopped now"
          PRINT, "type .c once to continue"
          PRINT, "the current image pointer is labelled p"
          PRINT, "the image is dereferenced bye (*(*p).im)"
          STOP
       END

   'Proc:ComplexData': BEGIN
      ComplexDataOp, /MENU
   END
   ELSE: BEGIN
          print, "% MM_event: feature "+MyString(uv)+" not yet implemented"
         END
 ENDCASE
END

Pro XMenu, DEVELOPER=developer
s= GetApplicationName() + " "+GetVersion()
base = WIDGET_BASE(TITLE = s, /COLUMN, MBAR=bar, XSIZE=500, YSize=0)
;;
;; logo=WIDGET_BUTTON(bar, VALUE='logo128.bmp', /BITMAP)
file_menu = WIDGET_BUTTON(bar, VALUE='File', /MENU)
file_open = WIDGET_BUTTON(file_menu, VALUE=' Open ', UVALUE='file:open')
file_importmenu = WIDGET_BUTTON(file_menu, VALUE=' Import Data', /MENU)
file_importlarge = WIDGET_BUTTON(file_importmenu, VALUE='Import Large Set', UVALUE='file:importlargeset')
;;file_save = WIDGET_BUTTON(file_menu, VALUE=' Save ', UVALUE='file:save_nonint')
file_save = WIDGET_BUTTON(file_menu, VALUE=' Save ', UVALUE='file:save')
file_save = WIDGET_BUTTON(file_menu, VALUE=' Export Display ', UVALUE='file:exportdisplay')
file_save = WIDGET_BUTTON(file_menu, VALUE=' Save and Export Display', UVALUE='file:saveexp')
file_about = WIDGET_BUTTON(file_menu, VALUE=' About ', UVALUE='File:About')
file_about = WIDGET_BUTTON(file_menu, VALUE=' News ', UVALUE='File:News')
file_prefmenu=WIDGET_BUTTON(file_Menu, VALUE=' Settings  ', /MENU)
file_license = WIDGET_BUTTON(file_prefmenu, VALUE=' License ', UVALUE='File:License')
file_ddir = WIDGET_BUTTON(file_prefmenu, VALUE=' Desktop Directory ', UVALUE='File:Ddir')
file_quit = WIDGET_BUTTON(file_menu, VALUE=' Quit ', UVALUE='file:quit')

Proc_menu = WIDGET_BUTTON(bar, VALUE='Transformations', /MENU)



Corr_menu = WIDGET_BUTTON(Proc_menu, VALUE='Corrections', /MENU)
corr_ds = WIDGET_BUTTON(Corr_menu, VALUE=' NBED Diffraction pattern shift ', UVALUE='corr:diffshift')
corr_ds = WIDGET_BUTTON(Corr_menu, VALUE=' Pick Center ', UVALUE='corr:simplediffshift')
corr_ds = WIDGET_BUTTON(Corr_menu, VALUE=' Fit Ronchigram shift ', UVALUE='corr:ronchishift')
corr_ds = WIDGET_BUTTON(Corr_menu, VALUE=' Beamstop ', UVALUE='corr:beamstop')
corr_ds = WIDGET_BUTTON(Corr_menu, VALUE=' Hot Pixel ', UVALUE='corr:hotpix')
corr_ds = WIDGET_BUTTON(Corr_menu, VALUE=' Dectris maxuint16 ', UVALUE='corr:fixdectrispix')
corr_thresh = WIDGET_BUTTON(Corr_menu, VALUE=' Threshold Cut-Off ', UVALUE='corr:threshold')
corr_thresh = WIDGET_BUTTON(Corr_menu, VALUE=' Subtract Rotational Average ', UVALUE='corr:subrotav')
corr_mskrows = WIDGET_BUTTON(Corr_menu, VALUE=' Mask Margin Rows ', UVALUE='corr:maskrows')
corr_mskcols = WIDGET_BUTTON(Corr_menu, VALUE=' Mask Margin Columns ', UVALUE='corr:maskcols')
CoordTF_menu = WIDGET_BUTTON(Proc_menu, VALUE='Coordinate Transformation', /MENU)
PolarToRect_ = WIDGET_BUTTON(CoordTF_menu, VALUE='Polar to Cartesian', UValue='TF:PolarToRect')
RadProf_ = WIDGET_BUTTON(CoordTF_menu, VALUE='Radial Profile', UValue='Transf:radialprofile')
;;Binning_ = WIDGET_BUTTON(VolumeTF_menu, VALUE='Binning (YZ)', UValue='Proc:Binning')
;;Scale_ = WIDGET_BUTTON(VolumeTF_menu, VALUE='Scale', UValue='Proc:Scale')
;;MirrorVol = WIDGET_BUTTON(VolumeTF_menu, VALUE='Mirror', UVALUE='Proc:MirrorAxis')
;;FlipVol = WIDGET_BUTTON(VolumeTF_menu, VALUE='Flip Axes', UVALUE='Proc:FlipAxes')
;;
CropMenu = WIDGET_BUTTON(Proc_menu, VALUE='Volume Transformations', /MENU)
;;CutVol = WIDGET_BUTTON(VolumeTF_cutmenu, VALUE='3D Crop',
;;UVALUE='Proc:CutVolume')
Reshape = WIDGET_BUTTON(CropMenu, VALUE='Reshape', UVALUE='Transf:reshape')
CutVol = WIDGET_BUTTON(CropMenu, VALUE='Spatial Rectangular Crop', UVALUE='Proc:CutVolume2D')
CutVol = WIDGET_BUTTON(CropMenu, VALUE='Spatial Binary Mask Crop', UVALUE='Proc:CutVolume2DMask')
AvgVol = WIDGET_BUTTON(Cropmenu, VALUE='Spatial Average', UVALUE='Proc:SpatialAvg')
AvgVol = WIDGET_BUTTON(Cropmenu, VALUE='Spatial Binning', UVALUE='Proc:SpatialBin')
AvgVol = WIDGET_BUTTON(Cropmenu, VALUE='Sum Pi-rotated Diffraction Frames', UVALUE='Proc:HalfRotAdd',/SEPARATOR)
AvgVol = WIDGET_BUTTON(Cropmenu, VALUE='Scale Diffraction Frames', UVALUE='Proc:FrameUpScale',/SEPARATOR)
AvgVol = WIDGET_BUTTON(Cropmenu, VALUE='Crop Diffraction Frames', UVALUE='Proc:CropFrames')
MaskCreationMenu = WIDGET_BUTTON(Proc_menu, VALUE='Mask or Filter', /MENU)
BinMask = WIDGET_BUTTON(MaskCreationMenu, VALUE='Create Binary Mask from Image', UVALUE='Proc:CreateBinaryMask')
uDiff_acorr = WIDGET_BUTTON(Proc_menu, VALUE='Average Diffraction Frames ', UVALUE='uDiff:distill:average')
;;RotProj = WIDGET_BUTTON(VolumeTF_cutmenu, VALUE='Cut at Distance', UVALUE='Proc:CutAtDistance')
;;RotProj = WIDGET_BUTTON(VolumeTF_cutmenu, VALUE='Cut Horizon', UVALUE='Proc:CutHorizon')
;;
;;PaddVol = WIDGET_BUTTON(VolumeTF_menu, VALUE='Padd', UVALUE='Proc:PaddVolume')
;;RotProj = WIDGET_BUTTON(VolumeTF_menu, VALUE='Shift', UVALUE='Proc:Translate')
;;VolumeTF_rotmenu = WIDGET_BUTTON(VolumeTF_menu, VALUE='Rotate', /MENU)
;;RotProj = WIDGET_BUTTON(VolumeTF_rotmenu, VALUE='Rotate', UVALUE='Proc:Rotate')
;;RotProj = WIDGET_BUTTON(VolumeTF_rotmenu, VALUE='Triple Shift Rotate', UVALUE='Proc:QuickRotate')
;;TFfromFile = WIDGET_BUTTON(VolumeTF_menu, VALUE='Shift, Scale, Rotate, Normalize by File Data', UVALUE='Proc:TFfromFile')
;;
;;SlcManip_menu = WIDGET_BUTTON(Proc_menu, VALUE='Slice Manipulation', /MENU)
;;DeleteSlc = WIDGET_BUTTON(SlcManip_menu, VALUE='Delete Theta Slices', UValue='Proc:DelSlc')
;;ExtractSlc = WIDGET_BUTTON(SlcManip_menu, VALUE='Extract Slice', UValue='Proc:ExtractSlc')


;;ProjTF_menu = WIDGET_BUTTON(Proc_menu, VALUE='Projections', /MENU)
;;RotProj = WIDGET_BUTTON(ProjTF_menu, VALUE='Rotate and Project', UVALUE='Proc:RotateProject')
;;RadProj = WIDGET_BUTTON(ProjTF_menu, VALUE='Radon Transform Projection', UVALUE='Proc:RadonProjection')
Expr_menu = WIDGET_BUTTON(Proc_menu, VALUE='Expression-based Transformations', /MENU)
Expr_unary = WIDGET_BUTTON(Expr_menu, VALUE='Scalar Frame Operation', UValue='Proc:ProcessFramesScalar')
Expr_lambda = WIDGET_BUTTON(Expr_menu, VALUE='Frame Transformation by Formula', UValue='Proc:ProcessFramesExpr')
Normalize_menu = WIDGET_BUTTON(Proc_menu, VALUE='Normalization  ', /MENU)
Normalize_smean = WIDGET_BUTTON(Normalize_menu, VALUE='Subtract 3D Mean Value', UValue='Proc:SubtractMean')
Normalize_dmean = WIDGET_BUTTON(Normalize_menu, VALUE='Divide by 3D Mean Value', UValue='Proc:DivideByMean')
Normalize_dmean = WIDGET_BUTTON(Normalize_menu, VALUE='Divide by 3D Standard Deviation', UValue='Proc:DivideByStddev')
Normalize_dmean = WIDGET_BUTTON(Normalize_menu, VALUE='ROI Normalize', UValue='Proc:ROINormalize')
;;Normalize_dmean = WIDGET_BUTTON(Normalize_menu, VALUE='Normalize to Reference', UValue='Proc:NormIntToRef')
Contrast_menu = WIDGET_BUTTON(Proc_menu, VALUE='Data Transformation  ', /MENU)
Contrast_Negative = WIDGET_BUTTON(Contrast_menu, VALUE='Negative', UValue='Proc:Negative')
Contrast_MinMax = WIDGET_BUTTON(Contrast_menu, VALUE='Min to Max Cut-off', UValue='Proc:MinMax')
Contrast_MaxToZero = WIDGET_BUTTON(Contrast_menu, VALUE='Zero to Maximum Cut-off', UValue='Proc:MaxToZero')
Contrast_Inverse = WIDGET_BUTTON(Contrast_menu, VALUE='Inverse', UValue='Proc:Inverse')
Contrast_Inverse = WIDGET_BUTTON(Contrast_menu, VALUE='Logarithmic', UValue='Proc:Log')
Contrast_SubtractAvg = WIDGET_BUTTON(Contrast_menu, VALUE='Subtract Average Frame', UValue='Proc:SubAvg')
;;Contrast_Replace = WIDGET_BUTTON(Contrast_menu, VALUE='Replace zeroes with slice mean', UValue='Proc:ContrastReplace')
;;
;;Artefact_menu = WIDGET_BUTTON(Proc_menu, VALUE='Artefact Correction', /MENU)
;;Artefact_finite = WIDGET_BUTTON(Artefact_menu, VALUE='Shear Correction', UValue='Proc:ShearCorrection')
;;Artefact_finite = WIDGET_BUTTON(Artefact_menu, VALUE='Sampling Correction', UValue='Proc:SamplingCorrection')
;;Artefact_finite = WIDGET_BUTTON(Artefact_menu, VALUE='Vertical Magnification Correction', UValue='Proc:MagYCorrection')
;;Artefact_finite = WIDGET_BUTTON(Artefact_menu, VALUE='Eliminate NAN and Infinity', UValue='Proc:FilterNAN')
;;FFTTF_menu = WIDGET_BUTTON(Proc_menu, VALUE='Fast Fourier Transform', /MENU)
;;FFT = WIDGET_BUTTON(FFTTF_menu, VALUE='3D Fast Fourier Transform', UVALUE='Proc:FFT')
;;
;;CMTF_menu = WIDGET_BUTTON(Proc_menu, VALUE='Mask Operations', /MENU)
;;MaskStack = WIDGET_BUTTON(CMTF_menu, VALUE='Step Mask', UVALUE='Proc:MaskStack')
;;HanningStack = WIDGET_BUTTON(CMTF_menu, VALUE='Hanning Window', UVALUE='Proc:Hanning')
;;HanningStack = WIDGET_BUTTON(CMTF_menu, VALUE='Smoothed Apodization', UVALUE='Proc:Apodization')

;; FilterTF_menu = WIDGET_BUTTON(Proc_menu, VALUE='Filters', /MENU)
;;RotProj = WIDGET_BUTTON(FilterTF_menu, VALUE='1D Ramp Filter', UVALUE='Proc:1DRampFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_menu, VALUE='2D Ramp Filter', UVALUE='Proc:RampFilter')
;; RotProj = WIDGET_BUTTON(FilterTF_menu, VALUE='2D Bandpass Filter', UVALUE='Proc:BPFilter')

;;FilterTF_more = WIDGET_BUTTON(FilterTF_menu, VALUE='More Filters', /MENU)

;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='FFT Butterworth Lowpass Filter', UVALUE='Proc:BWLowPassFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='FFT Butterworth Highpass Filter', UVALUE='Proc:BWHighPassFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='FFT Butterworth Bandpass Filter', UVALUE='Proc:BWBandPassFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='FFT Gaussian Bandpass Filter', UVALUE='Proc:GaussianBandPassFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='FFT Ideal Bandpass Filter', UVALUE='Proc:IdealBandPassFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='Convolution Highpass Filter', UVALUE='Proc:HiFilter', /SEPARATOR) ;
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='Convolution Lowpass Filter', UVALUE='Proc:LoFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='Convolution Laplace Filter', UVALUE='Proc:LaplaceFilter')
;;RotProj = WIDGET_BUTTON(FilterTF_more, VALUE='Binarize Intensity', UVALUE='Proc:Binarize', /SEPARATOR)
;;
;;StackArID = WIDGET_BUTTON(Proc_menu, VALUE='Stack Transformations', UValue='Proc:StackAr', /MENU)
;;TypeCastID = WIDGET_BUTTON(StackArID, VALUE='Scalar Arithmetics', UValue='Proc:ScalarTransform')
;;BinStackArID = WIDGET_BUTTON(StackArID, VALUE='Binary Stack Arithmetics', UValue='Proc:BinStackAr')
;;ScaleLineSpc = WIDGET_BUTTON(StackArID, VALUE='Scale Line Spacing', UValue='Proc:LineSpac')
;;ScaleLineSpcTietz = WIDGET_BUTTON(StackArID, VALUE='Scale Line Spacing (Tietz Schemes)', UValue='Proc:LineSpacTietz')

;;
TypeCastID = WIDGET_BUTTON(Proc_menu, VALUE='Type Cast', UValue='Proc:TypeCast')
;;MorphTFID = WIDGET_BUTTON(Proc_menu, VALUE='Morphological Transformation', UValue='Proc:MorphTF')
;;CoordTFID = WIDGET_BUTTON(Proc_menu, VALUE='Coordinate Transformation', UValue='Proc:CoordTF', /MENU)
;;CartToPolarProj = WIDGET_BUTTON(CoordTFID, VALUE='Cartesian to Polar', UVALUE='Proc:CartToPolar')
;;LogPolarProj = WIDGET_BUTTON(CoordTFID, VALUE='Cartesian to Logarithm Polar', UVALUE='Proc:LogPolar')
;;ExpPolarProj = WIDGET_BUTTON(CoordTFID, VALUE='Cartesian to Exponential Polar', UVALUE='Proc:ExpPolar')
;;VertDistProj = WIDGET_BUTTON(CoordTFID, VALUE='Vertical Distance Map', UVALUE='Proc:VertDist',/SEPARATOR)
;;ExpVertDistProj = WIDGET_BUTTON(CoordTFID, VALUE='Vertical Exponential Distance Map', UVALUE='Proc:ExpVertDist')
;;ExpVertDistProj = WIDGET_BUTTON(CoordTFID, VALUE='Vertical Logarithmic Distance Map', UVALUE='Proc:LogVertDist')
;;
;;ComplexID = WIDGET_BUTTON(Proc_menu, VALUE='Complex Data Operations', UValue='Proc:ComplexData')
;;

;;
Simulation_menu = WIDGET_BUTTON(Proc_menu, VALUE='Simulation', /MENU)
SparseDiff = WIDGET_BUTTON(Simulation_menu, VALUE=' Sparse Event Diffraction ', UVALUE='Sim:SparseDiff')

Image_menu = WIDGET_BUTTON(Proc_menu, VALUE='Image Transformations', /MENU)
SlidFFT= WIDGET_BUTTON(Image_menu, VALUE=' Sliding FFT ', UVALUE='ImageTF:SlidFFT')

AnalysisProc_menu = WIDGET_BUTTON(bar, VALUE='Analysis', /MENU)

VD_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Virtual Detectors', /MENU)

VD_bf = WIDGET_BUTTON(VD_menu, VALUE=' Bright-field ', UVALUE='VD:bf')
VD_adf = WIDGET_BUTTON(VD_menu, VALUE=' Annular Dark-field ', UVALUE='VD:adf')
VD_adf = WIDGET_BUTTON(VD_menu, VALUE=' Virtual Objective Aperture ', UVALUE='VD:VirtualAperture')
VD_adf = WIDGET_BUTTON(VD_menu, VALUE=' Sequence of Circular Annular Segments ', UVALUE='VD:AnnularSegments')
VD_segdet = WIDGET_BUTTON(VD_menu, VALUE=' Angular Segment Detector ', UVALUE='VD:segdet')
VD_ScatterPlot = WIDGET_BUTTON(VD_menu, VALUE=' Segmentation ', /MENU) 
VD_seghist2D = WIDGET_BUTTON(VD_ScatterPlot, VALUE=' 2D Scatterplot Segmentation ', UVALUE='VD:TwoSegHist')
VD_seghist3D = WIDGET_BUTTON(VD_ScatterPlot, VALUE=' 3D Scatterplot Segmentation ', UVALUE='VD:ThreeSegHist')
Filter_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Filter', /MENU)
Filter_segdet = WIDGET_BUTTON(Filter_menu, VALUE=' Angular Segment Filter ', UVALUE='Filter:segfilt')
WD_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Phase and Orientation Map', /MENU)
WD_omap = WIDGET_BUTTON(WD_menu, VALUE=' COM Orientation map ', UVALUE='WD:omap')
WD_omap = WIDGET_BUTTON(WD_menu, VALUE=' Correlation Orientation map ', UVALUE='WD:omap2')
WD_omap = WIDGET_BUTTON(WD_menu, VALUE=' Template Scale-Rotation Correlation Map ', UVALUE='WD:templatemap', /Separator)

PC_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Phase Contrast', /MENU,/SEPARATOR)
PC_COM = WIDGET_BUTTON(PC_menu, VALUE=' Centre of Mass ', UVALUE='PC:COM')
PC_COM = WIDGET_BUTTON(PC_menu, VALUE=' DPC ', UVALUE='PC:DPC')
PC_DiscShift = WIDGET_BUTTON(PC_menu, VALUE=' Disc Centre Shift ', UVALUE='PC:DiscShift')

uDiff_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='uDiffraction', /MENU,/SEPARATOR)
;; uDiff_align = WIDGET_BUTTON(uDiff_menu, VALUE=' Align Empad Frame
;; Stack ', UVALUE='uDiff:align')
uDiff_roi=WIDGET_BUTTON(uDiff_Menu, VALUE=' ROI Diffraction', /MENU)
uDiff_adf = WIDGET_BUTTON(uDiff_roi, VALUE=' ROI Diffraction ', UVALUE='uDiff:pickdiff')
uDiff_adf = WIDGET_BUTTON(uDiff_roi, VALUE=' Line Profile ', UVALUE='uDiff:picklinediff')
uDiff_ordermenu=WIDGET_BUTTON(uDiff_Menu, VALUE=' Fluctuation and Order', /MENU)
uDiff_fm = WIDGET_BUTTON(uDiff_ordermenu, VALUE=' Annular Ring Intensity Fluctuation or Entropy ', UVALUE='uDiff:fluct')

uDiff_fm = WIDGET_BUTTON(uDiff_ordermenu, VALUE=' Sequence of Annular Ring Intensity Fluctuation or Entropy', UVALUE='uDiff:fluctall')
uDiff_fm = WIDGET_BUTTON(uDiff_ordermenu, VALUE=' Angular Intensity Fluctuation Maps ', UVALUE='uDiff:fluctang', /SEPARATOR)
uDiff_fm = WIDGET_BUTTON(uDiff_ordermenu, VALUE=' Angular Intensity Fluctuation Maps To 1-255 Color Index', UVALUE='uDiff:fluctangmap')

uDiff_acorr = WIDGET_BUTTON(uDiff_ordermenu, VALUE=' Azimuthal Auto-correlation ', UVALUE='uDiff:acorr',/SEPARATOR)
uDiff_subsetmenu=WIDGET_BUTTON(uDiff_Menu, VALUE=' Extract Subset  ', /MENU)
uDiff_fluct = WIDGET_BUTTON(uDiff_subsetmenu, VALUE=' Annular Ring Fluctuation Threshold Subset ', UVALUE='uDiff:fluct:subset')
uDiff_acorr = WIDGET_BUTTON(uDiff_subsetmenu, VALUE=' Azimuthal Auto-correlation Threshold Subset ', UVALUE='uDiff:acorr:subset')

uDiff_psearch_menu = WIDGET_BUTTON(uDiff_Menu, VALUE=' Peak Search and Analysis ', /Menu)
uDiff_psearch = WIDGET_BUTTON(uDiff_psearch_menu, VALUE=' Peak Search ', UVALUE='uDiff:peaksearch')
uDiff_psearch = WIDGET_BUTTON(uDiff_psearch_menu, VALUE=' Debye-Scherrer Plot ', UVALUE='uDiff:peakanalysis:dsplot',/SEPARATOR)
uDiff_psearch = WIDGET_BUTTON(uDiff_psearch_menu, VALUE=' Distance Histogram and Powder Plot ', UVALUE='uDiff:peakanalysis:pseudopowderplot')
uDiff_psearch = WIDGET_BUTTON(uDiff_psearch_menu, VALUE=' Peak Data Orientation map ', UVALUE='uDiff:peakanalysis:omap')
uDiff_psearch = WIDGET_BUTTON(uDiff_psearch_menu, VALUE=' Export Frame Peak Data To JSON', UVALUE='uDiff:peakanalysis:exportjson')


Measure_pp=WIDGET_BUTTON(uDiff_menu, VALUE=' Peak-to-Peak Distances', UVALUE='Proc:MeasurePeaks')

Series_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Time Series', /MENU)
;; uDiff_align = WIDGET_BUTTON(uDiff_menu, VALUE=' Align Empad Frame
;; Stack ', UVALUE='uDiff:align')
Series_roi=WIDGET_BUTTON(Series_menu, VALUE=' Slice Series', UVALUE='Series:slice')

;; Measure_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Measure', /MENU)
Strain_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Strain Analysis', /MENU)
StrainMap_roi=WIDGET_BUTTON(Strain_menu, VALUE=' StrainMapping', UVALUE='Strain:Map')


Util_menu = WIDGET_BUTTON(bar, VALUE='Tools', /MENU)

;; DemoProgsReportArrayStatistics = WIDGET_BUTTON(Util_menu, VALUE='Statistics', UVALUE='DemoProgs:ReportArrayStatistics')

;;ROIMenu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='ROI Selection', /MENU)
;;ROIselection = WIDGET_BUTTON(ROIMenu, VALUE='2D ROI Selection', UVALUE='Proc:ROISelection')
CalibrationMenu = WIDGET_BUTTON(Util_menu, VALUE='Calibration', /MENU)
ROIselection = WIDGET_BUTTON(CalibrationMenu, VALUE='Select Calibration', UVALUE='Proc:CalibDB')
ROIselection = WIDGET_BUTTON(CalibrationMenu, VALUE='Calibrate Data', UVALUE='Proc:CalibData')

MarkerMenu = WIDGET_BUTTON(Util_menu, VALUE='Markers', /MENU)
ROIselection = WIDGET_BUTTON(MarkerMenu, VALUE='Circle', UVALUE='Proc:MarkerCircle')
ROIselection = WIDGET_BUTTON(MarkerMenu, VALUE='Pixel', UVALUE='Proc:MarkerPoint')

Vis_menu = WIDGET_BUTTON(Util_Menu, VALUE='Visualisation', /MENU)
Omap = WIDGET_BUTTON(Vis_menu, VALUE='Polar Color Image (Orientation Map)', UVALUE='Vis:OrientationMapColoring')
ColorWheel=WIDGET_BUTTON(Vis_menu, VALUE='Angular Map (Color Wheel)', UVALUE='Vis:ColorWheel')
;; iImage = WIDGET_BUTTON(Vis_menu, VALUE='Start iImage', UVALUE='Vis:iImage')
Fixes = WIDGET_BUTTON(Util_menu, VALUE='Dataset Fixes', /MENU)
FixFrameRot = WIDGET_BUTTON(Fixes, VALUE='EMPAD Fix Diffraction Rotation', /MENU)
FixFrameRotFrame = WIDGET_BUTTON(FixFrameRot, VALUE='Fix single frame rotation',UVALUE='FixDiffractionRotationFrame')
FixFrameRotStack = WIDGET_BUTTON(FixFrameRot, VALUE='Fix PAD stack rotation',UVALUE='FixDiffractionRotationStack')
DemoProgsReportArrayStatistics = WIDGET_BUTTON(Fixes, VALUE='EMPAD Fix Test Rows', UVALUE='FixTestRows')
Flip_XY = WIDGET_BUTTON(Fixes, VALUE='Flip scan XY', UValue='TF:FlipXY')
;;ROIselection3D = WIDGET_BUTTON(ROIMenu, VALUE='3D ROI Selection', UVALUE='Proc:ROISelection3D')
;;IF keyword_set(developer) THEN BEGIN
;;   SinAnalysis_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Sinogram Analysis', /MENU)
;;   SinogramCh = WIDGET_BUTTON(SinAnalysis_menu, VALUE='Sinogram Check', UVALUE='Proc:SinogramCheck')
;;   SinogramCh2 = WIDGET_BUTTON(SinAnalysis_menu, VALUE='Sinogram Filter', UVALUE='Proc:FilterSinogram')
;;END
;;ScaleAna = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Scale Analysis', UVALUE='Proc:ScaleAnalysis')
;;FSCAna = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Fourier Shell Correlation', UVALUE='Proc:FSCAnalysis');;
;; VolumeSlicer = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Butterworth Filter', UVALUE='Proc:FilterGUI')
;;VolumeSlicer = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Voxel Correlation', UVALUE='Proc:VolumeCorrelation')
;;VolumeSlicer = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Volume Slicer', UVALUE='Proc:VolumeSlicer')
;;VolumeSlicer = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Volume Rendering', UVALUE='Proc:VolumeRendering')
;;ClusterAnalysisProc_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Cluster Analysis', /MENU)
;;ScaleAna = WIDGET_BUTTON(ClusterAnalysisProc_menu, VALUE='Cluster Search', UVALUE='Proc:ClusterAnalysis')
;;ScaleAna = WIDGET_BUTTON(ClusterAnalysisProc_menu, VALUE='Refine Cluster Table Data', UVALUE='Proc:ClusterAnalysis:RefineTable')
;;ScaleAna = WIDGET_BUTTON(ClusterAnalysisProc_menu, VALUE='Map Table Data', UVALUE='Proc:ClusterAnalysis:MapClusterDataFromTable')
;;AnalysisDeconv_menu = WIDGET_BUTTON(AnalysisProc_menu, VALUE='Deconvolution', /Menu)
;;AnalysisPSF = WIDGET_BUTTON(AnalysisDeconv_menu, VALUE='Generate Model PSF', UVALUE='Proc:Deconv:PSF')
;;AnalysisFTDeconv = WIDGET_BUTTON(AnalysisDeconv_menu, VALUE='Fourier Deconvolution', UVALUE='Proc:Deconv:FourierDeconv')

;;AlignProc_menu = WIDGET_BUTTON(bar, VALUE='Alignment', /MENU)
;;
;;FiducialAlignID = WIDGET_BUTTON(AlignProc_menu, VALUE='Fiducial Alignment', UVALUE='Proc:FiducialAlignID', /MENU)
;;MarkerAlignX = WIDGET_BUTTON(FiducialAlignID, VALUE='Single Fiducial Alignment', UVALUE='Proc:SingleFiducial')
;; MarkerAlignX = WIDGET_BUTTON(FiducialAlignID, VALUE='Single Fiducial Alignment with Axis Tilt', UVALUE='Proc:MarkerAlignRotateAxisTilt')
;;MarkerAlignX = WIDGET_BUTTON(FiducialAlignID, VALUE='Fiducial Tracking', UVALUE='Proc:FiducialTracker')
;;MarkerAlignX = WIDGET_BUTTON(FiducialAlignID, VALUE='Fiducial Analysis', UVALUE='Proc:FiducialAnalysis')
;;
;MarkerAlignID = WIDGET_BUTTON(AlignProc_menu, VALUE='Marker Alignment', UVALUE='Proc:MarkerAlignID', /MENU)
;MarkerAlignX = WIDGET_BUTTON(MarkerAlignID, VALUE='Marker Alignment X', UVALUE='Proc:MarkerAlignX')
;MarkerAlignY = WIDGET_BUTTON(MarkerAlignID, VALUE='Marker Alignment Y', UVALUE='Proc:MarkerAlignY')
;;MarkerAlignXY = WIDGET_BUTTON(AlignProc_menu, VALUE='Marker Alignment', UVALUE='Proc:MarkerAlign')
;; FlipAlignID = WIDGET_BUTTON(, VALUE='Flip Alignment', UVALUE='Proc:FlipAlignID', /MENU)
;;TiltAxisAlign = WIDGET_BUTTON(FlipAlignID, VALUE='Flip Alignment', UVALUE='Proc:FlipAlign')
;;TiltAxisAlign = WIDGET_BUTTON(AlignProc_menu, VALUE='Flip Alignment (Experimental)', UVALUE='Proc:FlipAlign2')

;;TAAlignProc_menu = WIDGET_BUTTON(AlignProc_menu, VALUE='Tilt Axis Alignment', /MENU)
;;TiltAxisAlign = WIDGET_BUTTON(TAAlignProc_menu, VALUE='Tilt Axis Shift and Rotation', UVALUE='Proc:TiltAxisAlignment')
;;TiltAxisAlign = WIDGET_BUTTON(TAAlignProc_menu, VALUE='Tilt Axis Drift', UVALUE='Proc:TiltAxisDriftAlignment')
;;TiltAxisAlign = WIDGET_BUTTON(TAAlignProc_menu, VALUE='Tilt Axis Repositioning', UVALUE='Proc:TiltAxisRepositioning')

;;FeatureProc_menu = WIDGET_BUTTON(AlignProc_menu, VALUE='Feature Selection', /MENU)
;;TiltAxisAlign = WIDGET_BUTTON(FeatureProc_menu, VALUE='Centre Feature', UVALUE='Proc:RecVolPickCenter')
;;TiltAxisAlign = WIDGET_BUTTON(FeatureProc_menu, VALUE='Centre and Cut Feature', UVALUE='Proc:RecVolPickSubregion')

;;TiltAxisAlign = WIDGET_BUTTON(AlignProc_menu, VALUE='Sinogram Alignment', UVALUE='Proc:SinogramAlignment')
;;
;;CCAlignProc_menu = WIDGET_BUTTON(AlignProc_menu, VALUE='Cross-Correlation Alignment', /MENU)
;;TiltAxisAlign = WIDGET_BUTTON(CCAlignProc_menu, VALUE='Sequential Cross Correlation', UVALUE='Proc:SequentialCC',/SEPARATOR)
;;TiltAxisAlign = WIDGET_BUTTON(CCAlignProc_menu, VALUE='Align to Reference Images', UVALUE='Proc:AlignToProj')
;;TiltAxisAlign = WIDGET_BUTTON(CCAlignProc_menu, VALUE='Align to Projection Images and Normalize', UVALUE='Proc:AlignToProjNorm')
;;TiltAxisAlign = WIDGET_BUTTON(CCAlignProc_menu, VALUE='Correlate to Projection Images and Normalize', UVALUE='Proc:AlignToProjNormOnly')
;;ScaleCorr = WIDGET_BUTTON(CCAlignProc_menu, VALUE='Scale Correction', UVALUE='Proc:MagCalib')
;;GSAlignProc_menu = WIDGET_BUTTON(AlignProc_menu, VALUE='Iterative Grid Search Alignment', /MENU)
;;TiltAxisAlign = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Grid Search Alignment', UVALUE='Proc:GridSearchAli')
;;TiltAxisAlign = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Fast Grid Search Alignment', UVALUE='Proc:GridSearchAliFast')
;;TiltAxisAlign = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Grid Search Tilt Axis Shift', UVALUE='Proc:GridSearchTiltAxisShift',/Separator)
;;TiltAxisRotation = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Grid Search Tilt Axis Rotation', UVALUE='Proc:GridSearchTiltAxisRotation')
;;TiltAxisDrift = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Grid Search Tilt Axis Drift', UVALUE='Proc:GridSearchTiltAxisDrift')
;;TiltAxisDriftY = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Grid Search Drift Along Tilt Axis', UVALUE='Proc:GridSearchYDrift',/Separator)
;; ScaleCorrection = WIDGET_BUTTON(GSAlignProc_menu, VALUE='Grid Search Scale Correction', UVALUE='Proc:GridSearchMagFluct')
;;SAlignProc_menu = WIDGET_BUTTON(AlignProc_menu, VALUE='Search Alignment', /MENU)
;;TiltAxisAlign = WIDGET_BUTTON(SAlignProc_menu, VALUE='Backtracking Random Search Alignment', UVALUE='Proc:BTRandSearch',/SEPARATOR)
;;TiltAxisAlign = WIDGET_BUTTON(SAlignProc_menu, VALUE='Simulated Annealing Shift Alignment', UVALUE='Proc:SAOpt')
;;ThreeDProc_menu = WIDGET_BUTTON(bar, VALUE='3D-Transforms', /MENU)
;;RadonTr = WIDGET_BUTTON(ThreeDProc_menu, VALUE='Radon Transform', UVALUE='Proc:RadonTransform')
;;RadonBackTr = WIDGET_BUTTON(ThreeDProc_menu, VALUE='Radon Backtransform', UVALUE='Proc:RadonBackTransform')
;;SIRTTr = WIDGET_BUTTON(ThreeDProc_menu, VALUE='SIRT Reconstruction', UVALUE='Proc:SIRT')
;;DCONVr = WIDGET_BUTTON(ThreeDProc_menu, VALUE='Deconvolution Reconstruction', UVALUE='Proc:DCONV')
;;SIRTTr = WIDGET_BUTTON(ThreeDProc_menu, VALUE='Discretized SIRT Reconstruction', UVALUE='Proc:Discrete_SIRT')
;;IF keyword_set(developer) THEN BEGIN
;;   DASIRTTr = WIDGET_BUTTON(ThreeDProc_menu, VALUE='Dual Axis SIRT Reconstruction', UVALUE='Proc:DualAxisSIRT')
;;END
;;IF keyword_set(developer) THEN BEGIN
;;   TiltAxisAlign = WIDGET_BUTTON(ThreeDProc_menu, VALUE='Subset Reconstruction', UVALUE='Proc:ReconSubset')
;;END
;;
Window_menu = WIDGET_BUTTON(bar, VALUE='Windows', /MENU)
DataInsp = WIDGET_BUTTON(Window_menu, VALUE='DataInspector', UVALUE='Win:DataInspector')
;; DataInsp = WIDGET_BUTTON(Window_menu, VALUE='Contrast', UVALUE='Win:Contrast')
DataInsp = WIDGET_BUTTON(Window_menu, VALUE='ContrastInspector', UVALUE='Win:ContrastInspector')
DataInsp = WIDGET_BUTTON(Window_menu, VALUE='Console', UVALUE='Win:Console')

;;


;; ColInsp = WIDGET_BUTTON(Util_Menu, VALUE='Colors', UVALUE='Util:Color')
ColorMenu= WIDGET_BUTTON(Util_menu, VALUE='Display  ', /MENU)
GraphMode = WIDGET_BUTTON(ColorMenu, VALUE='Graphics Mode', UVALUE='Util:GraphicsMode')
BSMenu= WIDGET_BUTTON(Colormenu, VALUE='Backing Store  ', /MENU)
BSon = WIDGET_BUTTON(BSmenu, VALUE='IDL Backing Store On', UVALUE='Proc:IDLBackingstoreOn')
BSoff = WIDGET_BUTTON(BSmenu, VALUE='IDL Backing Store Off', UVALUE='Proc:IDLBackingstoreOff')

Macros_menu = WIDGET_BUTTON(bar, VALUE='Macros', /MENU)
MacroPurge = WIDGET_BUTTON(Macros_menu, VALUE='Purge All', UVALUE='Macros:Purge')
MacroDirRec =  WIDGET_BUTTON(Macros_menu, VALUE='Batch Processing', /MENU)
MacroDirList =  WIDGET_BUTTON(MacroDirRec, VALUE='Prepare File List', UVALUE='Macros:PrepMagCLTable')
MacroDirList =  WIDGET_BUTTON(Macros_menu, VALUE='Stack List Macros', /MENU)
MacroADFBF = WIDGET_BUTTON(MacroDirRec, VALUE='Batch ADF and BF', UVALUE='Macros:ADFBF',/SEPARATOR)
MacroPD = WIDGET_BUTTON(MacroDirRec, VALUE='Batch Peak Detection', UVALUE='Macros:PeakDetection')
MacroPD = WIDGET_BUTTON(MacroDirRec, VALUE='Batch ADF and BF and ROI Diff', UVALUE='Macros:DirADFBFPickDiff')
LoadMultiple=  WIDGET_BUTTON(MacroDirList, VALUE='Load Multiple PAD hdf5 Files', UVALUE='Macros:LoadMultiple')
MergeMultiple=  WIDGET_BUTTON(MacroDirList, VALUE='Merge Multiple Data Stacks', UVALUE='Macros:MergeMultiple')
MacrouDiffADF = WIDGET_BUTTON(Macros_menu, VALUE='Align uDiff and Store ADF', UVALUE='Macros:uDiffADF')
MacrouDiffROI = WIDGET_BUTTON(Macros_menu, VALUE='Align uDiff and Store ROI Diff', UVALUE='Macros:uDiffROI')
MacroQuadDodec = WIDGET_BUTTON(Macros_menu, VALUE='Quadrant-Dodecapole', UVALUE='Macros:QuadDodec')
MacroQuadOct = WIDGET_BUTTON(Macros_menu, VALUE='Quadrant-Octopole', UVALUE='Macros:QuadOctet')
;;TestStacks_menu = WIDGET_BUTTON(Util_menu, VALUE='3D Example Stacks', /MENU)
;;Util_Dot = WIDGET_BUTTON(TestStacks_menu, VALUE='Dot', UVALUE='Util:DotStack')
;;offsetY=0
;;Util_Dot = WIDGET_BUTTON(TestStacks_menu, VALUE='Line', UVALUE='Util:LineStack')
;;Util_SimpleSphere = WIDGET_BUTTON(TestStacks_menu, VALUE='Simple Sphere', UVALUE='Util:SimpleSphereStack')
;;Util_HalfDome = WIDGET_BUTTON(TestStacks_menu, VALUE='Half Dome', UVALUE='Util:HalfDomeStack')
;;Util_CappedSphere = WIDGET_BUTTON(TestStacks_menu, VALUE='Capped Sphere', UVALUE='Util:CappedSphereStack')
;;Util_Sphere = WIDGET_BUTTON(TestStacks_menu, VALUE='Double Sphere', UVALUE='Util:SphereStack')
;;Util_Cyl = WIDGET_BUTTON(TestStacks_menu, VALUE='Round Cylinder', UVALUE='Util:CylinderStack')
;;Util_Cyl = WIDGET_BUTTON(TestStacks_menu, VALUE='Hexagonal Cylinder', UVALUE='Util:HexagonStack')
;;Util_Cyl = WIDGET_BUTTON(TestStacks_menu, VALUE='Octagonal Cylinder', UVALUE='Util:OctagonStack')
;;Util_RS = WIDGET_BUTTON(TestStacks_menu, VALUE='Random Spheres', UVALUE='Util:RandomSpheres')
;;Util_RS = WIDGET_BUTTON(TestStacks_menu, VALUE='PSF Generating Sphere', UVALUE='Util:PSFSpheres')
;;offsetY=0

;; Debugging menu
Debug_Menu=WIDGET_BUTTON(Util_menu, VALUE='Debug', /MENU)
Toggle_Debug = WIDGET_BUTTON(Debug_menu, VALUE='Toggle Debug Mode', UVALUE='Util:Debug')
;;


;;
IF keyword_set(developer) THEN BEGIN
   Devel_menu = WIDGET_BUTTON(Util_menu, VALUE='Developer', /MENU)   
;;   MagWidthAlign = WIDGET_BUTTON(Devel_menu, VALUE='Grid Search Mag Width', UVALUE='Proc:GridSearchMagWidth',/SEPARATOR)
   
;;TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Shift Analysis',
;;UVALUE='Proc:ShiftAnalysis')
;;   TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Grid Search Shift Analysis', UVALUE='Proc:ShiftAnalysis')

;;   DemoProgsVolumePicker = WIDGET_BUTTON(Devel_menu, VALUE='VolumePicker', UVALUE='DemoProgs:VolumePicker')
;;   DemoProgsVolumePicker = WIDGET_BUTTON(Devel_menu, VALUE='GridSearch', UVALUE='DemoProgs:GSAlignDevel')
;;   DemoProgs=WIDGET_BUTTON(Devel_menu, VALUE='Demo Programs',/MENU)
;;   TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Randomize Alignment', UVALUE='Proc:RandomizeAlignment')
;;   SI_Menu = WIDGET_BUTTON(Devel_menu, VALUE='SpectrumImage',/MENU)
;;   SI_Align = WIDGET_BUTTON(SI_menu, VALUE='SI Align ZLP', UVALUE='Proc:SI_AlignZLP')
;;   DemoProgsNewStack = WIDGET_BUTTON(DemoProgs, VALUE='Create 3D Array', UVALUE='DemoProgs:Create3DArr')
;;   DemoProgsErrHandler = WIDGET_BUTTON(DemoProgs, VALUE='Error Handler', UVALUE='DemoProgs:ErrHandler')
;;   DemoProgsXYGraph = WIDGET_BUTTON(DemoProgs, VALUE='XY Graph', UVALUE='DemoProgs:XYGraph')
;;   DemoProgsUDialog = WIDGET_BUTTON(DemoProgs, VALUE='User Dialog', UVALUE='DemoProgs:UserDialog')
;;   DemoProgsROI = WIDGET_BUTTON(DemoProgs, VALUE='ROI Selection', UVALUE='DemoProgs:ROIExample')

;;
   TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Set Debug Flag', UVALUE='Proc:Debug',/Separator)
   TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Unset Debug Flag', UVALUE='Proc:NoDebug')
   TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Start Profiler', UVALUE='Proc:StartProfiler',/Separator)
   TiltAxisAlign = WIDGET_BUTTON(Devel_menu, VALUE='Stop Profiler', UVALUE='Proc:StopProfiler')
;;
   
END 

widpos=PlaceWidget(base, POSKEY="ul")
Widget_Control, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
;;
dummy=WidgetID(base, /SETGROUPLEADERTOP)
XMANAGER, 'MM', base, /NO_BLOCK
END








