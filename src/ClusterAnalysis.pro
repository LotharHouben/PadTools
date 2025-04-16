PRO PlotPeakLocations, p, f, Update=update, FILTERIMAGE=f_im
;;
;; p = pointer a stack entry in the stack list
;; d = i,j data indices
;; d(0,*) this contains the x centroid positions, in pixels.
;; d(1,*): this contains the y centroid positions, in pixels. 
;; get window pointer first 
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% PlotPeakLocations:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END
IF PTR_VALID(p) THEN BEGIN
   ;;
   e=(*p).datap
   IF PTR_VALID(e) THEN BEGIN
      IF NOT(keyword_set(f_im)) THEN BEGIN
         ;; redisplay original image
         TVDisplay, DIRECTP=p
      END 
      wdowinfocus=!D.WINDOW                      ;; store index of current plotting window c
      WSET, (*e).Window                          ;; plot into the right window
      IF (keyword_set(f_im)) THEN BEGIN
         ;; redisplay filtered image
         bin=(*e).binning
         N=SIZE(f_im)
         binSzX=N(1)
         binSzY=N(2)
         binSzX=ROUND(FLOOR(binSzX/bin)*bin)
         binSzY=ROUND(FLOOR(binSzY/bin)*bin)
         MyTVSCL, ALOG(REBIN(f_im, binSzX,binSzY))
      END 
      ;; plot crosses
      oplotmode=3                                ;; source copy plotting mode
      plcolor=1500L                              ;; some plot color
      device, get_graphics_function=gmode        ; store the current graphics mode.
      device, set_graphics_function=oplotmode    ; XOR mode.
      N=SIZE(f)
      CASE N[0] OF
         0: BEGIN
            device, set_graphics_function=gmode ; source copy mode.
            WSHOW, wdowinfocus                  ;; restore previous plotting window
            return
         END
         1: BEGIN
            ;; one peak found
            NPeaks=1
         END
         2: BEGIN
            NPeaks=N[2]
         END
         ELSE:
      END 
      FOR i=0,(NPeaks-1) Do BEGIN
         xdev0=FIX((f[0,i]/(*e).binning))              ;; device coordinates
         ydev0=FIX((f[1,i]/(*e).binning))
         plot_cross, xdev0, ydev0, COLOR=plcolor, SZ=5
      END
      device, set_graphics_function=gmode ; source copy mode.
      WSHOW, wdowinfocus                  ;; restore previous plotting window 
      ;;
   END ELSE BEGIN
      printToCon, "% PlotPeakLocations: Invalid data stack pointer."
   END 
END ELSE BEGIN 
   printToCon, "% PlotPeakLocations: Invalid list pointer"
END   
END 

PRO GetPeakFindParameters_event, ev
  WIDGET_CONTROL, ev.id, GET_UVALUE = uv
  CASE uv OF
     ELSE:
  END
END

Function GetPeakFindParameters, c, FILTERTYPE=filtertype, REFINEMENT=refinement, MEDFILT=medfilt
;;
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% GetPeakFindParameters:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return, 0 
 END
END 
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% GetPeakFindParameters: current stack pointer is invalid" 
     return, 0
  END 
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% GetPeakFindParameters: current data pointer is invalid" 
     return, 0
  END 
;; 
hlp=["", $
     "Peak Search", $
      "", $
      "Find peaks in the focussed image and return a list of peak locations.", $
      "", $
     "Parameters", $
      "", $
      "Characteristic length scale of noise: Parameter for the graininess of data noise.", $
      "   High frequency noise has a small length in pixels, low frequency noise a larger length.", $
      "Characteristic length scale of a peaks: The typical width of a peak in pixels.", $
      "Minimum Separation between peaks: The minimum distance between two peaks.", $
      "Minimum cutoff intensity: The lower threshold of the amplitude of a feature. Below ", $
      "   this theshold the feature is not considered as a peak.", $
      "Minimum peak mass: The lower threshold of the cumulative pixel count of a feature. Below ", $
      "   this theshold the feature is not considered as a peak. ", $
      "Margin: Pixels at the bottom, top, left and right of the image that are excluded from the ", $
      "   peak search. Set this parameter to some small value when artefacts at the margins appear.", $
      "   Hint: Another way of dealing with margin artefacts is to padd the image before doing a peak", $
     "   search.", $
     "Iterate: if the refined centroid position is too far from the initial estimate, iteratively",$
     "    recalculates the centroid using the last cetroid to position the mask", $
     " ", $
     "Options", $
     " ", $
     "Filter: Select a filter function.", $
     "         Gaussian smoothing is a convolution with a gaussian kernel with the width of the noise", $
     "         length scale. A Butterworth high pass filter will cut off frequency components finer than", $
     "         the noise length scale. ", $
     " Median Filter: Apply a median filter before the convolution or high-pass filtering to subtract a",$
     "         local background.", $
     " ", $
     " Refinement: Activate a gaussian peak fit refinement when the peak fit is applied to the full stack.", $
     " ", $
     "Button functions", $
      "", $
      "Run Peak Search: Accept the current settings and run the peak search an all stack slices.", $
      "   The peak positon list will appear as a new entry in the Blockdata tab in the ", $
      "   DataInspector. ", $
      "Trial Peak Search: Start a peak search with the current parameter settings. ", $
      "   Any previous search will be deleted. Observe the output in the Console.", $
      "CANCEL: Quit without further processing.", $
      "    "]
result=0
;; hlp:
pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     ErrMsg, "Fatal error: parameter object is not defined." 
     return, 0 
  END
  mincutoffvalue=10   ;; preset values for EMPAD                      
  masscut=100
  medwid=0
  lnoise=pobj->get_token_value('CLUSTER','NOISELEN', ERRSTATE=err)
  lobject=pobj->get_token_value('CLUSTER','DIAMETER', ERRSTATE=err)
  separation=pobj->get_token_value('CLUSTER','SEPARATION', ERRSTATE=err)
  masscut=pobj->get_token_value('CLUSTER','MINMASS', ERRSTATE=err)
  mincutoffvalue=pobj->get_token_value('CLUSTER','PEAKMIN', ERRSTATE=err)
  medwid=pobj->get_token_value('CLUSTER','MEDWID', ERRSTATE=err)
  iter=pobj->get_token_value('CLUSTER','ITERATE', ERRSTATE=err) ;; iter is a string variable
  marginpix=pobj->get_token_value('CLUSTER','MARGIN', ERRSTATE=err)
  iter=STRUPCASE(iter)
  CASE iter of
     'Y': iterate=1B
     ELSE: iterate=0B
  END
  
  ppd=((*e).data)
  IF PTR_VALID(ppd) THEN BEGIN
     CASE (*e).zcoord OF
        1: BEGIN
           m=Moment((*ppd)[(*e).slice,*,*], MAXMOMENT=2)
           maxslcind=LONG((*e).SzX-1)
        END
        2: BEGIN
           m=Moment((*ppd)[*,(*e).slice,*], MAXMOMENT=2)
           maxslcind=LONG((*e).SzY-1)
        END
        ELSE : BEGIN
           m=Moment((*ppd)[*,*,(*e).slice], MAXMOMENT=2)
           maxslcind=LONG((*e).SzZ-1)
        END
     END

      ;; widget dialog
      base = Widget_Base(TITLE="Peak Search", /COLUMN)
      input =  WIDGET_BASE(base, /COLUMN, XPAD=10)
      NPar=9
      x=STRARR(2,NPar)    ;;
      x[0,0]="Characteristic lengthscale of noise (pix): "
      x[1,0]=MyString(lnoise)
      x[0,1]="Characteristic lengthscale of peaks (pix): "
      x[1,1]=MyString(lobject)
      x[0,2]="Minimum separation between peaks    (pix): "
      x[1,2]=MyString(separation)
      x[0,3]="Minimum cut off intensity                : "
      x[1,3]=MyString(mincutoffvalue)
      x[0,4]="Minimum peak mass                        : "
      x[1,4]=MyString(masscut)
      x[0,5]="Median filter width                      : "
      x[1,5]=MyString(medwid)
      x[0,6]="Margin                              (pix): "
      x[1,6]=MyString(marginpix)
      x[0,7]="Iterate                                  : "
      x[1,7]=MyString(iter)
      x[0,8]="Slice                                    : "
      x[1,8]=MyString((*e).slice)

      x_label=LONARR(NPar) ;; parameter input
      FOR i=0,(NPar-1) DO BEGIN
         x_label(i) = CW_Field(input, TITLE=X[0,i], XSize=10, Frame=0, Value=X[1,i])
      END

      ftlbrow = Widget_Base(base, /ROW)
      text=Widget_Label(ftlbrow, VALUE='Filter:       ')
      ftlb = Widget_Base(ftlbrow, /ROW,/EXCLUSIVE)
      fbutton1 = Widget_Button(ftlb, Value='None',UValue="NoFilt", /NO_RELEASE)
      fbutton4 = Widget_Button(ftlb, Value='Wavelet Bandpass',UValue="BPass", /NO_RELEASE)
       fbutton2 = Widget_Button(ftlb, Value='Gaussian Smooth',UValue="GSmooth", /NO_RELEASE)
       fbutton3 = Widget_Button(ftlb, Value='Butterworth HiPass',UValue="BHiPass", /NO_RELEASE)
       rtlbrow = Widget_Base(base, /ROW)
       text=Widget_Label(rtlbrow, VALUE='Refinement:   ')
       rtlb = Widget_Base(rtlbrow, /ROW, /Exclusive)
       rbutton1 = Widget_Button(rtlb, Value='No', UVALUE="NoFit", /NO_RELEASE)
       rbutton2 = Widget_Button(rtlb, Value='Yes',UVALUE="GaussFit", /NO_RELEASE)
       
       Widget_Control, fbutton4, Set_Button=1
       filtertype="BPass"
       Widget_Control, rbutton2, Set_Button=1
       refinement="Yes"
       
       mtlbrow = Widget_Base(base, /ROW)
       text=Widget_Label(mtlbrow, VALUE='Median Filter:')
       mtlb = Widget_Base(mtlbrow, /ROW, /Exclusive)
       
       mbutton1 = Widget_Button(mtlb, Value='No', UVALUE="NoMedianFilt", /NO_RELEASE)
       mbutton2 = Widget_Button(mtlb, Value='Yes',UVALUE="MedianFilt", /NO_RELEASE)
       
       Widget_Control, mbutton1, Set_Button=1
       medfilt="No"
       
      b = CW_BGroup(base, /ROW, ["  Trial Peak Search "] , Button_UVALUE=['search'])
      b2 = CW_BGroup(base, /ROW, ["   Run Peak Search   ","   Help  ", "   Cancel   "] , Button_UVALUE=['accept','help','cancel'])
      widpos=PlaceWidget(base, POSKEY="cc")
      WIDGET_CONTROL, base, XOFFSET=widpos[0], YOFFSET=widpos[1], /REALIZE
      do_it_again = 0
      REPEAT BEGIN
         ev = WIDGET_Event(base)
         IF ((ev.id EQ b) OR (ev.id EQ b2)) THEN BEGIN                ;; Button-Menu
            CASE ev.value OF
               'cancel': BEGIN
                  do_it_again = NOT(do_it_again)
                  result=0
               END
               'help': BEGIN
                  msg = dialog_info(hlp, /Info)
               END
               'search': BEGIN
                  ;; For i=0,(NPar-1) DO BEGIN
                  For i=0,(NPar-1) DO BEGIN  ;; exclude slice setting, this will be read from the slider in XTabControl2, set in Update_XTabControl
                     TMP=''
                     WIDGET_Control, x_label(i), GET_VALUE=TMP
                     TMP=TMP(0)
                     x[1,i]=TMP
                  END
                  lnoise=FLOAT(x[1,0])
                  if (lnoise LT 0.) THEN lnoise=0. 
                   slice=LONG(x[1,8])
                   IF (slice GT maxslcind) THEN slice=maxslcind
                   IF (slice LT 0) THEN slice=0
                  (*e).slice=slice
                  ;; slice=(*e).slice
                  WIDGET_Control, x_label(8), SET_VALUE=slice
                  Update_XTabControl
                  lobject=ABS((FIX(x[1,1])/2) * 2)+1 ;; odd integer number, peak width
                  WIDGET_Control, x_label(1), SET_VALUE=lobject
                  if (lnoise GT lobject) THEN BEGIN
                     lnoise=lobject
                     ErrMsg, "Characteristic length of noise can be at most the characteristic length of a peak."
                     WIDGET_Control, x_label(0), SET_VALUE=lnoise
                  END
                  if (lnoise LT 0.) THEN BEGIN
                     lnoise=0.1*(lobject)
                     ErrMsg, "Characteristic length of noise has to be larger or equal to zero."
                     WIDGET_Control, x_label(0), SET_VALUE=lnoise
                  END
                  separation=CEIL(FLOAT(x[1,2]))
                  mincutoffvalue=FLOAT(x[1,3])
                  masscut=FLOAT(x[1,4])
                  medwid=FIX(x[1,5])/2*2+1
                  marginpix=ABS(FIX(x[1,6]))
                  iter=STRUPCASE(x[1,7])
                  CASE iter of
                     'Y': iterate=1B
                     ELSE: BEGIN
                        iterate=0B
                        iter='N'
                     END
                  END
                  IF (masscut LT 0) THEN masscut=0
                  ;; now search peaks
                  
                  CASE (*e).zcoord OF
                     1: BEGIN
                        filt_im=REFORM((*ppd)[(*e).slice,*,*])
                     END
                     2: BEGIN
                        filt_im=REFORM((*ppd)[*,(*e).slice,*])                    
                     END
                     ELSE : BEGIN
                        filt_im=REFORM((*ppd)[*,*,(*e).slice])
                     END
                  END
                  
                  ;; first apply the median filter
                  IF ((medfilt EQ "Yes")  AND (medwid GT 1)) THEN filt_im=filt_im-Median(filt_im,medwid)
                  
                  ;; cut off threshold, get locations
                  PixBelowThresh=WHERE(filt_im LT mincutoffvalue, cntpix)
                  ;; then apply the convolution band pass filter
                  ;; IF (lnoise gt 0) then filt_im=bpass(filt_im,lnoise,lobject)
                  CASE filtertype OF
                     "BHiPass": filt_im=BANDPASS_FILTER( filt_im, 0., 1./lnoise) ;;  [, /IDEAL] [, BUTTERWORTH=value] [, /GAUSSIAN] )
                     "GSmooth": filt_im=Gauss_smooth(filt_im,lnoise)
                     "BPass": filt_im=bpass(filt_im,lnoise,lobject,/normalize)
                     ELSE:
                  END
                  ;; now set places below cutoff to zero
                  IF (cntpix GT 0) THEN filt_im[PixBelowThresh]=0.
                  print, "* calling feature with "
                  print, "lnoise, lobject, separation, masscut, min = ", lnoise, ", ", lobject, ",", separation, ",", masscut, ",", mincutoffvalue 
                  f=feature(filt_im, lobject, separation, masscut = masscut, min = mincutoffvalue, ITERATE=iterate, MARGIN=marginpix)
                  ;; STOP ;;; what is happening, test yields TOTALLY DIFFERENT resluts that run WHYYYYYYYYYYYY?
                  N=SIZE(f)
                  
                  CASE N[0] OF
                     0: BEGIN
                        printToCon, "% FindPeakLocations: Warning - found no peaks. "
                       ;; PlotPeakLocations, c, f, /UPDATE
                     END
                     1: BEGIN
                       ;; one peak found
                       NPeaks=1
                       printToCon, "% FindPeakLocations: Found "+ MyString(NPeaks)+" peak."                       
                       ;; PlotPeakLocations, c, f, /UPDATE
                       
                       PlotPeakLocations, c, f, /UPDATE
                    END
                    2: BEGIN
                       NPeaks=N[2]
                       printToCon, "% FindPeakLocations: Found "+ MyString(NPeaks)+" unique features above threshold."
                       ;; trick display
                       a=(*ppd)[*,*,(*e).slice]
                       (*ppd)[*,*,(*e).slice]=filt_im
                       PlotPeakLocations, c, f, /UPDATE
                       (*ppd)[*,*,(*e).slice]=a
;;                       PlotPeakLocations, c, f, /UPDATE

                    END
                    ELSE:
                 END 
               END                
               'accept': BEGIN
                  do_it_again = NOT(do_it_again)      
                  pobj->set_token_value, 'CLUSTER','DIAMETER', FLOAT(lobject)
                  pobj->set_token_value, 'CLUSTER','NOISELEN', FLOAT(lnoise)
                  pobj->set_token_value, 'CLUSTER','SEPARATION', FLOAT(separation)
                  pobj->set_token_value, 'CLUSTER','MINMASS', FLOAT(masscut)
                  pobj->set_token_value, 'CLUSTER','PEAKMIN',FLOAT(mincutoffvalue)
                  pobj->set_token_value, 'CLUSTER','MEDWID',FIX(medwid)
                  pobj->set_token_value, 'CLUSTER','MARGIN',ABS(FIX(marginpix))
                  pobj->set_token_value, 'CLUSTER','ITERATE', STRUPCASE(iter)
                  ;; ThrowImage, filt_im
                  result=1
               END 
            ENDCASE
         ENDIF ELSE BEGIN
            WIDGET_CONTROL, ev.id, GET_UVALUE = uv
            CASE uv OF
               'NoFilt': filtertyope="None"
               'BHiPass': filtertype="BHiPass"
               'BPass': filtertype="BPass"
               'GSmooth': filtertype="GSmooth"
               'GaussFit': refinement="Yes"
               'NoFit': refinement="No"
               'NoMedianFilt': medfilt="No"
               'MedianFilt': medfilt="Yes"
               ELSE:
            ENDCASE
         END
      ENDREP UNTIL do_it_again
      Widget_Control, base, /DESTROY
;; end dialog

END ELSE BEGIN 
   printToCon, "% FindPeakLocations: invalid image pointer"
END  
return, result
END  


FUNCTION TransformStackToVectorList, p, NBINS=nbins, THRESH=thresh
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% TransformStackToVectorList:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return, 0 
 END
END 

;; assumes the stack was byte scaled
  
  Min=0
  Max=255
  IF (N_Elements(nbins) EQ 0) THEN nbins=16
  IF (N_Elements(thresh) EQ 0) THEN thresh=FIX(nbins*0.5)
  binsize=FIX(FLOAT((Max-Min))/nbins)
  vectorlist=list()
  dims = SIZE((*(*p).data), /DIMENSIONS)
  ; Compute the image histogram, using the default bin size of 1.
  pdf = HISTOGRAM((*(*p).data), LOCATIONS=xbin, MAX=max, MIN=min, NBINS=nbins, REVERSE_INDICES = R, /L64)
  ;; get subscripts of array elements in a single bin
  for i=thresh, nbins-1 DO BEGIN
     IF  (R[i+1] GT R[i]) THEN BEGIN
        bini=R(R[i] : R[i+1]-1)
        ;; each element of bini is one data point with intensity
        ;; between bin i and bin i+1
        ind = ARRAY_INDICES(dims, bini, /DIMENSIONS)
        For j=0,N_Elements(bini)-1 DO BEGIN
           vec=ind[*,j]
           ;; For k=thresh,xbin[i] do
           vectorlist.add, vec 
        END
     END
  END
  return, Transpose(vectorlist.ToArray())
END
  

PRO MyClusterAnalysis, NBINS=nbins, THRESH=thresh, NCLUSTERS=nclusters, DISPLAY=display
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% MyClusterAnalysis:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END 
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ClusterAnalysis: Root pointer is invalid" 
     return
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ClusterAnalysis: current stack pointer is invalid" 
     return
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% ClusterAnalysis: current data pointer is invalid" 
     return
  END
  IF (N_Elements(nbins) EQ 0) THEN nbins=16
  IF (N_Elements(thresh) EQ 0) THEN thresh=FIX(nbins*0.5)
  IF (N_Elements(nclusters) EQ 0) THEN nclusters=20
parA=0.25
parB=0.5
mydialog=obj_new("ListDialog",TITLE="ClusterAnalysis")
mydialog->AddOption,"Parameter A ", para
mydialog->AddOption,"Parameter B ", parB
IF (mydialog->UserDialog() GT 0) THEN BEGIN
   IF (mydialog->Value("Parameter A ",x)) THEN parA=x
   IF (mydialog->Value("Parameter B ",x)) THEN parB=x
END
obj_destroy, mydialog
d=TransformStackToVectorList(e, NBINS=nbins, THRESH=thresh)
;; create a distance map
;; DISTANCE = DISTANCE_MEASURE(d)
;; CLUSTERS = CLUSTER_TREE(distance, linkdistance)
weights = clust_wts(d, n_clusters=nclusters)
result = CLUSTER(d, weights, N_CLUSTERS = nclusters)

dims = SIZE((*(*e).data), /DIMENSIONS)
hlp=FLTARR(dims[0],dims[1])
x=FLTARR(dims[0],dims[1],nclusters)
printtocon, "# ClusterAnalysis: index x y"
clusterlist=list()
For i=1,nclusters DO BEGIN
cluster=d[*, WHERE(result eq (i-1))]
   indArr=cluster[1,*]*LONG(dims[1])+cluster[0,*]
   hlp[*,*]=0
   hlp[indArr]=FIX(FLOAT(i-1)/nclusters*255)
   x[*,*,i-1]=hlp
   clusterloc=[Mean(FLOAT(cluster[0,*])),Mean(FLOAT(cluster[1,*]))]
   printtocon, MyString(i)+ " " + MySTRING(clusterloc[0])+" "+MySTRING(clusterloc[1])
   clusterlist.add, clusterloc
END
IF (N_Elements(display) GT 0) THEN BEGIN
   ThrowImage, x
   ;;oplotmode=9
   ;;plcolor=100L
   ;;device, get_graphics_function=gmode0 ; store the current graphics mode.
   ;;device, set_graphics_function=oplotmode ; XOR mode.
   ;;For i=0,clusterlist.count()-1 DO BEGIN
   ;;   x1=FIX((clusterlist[i])[0]-5)
   ;;   x2=x1+10
   ;;   y1=FIX((clusterlist[i])[1]-5)
   ;;   y2=y1+10
   ;;   plot_Box, x1, x2, y1, y2, COLOR=plcolor
   ;;END
   ;;device, set_graphics_function=gmode0 ; XOR mode.
END
;; STOP
END


FUNCTION MyClusterAnalysis2D, DISPLAY=display
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% MyClusterAnalysis2D:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return, 0 
 END
END  
 ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ClusterAnalysis: Root pointer is invalid" 
     return, 0
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ClusterAnalysis: current stack pointer is invalid" 
     return, 0
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% ClusterAnalysis: current data pointer is invalid" 
     return, 0
  END
;; 
  ;; ++++++++++++++++++++++++ parameter declaration ++++++++++++++++++++++++
  pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     ErrMsg, "Fatal error: parameter object is not defined." 
     return, 0 
  END
  diameter=pobj->get_token_value('CLUSTER','DIAMETER', ERRSTATE=err)
  separation=pobj->get_token_value('CLUSTER','SEPARATION', ERRSTATE=err)
  masscut=pobj->get_token_value('CLUSTER','MINMASS', ERRSTATE=err)
  mincutoffvalue=pobj->get_token_value('CLUSTER','PEAKMIN', ERRSTATE=err)
  iter=pobj->get_token_value('CLUSTER','ITERATE', ERRSTATE=err)
  medwid=pobj->get_token_value('CLUSTER','MEDWID', ERRSTATE=err)
;; object is defined
  hlp=[""]
  mydialog=obj_new("ListDialog",TITLE="ClusterAnalysis")
  mydialog->AddOption,"Diameter     ", diameter
  mydialog->AddOption,"Separation   ", separation
  mydialog->AddOption,"Minimum mass ", masscut
  mydialog->AddOption,"Minimum peak ", min
  mydialog->AddOption,"Median filter", medwid
  mydialog->AddOption,"Iterate      ", iter
  IF (mydialog->UserDialog(HELP=hlp) GT 0) THEN BEGIN
     IF (mydialog->Value("Diameter     ",x)) THEN diameter=FIX(x)
     IF (mydialog->Value("Separation   ",x)) THEN separation=FIX(x)
     IF (mydialog->Value("Minimum mass ",x)) THEN masscut=LONG(x)
     IF (mydialog->Value("Minimum peak ",x)) THEN mincutoffvalue=FIX(x)
     IF (mydialog->Value("Median filter",x)) THEN medwid=(FIX(x)/2)*2+1
     IF (mydialog->Value("Iterate      ",x)) THEN BEGIN
        x=STRUPCASE(x)
        if (x EQ 'Y') THEN iterate=1B ELSE iterate=0B
        if (iterate EQ 1B) THEN iter='Y' ELSE iter='N'
     END
  END
  obj_destroy, mydialog
  pobj->set_token_value, 'CLUSTER','DIAMETER',diameter
  pobj->set_token_value, 'CLUSTER','SEPARATION',separation
  pobj->set_token_value, 'CLUSTER','MINMASS',masscut
  pobj->set_token_value, 'CLUSTER','PEAKMIN',mincutoffvalue
  pobj->set_token_value, 'CLUSTER','MEDWID', medwid
  pobj->set_token_value, 'CLUSTER','ITERATE', STRUPASE(iterate)
  pobj->report
;; ++++++++++++++++++++++++ end parameter declaration ++++++++++++++++++++++++
  ;;
dim=SIZE((*(*e).data),/DIMENSIONS)
slice=0L
f = feature(REFORM((*(*e).data),dim[0],dim[1]), diameter, separation, masscut = masscut, min = mincutoffvalue, iterate = iterate, medianfilt=medwid, /quiet)
printtocon, "# ClusterAnalysis: index x y mass gyration eccentricity"
clusterlist=list()
nfeatures=SIZE(f,/DIMENSIONS)
npeaks=nfeatures[1]
For i=0,npeaks-1 DO BEGIN
   printtocon, MyString(i)+ " " + MySTRING(f[0,i])+" "+MySTRING(f[1,i])+ " " + MySTRING(f[2,i])+" "+MySTRING(f[3,i])+ " " + MySTRING(f[4,i])
   clusterlist.add, f[*,i]
END
IF (N_Elements(display) GT 0) THEN BEGIN
   x=BYTARR(dim[0],dim[1])
   For i=0,clusterlist.count()-1 DO BEGIN
      x1=FIX((clusterlist[i])[0]-5)
      x2=x1+diameter
      y1=FIX((clusterlist[i])[1]-5)
      y2=y1+diameter
      x[x1:x2,(y1+y2)/2]=255
      x[(x1+x2)/2,y1:y2]=255
   END
   ThrowImage, x
END

END

PRO ThrowClusterMap, clusterlist
  CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% ThrowClusterMap:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END 
  IF NOT(GetStandardPointers(ptr,p,e,ppd)) THEN BEGIN
     Printtocon, "% ThrowClusterImage: Currect data stack is not defined."
     return
  END 
  dim=[(*e).SzX,(*e).SzY,(*e).SzZ]
  ;; TODO we have to permute columns?
  ;; x-y-z in the clusterlist are not x-y-z in the data stack
  ;; z is the projection coordinate of e
  ;; for now: flip the stack into the right orientation 
  image=FLTARR((*e).SzX,(*e).SzY,3)
  For i=0,clusterlist.count()-1 DO BEGIN
      m=FIX((clusterlist[i])[0])
      m=(m GT 0) ? m : 0
      m=(m LT (*e).SzX) ? m : (*e).SzX-1
      n=FIX((clusterlist[i])[1])
      n=(n GT 0) ? n : 0
      n=(n LT (*e).SzY) ? n : (*e).SzY-1
      image[m,n,0]=(clusterlist[i])[3]
      image[m,n,1]+=1.
      image[m,n,2]=1.
   END
  name="ClusterMap("+MyString((*e).id)+")"
   ThrowStack, PTR_NEW(image), name, TITLE=name
END


PRO ThrowClusterImage, clusterlist, CROSS=cross
  CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% ThrowClusterImage:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END 
  IF NOT(GetStandardPointers(ptr,p,e,ppd)) THEN BEGIN
     Printtocon, "% ThrowClusterImage: Currect data stack is not defined."
     return
  END 
  dim=[(*e).SzX,(*e).SzY,(*e).SzZ]
  ;; TODO we have to permute columns?
  ;; x-y-z in the clusterlist are not x-y-z in the data stack
  ;; z is the projection coordinate of e
  ;; for now: flip the stack into the right orientation 
  image=BYTSCL((*(*e).data), MIN=(*e).contrast[0],MAX=(*e).contrast[1],TOP=250)
  For i=0,clusterlist.count()-1 DO BEGIN
      IF keyword_set(cross) THEN BEGIN
         A=FIX((clusterlist[i])[0]-4) & x1=(A GT 0) ? A : 0
         B=x1+9 & x2=(B LT (*e).SzX) ? B : (*e).SzX-1
         A=FIX((clusterlist[i])[1]-4) & y1=(A GT 0) ? A : 0
         B=y1+9 & y2=(B LT (*e).SzY) ? B : (*e).SzY-1
         A=FIX((clusterlist[i])[2]-4) & z1=(A GT 0) ? A : 0
         B=z1+9 & z2=(B LT (*e).SzZ) ? B : (*e).SzZ-1
         image[x1:x2,(y1+y2)/2,(z1+z2)/2]=255
         image[(x1+x2)/2,y1:y2,(z1+z2)/2]=255
         image[(x1+x2)/2,(y1+y2)/2,z1:z2]=255
      END 
      m=FIX((clusterlist[i])[0])
      m=(m GT 0) ? m : 0
      m=(m LT (*e).SzX) ? m : (*e).SzX-1
      n=FIX((clusterlist[i])[1])
      n=(n GT 0) ? n : 0
      n=(n LT (*e).SzY) ? n : (*e).SzY-1
      o=FIX((clusterlist[i])[2])
      o=(o GT 0) ? o : 0
      o=(o LT (*e).SzZ) ? o : (*e).SzZ-1
      image[m,n,o]=255
   END
   ThrowImage, image, TITLE="ClusterImage("+MyString((*e).id)+")"
END

FUNCTION MyClusterAnalysis3D, DISPLAY=display, DIM=dim, INTERACTIVE=interactive, LIVE=live, REFINE=refine, FRAMELIST=framelist, RETURNCOM=returncom, REFFILT=reffilt, RETURNSTRONGEST=returnstrongest, RESTRICTRANGE=restrictrange
  ;;
  ;; PURPOSE:   Search for peaks in a three-dimensional array
  ;; PROCEDURE: Feature search in consecutive 2D slices
  ;; INPUT:     3D image stack, taken from managed pointer List
  ;; KEYWORDS:  display - create an array with byte 255 at peak
  ;;                      locations 
  ;; OUTPUT:    list of peak positions in the 2D slices
  ;;            list element is a 1D array with elements:
  ;;                       (x,y,z,mass,gyration,excenricity)
  ;;
 CATCH, Error_status
 IF NOT(GetDebugFlag()) THEN BEGIN
    quiet=1
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% MyClusterAnalysis3D:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return, 0
 END
END ELSE BEGIN
   quiet=0
END
if not(keyword_set(tophat)) then tophat=1
  clusterlist=list()
  flist=list()
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% ClusterAnalysis: Root pointer is invalid" 
     return, 0
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     printtocon, "% ClusterAnalysis: current stack pointer is invalid" 
     return, 0
  END
  e=(*c).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% ClusterAnalysis: current data pointer is invalid" 
     return, 0
  END
;; 
  ;; ++++++++++++++++++++++++ parameter declaration ++++++++++++++++++++++++
  pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     ErrMsg, "Fatal error: parameter object is not defined." 
     return, 0 
  END
  filtertype="BHiPass"
  refinement="Yes"
  medfilt="No"  
IF keyword_set(interactive) THEN BEGIN
   ;; open a dialog to adjust the peak search parameters
   IF NOT(GetPeakFindParameters(c, Filtertype=filtertype, REFINEMENT=refinement, MEDFILT=medfilt)) THEN BEGIN
      return, 0 
   END
END
  diameter=pobj->get_token_value('CLUSTER','DIAMETER', ERRSTATE=err)
  separation=pobj->get_token_value('CLUSTER','SEPARATION', ERRSTATE=err)
  masscut=pobj->get_token_value('CLUSTER','MINMASS', ERRSTATE=err)
  mincutoffvalue=pobj->get_token_value('CLUSTER','PEAKMIN', ERRSTATE=err)
  iter=pobj->get_token_value('CLUSTER','ITERATE', ERRSTATE=err)
  lnoise=pobj->get_token_value('CLUSTER','NOISELEN', ERRSTATE=err)
  medwid=pobj->get_token_value('CLUSTER','MEDWID', ERRSTATE=err)
  marginpix=pobj->get_token_value('CLUSTER','MARGIN', ERRSTATE=err)
  pobj->report, KEY='CLUSTER'
;; ++++++++++++++++++++++++ end parameter declaration ++++++++++++++++++++++++
  ;;
  iterate=0B
  if (iter EQ "Y") Then iterate=1
  lobject=ABS(FIX(diameter/2.) * 2)+1 ;; odd integer number
  kernel=FLTARR(2*lobject+1,2*lobject+1) ;; for fitting refinement
  kernel[*,*]=1.
  if (lnoise LE 0.) THEN lnoise=1. 
  noiselen=lnoise
  dim=SIZE((*(*e).data),/DIMENSIONS)
  detectordim=dim[0]
  mask=BYTARR([dim[0],dim[1]])
  mask[*,*]=1B
  if (SIZE(dim, /N_Elements) EQ 2) THEN dim=[dim,1]
  IF (dim[1] EQ (dim[0]+2)) THEN mask[0,128:129]=0B ;; EMPAD, mask detector test rows
NSlices=dim[(*e).zcoord-1]
InitProgressBar, MINV=[0L], MAXV=[LONG((NSlices-1))], TEXT=["Processing Slice Nr."]
peakcand=0L
CASE (*e).zcoord OF
   1: BEGIN
   END
   2: BEGIN
   END 
   3:BEGIN
      printtocon, "# ClusterAnalysis: Parameter Settings"
      printtocon, "#  Peak Diameter = "+MyString(diameter)
      printtocon, "#  Minimum Peak Separation = "+MyString(separartion)
      printtocon, "#  Minimum Peak Mass = "+MyString(masscut)
      printtocon, "#  Minimum Peak Value = "+MyString(mincutoffvalue)
      printtocon, "#  Noise feature length = "+MyString(noiselen)
      printtocon, "#  Median filter width  = "+MyString(medwid)
      IF keyword_set(refine) THEN  printtocon, "#  Peak optimization = Yes" ELSE printtocon, "#  Peak optimization = No"
      printtocon, "# ClusterAnalysis: slice index y z mass gyration"
      com=DBLARR(NSlices,5)
      strongest=DBLARR(NSlices,4)
      refcount=0L
      rejcount=0L
      For slice=0L,NSlices-1 DO BEGIN
         ProgressBarStep, 0, TEXT=STRING(slice)
         comx=0. & comy=0.
         comnorm=Double(0.)
         im = REFORM((*(*e).data)[*,*,slice],dim[0],dim[1])*mask
         ;; first apply the median filter, then find pixel mask for pixels below threshold, then bandpass, then mask pixels below threshold
;; first apply the median filter
         
         IF ((medfilt EQ "Yes")  AND (medwid GT 1)) THEN im=im-Median(im,medwid)
         ;; cut off threshold, get locations
         PixBelowThresh=WHERE(im LT mincutoffvalue, cntpix)
         CASE filtertype OF
            "BHiPass": im=BANDPASS_FILTER( im, 0., 1./lnoise) ;;  [, /IDEAL] [, BUTTERWORTH=value] [, /GAUSSIAN] )
            "GSmooth": im=Gauss_smooth(im,lnoise)
            "BPass": im=bpass(im,lnoise,lobject,/normalize)
            ELSE:
         END
           ;;  im=bpass(im,lnoise,lobject)
         IF (cntpix GT 0) THEN im[PixBelowThresh]=0.
         
          ;;IF (medwid gt 0) THEN BEGIN
               ;; create background
               ;;im_filt= MEDIAN(im, medwid)
               ;; keep most intense peak?
               ;;If keyword_set(keepstrongest) THEN  BEGIN
                  ;; take a circle with the diameter of medwid around the strongest peak and mask it
                  ;;index_array=where(im EQ Max(im), Count)
                  ;;IF (count GT 1) THEN index_array=index_array[0]
                  ;; refine max to com
                  ;;x=index_array mod dim[0] ;; position of the maximum in x and y
                  ;;y=(index_array/dim[0])
                  ;;x0=x-medwid/2 & x1=x+medwid/2  ;; serach range for refinement in absolute coordinates
                  ;;y0=y-refradiusmedwid/2 & y1=y+medwid/2
                  ;;patch=im[x0:x1,y0:y1]
                  ;;im = im - im_filt
                  ;;im[x0:x1,y0:y1]=patch
               ;;END  
          ;;  END
          ;;STOP
         f=feature(im, lobject, separation, masscut = masscut, min = mincutoffvalue, iterate = iterate, margin=marginpix, quiet=quiet)
         ;; N = SIZE(f)
         ;; NPeaks=N[2]
         fdim=SIZE(f,/N_DIMENSIONS)
         peakcand+=fdim
         IF (fdim EQ 1) THEN BEGIN
            ;; only one point, presumably the central diffraction spot
            x=f[0] & y=f[1]
            fitdata=[1.,10.,1.,1.,x,y,0.]
            ;      Par(0)   Constant baseline level
;      Par(1)   Peak value
;      Par(2)   Peak half-width (x) -- gaussian sigma or half-width at half-max
;      Par(3)   Peak half-width (y) -- gaussian sigma or half-width at half-max
;      Par(4)   Peak centroid (x)
;      Par(5)   Peak centroid (y)
;      Par(6)   Rotation angle (radians) if TILT keyword set
;      Par(6 or 7)   Moffat power law index if MOFFAT keyword set
            ;; fit to the filtered data
            IF keyword_set(refine) THEN  BEGIN
               IF keyword_set(reffilt) THEN BEGIN
                  Profile2DFindCentre, im, x, y, kernel, QUIET=quiet, FITDATA=fitdata
               END ELSE BEGIN
                  ;; refine on original data
                  Profile2DFindCentre, REFORM((*(*e).data)[*,*,(*e).slice]), x, y, kernel, QUIET=quiet, FITDATA=fitdata
               END 
            END 
            ;; fit to the unfiltered data
            ;; IF keyword_set(refine) THEN  
            ;; Profile2DFindCentre, REFORM((*(*e).data)[*,*,slice]), x, y, kernel, /QUIET, FITDATA=fitdata
            clusterlist.add, [x,y,slice*1.0,f[2:3],f[0:1], 0., 0.,fitdata]
            refcount+=1
         END
         IF (fdim EQ 2) THEN BEGIN ;; it is a two dimensional array of peaks
            nfeatures=SIZE(f,/DIMENSIONS)
            npeaks=nfeatures[1]
            if (npeaks GT 1) THEN flist.add, slice ;; add this slice to the list
;; sort according to strongest peak
            b=colsort(f,2,/reverse_sort)           ;; first row has strongest peak, supposedly the transmitted beam
            ;;
            IF keyword_set(live) THEN BEGIN
               (*e).slice=slice
               ;; trick display
               a=(*(*e).data)[*,*,(*e).slice]
               (*(*e).data)[*,*,(*e).slice]=im
               PlotPeakLocations, c, b, /UPDATE
               (*(*e).data)[*,*,(*e).slice]=a
            END
            parinfo = replicate({value: 0.D, fixed:0, limited:[0,0], limits:[0.D,0.D]},7)
            ;; parameter limits
            ;; base is positive
            parinfo[0].limited[0]=1
            parinfo[0].limits[0]=0.
            ;; peak amplitude is positive
            parinfo[1].limited[0]=1
            parinfo[1].limits[0]=0.
            ;;
            ;; peak width are positive and the maximum is the diameter
            parinfo[2].limited[1]=1
            parinfo[2].limits[1]=diameter
            parinfo[3].limited[1]=1
            parinfo[3].limits[1]=diameter
            ;;
            For i=0,npeaks-1 DO BEGIN
               ;; here is the peak refinement
               ;;
               ;; printtocon, MyString(slice)+" "+MyString(i)+ " " +
               ;; MySTRING(f[0,i])+" "+MySTRING(f[1,i])+ " " +
               ;; MySTRING(f[2,i])+" "+MySTRING(f[3,i])
               x=b[0,i] & Y=b[1,i] 
;    f[0,*]: x centroid positions, in pixels.
;    f[1,*[: y centroid positions, in pixels. 
;    f[2,*]: integrated brightness of the features.
;    f[3,*]: square of the radius of gyration
               fitdata=[0.,b[2,i],b[3,i],b[3,i],x,y,0.] ;; return gaussina parameters
               for j=0,6 DO parinfo[j].value=fitdata[j]
;      Par(0)   Constant baseline level
;      Par(1)   Peak value
;      Par(2)   Peak half-width (x) -- gaussian sigma or half-width at half-max
;      Par(3)   Peak half-width (y) -- gaussian sigma or half-width at half-max
;      Par(4)   Peak centroid (x)
;      Par(5)   Peak centroid (y)
;      Par(6)   Rotation angle (radians) if TILT keyword set
;      Par(6 or 7)   Moffat power law index if MOFFAT keyword set
               IF (refinement EQ "Yes") THEN BEGIN
                  IF keyword_set(reffilt) THEN BEGIN
                     Profile2DFindCentre, im, x, y, kernel, QUIET=quiet, FITDATA=fitdata, ALLOWTILT=0, PARINFO=parinfo
                  END ELSE BEGIN
                     ;; fit to original data
                     Profile2DFindCentre, REFORM((*(*e).data)[*,*,slice]), x, y, kernel, FITDATA=fitdata, QUIET=quiet, ALLOWTILT=0, PARINFO=parinfo
                  END
               END 
               IF (i EQ 0) THEN BEGIN
                  ;; strongest peak
                  centerx=x & centery=y
               END ELSE BEGIN
                  IF (i EQ 1) THEN BEGIN
                     strongest[slice,0]=b[2,1]
                     strongest[slice,1]=x-centerx
                     strongest[slice,2]=y-centery
                     strongest[slice,3]=Atan((y-centery),x-centerx)
                  END 
                  comx += (x-centerx)*b[2,i]
                  comy += (y-centery)*b[2,i]
                  comnorm += b[2,i]
               END
               IF (refinement EQ "Yes") THEN BEGIN
                  If NOT((fitdata[2] GT diameter) OR (fitdata[3] GT diameter)) THEN BEGIN
                     clusterlist.add, [x,y,slice*1.0,b[2:3,i], b[0:1,i],x-centerx,y-centery,fitdata]
                     refcount+=1
                  END ELSE BEGIN
                     rejcount+=1
                     IF NOT(quiet) THEN BEGIN
                        print, "rejecting peak ", i
                        print, "b=", b
                        print, "fitdata=",fitdata
                        STOP
                     END
                  END
               END ELSE clusterlist.add, [x,y,slice*1.0,b[2:3,i], b[0:1,i],x-centerx,y-centery,fitdata]
               
            END
            ;; Analyse for center of mass of diffraction pattern
            
         END
         com[slice, 0] = comnorm
         com[slice,1] = comx/comnorm
         com[slice,2] = comy/comnorm
         com[slice,3] = Sqrt(com[slice,1]*com[slice,1]+com[slice,2]*com[slice,2])
         com[slice,4] = atan(com[slice,2],com[slice,1])
      ENDFOR
   END 
   ELSE:
END
DestroyProgressBar
PrintToCon,"# ClusterAnalysis: Statistics"
PrintToCon,"    No of slices          = "+ MyString(NSlices)
PrintToCon,"    No of peak candidates = "+ MyString(peakcand)
PrintToCon,"    No of refined peaks   = "+ MyString(refcount)
PrintToCon,"    No of rejected peaks  = "+ MyString(rejcount)

;; IF (N_Elements(display)) THEN ThrowClusterImage, clusterlist
if keyword_set(framelist) THEN framelist=flist
if keyword_set(returncom) THEN returncom=com
if keyword_set(returnstrongest) THEN returnstrongest=strongest

return, clusterlist
END


function x_compare, v1, v2
  ; Return -1, 0, or 1 depending upon the name field
  return, ((v1)[0]).Compare((v2)[0])
end


FUNCTION NearestNeighbourList, inlist, maxdist, samp
;; returns an index list i,j for the nearest neighbours in inlist
;; inlist should be sorted for x coordinate!
  nn=list()
  IF (inlist.Count() GT 1) THEN BEGIN
     matched=BYTARR(inlist.Count()) ;; bookkeeping array , set element to 1B if aready matched
     For i=0L,inlist.Count()-1 Do BEGIN
        IF (matched[i] EQ 0) THEN BEGIN
           dist=maxdist
           FOR j=i+1,inlist.Count()-1 DO BEGIN
              IF (matched[j] EQ 0) THEN BEGIN
                 distx=(inlist[i])[0]-(inlist[j])[0]
                 IF (distx*samp[0] GT maxdist) THEN BEGIN
                    j=inlist.Count() ;; jump to finish the loop
                 END ELSE BEGIN
                    tmp=[distx,(inlist[i])[1]-(inlist[j])[1],(inlist[i])[2]-(inlist[j])[2]]*samp
                    tmp=SQRT(TOTAL(tmp*tmp))
                    if (tmp LT dist) THEN BEGIN
                       dist=tmp
                       nb=j
                    END
                 END 
              END 
           END 
           IF (dist LT maxdist) THEN BEGIN
              nn.Add, [i*1.,nb*1.,dist]
              matched[i]=1B
              matched[nb]=1B
           END
           ;; 
        END
     END
  END 
  return, nn
END 

FUNCTION NearestNeighbourList2, inarray, maxdist, samp
;; returns an index list i,j for the nearest neighbours in inlist
;; inlist should be sorted for x coordinate!
  nn=list()
  tmp=SIZE(inarray, /Dimensions)
  NElem=tmp[0]
  IF (NElem GT 1) THEN BEGIN
     matched=BYTARR(NElem) ;; bookkeeping array , set element to 1B if aready matched
     For i=0L,NElem-1 Do BEGIN
        IF (matched[i] EQ 0) THEN BEGIN
           dist=maxdist
           FOR j=i+1,NElem-1 DO BEGIN
              IF (matched[j] EQ 0) THEN BEGIN
                 distx=inarray[i,0]-inarray[j,0]
                 IF (distx*samp[0] GT maxdist) THEN BEGIN
                    j=NElem ;; jump to finish the loop
                 END ELSE BEGIN
                    disty=inarray[i,1]-inarray[j,1]
                    distz=inarray[i,2]-inarray[j,2]
                    tmp=[distx,disty,distz]*samp ;   the distance norm calculated
                                ;   non-isotropic sampling data can be
                                ;   the effective distance is the
                                ;   product of the distance vector times the
                                ;   sampling
;;                     STOP
                    tmp=SQRT(TOTAL(tmp*tmp))
                    if (tmp LT dist) THEN BEGIN 
                       dist=tmp
                       nb=j
                    END
                 END 
              END 
           END  
           IF (dist LT maxdist) THEN BEGIN
              nn.Add, [i*1.,nb*1.,dist]
              matched[i]=1B
              matched[nb]=1B
           END
           ;; 
        END
     END
  END 
  return, nn
END 

FUNCTION UniqueClusterDistanceMap, inlist, size, SAMP=samp, MAX=max
  ;; inlist:  list with cluster centres
  ;;          list elements: (x,y,z,mass,gyration, eccentricity)
  ;; size:    3D distance threshold vector 
  ;; Algorithm: iterate  - find nearest neighbour
  ;;                       if neaerest neighbour closer than distance
  ;;                       then merge list items
  ;;            until separation of nearest neighbour is larger than mindist
  outlist=list()
  cnt=inlist.Count()
  print, "Number of centers:", cnt
  IF (cnt LT 2) THEN return, inlist
  IF (N_Elements(samp) EQ 0) THEN samp=[1.,1.,1.]  
  IF (cnt GT 1) THEN BEGIN
     ;; sort the list
     print, "Sorting ..."
     sorted=inlist.Sort(COMPARE_FUNCTION='x_compare',/OVERWRITE)
     REPEAT BEGIN
        print, "Calculating nearest neighbour list ..."
        nn=NearestNeighbourList2(sorted.ToArray(), size, samp)
        rmlist=list()
        ;; remove pairs from inlist
        print, "Number of neighbour pairs:", nn.Count()
        print, "Collating ..."
        For k=0,nn.Count()-1 DO BEGIN

           i=LONG((nn[k])[0]) & j=LONG((nn[k])[1]) & dist=(nn[k])[2]
           massi = (sorted[i])[3] & massj=(sorted[j])[3]
           IF keyword_set(max) THEN BEGIN
              ;; only take the peak intensity
              IF (massj GT massi) THEN BEGIN
                 sorted[i]=sorted[j]
              END 
           END ELSE BEGIN
              ;; calculate centre of mass
              massnorm=massi+massj
              x=((sorted[i])[0]*massi+(sorted[j])[0]*massj)/massnorm
              y=((sorted[i])[1]*massi+(sorted[j])[1]*massj)/massnorm
              z=((sorted[i])[2]*massi+(sorted[j])[2]*massj)/massnorm
              sorted[i]=[x,y,z,massnorm,0.]
           END
           ;; replace element i
           rmlist.Add, j
        END
        if (rmlist.Count() GE 1) THEN BEGIN
           sorted.Remove, rmlist.ToArray()
        END
     END UNTIL nn.IsEmpty()
  END 
  print, "Remaining number of centers:", sorted.Count()
  return, sorted
END  

FUNCTION CleanClusterMap, inlist
  ;; ++++++++++++++++++++++++ parameter declaration ++++++++++++++++++++++++
  pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     ErrMsg, "Fatal error: parameter object is not defined." 
     return, 0 
  END
  diameter=pobj->get_token_value('CLUSTER','REFSIZE', ERRSTATE=err)
  sampx=pobj->get_token_value('CLUSTER','SAMPX', ERRSTATE=err)
  sampy=pobj->get_token_value('CLUSTER','SAMPY', ERRSTATE=err)
  sampz=pobj->get_token_value('CLUSTER','SAMPZ', ERRSTATE=err)
  repl=pobj->get_token_value('CLUSTER','REPL', ERRSTATE=err)
;; object is defined
hlp=["Refine Cluster Coordinates","","Remove clusters that are closer than a threshold distance","","Parameters:","Separation - The threshold for the nearest neighbour separation given in the same units as the pixel unit", "Sampling rate x,y,z - Pixel sampling rate. Used to calculate the cluster distance in pixels.","Note that the distance threshold becomes non-isotropic for non-isotropic sampling.","Increasing the sample size along one direction means a stretch of the distance along that dimension","and the distance threshold is reached with less pixels, less peaks will be rejected.","Replacement strategy - How to replace a pair that is too close. 0 = Keep cluster with higher mass, 1 = Use centre of mass.",""]
  mydialog=obj_new("ListDialog",TITLE="ClusterAnalysis")
  mydialog->AddOption,"Separation          ", diameter
  mydialog->AddOption,"Sampling rate x     ", sampx
  mydialog->AddOption,"Sampling rate y     ", sampy
  mydialog->AddOption,"Sampling rate z     ", sampz
  mydialog->AddOption,"Replacement strategy", repl
;;  mydialog->AddOption,"Iterate      ", iter
  IF (mydialog->UserDialog(HELP=hlp) GT 0) THEN BEGIN
     IF (mydialog->Value("Separation          ",x)) THEN diameter=x
     IF (mydialog->Value("Sampling rate x     ",x)) THEN sampx=x
     IF (mydialog->Value("Sampling rate y     ",x)) THEN sampy=x
     IF (mydialog->Value("Sampling rate z     ",x)) THEN sampz=x
     IF (mydialog->Value("Replacement strategy",x)) THEN repl=Byte(FIX(x))
  END
  obj_destroy, mydialog
  pobj->set_token_value, 'CLUSTER','REFSIZE',diameter
  pobj->set_token_value, 'CLUSTER','SAMPX', sampx
  pobj->set_token_value, 'CLUSTER','SAMPY', sampy
  pobj->set_token_value, 'CLUSTER','SAMPZ', sampz
  pobj->set_token_value, 'CLUSTER','REPL', repl
  pobj->report, KEY='CLUSTER'
;; ++++++++++++++++++++++++ end parameter declaration
  IF (repl EQ 0) THEN BEGIN
     return, UniqueClusterDistanceMap(inlist, diameter, SAMP=[sampx,sampy,sampz],/MAX)
  END ELSE BEGIN
     return, UniqueClusterDistanceMap(inlist, diameter, SAMP=[sampx,sampy,sampz])
  END
END


PRO RefineClusterMap, f, FROMTABLE=fromtable
  CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% RefineClusterMap:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END
  if keyword_set(fromtable) THEN BEGIN
   ptrTable=GetRootPTable()
   dTable=(*ptrTable).current
   IF NOT PTR_VALID(dTable) THEN BEGIN
      PrintToCon, "% RefineClusterMap:  Current data table not defined."
      return
   END
   list_struct=(*dTable).datap ;; poiter to the structure element in the list
   IF NOT(PTR_VALID(list_struct)) THEN BEGIN
      PrintToCon, "% RefineClusterMap:  Current data table container empty."
      return
   END
   mytable=(*list_struct).pobj
   IF NOT(OBJ_VALID(mytable)) THEN BEGIN
      PrintToCon, "% RefineClusterMap:  Table object invalid."
      return
   END
   ;; convert table data to array list 
   f=mytable->ToList(/SKIPINDEXCOL)
END
  ff=CleanClusterMap(f)
  mytable=TableImport2DArray(ff.ToArray(),TITLE="Cluster Data",COLUMNTITLES=["index","x","y","z","mass","gyration"])  
  IF OBJ_VALID(mytable) THEN BEGIN
     name="Cluster Table"
     newtable=GetNewTableContainer(Name)
     IF NOT(PTR_VALID(newtable)) THEN BEGIN
        ;; an error occured, delete the object
        ErrMsg, "NewTable: Could not create table list item."
        obj_destroy, mytable
     END ELSE BEGIN
        (*newtable).pobj=mytable
     END
  END
END

PRO ClusterAnalysis
CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% ClusterAnalysis:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     printtocon, "% Menu: Root pointer is invalid" 
     return
  END 
  porig=(*ptr).current
  b=["Refine","Redo Peak Search","Finish"]
  answer="Cancel"
  s=["","Do you want to proceed with the result and refine", "or redo the peak search or cancel the calculation?"]
  REPEAT BEGIN
     f=MyClusterAnalysis3D(DIM=dim,/NOISEFILT,/INTERACTIVE,/LIVE)
     if (f.Count() GT 10000) THEN BEGIN
        s[0]="Calculation result is a large number ("+MyString(f.Count())+")."
     END 
     answer=XMultiButtonDialog(s,b)
  END UNTIL ((answer EQ "Refine") OR (answer EQ "Finish"))
  ;; f is of type IDL list
  ;; each element is an array with 5 values x y z mass gyration
  ;; eccentricity"
  CASE answer OF
     "Refine": RefineClusterMap, f 
     ELSE: BEGIN
        mytable=TableImport2DArray(f.ToArray(),TITLE="Cluster Data",COLUMNTITLES=["index","x","y","z","mass","gyration"])  
        IF OBJ_VALID(mytable) THEN BEGIN
           name="Cluster Table"
           newtable=GetNewTableContainer(Name)
           IF NOT(PTR_VALID(newtable)) THEN BEGIN
              ;; an error occured, delete the object
              ErrMsg, "NewTable: Could not create table list item."
              obj_destroy, mytable
           END ELSE BEGIN
              (*newtable).pobj=mytable
           END
        END
     END
  ENDCASE
  return
END  


Pro MapClusterData
    CATCH, Error_status
IF NOT(GetDebugFlag()) THEN BEGIN
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% MapClusterData:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END 
ptrTable=GetRootPTable()
   dTable=(*ptrTable).current
   IF NOT PTR_VALID(dTable) THEN BEGIN
      PrintToCon, "% MapClusterData:  Current data table not defined."
      return
   END
   list_struct=(*dTable).datap ;; poiter to the structure element in the list
   IF NOT(PTR_VALID(list_struct)) THEN BEGIN
      PrintToCon, "% MapClusterData:  Current data table container empty."
      return
   END
   mytable=(*list_struct).pobj
   IF NOT(OBJ_VALID(mytable)) THEN BEGIN
      PrintToCon, "% MapClusterData:  Table object invalid."
      return
   END
   ;; convert table data to array list 
   f=mytable->ToList(/SKIPINDEXCOL)
   if (f.count() GE 1) THEN BEGIN
      ThrowClusterImage, f, /CROSS
   END ELSE BEGIN
       PrintToCon, "% MapClusterData: Empty cluster list."
   END
END



PRO ProcessClusterTable, POWDERPLOT=powderplot, DEBYESCHERRER=debyescherrer, XSIZE=xsize, YSIZE=ysize, THRESHOLD=threshold, HISTBINSIZE=histbinsize, DETSAMPX=detsampx, DETSAMPY=detsampy, MAPORIENT=maporient, DISTRANGE=distrange, ROTOFFS=rotoffs, SUBSET=subset, BRAGGPEAKS=braggpeaks, DYNAMICSCATT=dynamicscatt, SHEAR=shear, ELLIP=ellip
  ;;
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% RefineClusterMap:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      XConsole_PopState
      CATCH, /Cancel
    ErrMsg, !ERROR_STATE.MSG
    return
 END
END
  ;;
  ptr=GetRootPTable()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     Printtocon, "% ProcessClusterTable: No table entry found."
     return
  END
  d=(*ptr).current
  IF NOT(PTR_VALID(d)) THEN BEGIN
     Printtocon, "% ProcessClusterTable: No data linked to current table entry."
     return
  END
  t=(*d).datap
  IF NOT(PTR_VALID(t)) THEN BEGIN
     Printtocon, "% ProcessClusterTable: No table linked to current table entry."
     return
  END
  mytable=(*t).pobj
  IF NOT(OBJ_VALID(mytable)) THEN BEGIN
     Printtocon, "% ProcessClusterTable: Table object invalid."
     return
  END
  if NOT(keyword_set(detsampx)) THEN detsampx=1.
  if NOT(keyword_set(detsampy)) THEN detsampy=1.
  ;; get table data into array
  peakdata=FLTARR(mytable->NRows(),mytable->NColumns())
  collabels=*(mytable->GetColumnLabels())
;      4  - peak search mass   Constant baseline level
;      10 - Par(0)   Constant baseline level
;      11 - Par(1)   Peak value, fitted
;      12 - Par(2)   Peak half-width (x) -- gaussian sigma or half-width at half-max
;      13 - Par(3)   Peak half-width (y) -- gaussian sigma or half-width at half-max
;      14 - Par(4)   Peak centroid (x)
;      15 - Par(5)   Peak centroid (y)
;      16 - Par(6)   Rotation angle (radians) if TILT keyword set
  For i=0,N_Elements(collabels)-1 DO peakdata[*,i]=mytable->GetColumnData(collabels[i])
;; --------------------------------------------------------
;;
;; FIRST FILTER
;; - skip all data where peaks detected beyond the frame
;;   coordinates
;; - skip all peaks below threshold
;;
;; --------------------------------------------------------
  
  sel=INDGEN(mytable->NRows(),/LONG)
  if keyword_set(xsize) THEN BEGIN
     B=Where((peakdata[*,1] GT xsize) OR (peakdata[*,1] LT 0),count)
     IF (count GE 1) THEN sel[B]=-1
  END
  if keyword_set(ysize) THEN BEGIN
     B=Where((peakdata[*,2] GT ysize) OR (peakdata[*,2] LT 0),count)
     IF (count GE 1) THEN sel[B]=-1
  END
  if keyword_set(threshold) THEN BEGIN
     PrinttoCon, "% ProcessClusterTable: Cutting off below threshold = "+MyString(threshold)
     ;; B=Where((peakdata[*,11] LT threshold),count) ;; gaussian fit peak value
     B=Where((peakdata[*,11] LT threshold),count) ;; gaussian fit peak value
     IF (count GE 1) THEN BEGIN
        sel[B]=-1
        PrinttoCon, "%     Found "+MyString(count)+" peaks below threshold."
     END ELSE PrinttoCon, "%     Found no peaks below threshold."
  END
  ;; Finish selection
  B=WHERE(sel GT -1,count)
  IF (count GE 1) THEN sel=sel[B]
  nrows=N_Elements(sel)
  ;;;
 ;; --------------------------------------------------------
;;
;; readjust coordinates to the refined frame center
;;   
;; - skip all peaks below threshold
;; - compensate for image/diffraction rotation
;;   rotoffs is the rotation angle in radians
;;   if positive then the rotation of the diffraction data
;;   is ccw relative to the image  
;;
;; --------------------------------------------------------
  cx=0. & cy=0.

  anisotropy=detsampy/detsampx
 
  ;; recalculate positions relative to the refined centre

  ;; it is expected that the diffraction pattern center is in the first frame
  ;; in most cases of kinematic diffraction this will be the case, however in the case of dynamic  diffraction we may
  ;; pre-sort the array to find the strongest peak in a search range around the central pixel

  If keyword_set(dynamicscatt) THEN BEGIN
     if (not(keyword_set(xsize)) Or not (keyword_set(xsize))) THEN BEGIN
        printtocon, "% ProcessClusterTable: Frame sizes not set."
        return
     END
     framecx=FIX(xsize/2) & framecy=FIX(ysize/2)
     ;;
     slicedata=peakdata[sel[*],3] ;; Isolate frame indices
     uniqind=uniq(slicedata) ;; get uniq frame indices, the index chosen is of the last element, i.e. the one with the hIghest intesity
     ;; loop through the uniq slices,
     NFrames=N_Elements(uniqind)
     mybar=obj_new('ProgressBar', MINV=[0L],MAXV=[LONG(NFrames-1)], TEXT=["Refining centre with subpixel accuracy"], STATUS="Optimising on peak closest to the frame centre.", FRAMESTYLE=2)
     rangelo=0L
     for j=0,N_Elements(uniqind)-1 DO BEGIN
        mybar->Set, 0, j-1, TEXT=STRING(uniqind[(j-1)])
        rangehi=LONG(uniqind[j])
        if (rangehi GT rangelo) THEN BEGIN ;; skip single peak frames
           framedata=peakdata[rangelo:rangehi,*]
           ;;; analyse framedata
           ;; sort according to distance from center, use  framedata[*,16] to calculate distance to central pixel
           ;;
           framedata[*,16]=(framedata[*,14]-framecx)^2+(framedata[*,15]-framecy)^2
           ;; sort framedata 
           C=Sort(framedata[*,16])
           cx=framedata[C[0],14] & cy=framedata[C[0],15] ;; set center peak to the one closest to the center          
           ;; then iterate as long
           peakdata[rangelo:rangehi,1]-=cx & peakdata[rangelo:rangehi,2]-=cy
           peakdata[rangelo:rangehi,14]-=cx & peakdata[rangelo:rangehi,15]-=cy
        END
        rangelo=rangehi+1
     END    
   
      obj_destroy, mybar
   END  ELSE BEGIN
     index=0L
     ;;; index runs over all elements of sel
     slice=LONG(peakdata[sel[index],3]) - 1 ;; -1 since the loop has no previous member for the first slice 
      mybar=obj_new('ProgressBar',MINV=[0L], MAXV=[LONG(nrows-1)], TEXT=["Refining centre with subpixel accuracy"], STATUS="Optimizing on strongest peak location.")
     WHILE (index LT nrows) DO BEGIN
        mybar->Set, 0, index, TEXT=STRING(index)
        IF  (FIX(peakdata[sel[index],3]) NE slice) THEN BEGIN
           slice=FIX(peakdata[sel[index],3])
           cx=peakdata[sel[index],14] & cy=peakdata[sel[index],15] ;;; peak position of strongest peak
        END
        peakdata[sel[index],1]-=cx & peakdata[sel[index],2]-=cy
        peakdata[sel[index],14]-=cx & peakdata[sel[index],15]-=cy
        ;; distances are normalized to spacings along x in pixels
        index+=1
     END
 obj_destroy, mybar
  END
  
   IF (keyword_set(shear)) THEN BEGIN
;; shear transformation required
;; General form
;; shear along x is equivalent with theta=0
;; parameters eta, theta     
;; eta > 0 - will shear along the horizontal coordinate for theta = 0, upper half to the right
;;  eta > 0 - will shear along the horizontalvertical coordinate for theta = 90, upper half to the right right side up    
;;       
      eta=shear[0] & theta=shear[1]
      printtocon, ["% ProcessClusterTable: Correcting shear distortion.","     eta, theta = "+MyString(eta)+" ,"+MyString(theta)]
      Sin2T=Sin(2*theta) & Cos2T=Cos(2*theta)  
      T11=1.+0.5*eta*Sin2T & T12=0.5*eta*(1.+Cos2T)
      T21=0.5*eta*(Cos2T-1.) & T22=1.-0.5*eta*Sin2T
      ;; Det=1.+0.25*eta*eta*(1.-Cos2T*Cos2T-Sin2T*Sin2T) =1.
      ;; transform
      tmp=peakdata[*,1] 
      peakdata[*,1] = T11*tmp + T12*peakdata[*,2]
      peakdata[*,2] = T22*peakdata[*,2]
      peakdata[*,2] += T21*tmp      
   END


  IF (keyword_set(ellip)) THEN BEGIN
;; shearelliptical transformation required
;; General form
;; shearlong axis is  along x is equivalent with theta=0
;; parameters eta, theta     
;; eta > 0, theta=0  - will stretch  along the horizontal coordinate 
;; eta > 0, theta=45  - will stretch  along the horizontal coordinate     
;; 
     Eta=ellip[0] & theta=ellip[1]
     printtocon, ["% ProcessClusterTable: Correcting elliptical distortion.","     eta, theta = "+MyString(eta)+", "+MyString(theta)]
      SinT=Sin(theta) & CosT=Cos(theta)  
      Sin2T=Sin(2*theta) & Cos2T=Cos(2*theta)  
      T11=1.+eta*Cos2T & T12=-2*eta*(Cos2T*SinT)
      T22=1.-eta*Cos2T
      Det=1.-eta*eta
      T11=T11/Det & T12=T12/Det & T21=T12 & T22=T22/Det
      printtocon, ["    TMatrix |A  B, C  D| = |" +MyString(T11)+" "+MyString(T12)+", " +MyString(T21)+" "+MyString(T22)+"|"]
      ;; transform
      tmp=peakdata[*,1] 
      peakdata[*,1] = T11*tmp + T12*peakdata[*,2]
      peakdata[*,2] = T22*peakdata[*,2]
      peakdata[*,2] += T21*tmp      
   END
   

  If keyword_set(rotoffs) THEN BEGIN
     printtocon, "% ProcessClusterTable: Correcting rotation offset."
     IF (rotoffs NE 0) THEN BEGIN
        cosa=cos(rotoffs)
        sina=sin(rotoffs)
        If (rotoffs GT 0) THEN BEGIN
           printtocon, "    Rotating diffraction data clockwise by "+MyString(rotoffs/!DPI*180.)+" degrees."
        END ELSE BEGIN
           printtocon, "    Rotating diffraction data counterclockwise by "+MyString(rotoffs/!DPI*180.)+" degrees."
        END 
        tmp=peakdata[*,1]
        peakdata[*,1] = tmp*cosa + sina*peakdata[*,2]
        peakdata[*,2] = cosa*peakdata[*,2]
        peakdata[*,2] -= sina*tmp
     END
  END 
;; -----------------------------------------------
;;
;; DEBYESCHERRER
;;
;; -----------------------------------------------
  If keyword_set(debyescherrer) THEN BEGIN
 
     s = PLOT(peakdata[sel,1]*detsampx, peakdata[sel,2]*detsampy, $
              SYMBOL = 'dot', $
              /SYM_FILLED, $
              ASPECT_RATIO=1., $
              SYM_COLOR = 'blue', $
              LINESTYLE = 'none', $
              XTITLE = 'x centroid', $
              YTITLE = 'y centroid', $
              TITLE = 'Peak centroid scatter plot')
  END
;; -----------------------------------------------
;;
;; braggpeaklist
;;
;; -----------------------------------------------
  If keyword_set(braggpeaks) THEN BEGIN
     NPeaks=N_Elements(peakdata[sel,1])
     braggpeaks=FLTARR(6,NPeaks)
     braggpeaks[0,*]=peakdata[sel,3] ;; the frame index
     braggpeaks[1,*]=peakdata[sel,1]*detsampx ;; calibrated momentun data in x and y
     braggpeaks[2,*]=peakdata[sel,2]*detsampy
     braggpeaks[3,*]=peakdata[sel,1]  ;; uncalibrated momentun data in x and y
     braggpeaks[4,*]=peakdata[sel,2]
     braggpeaks[5,*]=peakdata[sel,4]  ;; peak mass, non-refined data
     s = PLOT(braggpeaks[1,*], braggpeaks[2,*], $
              SYMBOL = 'dot', $
              /SYM_FILLED, $
              ASPECT_RATIO=1., $
              SYM_COLOR = 'blue', $
              LINESTYLE = 'none', $
              XTITLE = 'x centroid', $
              YTITLE = 'y centroid', $
              TITLE = 'Peak centroid scatter plot')
     ;; export to a json string
     ;; formatting:
     ;;
     ;; get the unique indices
     ;; braggpeaks[0,*] are the frame indices and sorted
     ;; Note that the index of the last element in each set of non-unique elements is returned. 
     uniqframes=uniq(braggpeaks[0,*])
     rangelo=0
     h=orderedhash()
     for i=0,N_Elements(uniqframes)-1 DO BEGIN
        rangehi=uniqframes[i]
        if (rangehi GT rangelo) THEN BEGIN ;; skip single peak frames
        ;;; braggpeaks[0,rangelo:rangehi] is the index range for one frame
        ;;; braggpeaks[1,rangelo:rangehi] are the calibrated qx values for all peaks with the framenumber braggpeaks[0,rangelo]
           l=list()
           For j=rangelo,rangehi DO l.Add,braggpeaks[1:5,j]
           h(LONG(braggpeaks[0,rangelo]))=l
        END
        rangelo=rangehi+1
     END
     ;; Export to file

     ;; declare file type list here and hand it over to XFileDialog!
     tlist=PTR_NEW(['JSON'])
     
     ftype=0 & swp=0 & grp=0 & savefilter='*.json'
     IF (ftype GE N_ELEMENTS(*tlist)) THEN ftype=0
;; declare file type list here and hand it over to XFileDialog!
     cancelled=0
     wdir="."
     files = rmd_pickfile(title="ProcessClusterTable::Export", $
                          filter_in = savefilter,              $
                          filter_out = savefilter,              $
                          path = wdir,          $
                          get_path = wdir,          $
                          cancelled = cancelled,        $
                          swapen = swp,             $
                          type = ftype,             $
                          ftypes = tlist,     $
                          crgrp = 0,             $
                          /save)
     if (cancelled EQ 1) then begin
        printtocon, '% ProcessClusterTable::Export: User cancelled file dialog!'
        return
     END
     if (files[0] EQ '') THEN BEGIN
        printtocon, '% ProcessClusterTable::Export: User cancelled file dialog!'
        return
     END
     fn=files[0]

     lun=-1
     PrintToCon, "% ProcessClusterTable::Export: Data export to "+fn 
     GET_LUN, lun
     OPENW, lun, fn
     ;;
     s=''
     FOREACH vecs, h, key DO Begin
        tmp=JSON_SERIALIZE(hash(MyString(key), h[key]))
        tmp=tmp.Remove(0,0)
;; delete leading and trailing brackets
        tmp=tmp.Remove(-1,-1)
        s+=tmp+','
     END
     s=s.Remove(-1,-1)
;; remove last comma
     PRINTF, lun, '{'+s+'}'
     ;; STOP
     printtocon, '%  Wrote peak data for ' + MyString(N_Elements(h.keys())) + ' frames.'
     IF (lun GE 0) THEN BEGIN
         FREE_LUN, lun
      END 
     ;;
  END
;; -----------------------------------------------
;;
;; POWDERPLOT
;;
;; -----------------------------------------------
  If keyword_set(powderplot) THEN BEGIN
     ;; rows are sorted
     ;; a sequence of frame indix, peak mass in descending order
     ;; filter data first

     ;; Apply selection
     peakdata=peakdata[sel,*]
     ;; 
     nrows=LONG(N_Elements(peakdata[*,1]))
     
     index=0L
     slice=LONG(peakdata[index,3])
     distances=FLTARR(nrows)
     arg=FLTARR(nrows)
     cx=0. & cy=0.
     InitProgressBar, MINV=[0L], MAXV=[LONG(nrows-1)], TEXT=["Mapping distances and angles"]
     anisotropy=detsampy/detsampx
     WHILE (index LT nrows) DO BEGIN
        ProgressBarStep, 0, TEXT=STRING(index)
        ydist=anisotropy*peakdata[index,2]
        distances[index]=Sqrt(peakdata[index,1]*peakdata[index,1]+ydist*ydist)
        arg[index]=180./!DPI*ATAN(ydist,peakdata[index,1])
        ;; distances are normalized to spacings along x in pixels
        index+=1
     END
     DestroyProgressBar
     
     IF NOT(keyword_set(histbinsize)) THEN binsize=histbinsize ELSE binsize=0.3
     eventpdf = HISTOGRAM(distances, BINSIZE=binsize, LOCATIONS=xbin, REVERSE_INDICES = R)
     ;; PLOT, xbin, eventpdf, TITLE='Histogram of distances', XTITLE='Pixel value', YTITLE='Frequency', BACKGROUND=0
     cumulativesig=DOUBLE(eventpdf)
     peakdata[*,16]=peakdata[*,11]*peakdata[*,12]*peakdata[*,13]*!DPI ;;; integral of 2D gaussian
     
     For i=0, N_ELEMENTS(xbin)-1 DO BEGIN
        ind=cgreverseindices(R,i,Count=count)
        ;; IF (count GT 0) THEN cumulativesig[i]=Total(peakdata[ind,4]) ELSE cumulativesig[i]=0.
        ;; 4 = feature mass
        ;; 11 = peak amp
        ;; 16 = peak intensity
        IF (count GT 0) THEN cumulativesig[i]=Total(peakdata[ind,16]) ELSE cumulativesig[i]=0.
     END
     ;;  PLOT, xbin, cumulativesig, TITLE='Pseudo Powder Plot', XTITLE='Pixel value', YTITLE='Sum signal', BACKGROUND=0
     ;; STOP
     mygraph=obj_new('XYGraph', "DistanceHistogramData", XTITLE="Distance", YTITLE="Frequency")
     IF OBJ_VALID(mygraph) THEN BEGIN
        mygraph->AddSet, "DistanceHistogramData", xbin[1: N_ELEMENTS(xbin)-1]*detsampx, eventpdf[1: N_ELEMENTS(xbin)-1], PSYM=0, SYMSIZE=0.0001, /LINE, COLOR=RGBIndexColor(0,/LIGHT)
        ;; add graph to graph container
        newgraph=GetNewXYGraphContainer("DistanceHistogramData")
        IF NOT(PTR_VALID(newgraph)) THEN BEGIN
           ;; an error occurred, delete the object
           ErrMsg, "Could not create graph list item"
           mygraph->TidyUp
           obj_destroy, mygraph
        END ELSE BEGIN
           (*newgraph).pobj=mygraph
           mygraph->UpdateDisplay
        END
     END 
     
     mygraph=obj_new('XYGraph', "PseudoPowderData", XTITLE="Distance", YTITLE="Sum of amplitudes")
     IF OBJ_VALID(mygraph) THEN BEGIN
        mygraph->AddSet,"PseudoPowderData", xbin[1: N_ELEMENTS(xbin)-1]*detsampx, cumulativesig[1: N_ELEMENTS(xbin)-1], PSYM=0, SYMSIZE=0.0001, /LINE, COLOR=RGBColor(200,0,20)
        ;; add graph to graph container
        newgraph=GetNewXYGraphContainer("PseudoPowderData")
        IF NOT(PTR_VALID(newgraph)) THEN BEGIN
           ;; an error occurred, delete the object
           ErrMsg, "Could not create graph list item"
           mygraph->TidyUp
           obj_destroy, mygraph
        END ELSE BEGIN
           (*newgraph).pobj=mygraph
           mygraph->UpdateDisplay
        END
     END

     mygraph=obj_new('XYGraph', "Polar Plot", XTITLE="Angle", YTITLE="distance")
     IF OBJ_VALID(mygraph) THEN BEGIN
        mygraph->AddSet,"PolarPlotData", arg, distances*detsampx, PSYM=1, SYMSIZE=0.1, LINE=0, COLOR=RGBColor(20,200,200)
        ;; add graph to graph container
        newgraph=GetNewXYGraphContainer("PseudoPowderData")
        IF NOT(PTR_VALID(newgraph)) THEN BEGIN
           ;; an error occurred, delete the object
           ErrMsg, "Could not create graph list item"
           mygraph->TidyUp
           obj_destroy, mygraph
        END ELSE BEGIN
           (*newgraph).pobj=mygraph
           mygraph->UpdateDisplay
        END
     END  
  END 
  ;; -----------------------------------------------
;;
;; OrientationMap:
;; - Filter specific lattice spacings and return
;;   array of indices, amplitude, argument
;;
;; -----------------------------------------------
  If keyword_set(maporient) THEN BEGIN
     ;; rows are sorted
     ;; a sequence of frame indix, peak mass in descending order
     ;; filter data first
     peakdata=peakdata[sel,*]
     ;; 
     nrows=LONG(N_Elements(peakdata[*,1]))
     
     index=0L
     slice=LONG(peakdata[index,3])
     distances=FLTARR(nrows)
     arg=FLTARR(nrows)
     InitProgressBar, MINV=[0L], MAXV=[LONG(nrows-1)], TEXT=["Mapping distances and angles"]
     anisotropy=detsampy/detsampx
     WHILE (index LT nrows) DO BEGIN
        ProgressBarSet, 0, index, TEXT=STRING(index)
        ydist=anisotropy*peakdata[index,2]
        distances[index]=Sqrt(peakdata[index,1]*peakdata[index,1]+ydist*ydist)
        arg[index]=180./!DPI*ATAN(ydist,peakdata[index,1]) ;; atan(y,x) 
        ;; distances are normalized to spacings along x in pixels
        index+=1
     END
     DestroyProgressBar
     ;; Filter distances
     ;; 
     IF not(keyword_set(distrange)) THEN BEGIN
        distrange=[5,Max(distances)]
        Printtocon, "% ProcessClusterTable: No distance range defined, setting it to 5 to "+MyString(Max(distances))+" pix"
     END
     ;; reduce dataset to the frames with reflections in the distance range
     sel=Where((distances GE distrange[0]) AND (distances LT distrange[1]), count)
     IF (count LT 1) THEN BEGIN
        Printtocon, "% ProcessClusterTable: No peaks found in the search range."
        return
     END ELSE BEGIN
        Printtocon, "% ProcessClusterTable: Found "+MyString(count)+" peaks in the search range."
     END 
     ;; proceed with sub list
     filtpeakdata=peakdata[sel,*]
     Distances=distances[sel]
     arg=arg[sel]
     ;; reverse order, from weakest to strongest, the uniq function picks the last entry
     filtpeakdata=Reverse(filtpeakdata,1)
     distances=REVERSE(distances)
     arg=REVERSE(arg)
     ;; 
     slicedata=filtpeakdata[*,3] ;; isolate frame indices
     uniqind=uniq(slicedata) ;; get uniq frame indices, the index chosen is of the last element, i.e. the one with the hIghest intesity
     filtpeakdata=filtpeakdata[uniqind,*] ;; continue with unique frame selection and the strongest peak in the distance Range
     filtpeakdata=Reverse(filtpeakdata,1) ;; reverse back to original order
     distances=distances[uniqind]
     arg=arg(uniqind)
     distances=REVERSE(distances)
     arg=REVERSE(arg)
     res=FLTARR(N_ELEMENTS(distances),4)
     res[*,0]=filtpeakdata[*,3]
     res[*,1]=distances
     res[*,2]=arg
     res[*,3]=filtpeakdata[*,4] ;; alternative would be index 11
     ;; plot, res[*,0],res[*,1], BACKGROUND=0 ;; distances against slice index
     ;; plot, res[*,0],res[*,2], BACKGROUND=0 ;; angle in degrees  against slice index
     ;; plot, res[*,0],res[*,3], BACKGROUND=0 ;; peak mass  against slice index
     maporient=res ;; return data in maprient
     sel=uniqind
  END
  IF keyword_set(subset) THEN subset=sel ;; return frame selection for subset creation
END   
