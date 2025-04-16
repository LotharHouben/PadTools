;; MODIFICATONS
;; Contrast handling from version 5.5 on:
;; 
;; In general only TVDisplay sets contrast modes
;; - TVDisplay calls MyTVSCL which itself will
;;   * in manual mode set the contrast according to the settings in the stack meta data
;;   * in automatic mode set the contrast according to the mode settings using the routine SetAutoContrast in DataStackContrast
;; 
;; There should be no need to adjust contrast settings in a different manner, unless the data stack is not displayed 
;;
;; Obtaining contrast settings 
;; (1) Read contrast setting from data stack descriptor: Use GetStackContrastMode
;; (2) Use GetStackContrastValues to get the high and low cutoffs
;; (note  MyTVSCL uses GetStackContrastMode!)
;;
;; Setting contrast settings for a stack
;; (1) Use the ContrastInspector
;; (2) Use the procedure SetStackContrastMode to set parameters
;;     Syntax: PRO SetStackContrastMode, AUTO=auto, MANUAL=manual, SDEV=sdev,
;;     MDEV=mdev, MINMAX=minmax, QUARTER=quarter, FULL=full,
;;     SRANGE=srangeSetStackContrastMode, STACKP=stackp,
;;     AUTO=auto, MANUAL=manual, SDEV=sdev, MDEV=mdev, MINMAX=minmax,
;;     QUARTER=quarter, FULL=full, SPARSE=sparse, SRANGE=srange,
;;     HILO=hilo, DIFF=diff, HISTEQUAL=histequal

FUNCTION SHOWWIN, wdow_nr
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% ShowWin: Error while displaying image with index " + MyString(wdow_nr)
    Print, "%            " + !ERR_STRING
    Print, "%            Need to reopen window?"
;;     (*winp).wdow = -1
    CATCH, /Cancel
    return, -1
END
WSHOW, wdow_nr, 1
return, wdow_nr
END


PRO SetContrastPrefs
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
frame='quarter' ;; quarter frame contrast evaluation
;; frame='fullframe' 
;; mode='twosdev' ;; 2 times standard deviation
mode='minmax'  ;; minimum-maximium
;; mode='userminmax' ;; minimum, maximum given by user
low=0.
hi=1.
scaling='lin'
gma=0.65
END

FUNCTION GetContrastFrame
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
 return, frame
END

PRO SetContrastFrame, m
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
frame=m
END


FUNCTION GetContrastLowHi
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
return, [low,hi]
END

PRO SetContrastLowHi, l, h
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
low=l
hi=h
END


PRO SetContrastMode, m
COMMON PCONTRAST, mode, autoval
;; possible values: 'auto' or 'manual'
mode=m
END

FUNCTION GetContrastMode
COMMON PCONTRAST, mode, autoval
;; possible values: 'auto' or 'manual'
return, mode
END

FUNCTION GetContrastScaling, GAMMA=gamma
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
;; possible values: 'lin' or 'log' or 'exp'
IF keyword_set(gamma) THEN gamma=gma
return, scaling
END

PRO SetContrastScaling, v, GAMMA=gamma
COMMON CONTRAST, frame, mode, low, hi, scaling, gma
;; possible values: 'lin' or 'log' or 'exp' 
IF keyword_set(gamma) THEN gma=gamma
scaling=v
END

PRO SetAutoContrastValues, v
COMMON PCONTRAST, mode, autoval
autoval=v
END

FUNCTION GetAutoContrastValues, v
COMMON PCONTRAST, mode, autoval
return, autoval
END


Function TVBin, x, y, MAXIMISE=maximise, MAXFRAC=maxfrac, MAXZOOM=maxzoom
  ;;sizex=1600 & sizey=1284
  ssize=GET_SCREEN_SIZE(RESOLUTION=resolution)
  sizex=ssize[0] & sizey=ssize[1]
  IF not(keyword_set(maxfrac)) THEN maxfrac=0.66667
  IF keyword_set(maximise) THEN BEGIN
     IF ((x LT sizex) AND (y LT sizey)) THEN BEGIN
        binx=FLOOR(FLOAT(sizex)*maxfrac/FLOAT(x)) ;;; this is how much we can scale, binning factor is the inverse, the smaller factor counts
        biny=FLOOR(FLOAT(sizex)*maxfrac/FLOAT(x))
        if (binx LT biny) THEN return, 1./binx ELSE return, 1./biny 
     END ELSE BEGIN
        binx=CEIL(FLOAT(x)/(FLOAT(sizex)*maxfrac))
        biny=CEIL(FLOAT(y)/(FLOAT(sizey)*maxfrac))
     END
  END ELSE BEGIN
;; image display may extend to a maximum of 2/3 of the screen size
     binx=CEIL(FLOAT(x)/(FLOAT(sizex)*maxfrac))
     biny=CEIL(FLOAT(y)/(FLOAT(sizey)*maxfrac))
  END
;;
  if keyword_set(maxzoom) THEN BEGIN
     t=1./maxzoom
     if (binx LT t) THEN binx=t
     if (binx LT t) THEN biny=t
  END
  if (binx GT biny) THEN return, binx ELSE return, biny 
END 

FUNCTION WinPosX, SX
;; generates a random x position on the screen 
COMMON DISPLAYSCOPE, offsetx, offsety, sizex, sizey, MonNum, MonNames, MonRects, MonPrim
cascadeX=FIX(((randomu(seed,1)*0.25)+0.25)*sizex)
return, cascadeX[0]
END 

FUNCTION WinPosY, SY
COMMON DISPLAYSCOPE, offsetx, offsety, sizex, sizey, MonNum, MonNames, MonRects, MonPrim
cascadeY=FIX(((randomu(seed,1)*0.25)+0.25)*sizey)
return, cascadeY[0]
END 

Pro CreateWindow, BIN=bin, XBin=xbin, YBin=ybin, ZBin=zbin, TITLE=title
;;
;; create a display window for the current pointer
;; if redisplay is set, only existing windows will be reopened
;; 
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% CreateWindows:  Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END 
ptr=GetRootP()
IF (NOT(PTR_VALID(ptr))) THEN BEGIN
   print, "% CreateWindow: Root pointer is invalid"
   return
END
c=(*ptr).current
IF (NOT(PTR_VALID(c))) THEN BEGIN
   print, "% CreateWindow: Current stack pointer is invalid"
   return
END
mytitle=""
p=(*c).datap
IF Ptr_Valid(p) THEN BEGIN
   IF ((*p).window GT -1) THEN WDelete, (*p).window
   CASE (*p).zcoord OF
      1: BEGIN
         SzHoriz=(*p).SzY & SzVert=(*p).SzZ & mytitle="YZ"
      END
      2: BEGIN
         SzHoriz=(*p).SzX & SzVert=(*p).SzZ & mytitle="XZ"
      END
      3: BEGIN
         SzHoriz=(*p).SzX & SzVert=(*p).SzY  & mytitle="XY"
      END
      ELSE: BEGIN
         print, "% CreateWindow: Invalid projection specifier"
         return
      END 
   END
   IF NOT(KEYWORD_SET(bin)) THEN bin=TVBin(SzHoriz,SzVert)  
   mytitle=mytitle+' 1:'+STRING(0.5,FORMAT='(D0.2)')+' '+((*p).id)
   Window, /FREE,TITLE=mytitle, XSIZE=FIX(SzHoriz/bin), YSIZE=FIX(SzVert/bin), XPOS=WinPosX(), YPOS=WinPosY()
   (*p).window = !D.Window
   (*p).binning = bin
END ELSE BEGIN
   printtocon, "% CreateWindow: invalid pointer"
   return
END 
return
END 




FUNCTION GetContrast
  lo=0. & hi=1.
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     PrintToCon, "% GetContrast: Invalid root pointer."
     return, [lo,hi]
  END
  c=(*ptr).current
  IF (NOT(PTR_VALID(c))) THEN BEGIN
     PrintToCon, "% GetContrast: Invalid data stack pointer."
     return, [lo,hi]
  END
  e=(*c).datap
  IF NOT(PTR_VALID(e)) THEN BEGIN
     PrintToCon, "% GetContrast: Invalid data pointer."
     return, [lo,hi]
  END
  return, (*e).contrast
  ;; print, "% GetContrast: Contrast values = ", (*e).contrast
END 

PRO TVSubFrameDirect, e, frm, Lo=lo, Hi=hi
  IF NOT(PTR_VALID(e)) THEN BEGIN
     PrintToCon, "% TVSubFrameDirect: Invalid data pointer."
     return
  END
  IF NOT(PTR_VALID((*e).data)) THEN BEGIN
     PrintToCon, "% TVSubFrameDirect: Invalid data array pointer."
     return
  END
  frm[1]=FIX(frm[0]+ FLOOR((frm[1]-frm[0]+1) / (*e).binning) * (*e).binning -1)
  frm[3]=FIX(frm[2]+ FLOOR((frm[3]-frm[2]+1) / (*e).binning) * (*e).binning -1)
  Case (*e).zcoord OF
     1: BEGIN             
        image=REFORM((*(*e).data)[(*e).slice,frm[0]:frm[1],frm[2]:frm[3]])
     END
     2: BEGIN             
        image=REFORM((*(*e).data)[frm[0]:frm[1],(*e).slice,frm[2]:frm[3]])
     END
     3: BEGIN             
        image=REFORM((*(*e).data)[frm[0]:frm[1],frm[2]:frm[3],(*e).slice])
     END
     ELSE: BEGIN
        PrintToCon, "% TVSubFrameDirect: Invalid projection."
        return
     END
  END 
  ;; bin the image
  Sz=SIZE(image, /DIMENSIONS)
  image=Rebin(image, FLOOR(Sz(0)/(*e).binning),FLOOR(Sz(1)/(*e).binning))
  IF NOT(keyword_set(lo) AND (keyword_set(hi))) THEN BEGIN
     ;; set constrast
     mode=GetContrastMode()
     CASE mode OF
        'minmax': BEGIN
              lo=MIN(image,MAX=hi)
           END
        'mdev': BEGIN
           result=Moment(image,MDEV=mdev)
           lo=result(0)-mdev
           hi=result(0)+mdev
        END
        'twosdev': BEGIN
           result=Moment(image,MAXMOMENT=2)
           sdev=Sqrt(result(1))
           lo=result(0)-2*sdev
           hi=result(0)+2*sdev
        END
        'userminmax': BEGIN   
           lohi=GetContrastLowHi()
           lo=lohi[0] & hi=lohi[1]
        END
        'diff': BEGIN   
           result=Moment(image,MAXMOMENT=2)
           sdev=Sqrt(result(1))
           hi=result(0)+2*sdev
           lo=-hi/10.
        END
        'histequal': BEGIN   
           hilo=CumulativeHistogramEqual(image, Min=0.1, Max=0.95)
           lo=hilo[0] & hi=hilo[1]
        END
        ELSE: BEGIN
           lo=MIN(image,MAX=hi)
        END
     END
  END  
  ;; set display position
  XPOS=FLOOR(frm[0]/(*e).binning) & YPOS=FLOOR(frm[2]/(*e).binning)
  g=1.0 & m=GetContrastScaling(GAMMA=g)
  CASE m OF
     'log': TV, IMGSCL(image,MIN=lo,Max=hi,/LOG), XPOS, YPOS
     'exp': TV, IMGSCL(image,MIN=lo,Max=hi,/EXP), XPOS, YPOS
     'gamma': TV, IMGSCL(image,MIN=lo,Max=hi,GAMMA=g), XPOS, YPOS
     ELSE: TV, IMGSCL(image,MIN=lo,Max=hi), XPOS, YPOS
  END 
  ;; TV, BYTSCL(image,MIN=lo,Max=hi), XPOS, YPOS
END 

PRO TVDirect, e, lo, hi
  IF NOT(PTR_VALID(e)) THEN BEGIN
     PrintToCon, "% TVDirect: Invalid data pointer."
     return
  END
  IF NOT(PTR_VALID((*e).data)) THEN BEGIN
     PrintToCon, "% TVDirect: Invalid data array pointer."
     return
  END
  Case (*e).zcoord OF
     1: BEGIN             
        image=REFORM((*(*e).data)[(*e).slice,frm[0]:frm[1],frm[2]:frm[3]])
     END
     2: BEGIN             
        image=REFORM((*(*e).data)[frm[0]:frm[1],(*e).slice,frm[2]:frm[3]])
     END
     3: BEGIN             
        image=REFORM((*(*e).data)[frm[0]:frm[1],frm[2]:frm[3],(*e).slice])
     END
     ELSE: BEGIN
        PrintToCon, "% TVDirect: Invalid projection."
        return
     END
  END
  g=1.0 & m=GetContrastScaling(GAMMA=g)
  CASE m OF
     'log': TV, IMGSCL(image,MIN=lo,Max=hi,/LOG)
     'exp': TV, IMGSCL(image,MIN=lo,Max=hi,/EXP)
     'gamma': TV, IMGSCL(image,MIN=lo,Max=hi,GAMMA=g)
     ELSE: TV, IMGSCL(image,MIN=lo,Max=hi)
  END 
  ;; TV, BYTSCL(image,MIN=lo,Max=hi)
END 

Pro GetBinning, e, binx, biny
  IF NOT(PTR_VALID(e)) THEN BEGIN
     PrintToCon, "% GetBinning: Invalid data pointer."
     return
  END
  ;; so far only isotropic binning
  binx=(*e).binning
  biny=(*e).binning
END

FUNCTION MyTVSCL, image, ROI=roi, VERBOSE=verbose, NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite
  ;; mode=GetContrastMode()
  verbose=GetDebugFlag()
  submode='undef'
  frameroi='undef'
  sdevrange=2.
  hilo=[0.,1.]
  mode=GetStackContrastMode(SUBMODE=submode, ROI=frameroi, SRANGE=srange);; why is the mode important? hilo for the Stack meta data sopuld be used
  if verbose then print, "% MyTVSCL: Contrast Mode="+mode
  N=SIZE(image)
  IF (N(0) LT 2) THEN BEGIN
     lo=MIN(image,MAX=hi)
     g=1.0
     m=GetContrastScaling(GAMMA=g)
     CASE m OF
        'log': TV, IMGSCL(image,MIN=lo,Max=hi,/LOG)
        'exp': TV, IMGSCL(image,MIN=lo,Max=hi,/EXP)
        'gamma': TV, IMGSCL(image,MIN=lo,Max=hi,GAMMA=g)
        ELSE: TV, IMGSCL(image,MIN=lo,Max=hi)
     END 
     ;; TV, BYTSCL(image,MIN=lo,Max=hi)
     if keyword_set(cutoff) THEN cutoff=[lo,hi]
      if verbose then print, "% MyTVSCL: Error - Image Dimensions less than 2."
     return, hilo
  END
  ;; auto or manual mode?
  IF ((mode NE 'auto') AND NOT(keyword_set(allowmanualoverwrite))) THEN BEGIN
     ;; manual mode
     ;; get hilo values
     ;;
     hilo=GetStackContrastValues() ;; read values without calculating
     ;; v=GetContrast()
     ; print, "% MyTVSCL: Manual contrast =", hilo
     ;; TV, BYTSCL(image,MIN=hilo[0],Max=hilo[1])
      g=1.0 & m=GetContrastScaling(GAMMA=g)
      CASE m OF
         'log': TV, IMGSCL(image,MIN=hilo[0],Max=hilo[1],/LOG)
         'exp': TV, IMGSCL(image,MIN=hilo[0],Max=hilo[1],/EXP)
         'gamma': TV, IMGSCL(image,MIN=hilo[0],Max=hilo[1],GAMMA=g)
         ELSE: TV, IMGSCL(image,MIN=hilo[0],Max=hilo[1])
      END
      if verbose then BEGIN
         print, "% MyTVSCL: Manual contrast mode."
         print, "%   Cut-off values - ", hilo[0], ":", hilo[1]
      END
      return, hilo
   END
  ;;
  ;; it is auto mode or manual overwrite
  ;;
  ;; determine auto contrast values
  hilo=SetAutoContrast(image, ROI=roi, FRAMEROI=frameroi, SUBMODE=submode)
  SetStackContrastValues, hilo
  lo=hilo[0] & hi=hilo[1]
  ;; TV, BYTSCL(image,MIN=lo,Max=hi)
  if NOT(keyword_set(noupdateui)) THEN dummy=XTabContrastDialogUpdateContrastRange([lo,hi])
   g=1.0
  m=GetContrastScaling(GAMMA=g)
  CASE m OF
     'log': TV, IMGSCL(image,MIN=lo,Max=hi,/LOG)
     'exp': TV, IMGSCL(image,MIN=lo,Max=hi,/EXP)
     'gamma': BEGIN
        TV, IMGSCL(image,MIN=lo,Max=hi,GAMMA=g)
     END 
     ELSE: TV, IMGSCL(image,MIN=lo,Max=hi)
  END
  ;; Update info field in XTabContrastDialog is missing, we return
  ;; cutoff for that matter
  ;; 
  if verbose then BEGIN
     print, "% MyTVSCL: Auto contrast mode."
     print, "%    ROI settings - ", frameroi
     print, "%    Minor mode - ", submode
     print, "%    Scaling - ", m
     print, "%   Cut-off values - ", lo, ":", hi
  END
  return, [lo,hi]
END     


Pro TVDisplay, CLEAR=clear, DIRECTP=directp, DIRECTDATAP=directdatap, ROI=roi, XORMask=xormask, NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite
;;
;; displays image index in data array p on the screen, 
;; if clear is set, the current window content will be deleted 
;;
;; Default is contrast setting on central quarter frame
;; 
;;  COMMON CONTRASTWINIDS, base, buttons, auto, manual, quarter, full, sdev, minmax, lo, hi
;;  Widget_Control, base, Get_UValue=info
;;   print, "widget info=", info
  IF (GetDebugFlag()) THEN BEGIN
  END ELSE BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        PrintToCon, "% TVDisplay:  Fatal error "
        PrintToCon, "%   Error status  - " + STRING(error_status)
        PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
        ErrMsg, !ERROR_STATE.MSG
        CATCH, /Cancel
        return
     END
  END
  IF KEYWORD_SET(directp) THEN BEGIN
     p=directp
     IF (NOT(PTR_VALID(p))) THEN BEGIN
        print, "% TVDisplay: current pointer is invalid"
        return
     END
     p=(*p).datap
  END ELSE BEGIN
     IF KEYWORD_SET(directdatap) THEN BEGIN
        p=directdatap
     END ELSE BEGIN
        ptr=GetRootP()
        IF (NOT(PTR_VALID(ptr))) THEN BEGIN
           print, "% TVDisplay: Root pointer is invalid"
           return
        END
        p=(*ptr).current
        IF (NOT(PTR_VALID(p))) THEN BEGIN
           print, "% TVDisplay: current pointer is invalid"
           return
        END
        p=(*p).datap
     END 
  END 
;;
  IF (PTR_VALID(p))THEN BEGIN
     index=(*p).slice
     wid=(*p).slicewidth
     IF ((*p).window GT -1) THEN BEGIN
        WSET, (*p).window
        IF (KEYWORD_SET(clear)) THEN ERASE
        ;; bug fix: make sure that the rebin size is an integer
        ;; multiple of the binning size
        binSzX=(*p).SzX
        binSzY=(*p).SzY
        binSzZ=(*p).SzZ
        IF ((*p).binningx NE 1) THEN BEGIN
           binSzX=FLOOR((((*p).SzX)/(*p).binningx))
           X1=FLOOR(((*p).binningx*binSzX)-1)
        END
        IF ((*p).binningy NE 1) THEN BEGIN
           binSzY=FLOOR(((*p).SzY)/(*p).binningy)
           Y1=FLOOR(((*p).binningy*binSzY)-1)
        END
        IF ((*p).binningz NE 1) THEN BEGIN
           binSzZ=FLOOR(((*p).SzZ)/(*p).binningz)
           Z1=FLOOR(((*p).binningz*binSzZ)-1)
        END
        IF ((*p).binning NE 1) THEN BEGIN
           binSzX=FLOOR(((*p).SzX)/(*p).binning)
           binSzY=FLOOR(((*p).SzY)/(*p).binning)
           binSzZ=FLOOR(((*p).SzZ)/(*p).binning)
           X1=FLOOR((*p).binning*binSzX-1)
           Y1=FLOOR(((*p).binning*binSzY)-1)
           Z1=FLOOR(((*p).binning*binSzZ)-1)
        END
        IF (wid LE 1) THEN BEGIN
           CASE 1 OF
              IN((*p).type,[1,2,3,4,5,12,13]): BEGIN
                 IF ((*p).binning NE 1) THEN BEGIN
                    CASE (*p).zcoord OF
                       3: hilo= MyTVSCL(REBIN(REFORM(((*(*p).data)[0:X1,0:Y1,index])), binSzX,binSzY), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REBIN(REFORM(((*(*p).data)[0:X1,index,0:Z1])), binSzX,binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REBIN(REFORM(((*(*p).data)[index,0:Y1,0:Z1])), binSzY,binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    ENDCASE
                 ENDIF ELSE BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REFORM((*(*p).data)[*,*,index]), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REFORM((*(*p).data)[*,index,*]), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REFORM((*(*p).data)[index,*,*]), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    END
                 ENDELSE
              END     
              IN((*p).type,[6,9]): BEGIN
                 IF ((*p).binning NE 1) THEN BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REBIN(ABS(REFORM((*(*p).data)[0:X1,0:Y1,index])), binSzX,binSzY), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REBIN(ABS(REFORM((*(*p).data)[0:X1,index,0:Z1])), binSzX, binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REBIN(ABS(REFORM((*(*p).data)[index,0:Y1,0:Z1])), binSzY,binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    ENDCASE
                    
                 ENDIF  ELSE BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REFORM(ABS((*(*p).data)[*,*,index])), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REFORM(ABS((*(*p).data)[*,index,*])), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REFORM(ABS((*(*p).data)[index,*,*])), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    END
                 END
              END          
              ELSE: print, "% TVDisplay: error - unknown image type"
           ENDCASE
        END ELSE BEGIN ;; averaging width larger than 1
           ind1=index & ind2=ind1+wid-1
           norm=1./wid
           CASE 1 OF
              IN((*p).type,[1,2,3,4,5,12,13]): BEGIN
                 IF ((*p).binning NE 1) THEN BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REBIN(REFORM(norm*Total((*(*p).data)[0:X1,0:Y1,ind1:ind2],3)), binSzX,binSzY), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REBIN(REFORM(norm*Total((*(*p).data)[0:X1,ind1:ind2,0:Z1],2)), binSzX,binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REBIN(REFORM(norm*Total((*(*p).data)[ind1:ind2,0:Y1,0:Z1],1)), binSzY,binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    ENDCASE
                 ENDIF ELSE BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REFORM(norm*Total((*(*p).data)[*,*,ind1:ind2],3)), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REFORM(norm*Total((*(*p).data)[*,ind1:ind2,*],2)), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REFORM(norm*Total((*(*p).data)[ind1:ind2,*,*],1)), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    END
                 ENDELSE
              END     
              IN((*p).type,[6,9]): BEGIN
                 IF ((*p).binning NE 1) THEN BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REBIN(ABS(REFORM(norm*Total((*(*p).data)[0:X1,0:Y1,ind1:ind2],3))), binSzX,binSzY), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REBIN(ABS(REFORM(norm*Total((*(*p).data)[0:X1,ind1:ind2,0:Z1],2))), binSzX, binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REBIN(ABS(REFORM(norm*Total((*(*p).data)[ind1:ind2,0:Y1,0:Z1],1))), binSzY,binSzZ), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    ENDCASE
                    
                 ENDIF  ELSE BEGIN
                    CASE (*p).zcoord OF
                       3: hilo=MyTVSCL(REFORM(ABS(norm*Total((*(*p).data)[*,*,ind1:ind2],3))), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       2: hilo=MyTVSCL(REFORM(ABS(norm*Total((*(*p).data)[*,ind1:ind2,*],2))), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       1: hilo=MyTVSCL(REFORM(ABS(norm*Total((*(*p).data)[ind1:ind2,*,*],1))), NOUPDATEUI=noupdateui, ALLOWMANUALOVERWRITE=allowmanualoverwrite)
                       ELSE:
                    END
                 END
              END          
              ELSE: print, "% TVDisplay: error - unknown image type"
           ENDCASE
        END
        IF  PTR_VALID((*p).ScaleBar) THEN DrawScaleBar, (*(*p).ScaleBar), POS='ll', WDOW=(*p).window, BINNING=(*p).binning, CHARSIZE=3
     ENDIF
     ;; update stack inpo
     SetStackContrastValues, hilo
     ;;
     IF NOT(keyword_set(noupdateui)) THEN BEGIN
        ;; update user interface
        dummy=XTabContrastDialogUpdateContrastRange(hilo)
     END
  ENDIF 
END



