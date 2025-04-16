PRO MeasurePeaks, PEAKTOPEAK=peaktopeak, SEQUENCE=sequence, NOLOOP=noloop, NLINES=nlines, NOFIT=nofit
;;
;; KEYWORDS:
;;
;; NOFIT: Take disc centres as marked, useful for CBED
;; 
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% MeasurePeaks:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
END

  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% MeasurePeaks: No 3D Data found."
     return
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% MeasurePeaks: Current 3D Data invalid."
     return
  END
  e=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  MeasurePeaks: Current 3D Data holds no data."
     return
  END


NoImages=0
NoScans=0
DType=0


    ;; Determine Data Type
    CASE 1 OF
        IN((*e).type,[1,2,3,4,5,12,13]): BEGIN 
            DType=4
            NoCols=2
        END
        (((*e).type EQ 6) OR  ((*imp).type EQ 9)): BEGIN
            DType=6
            NoCols=3
        END
        ELSE: DType=0
    ENDCASE
    
    If (DType EQ 0) THEN BEGIN
        PrintToCon, "% MeasurePeaks: indefinite data type ... returning!"
        return
     END

    CASE (*e).zcoord OF
       1:BEGIN
          im=(*(*e).data)[(*e).slice,*,*]
       END
       2: BEGIN
          im=(*(*e).data)[*,(*e).slice,*]
       END
       ELSE: im=(*(*e).data)[*,*,(*e).slice]
    END
    N=SIZE(im)
    ;; line scan, coordinate preset values
    ;; a horizontal line of witdh 1 and a length of half the image size 
    X0 = FIX(N[1]/4)
    Y0 = FIX(N[2]/2)
    X1 = FIX(3*N[1]/4)
    Y1 = FIX(N[2]/2)
    width = 0.0
    wx=0.0 & wy =0.0
    

    ;;
    ;; now this is the line profile dialog loop
    ;;
    Quit=0
    r1=3 & r2=r1
    index=0
    scalex2=(*e).xsamp*(*e).xsamp
    scaley2=(*e).ysamp*(*e).ysamp

    IF KEYWORD_SET(peaktopeak) THEN BEGIN
       ;; let the user mark  the central  extrema
       mytable=obj_new('DTable', 'Peak-to-Peak Separations', 10, NLAYERS=1)
       mytable->SetColumnLabels, ["index","X0","Y0","X1","Y1","DX","DY","D","D scaled","Angle"]
       mytable->SetDescriptor, ["Peak table","X0, Y0, X1, Y1, DX, DY, D are in pixel units.","D scaled is in uits of the sampling, including anisotropic sampling","Angle is the angle between the vector #1-#0  relative to 3 o clock in degrees"]
       newtable=GetNewTableContainer("Peak-to-Peak Separations")
       IF NOT(PTR_VALID(newtable)) THEN BEGIN
          ;; an error occured, delete the object
          ErrMsg, "NewTable: Could not create table list item."
          obj_destroy, mytable
       END ELSE BEGIN
          (*newtable).pobj=mytable
       END
 
       repeat BEGIN
        ;; focus to the right window first
        ;;FocusToImage, p
          index +=1
          ScanCoordSel=0
          IF (XCircle(X0, Y0, r1, bin=(*e).binning, /STICKY, TITLE="Extr. #1") GT 0) THEN BEGIN
             IF NOT(keyword_set(nofit)) THEN BEGIN
                kernel=FLTARR(2*r1+1, 2*r1+1)
                kernel[*,*]=1.
                Profile2DFindCentre, im, X0, Y0, kernel, /INFO
             END
             IF (XCircle(X1, Y1, r2, bin=(*e).binning, /STICKY, TITLE="Extr. #2") GT 0) THEN BEGIN
                IF NOT(keyword_set(nofit)) THEN BEGIN
                   kernel=FLTARR(2*r2+1, 2*r2+1)
                   kernel[*,*]=1.
                   Profile2DFindCentre, im, X1, Y1, kernel, /INFO
                END
                DY=Y1-Y0 & DX=X1-X0
                Distscaled=Sqrt(DY*DY*scaley2+DX*DX*scalex2)
                Dist=Sqrt(DY*DY+DX*DX)
                ang=atan(DY*(*e).ysamp,DX*(*e).xsamp)*180./!DPI
                mytable->AddRow, list(index, X0, Y0, X1, Y1, DX,DY,Dist,Distscaled,ang)
                wx=0. & wy=0.
                ;;  FocusToImage, winp
                lx0= ROUND(X0/(*e).binning) & ly0= ROUND(Y0/(*e).binning)
                lx1= ROUND(X1/(*e).binning) & ly1= ROUND(Y1/(*e).binning)
                device, get_graphics=gmode0    ; Entry mode.
                device, set_graphics=6         ; XOR mode.
                PlotLine, lx0, ly0, lx1, ly1, 0, 0
                device, set_graphics=gmode0 ; Entry mode
                ;; ScanCoordSel=1
                ;; NoScans=1 ;; just one scan per image
                
             END ELSE Quit=1 
          END ELSE Quit=1
       endrep  UNTIL (Quit EQ 1)
       mytable->WidgetEditor
    END  
 END  




