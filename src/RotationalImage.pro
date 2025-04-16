FUNCTION RotationalImageData, data, x, y, r, angwidth, FILL=fill, RADIUS_=radius_, ANGLE_=angle_, VERBOSE=verbose, TMATRIX=tmatrix, INNER=inner
;; PARAMETERS:
;; im         = image
;; x, y      = centre 
;; r         = radius in pix, Type: Integer
;; angwidth = angular step width in deg, Type: Float
;; radius_   = 2D array with radius values: radius_ = RadiusIm(r)) 
;; angle_    =2D arra wit angle values: angle_ = AngleIm(r)
;;
;; RETURNS: image with x=radius and y=angle
;;  
;;
  
;;CATCH, Error_status
;;IF (Error_status NE 0) THEN BEGIN
;;    PrintToCon, "% RotationalImageData:  Fatal error "
;;    PrintToCon, "%   Error status  - " + STRING(error_status)
;;    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
;;    CATCH, /Cancel
;;    return, 0
;;END
r=FLOOR(r) ;; should be integer
IF (angwidth LT 0.1) THEN BEGIN 
    angwidth=0.1
    PrintToCon, "% RotationalImage: warning - minimung angle width=0.1" 
END 
x0=x-r & x1=x+r & y0=y-r & y1=y+r
scan_im=data(x0:x1,y0:y1)
IF NOT(keyword_set(radius_)) THEN radius_ =RadiusIm(r)
IF NOT(keyword_set(angle_)) THEN angle_ = AngleIm(r) ;; 2D-arrays
;;
;; new, faster code
;;
;; create array to hold the radial scan data
xdim=r & ydim=CEIL(360./angwidth)
type=SIZE(data, /TYPE)
CASE 1 OF
    IN(type,[1,2,3,4,12]): Im=FLTARR(xdim,ydim)
    IN(type,[5,13]): Im=DBLARR(xdim,ydim)
    (type EQ 6): Im=COMPLEXARR(xdim,ydim)
    (type EQ 9): Im=DCOMPLEXArr(xdim,ydim)
    ELSE:
ENDCASE 
Im[*,*]=0
IF (NOT(PTR_VALID(tmatrix))) THEN BEGIN
;; we'll have to calculate the transformation matrix
;; tmatrix.m is a 2r+1, 2r+1, 2 int array to hold the target indices
;; in the rad, ang array for each pixel in the polar image
;; tmatrix.norm is a xdim, ydim long array of normalization values.
PrintToCon, "% RotationalImageData:  Establishing TMatrix."
tmatrix=PTR_NEW({mx:INTARR(2*r+1,2*r+1),my:INTARR(2*r+1,2*r+1),norm:LONARR(xdim,ydim)})
;; the number of pixel contributing to the bin between r' and r'+1
;; 
spix=0L
npix=0L
for i=0,(x1-x0) do begin
 for j=0,(y1-y0) do begin
        aind=ROUND(angle_(i,j)/(!DPI)*180/angwidth)
        ;; the closest integer Radius
        rind= Round(radius_(i,j))
        ;; add image data
        IF ((aind LT ydim) And (rind LT xdim)) THEN BEGIN
           (*(tmatrix)).mx[i,j]=rind
           (*(tmatrix)).my[i,j]=aind
            Im[rind,aind] = Im[rind,aind] + scan_im[i,j]
            (*(tmatrix)).norm[rind,aind]+=1
           
        END ELSE BEGIN
           (*(tmatrix)).mx[i,j]=-1
           (*(tmatrix)).my[i,j]=-1
            spix+=1
        END 
        npix+=1
    endfor 
ENDFOR
END  ELSE BEGIN ;;  IF (NOT(PTR_VALID(tmatrix)))
   for i=0,(x1-x0) do begin
      for j=0,(y1-y0) do begin
       IF ((*(tmatrix)).mx[i,j] GT 0) THEN Im[(*(tmatrix)).mx[i,j],(*(tmatrix)).my[i,j]] += scan_im[i,j]
    end 
   end 
END 
;; PrintToCon, "% RotationalImageData: processed "+MyString(npix) + " pixels"
;; PrintToCon, "        skipped "+MyString(spix) + " pixels"
;; Normalize
B=WHERE((*(tmatrix)).norm GT 0 )
Im[B] = Im[B]/(*(tmatrix)).norm[B] 
;; find all non-zero data points
IF KEYWORD_SET(fill) THEN BEGIN
    IF keyword_set(verbose) THEN PrintToCon, "% RotationalImageData:  Filling missing pixels with neighbour data"
    FOR i=0,(xdim-1) DO BEGIN
        pixdata=0
        FOR j=0,(ydim-1) DO BEGIN
            IF (Im[i,j] NE 0.0) THEN pixdata=Im[i,j] ELSE Im[i,j]=pixdata
        END
    END
 END
IF KEYWORD_SET(inner) THEN BEGIN
IF (inner LT xdim) THEN Im[0:inner,*]=0.
END   
return, Im
;; done
END 


    
    
