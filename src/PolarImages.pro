FUNCTION ExpRadiusIm, r
;;
;; returns a quadratic image of dimensions (2r+1,2r+1)
;; containing in each pixel an exponential distance
;; to the central pixel  
;; 
;; 
Radius = fltarr(2*r+1,2*r+1)

x=FINDGEN(r+1)     ;; x = 0 ... r+1  radius vector
ID=LONARR(r+1)+1 ;; ID = 1 1 1 1 1 1 ...
tmp=(x*x)#ID     ;; tmp = 0 1 4 9 ...
                 ;;       repeated in r+1 rows
help=tmp + TRANSPOSE(tmp)  ;; help(i,j)=i^2+j^2, i=0..r, j=0..r
help=SQRT(FLOAT(help))  ;; help=euclidian distance to zero
help=help/r ;; normalized distances
help=help*ALog(r)
help=Exp(help)
;; 
i0=r & i1= 2*r
Radius(i0:i1,i0:i1) = help  ;; first quad
Radius(0:r,i0:i1) = REVERSE(help) ;; second quad is mirrored
Radius(0:i1,0:r) = REVERSE(Radius(0:i1,r:i1),2) ;; third and fourth quad
return, Radius
END

FUNCTION RadiusIm, r
;;
;; returns a quadratic image of dimensions (2r+1,2r+1)
;; containing in each pixel the distance
;; to the central pixel  
;; 

Radius = fltarr(2*r+1,2*r+1)
x=INDGEN(r+1, /LONG)    ;; x = 0 1 2 3 ... r
ID=LONARR(r+1)+1 ;; ID = 1 1 1 1 1 1 ...
tmp=(x*x)#ID     ;; tmp = 0 1 4 9 ...
                 ;;       repeated in r+1 rows
help=tmp + TRANSPOSE(tmp)  ;; help(i,j)=i^2+j^2, i=0..r, j=0..r
help=SQRT(FLOAT(help))  ;; help=euclidian distance to zero
i0=r & i1= 2*r
Radius(i0:i1,i0:i1) = help  ;; first quad
Radius(0:r,i0:i1) = REVERSE(help) ;; second quad is mirrored
Radius(0:i1,0:r) = REVERSE(Radius(0:i1,r:i1),2) ;; third and fourth quad
return, Radius
END

FUNCTION AngleIm, r, MULTIPLICITY=multiplicity
;;
;; returns a quadratic image of dimensions (2r+1,2r+1) 
;; containing in each pixel the polar
;; angle with respect to the centre in radians  
;;
TheAngle = fltarr(2*r+1,2*r+1)
x=INDGEN(r, /LONG)+1    ;; x =  1 2 3 ... r, avoid 0 !!
ID=LONARR(r)+1 ;; ID = 1 1 1 1 1 1 ...
tmp=x#ID       ;; x repeated in r rows
help=ATAN(TRANSPOSE(tmp)/FLOAT(tmp))
i0=r+1
i1= 2*r
i2=r-1
TheAngle(i0:i1,i0:i1)=help  ;; first quad
TheAngle(0:i2,i0:i1) = !DPI-REVERSE(help) ;; second quad, mirrored + pi
TheAngle(0:i2,r)=!DPI
TheAngle(i0:i1,r)=0
TheAngle(r,i0:i1)=0.5*!DPI
TheAngle(0:i1,0:i2) = 2*!DPI-REVERSE(TheAngle(0:i1,i0:i1),2) ;; third and fourth quad
IF keyword_set(multiplicity) THEN BEGIN
   IF multiplicity GT 1 THEN BEGIN
      TheAngle =  TheAngle MOD ((2 * !PI)/multiplicity)
   END
END
return, TheAngle
END
