FUNCTION HIST, A, BIN=bin, MIN=min, MAX=max
;;
;; returns a histogram for the image A with bin elements between min
;; and max
;; if min and max are not set, Min and Max of the image are determined 
;;

;; the histogramm: 2-dimensional array containing the image value
;; and the number of pixels for that image value
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A,MIN=min)
END
If not(keyword_set(bin)) then bin=256
h=FLTARR(2, bin)
h(0,*)=FLOAT(max-min)/FLOAT(bin)*INDGEN(bin) + FLOAT(min)
h(1,*) = FLOAT(Histogram(A, NBINS=bin, MIN=min, max=max))
return, h
END

FUNCTION HIST_SKEWNESS, A, fraction, MIN=min, MAX=max
;; A is histogramdata returned by HIST()
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A(0,*),MIN=min)
END
range=max-min
low=WHERE(A(0,*) LT min+range*fraction)
hi=WHERE(A(0,*) GT max-range*fraction)
hitotal=TOTAL(A(1,hi))
lowtotal=TOTAL(A(1,low))
Window, 1
plot, A(0,*), A(1,*), Background=0 
return, (hitotal/lowtotal)
END

FUNCTION HIST_SKEWNESS_B, A, fraction, MIN=min, MAX=max
;; A is histogramdata returned by HIST()
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A(0,*),MIN=min)
END
range=max-min
low=WHERE(A(0,*) LT min+range*fraction)
hi=WHERE(A(0,*) GT max-range*fraction)
hitotal=TOTAL(A(1,hi))
lowtotal=TOTAL(A(1,low))
Window, 1
plot, A(0,*), A(1,*), Background=0 
return, (hitotal*lowtotal)
END


FUNCTION Hist_Equal2, A, minperc, maxperc, MIN=min, MAX=max
  IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
     max=MAX(A,MIN=min)
  END
 ;; make 100 bins correspoding to 100% 
 binsize=DOUBLE((max-min)/100) ;
 ;; evaluate histogram
 h=HISTOGRAM(A, MIN = min, MAX = max, /NAN, BINSIZE = binsize)
 ;; create cumulative
 for i=1,n_elements(h)-1 do h[i] = h[i]+h[i-1]
 Maxh=Max(h)
 min_c=0
 WHILE ((h[min_c] LT (FLOAT(minperc)/100)*Maxh) AND (min_c LT (N_ELEMENTS(h)-1))) DO min_c=min_c+1
 max_c=N_ELEMENTS(h)-1
 WHILE ((h[max_c] GT (FLOAT(maxperc)/100)*Maxh) AND (max_c GT (min_c + 1))) DO max_c=max_c-1
 min_c=min_c*binsize+Min
 max_c=max_c*binsize+Min
return, [min_c,max_c]
END 

FUNCTION HIST_Balance, A, division, MIN=min, MAX=max
;; A is histogramdata returned by HIST()
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A(0,*),MIN=min)
END
range=max-min
low=WHERE(A(0,*) LT ((division*range)+min))
hi=WHERE(A(0,*) GT ((division*range)+min))
hitotal=TOTAL(A(1,hi))
lowtotal=TOTAL(A(1,low))
Window, 1
plot, A(0,*), A(1,*), Background=0 
return, (hitotal/lowtotal)
END


FUNCTION HIST_UpperMoment, A, division, MIN=min, MAX=max
;; A is histogramdata returned by HIST()
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A(0,*),MIN=min)
END
range=(max-min)
hi=WHERE(A(0,*) GT ((division*range)+min))
hitotal=TOTAL(A(1,hi))
mom=0
;; Delta=A[0,1]-A[0,0]
print, "Upper hist=", hi[0], " ", hi[(N_Elements(hi)-1)]
For j=hi[0],hi[(N_Elements(hi)-1)] Do BEGIN
   mom=mom+A(0,j)*A(1,j)
END
;;Window, 1
;;plot, A(0,hi), A(1,hi), Background=0 
return, (mom/hitotal)
END

FUNCTION HIST_LowerMoment, A, division, MIN=min, MAX=max
;; A is histogramdata returned by HIST()
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A(0,*),MIN=min)
END
range=(max-min)
low=WHERE(A(0,*) LT ((division*range)+min))
lowtotal=TOTAL(A(1,low))
mom=0
;; Delta=A[0,1]-A[0,0]
print, "Histogram evaluation interval=", A(0,low[0]), ":", A(0,low[(N_Elements(low)-1)])
For j=low[0],low[(N_Elements(low)-1)] Do BEGIN
   mom=mom+A(0,j)*A(1,j)
END
Window, 1
plot, A(0,low), A(1,low), Background=0 
return, (mom/lowtotal)
END


FUNCTION HIST_LowerSum, A, division, MIN=min, MAX=max
;; A is histogramdata returned by HIST()
IF (Not(keyword_set(min)) OR NOT(keyword_set(max))) THEN BEGIN
   max=MAX(A(0,*),MIN=min)
END
range=(max-min)
low=WHERE(A(0,*) LT ((division*range)+min))
return, TOTAL(A(1,low))
END

FUNCTION HIST_IntervalMoment, A, min, max
;; A is histogramdata returned by HIST()
B=WHERE(A(0,*) GE min)
C=WHERE(A(0,*) GE max)
;; Delta=A[0,1]-A[0,0]
print, "Histogram evaluation interval=", A(0,B[0]), ":", A(0,C[0])
mom0=0
;; first_moment
mom1=0
;; second moment
mom2=0
;;
For j=B[0],C[0] Do BEGIN
   mom0=mom0+A(1,j)
   mom1=mom1+A(0,j)*A(1,j)
   mom2=mom2+A(0,j)*A(0,j)*A(1,j)
END
Window, 1
plot, A(0,B[0]:C[0]), A(1,B[0]:C[0]), Background=0 
return, mom1/mom0
END
