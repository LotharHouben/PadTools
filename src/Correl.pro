FUNCTION VectorCorr, a, b, MAXINDEX=maxindex, MAXVAL=maxval
  len=N_Elements(a)
  c=FLTARR(len)
  FOR i=0,len-1 DO c[i]=C_CORRELATE(a,b[0:(len-1)],i)
  IF Keyword_SET(maxindex) THEN BEGIN
     maxindex=WHERE(c EQ Max(c),count)
     if (count GT 1) THEN maxindex=maxindex[0]
     IF keyword_set(maxval) THEN maxval=Max(c)
  END
  return, c
END
    

FUNCTION PeriodicVectorCorr, a, b, MAXINDEX=maxindex, MAXVAL=maxval
  len=N_Elements(a)
  c=FLTARR(len)
  FOR i=0,len-1 DO BEGIN
     c[i]=CORRELATE(shift(a,i),b[0:(len-1)])
  END
  IF Keyword_SET(maxindex) THEN BEGIN
     maxindex=WHERE(c EQ Max(c),count)
     if (count GT 1) THEN maxindex=maxindex[0]
     IF keyword_set(maxval) THEN maxval=Max(c)
  END
  return, c
END


FUNCTION imCorr, P, G, SHIFTVEC=shiftvec, CORRMAX=CorrMax, CORRNORM=CorrNorm, SILENT=silent
;;;
;;;
;;;  cross-corelate two identical size images
;;;
;;;  example:
;;;  
;;;  shvec=[1.,1.] & CMax=1.
;;;  ic=imcorr(test,pattern, SHIFTVEC=shvec, CORRMAX=CMax, /SILENT)
;;;  print, shvec
;;;  print, CMax  
;;;   
IX=0 & IY=0
N=SIZE(P)
PSzX=N(1) & PSzY=N(2)
N=SIZE(G)
GSzX=N(1) & GSzY=N(2)
SzX=PSzX & SzY=PSzY
IF NOT(keyword_set(CorrNorm)) THEN CorrNorm="SDEV"
;; initialize frame coordinates in P and G
X1=0 & X2=SzX-1 & Y1=0 & Y2=SzY-1
gX1=0 & gX2=GSzX-1 & gY1=0 & gY2=GSzY-1
IF ((PSzX NE GSzX) OR (PSzY NE GSzY)) THEN BEGIN
   ;; P and G are not of the same size
   printtocon, "% imCorr: Size mismatch - image sizes do not match."
   printtocon, "%    returning."
   return, 0
END
CorrIm=0
CASE CorrNorm OF 
   'SDEV': BEGIN
      mdev = moment(P[X1:X2,Y1:Y2], SDEV=psdev)
      IF Keyword_set(momentp) THEN momentp=mdev
      pfft=Conj(FFT(P[X1:X2,Y1:Y2]-mdev(0)))
      mdev = moment(G[gX1:gX2,gY1:gY2], SDEV=gsdev)
      IF Keyword_set(momentg) THEN momentg=mdev
      gfft=FFT((G[gX1:gX2,gY1:gY2]-mdev(0)))
      CorrIm=1.0/psdev/gsdev*SHIFT((FFT((pfft*gfft),/INVERSE)), FIX((X2-X1+1)/2), FIX((Y2-Y1+1)/2))
   END
   'AMP': BEGIN
      mdev = moment(P[X1:X2,Y1:Y2], SDEV=psdev)
      IF Keyword_set(momentp) THEN momentp=mdev
      pfft=Conj(FFT(P[X1:X2,Y1:Y2]))
      mdev = moment(G[gX1:gX2,gY1:gY2], SDEV=gsdev)
      IF Keyword_set(momentg) THEN momentg=mdev
      gfft=FFT((G[gX1:gX2,gY1:gY2]))
      tmpcorr=pfft
      FOR ii=0,(SzX-1) DO BEGIN
         FOR jj=0,(SzY-1) DO BEGIN
            a=pfft[ii,jj] & b=gfft[ii,jj]
            tmpcorr[ii,jj]=a*b/(ABS(a*b))
         END
      END 
      CorrIm=SHIFT((FFT(tmpcorr,/INVERSE)), FIX((X2-X1+1)/2), FIX((Y2-Y1+1)/2))
   END
   'INT': BEGIN
      mdev = moment(P[X1:X2,Y1:Y2], SDEV=psdev)
      IF Keyword_set(momentp) THEN momentp=mdev
      pfft=Conj(FFT(P[X1:X2,Y1:Y2]))
      mdev = moment(G[gX1:gX2,gY1:gY2], SDEV=gsdev)
      IF Keyword_set(momentg) THEN momentg=mdev
      gfft=FFT(G[gX1:gX2,gY1:gY2])
      tmpcorr=pfft
      FOR ii=0,(SzX-1) DO BEGIN
         FOR jj=0,(SzY-1) DO BEGIN
            a=SQRT(pfft[ii,jj]) & b=SQRT(gfft[ii,jj])
            tmpcorr[ii,jj]=a*b/(ABS(a*b))
         END
      END 
      B=WHERE(tmpcorr NE tmpcorr) ;; test for NANs
      tmpcorr(B)=0.
      CorrIm=SHIFT((FFT(tmpcorr,/INVERSE)), FIX((X2-X1+1)/2), FIX((Y2-Y1+1)/2))
   END
   'NONE': BEGIN
      mdev = moment(P[X1:X2,Y1:Y2], SDEV=psdev)
      IF Keyword_set(momentp) THEN momentp=mdev
      pfft=Conj(FFT(P[X1:X2,Y1:Y2]-mdev(0)))
      mdev = moment(G[gX1:gX2,gY1:gY2], SDEV=gsdev)
      IF Keyword_set(momentg) THEN momentg=mdev
      gfft=FFT((G[gX1:gX2,gY1:gY2]-mdev(0)))
      CorrIm=SHIFT((FFT((pfft*gfft),/INVERSE)), FIX((X2-X1+1)/2), FIX((Y2-Y1+1)/2))
   END
   ELSE: BEGIN
      mdev = moment(P[X1:X2,Y1:Y2], SDEV=psdev)
      IF Keyword_set(momentp) THEN momentp=mdev
      pfft=Conj(FFT(P[X1:X2,Y1:Y2]-mdev(0)))
      mdev = moment(G[gX1:gX2,gY1:gY2], SDEV=gsdev)
      IF Keyword_set(momentg) THEN momentg=mdev
      gfft=FFT((G[gX1:gX2,gY1:gY2]-mdev(0)))
      CorrIm=1.0/psdev/gsdev*SHIFT((FFT((pfft*gfft),/INVERSE)), FIX((X2-X1+1)/2), FIX((Y2-Y1+1)/2))
   END
END
CorrIm=ABS(CorrIm)
     N=SIZE(CorrIm)
     CorrImSzX=N(1) &  CorrImSzY=N(2)
     Max_sub=0L
     ;; there might be more than one extermum pixels
     CMax=MAX(ABS(CorrIm), Max_sub)
     ind = ARRAY_INDICES(CorrIm, Max_sub)  
     IX = ind[0] & IY=ind[1]
     IF NOT(keyword_set(silent)) THEN printtocon, "% CCorr: Maximum value=" + MyString(CMax) + " at (" + MyString(IX-CorrImSzX/2) + "," + MyString(IY-CorrImSzY/2)+")"
     MaxOffsX=[IX-CorrImSzX/2,IY-CorrImSzY/2] ;; Offset of the maximum position
     SX=0. & SY=0.
     ;; STOP
     ;; search maximum in correlation image with sub-pixel accuracy
IF keyword_set(corrmax) THEN corrmax=CMax
IF keyword_set(shiftvec) THEN shiftvec=MaxOffsX
return, CorrIm
END 
