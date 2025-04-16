Function ImageResize, im, DX, DY, FILLZERO=fillzero, FILLMEAN=fillmean, FILLVALUE=fillvalue
  ;;
  t=Size(im, /TYPE)
  val=0
  if keyword_set(fillvalue) Then val=fillvalue
  if keyword_set(fillzero) Then val=0
  if keyword_set(fillmean) THEN Begin
     ;; five sigma threshold
     val = Robust_Mean( im, 5, Sigma_Mean, Num_RejECTED)
  END
  res=Make_Array(DX, DY, TYPE=t)
  ;; print, "fillvalue=" + MySTRING(val)
  res[*,*]=val
  N=Size(im)
  if NOT(N[0] EQ 2) THEN BEGIN
     printtocon, "% ImageResize: Image does not have dimension 2."
    return, res
  END
  ;; now place or cut im
  ;; tx1:tx2, ty1:ty2 = target array indices
  ;; sx1:sx2, sy1:sy2 = source array indices
  if (DX LT N[1]) THEN BEGIN
     tx1=0 & tx2=DX-1
     sx1=(N[1]-DX)/2  & sx2=sx1+DX-1
  END ELSE BEGIN
     sx1=0 & sx2=N[1]-1
     tx1=(DX-N[1])/2 & tx2=tx1+N[1]-1
  END
  if (DY LT N[2]) THEN BEGIN
     ty1=0 & ty2=DY-1
     sy1=(N[2]-DY)/2  & sy2=sy1+DY-1
  END ELSE BEGIN
     sy1=0 & sy2=N[2]-1
     ty1=(DY-N[2])/2 & ty2=ty1+N[2]-1
  END
  res[tx1:tx2,ty1:ty2]=im[sx1:sx2,sy1:sy2]
  return, res
END
