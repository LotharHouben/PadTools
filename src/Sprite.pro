FUNCTION CreateSprite, pix, Color=color, CHANNEL=channel
 x=FIX(pix/2) & y=FIX(pix/2) & x0=0 & y0=0
 s=PTR_NEW(CREATE_STRUCT('sp', PTR_NEW(BYTARR(pix,pix)), 'im', PTR_NEW(BYTARR(pix,pix)), 'Sz', pix, 'x', x, 'y', y, 'x0', x0, 'y0', y0, 'colchan', 1B))
 IF NOT(KEYWORD_SET(color)) THEN Color=!D.TABLE_SIZE
 (*(*s).sp)[*,*]=color
 IF NOT(KEYWORD_SET(channel)) THEN channel=1 ;; red
 (*s).colchan=channel
 return, s
END


Pro PutSprite, s,  x, y
;; 
;; get image data first
;;
 IF (x LT 0) THEN x=0 & IF (x GE !D.X_VSIZE) THEN x=!D.X_VSIZE-1
 IF (y LT 0) THEN y=0 & IF (y GE !D.Y_VSIZE) THEN y=!D.Y_VSIZE-1
 (*s).x=x & (*s).y=y
 (*s).x0= x-FIX((*s).Sz/2) & (*s).y0=y-FIX((*s).Sz/2) 
 IF ((*s).x0 LT 0) THEN (*s).x0=0 $ 
   ELSE IF (((*s).x0+(*s).Sz) GT !D.X_VSIZE) THEN (*s).x0=!D.X_VSIZE-(*s).Sz
 IF ((*s).y0 LT 0) THEN (*s).y0=0 $ 
   ELSE IF (((*s).y0+(*s).Sz) GT !D.Y_VSIZE) THEN (*s).y0=!D.Y_VSIZE-(*s).Sz

 (*(*s).im)=TVRD((*s).x0, (*s).y0, (*s).Sz, (*s).Sz)
;;
;; draw sprite in window
;;
  zeroim=0*(*(*s).sp)
  TV, zeroim, (*s).x0, (*s).y0, 0 ;; set all channels zero
  TV, (*(*s).sp), (*s).x0, (*s).y0, (*s).colchan
END

Pro RestoreSprite, s
  TV, (*(*s).im), (*s).x0, (*s).y0
END

FUNCTION InSprite, s, x, y, TOL=tol
 res=0
 IF NOT(KEYWORD_SET(tol)) THEN tol=1
 dx=ABS(x-(*s).x) & dy=ABS(y-(*s).y)
 IF ((dx LE (tol*(*s).Sz)) AND (dy LE (tol*(*s).Sz))) THEN res=1	
 return, res
END    

PRO DeleteSprite, s
IF PTR_VALID(s) THEN BEGIN
    IF PTR_VALID((*s).im) THEN PTR_FREE, (*s).im
    IF PTR_VALID((*s).sp) THEN PTR_FREE, (*s).sp
    PTR_FREE, s
END
END
