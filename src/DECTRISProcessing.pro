PRO DECTRIS_FixPix, p, AUTO=auto
  ;; p pointer to the current data stack
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     printtocon, "% DECTRIS_FixPix: Current stack pointer is invalid." 
     return
  END
  e=(*p).datap
  IF (NOT(PTR_VALID(e))) THEN BEGIN
     printtocon, "% DECTRIS_FixPix:: Current data pointer is invalid." 
     return
  END
  ;; auto-determine stack size
  datap=(*e).data
  N=Size(*datap)
  DimX=N[1] & DimY=N[2] & DimZ=N[3]
  detectordim=DimX
  if keyword_set(auto) THEN BEGIN
     seed = !NULL
     randomValue = RANDOMU(seed,10 )
     slices=LONG(randomvalue*DimZ) ;; ten random slices
     pixlist=list()
     For i=0L,N_ELEMENTS(slices)-1 Do BEGIN
        ind=WHERE((*datap)[*,*,slices[i]] EQ 16535, count)
        IF (count GE 1) THEN pixlist.Add, ind, /EXTRACT
     END
     IF (pixlist.Count() GT 0) THEN BEGIN
        pix=pixlist.ToArray()
        uniqpix=(pix[sort(pix)])[uniq(pix[sort(pix)])]  
        ;; STOP
        printtocon, "% DECTRIS_FixPix: max_uint16 pixels (x,y)"
        For i=0,N_ELEMENTS(uniqpix)-1 Do BEGIN
           x=uniqpix[i] MOD DimX
           y=uniqpix[i]/DimX
           printtocon, "  ("+MyString(x)+","+MyString(y)+")"
           (*datap)[x,y,*]=0
        END     
     END ELSE BEGIN
        printtocon, "% DECTRIS_FixPix: No max_uint16 pixels detected."
     END
  END
END  

