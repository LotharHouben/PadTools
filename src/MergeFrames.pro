PRO MergeFrames
  p=GetCurrentP()
  list=XSelectMultipleImages(p, TITLE="Select Frames to Merge")
  ;;; merge only if there are more than two elements
  IF (N_ELEMENTS(list) GE 2) THEN BEGIN
     ;; number of valid poiters in the list
     B=WHERE(PTR_VALID(list) EQ 1, NFrames)
     IF (NFrames LT 2) THEN BEGIN
        printtocon, "% MergeFrames: Less than two frames have valid data."
        return
     END
     ;; dimensions
     DimX=INTARR(NFrames)
     DimY=INTARR(NFrames)
     FOR i=0,NFrames-1 DO BEGIN
        IF PTR_VALID(list[B[i]]) THEN BEGIN
           d=(*list[B[i]]).datap
           IF PTR_VALID(d) THEN BEGIN
              DimX[B[i]]=(*d).SzX
              DimY[B[i]]=(*d).SzY
           END
        END
     END
     DimX=DimX(uniq(DimX)) & DimY=DimY(uniq(DimY))
     IF NOT((N_ELEMENTS(DimX) EQ 1) AND (N_ELEMENTS(DimY) EQ 1)) THEN BEGIN
        printtocon, "% MergeFrames: Mismatch in frame dimensions."
        return
     END
     
     ;; replicate data of first frame 
     ptr=GetRootP()
     (*ptr).current=list[B[0]]
     p=GetCurrentP()
     datap=(*p).datap    
     images=Make_Array(DimX,DimY,NFrames)
     names=''
     For i=0,NFrames-1 DO BEGIN
        if (i GT 0) then names+=','
        names+=(*list[B[i]]).name
        d=(*list[B[i]]).datap
        images[*,*,i]=*(*d).data
     END
     CopyStack, /NODATACOPY, TITLE="Merge("+names+")"
     ;;
     p=GetCurrentP()
     (*(*p).datap).data=PTR_NEW(images)
     ;; update No frames
     (*(*p).datap).SzZ=NFrames
     ;;
  END
END
