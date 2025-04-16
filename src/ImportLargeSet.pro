FUNCTION ImportLargeSet, BASEPATH=basepath, FILTER=filter, BINNING=binning, SPATBIN=spatbin, crop=crop, FT=ft, GUI=gui
  IF NOT(keyword_set(binning)) THEN binning=1
  IF NOT(keyword_set(spatbin)) THEN spatbin=1
  IF NOT(keyword_set(ft)) THEN ft=0
  IF NOT(keyword_set(crop)) THEN crop=0
  dirhash=DirGetBigFileList(BASEPATH=basepath, FILTER=filter)
  ;; check that a directory was selected
  IF NOT(dirhash.IsEmpty()) THEN BEGIN
     ;; check that the file list is not empty
     IF NOT(dirhash("files").IsEmpty()) THEN BEGIN
        ;; take first file and try to determine the file type
        NIm=(dirhash("files")).Count()
        path=dirhash("dir")
        fn=path+Path_Sep()+(dirhash("files"))[0]
        ok=Query_Image(fn,info)
        IF ok THEN BEGIN
           ;; one of the IDL readable images
           ;;
           printtocon, "% ImportLargeSet: AutoDetect format on first file"
           printtocon, "     Type      : "+info.TYPE
           printtocon, "     Channels  : "+MyString(info.CHANNELS)
           printtocon, "     Bit Depth : "+MyString(info.BITS_PER_SAMPLE)
           printtocon, "     Dimensions: "+MyString((info.DIMENSIONS)[0]) +"x"+MyString((info.DIMENSIONS)[1])
           ;; channels > 1 are not supported
           IF (info.CHANNELS GT 1) THEN BEGIN
              printtocon, "% ImportLargeSet: Fatal, RGB images are not supported"
              return, PTR_NEW()
;; return empty array
           END
           ;; continue reading
           ;; determine target size
           ;; for now simply read
           image=Read_Image(fn)
           dtype=SIZE(image, /TYPE)
           DimX=(info.DIMENSIONS)[0]
           DimY=(info.DIMENSIONS)[1]
           FileDimX=DimX 
           FileDimY=DimY
           DimZ=NIm
           if keyword_set(crop) THEN BEGIN
              help=["Cropping Parameters",$
                    "", $
                    "X0: Horizontal start pixel", $
                    "XW: Horizontal width", $
                    "Y0: Vertical start pixel.", $
                    "YW: Vertical height.", $
                    "Shift half frame before cropping", $ 
                    ""]
              X0=FLOOR(DimX*1./4)
              XW=FLOOR(DimX/2.)
              Y0=FLOOR(DimY*1./4)
              YW=FLOOR(DimY/2.)
              s=list()
              s.Add, {value:X0,label:"X0, XW: ",newrow:0B}
              s.Add, {value:XW,label:" ",newrow:0B}
              s.Add, {value:Y0,label:"Y0, YW: ",newrow:1B}
              s.Add, {value:YW,label:" ",newrow:0B}
              c=list()
              c.Add, {value:ft,label:"FFT upon loading",newrow:1B}
              c.Add, {value:0B,label:"Shift half frame before cropping",newrow:1B}
              check=0B
              shift=0B
              Repeat BEGIN
                 IF (XMDataChoiceField(s, c, TITLE="Cropping Parameters", HELP=help) EQ 1) THEN BEGIN
                    X0=s[0].value & XW=s[1].value & Y0=s[2].value & YW=s[3].value & ft=c[0].value & shift=c[1].value
                    IF (((X0 GE 0) AND ((X0+XW) LE DimX )) AND ((Y0 GE 0) AND ((Y0+YW) LE DimY))) THEN check=1B
                 END ELSE BEGIN
                    return, PTR_NEW()
                 END
              ENDREP UNTIL check
              DimX=XW & DimY=YW
           END ;; crop
           IF ft THEN dtype=6
            case dtype of
               1 : BEGIN
                  APtr=PTR_NEW(BYTARR(DimX,DimY,DimZ))
               END
               2 : BEGIN
                  APtr=PTR_NEW(INTARR(DimX,DimY,DimZ))
               END
               3 : BEGIN
                  APtr=PTR_NEW(LONARR(DimX,DimY,DimZ))
               END
               4 : BEGIN
                  APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
               END
               5 : BEGIN
                  APtr=PTR_NEW(DBLARR(DimX,DimY,DimZ))
               END
               6 : BEGIN
                  APtr=PTR_NEW(COMPLEXARR(DimX,DimY,DimZ))
               END
               9 : BEGIN
                  APtr=PTR_NEW(DCOMPLEXARR(DimX,DimY,DimZ))
               END
               12 : BEGIN
                  APtr=PTR_NEW(UINTARR(DimX,DimY,DimZ))
               END
               else: BEGIN
                  print, "error: invalid data type " + MyString(dtype)
                  return, PTR_NEW()
               END
            endcase
            ;; Start loading data
            IF keyword_set(crop) THEN BEGIN
               X1=(X0+DimX-1) & Y1=(Y0+DimY-1)
            END
            minv=[0]
            maxv=[100]
            pbarperc=0
            pbarstep=5
            mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Loading frames ...", FRAMESTYLE=2)
            FOR i=1,DimZ do BEGIN
               perc=LONG(i*100. / DimZ)
               if (FIX(perc) GT pbarperc) THEN BEGIN
                  pbarperc += pbarstep
                  mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
               END
               ;; shift image
               IF ft THEN image=FFT(image)
               IF shift THEN image= SHIFT(image, -FLOOR(FileDimX/2), -FLOOR(FileDimY/2))
               ;; crop image
               IF keyword_set(crop) THEN image=image[X0:X1,Y0:Y1]
               ;; add to array
               (*APtr)[*,*,i-1]=image
               ;; load next
               IF (i LE (DimZ-1)) THEN BEGIN
                  fn=path+Path_Sep()+(dirhash("files"))[i]
                  image=Read_Image(fn)
               END
            END
            obj_destroy, mybar
            IF keyword_set(gui) THEN BEGIN
               name="LargeSet(" + (dirhash("files"))[0]+","+(dirhash("files"))[-1]+")"
               ThrowStack, APtr, name
            END
            return, APtr
            ;;
         END ELSE BEGIN
            ;; raw data?
           
           ;;
            printtocon, "% ImportLargeSet: Trying 2D Raw Data"
           
           ;;
           ;; User dialog for input format
           DimX=128L & DimY=128L & type=4 & offset=0
           IF NOT(XRawDialog2D(DimX, DimY, type, offset)  EQ 1) THEN return, PTR_NEW()
           ;; continue reading
           ;; determine target size
           ;; for now simply read
           ;; read first image
           case type of
               1 : BEGIN
                  image=BYTARR(DimX,DimY)
               END
               2 : BEGIN
                  image=INTARR(DimX,DimY)
               END
               3 : BEGIN
                  image=LONARR(DimX,DimY)
               END
               4 : BEGIN
                  image=FLTARR(DimX,DimY)
               END
               5 : BEGIN
                  image=DBLARR(DimX,DimY)
               END
               6 : BEGIN
                  image=COMPLEXARR(DimX,DimY)
               END
               9 : BEGIN
                  image=DCOMPLEXARR(DimX,DimY)
               END
               12 : BEGIN
                  image=UINTARR(DimX,DimY)
               END
               else: BEGIN
                  print, "error: invalid data type " + MyString(dtype)
                  return, PTR_NEW()
               END
            endcase
           swap=0
           ReadImageToArray, fn, image, offset, swap
           FileDimX=DimX 
           FileDimY=DimY
           DimZ=NIm
           if keyword_set(crop) THEN BEGIN
              help=["Cropping Parameters",$
                    "", $
                    "X0: Horizontal start pixel", $
                    "XW: Horizontal width", $
                    "Y0: Vertical start pixel.", $
                    "YW: Vertical height.", $
                    "Shift half frame before cropping", $ 
                    ""]
              X0=FLOOR(DimX*1./4)
              XW=FLOOR(DimX/2.)
              Y0=FLOOR(DimY*1./4)
              YW=FLOOR(DimY/2.)
              s=list()
              s.Add, {value:X0,label:"X0, XW: ",newrow:0B}
              s.Add, {value:XW,label:" ",newrow:0B}
              s.Add, {value:Y0,label:"Y0, YW: ",newrow:1B}
              s.Add, {value:YW,label:" ",newrow:0B}
              c=list()
              c.Add, {value:ft,label:"FFT upon loading",newrow:1B}
              c.Add, {value:0B,label:"Shift half frame before cropping",newrow:1B}
              check=0B
              shift=0B
              Repeat BEGIN
                 IF (XMDataChoiceField(s, c, TITLE="Cropping Parameters", HELP=help) EQ 1) THEN BEGIN
                    X0=s[0].value & XW=s[1].value & Y0=s[2].value & YW=s[3].value & ft=c[0].value & shift=c[1].value
                    IF (((X0 GE 0) AND ((X0+XW) LE DimX )) AND ((Y0 GE 0) AND ((Y0+YW) LE DimY))) THEN check=1B
                 END ELSE BEGIN
                    return, PTR_NEW()
                 END
              ENDREP UNTIL check
              DimX=XW & DimY=YW
           END ;; crop
           dtype =type
           IF ft THEN dtype=6
            case dtype of
               1 : BEGIN
                  APtr=PTR_NEW(BYTARR(DimX,DimY,DimZ))
               END
               2 : BEGIN
                  APtr=PTR_NEW(INTARR(DimX,DimY,DimZ))
               END
               3 : BEGIN
                  APtr=PTR_NEW(LONARR(DimX,DimY,DimZ))
               END
               4 : BEGIN
                  APtr=PTR_NEW(FLTARR(DimX,DimY,DimZ))
               END
               5 : BEGIN
                  APtr=PTR_NEW(DBLARR(DimX,DimY,DimZ))
               END
               6 : BEGIN
                  APtr=PTR_NEW(COMPLEXARR(DimX,DimY,DimZ))
               END
               9 : BEGIN
                  APtr=PTR_NEW(DCOMPLEXARR(DimX,DimY,DimZ))
               END
               12 : BEGIN
                  APtr=PTR_NEW(UINTARR(DimX,DimY,DimZ))
               END
               else: BEGIN
                  print, "error: invalid data type " + MyString(dtype)
                  return, PTR_NEW()
               END
            endcase
            ;; Start loading data
            IF keyword_set(crop) THEN BEGIN
               X1=(X0+DimX-1) & Y1=(Y0+DimY-1)
            END
            minv=[0]
            maxv=[100]
            pbarperc=0
            pbarstep=5
            mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["% done"], STATUS="Loading frames ...", FRAMESTYLE=2)
            FOR i=1,DimZ do BEGIN
               perc=LONG(i*100. / DimZ)
               if (FIX(perc) GT pbarperc) THEN BEGIN
                  pbarperc += pbarstep
                  mybar->Set, 0, pbarperc, TEXT=STRING(pbarperc)
               END
               ;; shift image
               IF ft THEN image=FFT(image)
               IF shift THEN image= SHIFT(image, -FLOOR(FileDimX/2), -FLOOR(FileDimY/2))
               ;; crop image
               IF keyword_set(crop) THEN (*APtr)[*,*,i-1]=image[X0:X1,Y0:Y1] ELSE (*APtr)[*,*,i-1]=image
               ;; load next
               if (i LE (DimZ-1)) THEN BEGIN
                  fn=path+Path_Sep()+(dirhash("files"))[i]
                  ReadImageToArray, fn, image, offset, swap
               END
            END
            obj_destroy, mybar
            IF keyword_set(gui) THEN BEGIN
               name="LargeSet(" + (dirhash("files"))[0]+","+(dirhash("files"))[-1]+")"
               ThrowStack, APtr, name
            END
            return, APtr
           
        END
     END
  END
END


