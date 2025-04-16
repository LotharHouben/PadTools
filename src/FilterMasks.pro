PRO CreateBinaryMask, THRESHOLD=thresh, INVERSE=inverse
  ptr=GetRootP()
  If NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: No 3D Data found."
     return
  END
  ptr=(*ptr).current
  curr=ptr
  If NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% Menu: Current 3D Data invalid."
     return
  END
  ptr=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  Menu: Current 3D Data holds no data."
     return
  END
  N=Size(*(*ptr).data)
  IF (N[0] EQ 2) THEN BEGIN
     IF NOT(keyword_set(inverse)) THEN inverse=0B
     IF NOT(keyword_set(threshold)) THEN BEGIN
        threshold=((*ptr).contrast)[0]
                 help=["Binarize Image",$
                       "", $
                       "Creates a binary image from the current image given a cutoff value.", $
                       " The input image has to be two-dimensional. ", $
                       "", $
               "Parameters:", $
               "", $
               "Cutoff: Threshold value for 0-1 conversion.", $
               "        Values higher than the cutoff are mapped to 1.", $      
               "Invert: Invert the binary map. Values below the cutoff will be mapped tp 1.", $
         ""]
         s=list()
         s.Add, {value:threshold,label:"Cutoff: ",newrow:0B}
         c=list()
         c.Add, {value:inverse,label:"Invert",newrow:0B}
         
         IF (XMDataChoiceField(s, c, TITLE="Binary Image  Parameters", HELP=help) EQ 1) THEN BEGIN
            threshold=s[0].value
            inverse=c[0].value
         END ELSE return
         maskim=BYTARR(N[1],N[2])
            op=" > "
         IF inverse THEN BEGIN
            B=Where((*(*ptr).data) LT threshold, count)
            op=" < "
         END ELSE B=Where((*(*ptr).data) GE threshold, count)
         maskim(B)=1B
         ThrowImage, maskim, TITLE="BinaryMask("+(*curr).name+op+MyString(threshold)+")"
     END
  END ELSE BEGIN
     printtocon, "%  Menu: Current data is not a two-dimensional array."
  END
END
