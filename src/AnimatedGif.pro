Pro AnimatedGif_IDL, DELAY=delay, LOOP=loop, ITERATIONS=iterations
;; 
;; takes the group pointer g and writes the images 
;; to an animated gif file with name fname
;;
;; requires IDL v6.1 and newer for GIF support
;; delay = delay in 1/100 s
;;
IF LMGR(/Demo) THEN BEGIN
    Print, '% AnimatedGif_IDL: IDL is running in DEMO mode, no file IO'
END Else BEGIN
   ptr=GetRootP()
   IF (NOT(PTR_VALID(ptr))) THEN BEGIN
      print, "% AnimatedGif: Root pointer is invalid"
      return
   END
   c=(*ptr).current
   IF (NOT(PTR_VALID(c))) THEN BEGIN
      print, "% AnimatedGif: Current stack pointer is invalid"
      return
   END
   mytitle=""
   p=(*c).datap
   IF (NOT(Ptr_Valid(p))) THEN BEGIN   
      print, "% AnimatedGif: Stack data pointer is invalid"
      return
   END
   if NOT(keyword_set(delay)) THEN delay=100 ELSE delay=FIX(delay)
   ;;fname=dialog_pickfile(PATH=workingdir, Title="Save Animated Gif to")
   swapend=0
   crgrpd=1
   type=1
   filter="*.gif"
   ftypes=PTR_NEW(["Animated Gif"])
   files = rmd_pickfile(   $
                                ; group_leader = event.top,     $
           filter_in = filter,              $
           get_path = out_path,          $
           cancelled = cancelled,        $
           swapen = swapend,             $
           type = type,             $
           ftypes = ftypes,     $
           crgrp = crgrpd,             $
;                              /multiple, $
           /save)
   if (cancelled EQ 1) then begin
      print, "% AnimatedGIF: cancelled"
      return
   END
   fname=files[0]
   IF (fname NE "") THEN BEGIN
      f="" & path="" & SplitPath, fname, path, f & tmpdir=path
      ;; CHECK WHETHER FILE ALREADY EXISTS
      IF (FileIsWriteable(fname) EQ 1) THEN BEGIN
      END ELSE BEGIN
         Print, "% AnimatedGIF: Export cancelled" 
         return
      END
      CASE (*p).zcoord OF
         1: BEGIN
            N=(*p).SzX
         END
         2: BEGIN
            N=(*p).SzY
         END
         3: BEGIN
            N=(*p).SzZ
         END
         ELSE: BEGIN
            Print, "% AnimatedGIF: Invalid projection identifier" 
            return
         END
      END
      IF ((*p).slicewidth LE 1L) THEN BEGIN      
         Print, "% AnimatedGif: writing "+ MyString(N)+" images to "+fname
         minv=[1] & maxv=[N] & ptext=["adding image nr."]
         InitProgressBar, MINV=minv, MAXV=maxv, TEXT=ptext
         ;; now write images
         FOR K=1,N DO BEGIN
            ProgressBarSet, 0, K, TEXT=STRING(K)
            ;; determine contrast clip off values
            IF ((*p).contrastmode EQ "auto") THEN BEGIN
               CASE (*p).zcoord OF
                  1: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM((*(*p).data)[k-1,*,*])), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  2: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM((*(*p).data)[*,k-1,*])), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  3: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM((*(*p).data)[*,*,k-1])), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  ELSE:
               END 
            END ELSE BEGIN
               CASE (*p).zcoord OF
                  1: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM((*(*p).data)[k-1,*,*]), MIN=((*p).contrast)[0], MAX=((*p).contrast)[1]), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  2: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM((*(*p).data)[*,k-1,*]), MIN=((*p).contrast)[0], MAX=((*p).contrast)[1]), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  3: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM((*(*p).data)[*,*,k-1]), MIN=((*p).contrast)[0], MAX=((*p).contrast)[1]), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  ELSE:
               END 
               
            END    
         END ;; For loop
      END ELSE BEGIN
         maxN=N-((*p).slicewidth-1)
 Print, "% AnimatedGif: writing "+ MyString(maxN)+" images to "+fname
         minv=[1] & maxv=[maxN] & ptext=["adding image nr."]
         InitProgressBar, MINV=minv, MAXV=maxv, TEXT=ptext
         ;; now write images
         FOR K=1,maxN DO BEGIN
            ProgressBarSet, 0, K, TEXT=STRING(K)
            ;; determine contrast clip off values
            ind1=k-1
            ind2=ind1+(*p).slicewidth-1
            norm=1./(ind2-ind1+1.)
            IF ((*p).contrastmode EQ "auto") THEN BEGIN
               CASE (*p).zcoord OF
                  1: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM(norm*Total((*(*p).data)[ind1:ind2,*,*],1))), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  2: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM(norm*Total((*(*p).data)[*,ind1:ind2,*],2))), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  3: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM(norm*Total((*(*p).data)[*,*,ind1:ind2],3))), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  ELSE:
               END 
            END ELSE BEGIN
               CASE (*p).zcoord OF
                  1: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM(norm*Total((*(*p).data)[ind1:ind2,*,*],1)), MIN=((*p).contrast)[0], MAX=((*p).contrast)[1]), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  2: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM(norm*Total((*(*p).data)[*,ind1:ind2,*],2)), MIN=((*p).contrast)[0], MAX=((*p).contrast)[1]), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  3: WRITE_ANIMATEDGIF, fname, BYTSCL(REFORM(norm*Total((*(*p).data)[*,*,ind1:ind2],3)), MIN=((*p).contrast)[0], MAX=((*p).contrast)[1]), /MULTIPLE, DELAY=delay, LOOP=loop, ITERATIONS=iterations
                  ELSE:
               END 
               
            END    
         END ;; For loop
      END
      Print, "% AnimatedGif_IDL: closing file"   
      WRITE_ANIMATEDGIF, fname, /CLOSE
      DestroyProgressBar
   END  ;; if (fname NE "")   
END ;; if lmgr else 
END ;; PRO 
