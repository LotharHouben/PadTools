FUNCTION PermuteArray, array, sequence
  imagesequence=array
  NImages=N_Elements(imagesequence)
   CASE sequence OF
       'reverse': imagesequence=REVERSE(imagesequence)
       'insideout': BEGIN
          tmp=imagesequence
          i=0
          IF ((NImages MOD 2) GT 0) THEN BEGIN
             mid=FLOOR(FLOAT(NImages)/2)
             While (i LE (2*mid)) DO BEGIN
                ;; fill even indices
                if ((i MOD 2) EQ 0) THEN BEGIN
                   imagesequence(i)=tmp(mid-FLOOR(FLOAT(i)/2))                
                END
                ;; fill odd indices
                if ((i MOD 2) EQ 1) THEN BEGIN
                   imagesequence(i)=tmp(mid+CEIL(FLOAT(i)/2))                
                END
                i=i+1
             END
          END ELSE BEGIN
             mid=CEIL(FLOAT(NImages)/2)
             While (i LT (2*mid)) DO BEGIN
                ;; fill even indices
                if ((i MOD 2) EQ 1) THEN BEGIN
                   imagesequence(i)=tmp(mid-CEIL(FLOAT(i)/2))                
                END
                ;; fill odd indices
                if ((i MOD 2) EQ 0) THEN BEGIN
                   imagesequence(i)=tmp(mid+FLOOR(FLOAT(i)/2))                
                END
                i=i+1
             END
          END
       END
       'outsidein': BEGIN
          tmp=imagesequence
          i=0
          IF ((NImages MOD 2) GT 0) THEN BEGIN
             mid=FLOOR(FLOAT(NImages)/2)
             While (i LE (2*mid)) DO BEGIN
                ;; fill even indices
                if ((i MOD 2) EQ 0) THEN BEGIN
                   imagesequence(i)=tmp(mid-FLOOR(FLOAT(i)/2))                
                END
                ;; fill odd indices
                if ((i MOD 2) EQ 1) THEN BEGIN
                   imagesequence(i)=tmp(mid+CEIL(FLOAT(i)/2))                
                END
                i=i+1
             END
          END ELSE BEGIN
             mid=CEIL(FLOAT(NImages)/2)
             While (i LT (2*mid)) DO BEGIN
                ;; fill even indices
                if ((i MOD 2) EQ 1) THEN BEGIN
                   imagesequence(i)=tmp(mid-CEIL(FLOAT(i)/2))                
                END
                ;; fill odd indices
                if ((i MOD 2) EQ 0) THEN BEGIN
                   imagesequence(i)=tmp(mid+FLOOR(FLOAT(i)/2))                
                END
                i=i+1
             END
          END
          imagesequence=reverse(imagesequence)
       END
       'random': BEGIN
          tmp=randomu(seed,NImages)
          imagesequence=imagesequence[SORT(tmp)]
       END
       ELSE:
    END
   return, imagesequence
END 
