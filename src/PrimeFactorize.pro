FUNCTION LargestPrimeFactor, n_
n=n_
largestPrimeFactor=n
;; largest composite factor must be smaller than sqrt
sqrt_max = long(ceil(sqrt(double(n)))) ;
largest = -1L                          ;
for i = 2L, sqrt_max DO BEGIN
   if ((n MOD i) EQ 0) THEN BEGIN
      test = largestPrimeFactor(n/i) ;
      if (test GT largest) THEN BEGIN
         largest = test         ;
      END
   END  
END
if (largest NE -1) THEN return, largest
;;  number is prime
return, n                        ;
END 

FUNCTION PrimeFactorLE, n_, k
;; is the largest prime factor of n larger than k?
;; k=2,3,5,7
  n=n_
  ;; print, "PrimeFactorLE checking:", n

rem=(n mod 2) 
while (rem EQ  0) DO BEGIN
   n=n/2
   rem=(n mod 2) 
END
IF (k EQ 2) THEN return, (n EQ 1) ;; n is a power of 2 exactly when the last division results in 1
;; if k is two then we are done, otherwise we test the remainder for k=3
rem=(n mod 3) 
while (rem EQ  0) DO BEGIN
   n=n/3
   rem=(n mod 3) 
END
IF (k EQ 3) THEN return, (n EQ 1)
rem=(n mod 5) 
while (rem EQ  0) DO BEGIN
   n=n/5
   rem=(n mod 5) 
END
IF (k EQ 5) THEN return, (n EQ 1)
rem=(n mod 7) 
while (rem EQ  0) DO BEGIN
   n=n/7
   rem=(n mod 7) 
END
IF (k EQ 7) THEN return, (n EQ 1)
END

Function primefactortable, k, Nmax
  l=list()
  For i=2, Nmax DO BEGIN
     IF (PrimeFactorLE(i, k)) THEN l.Add,i
  END
  return, l
END

Function closestprimefactor, k, N_, LARGER=larger, MINDIST=mindist
  ;; k=radix
  ;; STOP
  IF (N_ LT k) THEN return, 2
  if (k LE 1) THEN return, N_
  n=N_
  if keyword_set(larger) then BEGIN
     ;; larger or equal N_
     While Not(PrimeFactorLE(n, k)) DO n=n+1 
     return, n
  END 
  if keyword_set(mindist) THEN BEGIN
     m=N_
     While Not(PrimeFactorLE(m, k)) DO m=m+1
     While (Not(PrimeFactorLE(n, k)) AND (n GE k)) DO n=n-1
     IF ((m-N_) GT (N_-n)) THEN return, n ELSE return, m 
  END
  ;; smaller or equal N_
  While (Not(PrimeFactorLE(n, k)) AND (n GE k)) DO n=n-1
  return, n
END

