FUNCTION  ROBUST_SIGMA,Y, ZERO=REF, GOODVEC = Q
;
;+
; NAME:
;	ROBUST_SIGMA  
;
; PURPOSE:
;	Calculate a resistant estimate of the dispersion of a distribution.
; EXPLANATION:
;	For an uncontaminated distribution, this is identical to the standard
;	deviation.
;
; CALLING SEQUENCE:
;	result = ROBUST_SIGMA( Y, [ /ZERO, GOODVEC = ] )
;
; INPUT: 
;	Y = Vector of quantity for which the dispersion is to be calculated
;
; OPTIONAL INPUT KEYWORD:
;	/ZERO - if set, the dispersion is calculated w.r.t. 0.0 rather than the
;		central value of the vector. If Y is a vector of residuals, this
;		should be set.
;
; OPTIONAL OUPTUT KEYWORD:
;       GOODVEC = Vector of non-trimmed indices of the input vector
; OUTPUT:
;	ROBUST_SIGMA returns the dispersion. In case of failure, returns 
;	value of -1.0
;
; PROCEDURE:
;	Use the median absolute deviation as the initial estimate, then weight 
;	points using Tukey's Biweight. See, for example, "Understanding Robust
;	and Exploratory Data Analysis," by Hoaglin, Mosteller and Tukey, John
;	Wiley & Sons, 1983, or equation 9 in Beers et al. (1990, AJ, 100, 32)
;
; REVSION HISTORY: 
;	H. Freudenreich, STX, 8/90
;       Replace MED() call with MEDIAN(/EVEN)  W. Landsman   December 2001
;       Don't count NaN values  W.Landsman  June 2010
;
;-
; On_error,2
 compile_opt idl2
 
 EPS = 1.0E-20
 IF KEYWORD_SET(REF) THEN Y0=0. ELSE Y0  = MEDIAN(Y,/EVEN)

; First, the median absolute deviation MAD about the median:

 MAD = MEDIAN( ABS(Y-Y0), /EVEN )/0.6745

; If the MAD=0, try the MEAN absolute deviation:
 IF MAD LT EPS THEN MAD = MEAN( ABS(Y-Y0) )/.80
 IF MAD LT EPS THEN RETURN, 0.0

; Now the biweighted value:
 U   = (Y-Y0)/(6.*MAD)
 UU  = U*U
 Q   = WHERE(UU LE 1.0, COUNT)
 IF COUNT LT 3 THEN BEGIN
   PRINT,'ROBUST_SIGMA: This distribution is TOO WEIRD! Returning -1'
   SIGGMA = -1.
   RETURN,SIGGMA
 ENDIF

 N = TOTAL(FINITE(Y),/INT)      ;In case Y has NaN values          ;
 NUMERATOR = TOTAL( (Y[Q]-Y0)^2 * (1-UU[Q])^4 )
 DEN1  = TOTAL( (1.-UU[Q])*(1.-5.*UU[Q]) )
 SIGGMA = N*NUMERATOR/(DEN1*(DEN1-1.))
 
 IF SIGGMA GT 0. THEN RETURN, SQRT(SIGGMA) ELSE RETURN, 0.

END



FUNCTION AVG,ARRAY,DIMENSION, NAN = NAN, DOUBLE = DOUBLE
;+
; NAME:
;       AVG
; PURPOSE:
;       Return the average value of an array, or 1 dimension of an array
; EXPLANATION:
;       Calculate the average value of an array, or calculate the average
;       value over one dimension of an array as a function of all the other
;       dimensions.
;
;       In 2009, a DIMENSION keyword was added to the IDL MEAN() function,
;       giving it the same capability as AVG().  Thus, the use of AVG() is now
;       **deprecated** in favor of the MEAN() function.    
; CALLING SEQUENCE:
;       RESULT = AVG( ARRAY, [ DIMENSION, /NAN, /DOUBLE ] )
;
; INPUTS:
;       ARRAY = Input array.  May be any type except string.
;
; OPTIONAL INPUT PARAMETERS:
;       DIMENSION = Optional dimension to do average over, integer scalar
;
; OPTIONAL KEYWORD INPUT:
;      /NAN - Set this keyword to cause the routine to check for occurrences of
;            the IEEE floating-point value NaN in the input data.  Elements with
;            the value NaN are treated as missing data.
;      /DOUBLE - By default, if the input Array is double-precision, complex, 
;                or double complex, the result is of the same type;  64 bit
;                integers are also returned as double.   Otherwise the result
;                the  result is floating-point.   Use of the /DOUBLE keyword 
;                forces a double precision output.   Note that internal 
;                computations are always done in double precision.
; OUTPUTS:
;       The average value of the array when called with one parameter.
;
;       If DIMENSION is passed, then the result is an array with all the
;       dimensions of the input array except for the dimension specified,
;       each element of which is the average of the corresponding vector
;       in the input array.
;
;       For example, if A is an array with dimensions of (3,4,5), then the
;       command B = AVG(A,1) is equivalent to
;
;                       B = FLTARR(3,5)
;                       FOR J = 0,4 DO BEGIN
;                               FOR I = 0,2 DO BEGIN
;                                       B[I,J] = TOTAL( A[I,*,J] ) / 4.
;                               ENDFOR
;                       ENDFOR
;
; RESTRICTIONS:
;       Dimension specified must be valid for the array passed; otherwise the
;       input array is returned as the output array.
; PROCEDURE:
;       AVG(ARRAY) = TOTAL(ARRAY, /DOUBLE)/N_ELEMENTS(ARRAY) when called with 
;       one parameter.
; MODIFICATION HISTORY:
;       William Thompson        Applied Research Corporation
;       July, 1986              8201 Corporate Drive
;                               Landover, MD  20785
;       Converted to Version 2      July, 1990
;       Replace SUM call with TOTAL    W. Landsman    May, 1992
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Added /NAN keyword   W. Landsman      July 2000
;       Accept a scalar input value    W. Landsman/jimm@berkeley   November 2000
;       Internal calculations always in double precision W. Landsman March 2002
;       Return NAN if all values in array are NAN  W. Landsman April 2002
;       Fixed coding bug if all values in array are NAN W. Landsman Jan 2004
;-
; ON_ERROR,2
 S = SIZE(ARRAY,/STR)
 IF S.N_ELEMENTS EQ 1 THEN RETURN, array[0]
 IF S.N_ELEMENTS EQ 0 THEN $
        MESSAGE,'Variable must be an array, name= ARRAY'
;
    IF N_PARAMS() EQ 1 THEN BEGIN
        IF KEYWORD_SET(NAN) THEN NPTS = TOTAL(FINITE(ARRAY) ) $
                            ELSE NPTS = N_ELEMENTS(ARRAY)
        IF NPTS EQ 0 THEN AVERAGE = !VALUES.F_NAN ELSE $
                          AVERAGE = TOTAL(ARRAY, NAN=NAN,/DOUBLE) / NPTS
    ENDIF ELSE BEGIN
        IF ((DIMENSION GE 0) AND (DIMENSION LT S.N_DIMENSIONS)) THEN BEGIN
                AVERAGE = TOTAL(ARRAY,DIMENSION+1,NAN=NAN,/DOUBLE) 
; Install a bug workaround since TOTAL(A,/NAN) returns 0 rather than NAN if 
; all A values are NAN. 
                IF KEYWORD_SET(NAN) THEN BEGIN
                     NPTS = TOTAL(FINITE(ARRAY),DIMENSION+1 ) 
                     BAD = WHERE(NPTS EQ 0, NBAD)
                     AVERAGE = AVERAGE/(NPTS>1)
                     IF NBAD GT 0 THEN AVERAGE[BAD] = !VALUES.D_NAN
                 ENDIF ELSE AVERAGE = AVERAGE/S.DIMENSIONS[DIMENSION]
                   
        END ELSE $
                MESSAGE,'*** Dimension out of range, name= ARRAY'
    ENDELSE

; Convert to floating point unless of type double, complex, or L64, or
; if /DOUBLE is set.

 IF NOT KEYWORD_SET(DOUBLE) THEN BEGIN 
    CASE S.TYPE OF
     5: RETURN, AVERAGE
     6: RETURN, COMPLEXARR( FLOAT(AVERAGE), FLOAT(IMAGINARY(AVERAGE)) )
     9: RETURN, AVERAGE
    14: RETURN, AVERAGE
    15: RETURN, AVERAGE
    ELSE: RETURN, FLOAT(AVERAGE)
  ENDCASE
  ENDIF ELSE RETURN, AVERAGE
END

function robust_mean,Y,CUT, Sigma,Num_Rej,GoodInd=GoodInd
;+
; NAME:
;    Robust_Mean 
;
; PURPOSE:
;    Outlier-resistant determination of the mean and standard deviation.
;
; EXPLANATION:
;    Robust_Mean trims away outliers using the median and the median
;    absolute deviation.    An approximation formula is used to correct for
;    the trunction caused by trimming away outliers
;
; CALLING SEQUENCE:
;    mean = Robust_Mean( VECTOR, Sigma_CUT, Sigma_Mean, Num_RejECTED)
;
; INPUT ARGUMENT:
;       VECTOR    = Vector to average
;       Sigma_CUT = Data more than this number of standard deviations from the
;               median is ignored. Suggested values: 2.0 and up.
;
; OUTPUT ARGUMENT:
;       Mean  = the mean of the input vector, numeric scalar
;
; KEYWORDS:
;
;       GoodInd = The indices of the values not rejected
;
; OPTIONAL OUTPUTS:
;Sigma_Mean = the approximate standard deviation of the mean, numeric
;            scalar.  This is the Sigma of the distribution divided by sqrt(N-1)
;            where N is the number of unrejected points. The larger
;            SIGMA_CUT, the more accurate. It will tend to underestimate the
;            true uncertainty of the mean, and this may become significant for
;            cuts of 2.0 or less.
;       Num_RejECTED = the number of points trimmed, integer scalar
;
; EXAMPLE:
;       IDL> a = randomn(seed, 10000)    ;Normal distribution with 10000 pts
;       IDL> Robust_Mean,a, 3, mean, meansig, num    ;3 Sigma clipping   
;       IDL> print, mean, meansig,num
;
;       The mean should be near 0, and meansig should be near 0.01 ( =
;        1/sqrt(10000) ).    
; PROCEDURES USED:
;       AVG() - compute simple mean
; REVISION HISTORY:
;       Written, H. Freudenreich, STX, 1989; Second iteration added 5/91.
;       Use MEDIAN(/EVEN)    W. Landsman   April 2002
;       Correct conditional test, higher order truncation correction formula
;                R. Arendt/W. Landsman   June 2002
;       New truncation formula for sigma H. Freudenriech  July 2002
;-  
;On_Error,2
 if N_params() LT 2 then begin
     print,'Syntax - Robust_Mean(Vector, Sigma_cut, [ Sigma_mean, '
     print,'                                  Num_Rejected ])'
     return,1
 endif  
 Npts    = N_Elements(Y)
 YMed    = MEDIAN(Y,/EVEN)
 AbsDev  = ABS(Y-YMed)
 MedAbsDev = MEDIAN(AbsDev,/EVEN)/0.6745
 IF MedAbsDev LT 1.0E-24 THEN MedAbsDev = AVG(AbsDev)/.8  
 Cutoff    = Cut*MedAbsDev  
 GoodInd = WHERE( AbsDev LE Cutoff, Num_Good )
 GoodPts = Y[ GoodInd ]
 Mean    = AVG( GoodPts )
 Sigma   = SQRT( TOTAL((GoodPts-Mean)^2)/Num_Good )
 Num_Rej = Npts - Num_Good ; Compenate Sigma for truncation (formula by HF):
 SC = Cut > 1.0
 IF SC LE 4.50 THEN $
    SIGMA=SIGMA/(-0.15405+0.90723*SC-0.23584*SC^2+0.020142*SC^3)  
 Cutoff = Cut*Sigma  
 GoodInd = WHERE( AbsDev LE Cutoff, Num_Good )
 GoodPts = Y[ GoodInd ]
 mean    = AVG( GoodPts )
 Sigma   = SQRT( TOTAL((GoodPts-mean)^2)/Num_Good )
 Num_Rej = Npts - Num_Good ; Fixed bug (should check for SC not Sigma) & add higher order correction
 SC = Cut > 1.0
 IF SC LE 4.50 THEN $
    SIGMA=SIGMA/(-0.15405+0.90723*SC-0.23584*SC^2+0.020142*SC^3) 
; Now the standard deviation of the mean:
 Sigma = Sigma/SQRT(Npts-1.)  
return,mean
 END

;+
; NAME:
;      PEAKS
;
;
; PURPOSE:
;      Find the peaks in a vector (spectrum) which lie
;      NSIG above the standard deviation of all peaks in
;      the spectrum
;
; CALLING SEQUENCE:
;      result = peaks(y, nsig [,npk])
;
;
; INPUTS:
;      Y - Vector (usually a spectrum) in which you want to 
;          locate peaks.
;   NSIG - Number of sigma above the standard deviation of 
;          all peaks to search.
;
; OUTPUTS:
;
; RESULT - Vector holding the indecies of the peaks in Y
;
;
; OPTIONAL OUTPUTS:
;
;    NPK - The number of peaks located
;
; NOTES:
;
;    NSIG is NOT the number of sigma above the noise in the spectrum. 
;    It's instead a measure of the significance of a peak. First, all
;    peaks are located. Then the standard deviation of the peaks is 
;    calculated using ROBUST_SIGMA (see Goddard routines online). Then
;    peaks which are NSIG above the sigma of all peaks are selected.
; 
; EXAMPLE:
;
; IDL> y = randomn(seed,2000)
; IDL> pk = peaks(y,2)
; IDL> plot,y
; IDL> oplot,pk,y[pk],ps=2
;
; MODIFICATION HISTORY:
;
;-


function peaks,y,nsig,dum,level=level,count=npk
; on_error,2
d0 = y - shift(y,1)
d1 = y - shift(y,-1)
pk = where(d0 gt 0 and d1 gt 0,npk)
if keyword_set(level) then begin
    bigind = where(y[pk] gt level, npk)
    return,pk[bigind]
endif

if n_elements(nsig) gt 0 then begin
    yp = y[pk]
    mn = robust_mean(yp,4)
    sig = robust_sigma(yp)
    bigind = where(yp gt mn + nsig*sig, npk)
    if npk gt 0 then big = pk[bigind] else big = -1
endif else big = pk

return,big
end

