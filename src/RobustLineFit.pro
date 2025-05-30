FUNCTION  ROB_SIGMA,Y, ZERO=REF, GOODVEC = Q
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
 On_error,2
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
   PRINT,'ROB_SIGMA: This distribution is TOO WEIRD! Returning -1'
   SIGGMA = -1.
   RETURN,SIGGMA
 ENDIF

 N = TOTAL(FINITE(Y),/INT)      ;In case Y has NaN values          ;
 NUMERATOR = TOTAL( (Y[Q]-Y0)^2 * (1-UU[Q])^4 )
 DEN1  = TOTAL( (1.-UU[Q])*(1.-5.*UU[Q]) )
 SIGGMA = N*NUMERATOR/(DEN1*(DEN1-1.))
 
 IF SIGGMA GT 0. THEN RETURN, SQRT(SIGGMA) ELSE RETURN, 0.

END

FUNCTION ROB_CHECKFIT,Y, YFIT, EPS, DEL, SIG, FRACDEV, NGOOD,W,B,$
                      BISQUARE_LIMIT=BLIM
;+
; NAME:
;	ROB_CHECKFIT
; PURPOSE:
;	Used by ROBUST_... routines to determine the quality of a fit and to
;	return biweights.
; CALLING SEQUENCE:
;	status = ROB_CHECKFIT( Y, YFIT, EPS, DEL, SIG, FRACDEV, NGOOD, W, B
;				BISQUARE_LIMIT = )
; INPUT:
;	Y     = the data
;	YFIT  = the fit to the data
;	EPS   = the "too small" limit
;	DEL   = the "close enough" for the fractional median abs. deviations
; RETURNS:
;	Integer status. if =1, the fit is considered to have converged
;
; OUTPUTS:
;	SIG   = robust standard deviation analog
;	FRACDEV = the fractional median absolute deviation of the residuals
;	NGOOD   = the number of input point given non-zero weight in the 
;		calculation
;	W     = the bisquare weights of Y
;	B     = residuals scaled by sigma
;
; OPTIONAL INPUT KEYWORD:
;	BISQUARE_LIMIT = allows changing the bisquare weight limit from 
;			default 6.0
; PROCEDURES USED:
;       ROB_SIGMA()
; REVISION HISTORY:
;	Written, H.T. Freudenreich, HSTX, 1/94
;-

  ISTAT = 0

  IF KEYWORD_SET(BLIM) THEN BFAC=BLIM ELSE BFAC=6.

  DEV = Y-YFIT

  SIG=ROB_SIGMA(DEV,/ZERO)
; If the standard deviation = 0 then we're done:
  IF SIG LT EPS THEN GOTO,DONE 

  IF DEL GT 0. THEN BEGIN
   ; If the fraction std. deviation ~ machine precision, we're done:
     Q=WHERE( ABS(YFIT) GT EPS, COUNT )
     IF COUNT LT 3 THEN FRACDEV = 0. ELSE $
                        FRACDEV = MEDIAN(ABS( DEV[Q]/YFIT[Q] ),/EVEN )
     IF FRACDEV LT DEL THEN GOTO,DONE
  ENDIF

  ISTAT = 1

; Calculate the (bi)weights:
  B = ABS(DEV)/(BFAC*SIG)
  S = WHERE( B GT 1.0,COUNT )  &  IF COUNT GT 0 THEN B[S] = 1.
  NGOOD = N_ELEMENTS(Y)-COUNT
  
  W=(1.-B^2)
  W=W/TOTAL(W)
DONE:
RETURN, ISTAT
END

FUNCTION  ROBUST_LINEFIT,XIN,YIN,YFIT,SIG,SS, NUMIT=THIS_MANY, BISECT=TYPE, $
                         Bisquare_Limit=Bisquare_Limit, $
                         Close_Factor=Close_Factor
;+
; NAME:
;       ROBUST_LINEFIT
;
; PURPOSE:
;       An outlier-resistant two-variable linear regression. 
; EXPLANATION:
;       Either Y on X or, for the case in which there is no true independent 
;       variable, the bisecting line of Y vs X and X vs Y is calculated. No 
;       knowledge of the errors of the input points is assumed.
;
; CALLING SEQUENCE:
;       COEFF = ROBUST_LINEFIT( X, Y, YFIT, SIG, COEF_SIG, [ /BISECT,
;                       BiSquare_Limit = , Close_factor = , NumIT = ] )
;
; INPUTS:
;       X = Independent variable vector, floating-point or double-precision
;       Y = Dependent variable vector
;
; OUTPUTS:
;       Function result = coefficient vector. 
;       If = 0.0 (scalar), no fit was possible.
;       If vector has more than 2 elements (the last=0) then the fit is dubious.
;
; OPTIONAL OUTPUT PARAMETERS:
;       YFIT = Vector of calculated y's
;       SIG  = The "standard deviation" of the fit's residuals. If BISECTOR 
;               is set, this will be smaller by ~ sqrt(2).
;       COEF_SIG  = The estimated standard deviations of the coefficients. If 
;               BISECTOR is set, however, this becomes the vector of fit 
;               residuals measured orthogonal to the line.
;
; OPTIONAL INPUT KEYWORDS:
;       NUMIT = the number of iterations allowed. Default = 25
;       BISECT  if set, the bisector of the "Y vs X" and "X vs Y" fits is 
;               determined.  The distance PERPENDICULAR to this line is used 
;               in calculating weights. This is better when the uncertainties 
;               in X and Y are comparable, so there is no true independent 
;               variable.  Bisquare_Limit  Limit used for calculation of 
;               bisquare weights. In units of outlier-resistant standard 
;               deviations. Default: 6.
;               Smaller limit ==>more resistant, less efficient
; Close_Factor  - Factor used to determine when the calculation has converged.
;               Convergence if the computed standard deviation changes by less 
;               than Close_Factor * ( uncertainty of the std dev of a normal
;               distribution ). Default: 0.03.
; SUBROUTINE CALLS:
;       ROB_CHECKFIT
;       ROB_SIGMA, to calculate a robust analog to the std. deviation
;
; PROCEDURE:
;       For the initial estimate, the data is sorted by X and broken into 2
;       groups. A line is fitted to the x and y medians of each group.
;       Bisquare ("Tukey's Biweight") weights are then calculated, using the 
;       a limit of 6 outlier-resistant standard deviations.
;       This is done iteratively until the standard deviation changes by less 
;       than CLOSE_ENOUGH = CLOSE_FACTOR * {uncertainty of the standard 
;               deviation of a normal distribution}
;
; REVISION HISTORY:
;       Written, H. Freudenreich, STX, 4/91.
;       4/13/93 to return more realistic SS's HF
;       2/94 --more error-checking, changed convergence criterion HF
;       5/94 --added BISECT option. HF.
;       8/94 --added Close_Factor and Bisquare_Limit options  Jack Saba.
;       4/02 --V5.0 version, use MEDIAN(/EVEN)  W. Landsman
;-

ON_ERROR,2

IF N_ELEMENTS(THIS_MANY) GT 0 THEN ITMAX = THIS_MANY ELSE ITMAX=25

IF  N_elements(Close_Factor) EQ 0  THEN Close_Factor = 0.03

DEL = 5.0E-07
EPS = 1.0E-20

N = N_ELEMENTS(XIN)

; First, shift X and Y to their centers of gravity:
 X0 = TOTAL(XIN)/N  &  Y0=TOTAL(YIN)/N
 X = XIN-X0       &  Y = YIN-Y0 

 CC=FLTARR(2)
 SS=FLTARR(2)
 SIG=0.
 YFIT=YIN
 BADFIT=0
 NGOOD=N

; Make sure the independent variables are not all the same.
 XRANGE=MAX(X)-MIN(X)
 AVEX= (TOTAL(ABS(X))/N) > EPS
 IF (XRANGE LT EPS) OR (XRANGE/AVEX LT DEL) THEN BEGIN
   message,'Independent variables the same. No fit possible.',/CON
   RETURN,0.
ENDIF 

; First guess: 
LSQ=0
YP=Y
IF N GT 5 THEN BEGIN
;  We divide the data into 2 groups and fit a line to their X and Y medians.
   S=SORT(X) &  U=X[S]  &  V=Y[S]
   NHALF=N/2-1
   X1=MEDIAN(U[0:NHALF],/EVEN) & X2=MEDIAN(U[NHALF+1:N-1],/EVEN)
   Y1=MEDIAN(V[0:NHALF],/EVEN) & Y2=MEDIAN(V[NHALF+1:N-1],/EVEN)
   IF ABS(X2-X1) LT EPS THEN BEGIN
;     The X medians are too close. Select the end-points instead.
      X1=U[0]  &  X2=U[N-1]
      Y1=V[0]  &  Y2=V[N-1]
   ENDIF
   CC[1]=(Y2-Y1)/(X2-X1)  & CC[0]=Y1-CC[1]*X1
   YFIT = CC[0]+CC[1]*X
   ISTAT = ROB_CHECKFIT(YP,YFIT,EPS,DEL,  SIG,FRACDEV,NGOOD,W,S)
   IF NGOOD LT 2 THEN LSQ=1
ENDIF 
IF (LSQ EQ 1) OR (N LT 6) THEN BEGIN  ; Try a least-squares fit
   SX=TOTAL(X) & SY=TOTAL(Y) & SXY=TOTAL(X*Y) & SXX=TOTAL(X*X) 
   D=SXX-SX*SX
   IF ABS(D) LT EPS THEN BEGIN
      PRINT,'ROBUST_LINEFIT: No fit possible.'
      RETURN,0.
   ENDIF 
   YSLOP=(SXY-SX*SY)/D      &   YYINT=(SXX*SY-SX*SXY)/D 

   IF KEYWORD_SET(TYPE) THEN BEGIN    
;     Get the X vs Y line.
      SYY=TOTAL(Y*Y)
      D=SYY-SY*SY
      IF ABS(D) LT EPS THEN BEGIN
         PRINT,'ROBUST_LINEFIT: No fit possible.'
         RETURN,0.
      ENDIF
      TSLOP=(SXY-SY*SX)/D   &   TYINT=(SYY*SX-SY*SXY)/D 
;     Now invert it to get the form Y=a+bX:
      IF ABS(TSLOP) LT EPS THEN BEGIN
         message,'No fit possible.',/CON
         RETURN,0.
      ENDIF
      XSLOP = 1./TSLOP       &   XYINT=-TYINT/TSLOP
;     Now calculate the equation of the bisector of the 2 lines:
      IF YSLOP GT XSLOP THEN BEGIN
         A1=YYINT  &  B1=YSLOP  &  R1=SQRT(1.+YSLOP^2)
         A2=XYINT  &  B2=XSLOP  &  R2=SQRT(1.+XSLOP^2)
      ENDIF ELSE BEGIN
         A2=YYINT  &  B2=YSLOP  &  R2=SQRT(1.+YSLOP^2)
         A1=XYINT  &  B1=XSLOP  &  R1=SQRT(1.+XSLOP^2)
      ENDELSE
      YINT = (R1*A2+R2*A1)/(R1+R2) 
      SLOP = (R1*B2+R2*B1)/(R1+R2)
;     Now find the orthogonal distance to the line. Convert to normal
;     coordinates.
      R = SQRT(1.+SLOP^2)  & IF YINT GT 0. THEN R=-R
      U1 = SLOP/R  & U2=-1./R  &  U3=YINT/R 
      YP = U1*X+U2*Y+U3  ; = orthog. distance to line
      YFIT = FLTARR(N)   ; to fool ROB_CHECKFIT
      SS=YP
   ENDIF ELSE BEGIN
      SLOP=YSLOP               &   YINT=YYINT
      YFIT = YINT+SLOP*X
   ENDELSE
   CC = [YINT,SLOP]
   ISTAT = ROB_CHECKFIT(YP,YFIT,EPS,DEL,  SIG,FRACDEV,NGOOD,W,S)
ENDIF

 IF ISTAT EQ 0 THEN GOTO,AFTERFIT

 IF NGOOD LT 2 THEN BEGIN
   message,'Data Dangerously Weird. Fit Questionable.',/CON
   BADFIT=1
   GOTO,AFTERFIT
ENDIF

; Now iterate until the solution converges:
 SIG_1= (100.*SIG) < 1.0E20
 CLOSE_ENOUGH = Close_Factor * SQRT(.5/(N-1)) > DEL
 DIFF= 1.0E20
 NIT = 0
 WHILE( (DIFF GT CLOSE_ENOUGH) AND (NIT LT ITMAX) ) DO BEGIN
  NIT=NIT+1
  SIG_2=SIG_1
  SIG_1=SIG
  SX=TOTAL(W*X) & SY=TOTAL(W*Y) & SXY=TOTAL(W*X*Y) & SXX=TOTAL(W*X*X) 
  D=SXX-SX*SX
  IF ABS(D) LT EPS THEN BEGIN
     message,'No fit possible.',/CON
     RETURN,0.
  ENDIF 
  YSLOP = (SXY-SX*SY)/D      &   YYINT = (SXX*SY-SX*SXY)/D 
  SLOP = YSLOP               &   YINT = YYINT
  IF KEYWORD_SET(TYPE) THEN BEGIN    
;    Get the X vs Y line.
     SYY=TOTAL(W*Y*Y) 
     D=SYY-SY*SY
     IF ABS(D) LT EPS THEN BEGIN
        PRINT,'ROBUST_LINEFIT: No fit possible.'
        RETURN,0.
     ENDIF
     TSLOP=(SXY-SY*SX)/D   &   TYINT=(SYY*SX-SY*SXY)/D 
;    Now invert it to get the form Y=a+bX:
     IF ABS(TSLOP) LT EPS THEN BEGIN
        PRINT,'ROBUST_LINEFIT: No fit possible.'
        RETURN,0.
     ENDIF
     XSLOP=1./TSLOP       &   XYINT=-TYINT/TSLOP
;    Now calculate the equation of the bisector of the 2 lines:
     IF YSLOP GT XSLOP THEN BEGIN
        A1=YYINT  &  B1=YSLOP  &  R1=SQRT(1.+YSLOP^2)
        A2=XYINT  &  B2=XSLOP  &  R2=SQRT(1.+XSLOP^2)
     ENDIF ELSE BEGIN
        A2=YYINT  &  B2=YSLOP  &  R2=SQRT(1.+YSLOP^2)
        A1=XYINT  &  B1=XSLOP  &  R1=SQRT(1.+XSLOP^2)
     ENDELSE
     YINT=(R1*A2+R2*A1)/(R1+R2)
     SLOP=(R1*B2+R2*B1)/(R1+R2)
     R=SQRT(1.+SLOP^2)  & IF YINT GT 0. THEN R=-R
     U1=SLOP/R  & U2=-1./R  &  U3=YINT/R 
     YP=U1*X+U2*Y+U3  ; = orthog distance to line
     YFIT=FLTARR(N) & YFIT[*]=0.
     SS=YP
  ENDIF ELSE BEGIN
     YFIT = YINT+SLOP*X
  ENDELSE
  CC=[YINT,SLOP] 
  ISTAT=ROB_CHECKFIT(YP,YFIT,EPS,DEL,  SIG,FRACDEV,NGOOD,W,S, $
                     Bisquare_Limit=Bisquare_Limit )

  IF ISTAT EQ 0 THEN GOTO,AFTERFIT
  IF NGOOD LT 2 THEN BEGIN
     PRINT,'ROBUST_LINEFIT: Data Dangerously Weird. Fit Questionable.'
     BADFIT=1
     GOTO,AFTERFIT
  ENDIF
  DIFF = (ABS(SIG_1-SIG)/SIG) < (ABS(SIG_2-SIG)/SIG)
ENDWHILE

AFTERFIT:
; Untranslate the coefficients
 CC[0] = CC[0]+Y0-CC[1]*X0

IF N_PARAMS(0) GT 2 THEN YFIT = CC[0] + CC[1]*XIN
 IF KEYWORD_SET(BISECT) THEN RETURN,CC

 IF (N_PARAMS(0) GT 3) AND (SIG GT EPS) AND (NGOOD GT 2) THEN BEGIN
   ; Here we use an empirical formula to approximate the standard deviations
   ; of the coefficients. They are usually accurate to ~ 25%.
   SX2 = TOTAL(W*X*X) 
   UU = S*S
   DEV = YIN-YFIT
   Y0 = TOTAL( W*DEV )
   Q = WHERE(UU LE 1.0,COUNT)
   DEN1 = ABS(TOTAL( (1.-UU[Q])*(1.-5.*UU[Q]) ))
   SIG = ROB_SIGMA(DEV,/ZERO)
   ; Now empirically derived estimates of the uncertainties:
   SS[0] = SIG/SQRT(DEN1)/1.105 
   SS[1] = SS[0]/SQRT(SX2)
   ; Take the X shift into account:
   SS[0] = SQRT(SS[0]^2+X0*SS[1]^2)
 ENDIF

 IF BADFIT EQ 1 THEN CC=[CC,0.]

 RETURN,CC
 END
