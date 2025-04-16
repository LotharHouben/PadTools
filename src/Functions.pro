forward_function gauss1D
forward_function parabola2D
forward_function skeweddgauss1D
forward_function sin1D
forward_function sin1Dlin

;+
; NAME:
;   GAUSS1D
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute Gaussian curve given the mean, sigma and area.
;
; MAJOR TOPICS:
;   Curve and Surface Fitting
;
; CALLING SEQUENCE:
;   YVALS = GAUSS1(XVALS, [MEAN, SIGMA, PEAK, CONSTANT], SKEW=skew, PEAK=peak)
;
; DESCRIPTION:
;
;  This routine computes the values of a Gaussian function whose
;  X-values, mean, sigma, and total area are given.  It is meant to be
;  a demonstration for curve-fitting.
;
;  XVALS can be an array of X-values, in which case the returned
;  Y-values are an array as well.  The second parameter to GAUSS1
;  should be an array containing the MEAN, SIGMA, total AREA or PEAK
;  value, and a CONSTANT in that order.
;
; INPUTS:
;   X - Array of X-values.
;
;   [MEAN, SIGMA, AREA] - the mean, sigma and total area of the 
;                         desired Gaussian curve.
;
; INPUT KEYWORD PARAMETERS:
;
;   SKEW - You may specify a skew value.  Default is no skew.
;
;   AREA - if set then PEAK is interpreted as the area value rather
;          than the peak value.
;
; RETURNS:
;
;   Returns the array of Y-values.
;
; EXAMPLE:
;
;   p = [2.2D, 1.4D, 3000.D,0.D]
;   x = dindgen(200)*0.1 - 10.
;   y = gauss1(x, p)
;
;   Computes the values of the Gaussian at equispaced intervals
;   (spacing is 0.1).  The gaussian has a mean of 2.2, standard
;   deviation of 1.4, and total area of 3000.
;
; REFERENCES:
;
; MODIFICATION HISTORY:
;   Written, Jul 1998, CM
;   Correct bug in normalization, CM, 01 Nov 1999
;   Optimized for speed, CM, 02 Nov 1999
;   Added copyright notice, 25 Mar 2001, CM
;   Added PEAK keyword, 30 Sep 2001, CM
;
;  $Id: gauss1.pro,v 1.4 2001/10/13 17:41:48 craigm Exp $
;
;-
; Copyright (C) 1998,1999,2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function gauss1D, x, p, skew=skew, AREA=area, _EXTRA=extra

  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  if n_elements(p) GE 3 then norm = p(2) else norm = x(0)*0 + 1

  u = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  mask = u LT (smax^2)
;; area normalization p(2) would be area then
  if keyword_set(area) then norm = norm / (sqrt(2.D * !dpi)*p(1))
  f = norm * mask * exp(-0.5*temporary(u) * mask)
  mask = 0

  if n_elements(skew) GT 0 then begin
     f = (1.D + skew * (x-p(0))/p(1))*f + p(3)
  end else begin
     f=f+p(3)
  end

  return, f
end

function gauss1D, x, p, skew=skew, AREA=area, _EXTRA=extra

  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  if n_elements(p) GE 3 then norm = p(2) else norm = x(0)*0 + 1

  u = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  mask = u LT (smax^2)
;; area normalization p(2) would be area then
  if keyword_set(area) then norm = norm / (sqrt(2.D * !dpi)*p(1))
  f = norm * mask * exp(-0.5*temporary(u) * mask)
  mask = 0

  if n_elements(skew) GT 0 then begin
     f = (1.D + skew * (x-p(0))/p(1))*f + p(3)
  end else begin
     f=f+p(3)
  end

  return, f
end


function skewnormal1D, x, p, AREA=area, _EXTRA=extra
;; skewed gaussian
;; p(0) = position 1
;; p(1) = 1 sigma width 1
;; p(2) = amplitude 1
;; p(3) = skew parameter
;; p(4) = constant
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  norm = x(0)*0 + 1
  t= ((x-p(0))/(abs(p(1)) > 1e-20))
  u = t*t
;;  u = ((t > 1e-20))^2
  mask = u LT (smax^2)
;; area normalization p(2) would be area then
  if keyword_set(area) then norm = norm / (sqrt(2.D * !dpi)*p(1))
  f = norm * mask * exp(-0.5*temporary(u) * mask)
  mask = 0
;;  f = 1.0D/p(1)*(1.0D+erf(p(3)/sqrt(2)*t))
  f = 1.D/p(1)*((erf(p(3)/sqrt(2)*t))+1.)*f
  f=f+p(4)
  return, f
end

function dgauss1D, x, p, _EXTRA=extra
;; double gauss function 
;; p(0) = position 1
;; p(1) = 1 sigma width 1
;; p(2) = amplitude 1
;; p(3) = position 2
;; p(4) = 1 sigma width 2
;; p(5) = amplitude 2
;; p(6) = constant
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.
  u1 = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  u2 = ((x-p(3))/(abs(p(4)) > 1e-20))^2
  mask = u1 LT (smax^2)
  f = mask * (p(2) * exp(-0.5*temporary(u1) * mask) + p(5) * exp(-0.5*temporary(u2) * mask))
  mask = 0
  f=f+p(6)
  return, f
end

function skeweddgauss1D, x, p, _EXTRA=extra
;; skewed double gauss function 
;; p(0)=position 1
;; p(1)=width 1
;; p(2)=amp 1
;; p(3)=skew 1
;; p(4)=position 2
;; p(5)=width 2
;; p(6)=amp 2
;; p(7)=skew 2
;; p(8)=constant
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.
  u1 = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  u2 = ((x-p(4))/(abs(p(5)) > 1e-20))^2
  mask1 = u1 LT (smax^2)
  mask2 = u2 LT (smax^2)
  f = (1.0D + p(3) * (x-p(0))/p(1)) * (p(2) * exp(-0.5*temporary(u1) * mask1))
  f=f+ (1.0D + p(7) * (x-p(4))/p(5)) * (p(6) * exp(-0.5*temporary(u2) * mask2))
  mask = 0
  f=f+p(8)
  return, f
end



FUNCTION parabola2D, X, Y, P
;;
;; 2D parabola with constant
;;
;; P(0)+P(1)*(X-P(3))^2+P(2)*(Y-P(4))^2
U=FLOAT(X-P(3))^2
V=FLOAT(Y-P(4))^2
;;STOP
return, P(0)+P(1)*U+P(2)*V
END


function pvoigt1D, x, p, _EXTRA=extra
;; gauss plus lorentzian function 
;; p(0)=position 1
;; p(1)=width 1
;; p(2)=amp Lorentz
;; p(3)=position offset Lorentz, fix if required
;; p(4)=width Lorentz
;; p(5)=amp Lorentz
;; p(6)=constant
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.
  u1 = ((x-p(0))/(abs(p(1)) > 1e-20))^2
  u2 = ((x-p(0)-p(3))/(abs(p(4)) > 1e-20))^2
  mask1 = u1 LT (smax^2)
  mask2 = u2 LT (smax^2)
  f =  p(2) * exp(-0.5*temporary(u1) * mask1) + p(5)/(1.+ temporary(u2) * mask2)
  f=f+p(6)
  return, f
end


function sin1D, x, p, _EXTRA=extra
;; sine function with an offset
;; argument needs to be in radians
  f=p[0]+p[1]*Sin(x-p[2])
  return, f
end

function cos1D, x, p, _EXTRA=extra
;; sine function with an offset
;; argument needs to be in radians
  f=p[0]+p[1]*COS(x-p[2])
  return, f
end

function phasemodulus, p
  return, p-!PI*FLOOR(p/!PI)
END

function sin1Dlin, x, p, _EXTRA=extra
;; sine function with an offset
;; argument needs to be in radians
  f=p[0]+p[3]*x+p[1]*Sin(x-p[2])
  return, f
end


function twofiducialtrace, x, p, _EXTRA=extra
;; two fiducial marker rotation 
;; measure traces of two fiducials and calculate the distances DX and
;; DY as a function of the tilt angle X
;; DX^2+DY^2 should follow f with five parameters
;; p[0]: distance of particle 1 from the tilt axis
;; p[1]: phase offset for the rotation of particle 1
;; p[2]: distance of particle 2 from the tilt axis
;; p[3]: phase offset for the rotation of particle 2
;; p[4]: distance between the particles along the tilt axis 
  f=p[0]*(Sin(x-p[1])-p[2]*Sin(x-p[3]))+p[4]
  return, f
end
;;

function fiducialtrace, x, p, _EXTRA=extra
;; two fiducial marker rotation 
;; measure traces of two fiducials and calculate the distances DX and
;; DY as a function of the tilt angle X
;; DX^2+DY^2 should follow f with five parameters
;; p[0]: distance of particle tilt axis
;; p[1]: phase offset for the rotation 
;; p[2]: distance of rotation center from axis
  f=p[0]*Sin(x-p[1])+p[2]
  return, f
end

function fiducialfit, x, p, _EXTRA=extra
;; two fiducial marker rotation 
;; measure traces of two fiducials and calculate the distances DX and
;; DY as a function of the tilt angle X
;; DX^2+DY^2 should follow f with five parameters
;; p[0]: distance of particle tilt axis
;; p[1]: phase offset for the rotation 
;; p[2]: distance of rotation center from axis
  f=p[0]*Cos(x)+p[1]*Sin(x)+p[2]
  return, f
end
