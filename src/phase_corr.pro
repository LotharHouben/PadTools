;+ 
; NAME: 
;       PHASE_CORR 
; PURPOSE: 
;       Returns relative offset [xoff,yoff] of two images 
;       using phase correlation 
;       Maximum offset should not exceed +- 5 pixels 
;       in each dimension 
;       Returns -1 if dimensions are not equal 
;       Ref: H, Shekarforoush et al. (1995) INRIA 2707 
; AUTHOR 
;       Mort Canty (2006) 
;       Juelich Research Center 
;       m.canty@fz-juelich.de 
; CALLING SEQUENCE: 
;       shft = Phase_Corr(im1,im2,display=display, $ 
;       subpixel=subpixel) 
; ARGUMENTS: 
;       im1, im2: the images to be correlated 
; KEYWORDS: 
;       Display: (optional) show a surface plot if the 
;                correlation in window with display 
;                number display 
;       Subpixel: returns result to subpixel accuracy if 
;                 set, otherwise nearest integer (default) 
; DEPENDENCIES: 
;       None 
;-------------------------------------------------------- 
;    This program is free software; you can redistribute it and/or modify 
;    it under the terms of the GNU General Public License as published by 
;    the Free Software Foundation; either version 2 of the License, or 
;    (at your option) any later version. 
; 
;    This program is distributed in the hope that it will be useful, 
;    but WITHOUT ANY WARRANTY; without even the implied warranty of 
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;    GNU General Public License for more details. 
; 
;    You should have received a copy of the GNU General Public License 
;    along with this program; if not, write to the Free Software 
;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
;---------------------------------------------------------- 
 
FUNCTION phase_corr, im1, im2, CORRIM = corrim, subpixel = subpixel, kernel=kernel, Filter=filter
if n_elements(subpixel) eq 0 then subpixel = 0 
if n_elements(kernel) eq 0 then kernel = 2 
sz1 = size(im1) 
sz2 = size(im2) 
if (sz1[1] eq sz2[1]) and (sz1[2] eq sz2[2]) then begin
   If keyword_set(filter) THEN BEGIN
      f1 = fft(im1, /double) * filter
      f2 = fft(im2, /double) * filter
   END ELSE BEGIN
      f1 = fft(im1, /double) 
      f2 = fft(im2, /double)
   END
   den = abs(f2*conj(f1)) 
   indices = where(den eq 0, count) 
   if count gt 0 then den[indices]= 0.0000001 
; back-transformed cross power spectrum 
   g =  abs(fft( f2*conj(f1)/den, /inverse, /double )) 
; shift sx,sy pixels
   sx=FIX(0.5*sz1[1]) & sy=FIX(0.5*sz1[2])
   g = shift(g,[sx,sy]) 
; get maximum position 
   pos = (where(g eq max(g)))[0] 
   xoff = pos mod sz1[1] 
   yoff = pos/sz1[1] 
; get 2kernel+1 x 2kernel+1 window centered at maximum 
   gg = g[xoff-kernel:xoff+kernel,yoff-kernel:yoff+kernel] 
; calculate offsets to nearest integer 
   xoff=xoff-sx 
   yoff=yoff-sy 
   if subpixel then begin 
; subtract noise level from window 
      nlev = total(g[20:sz1[1]-1,20:sz1[2]-1])/((sz1[1]-20)*(sz1[2]-20)) 
      gg = gg - nlev 
; get center of gravity in window 
      arr = fltarr(2*kernel+1,2*kernel+1) 
      for i=0,2*kernel do arr[*,i] = indgen(2*kernel+1) 
; correct offset to subpixel accuracy 
      xoff = xoff+total(gg*arr)/total(gg)-FLOAT(kernel) 
      yoff = yoff+total(gg*transpose(arr))/total(gg)-FLOAT(kernel) 
   end 
   if n_elements(corrim) ne 0 then begin 
      corrim=g
   endif 
   return, [xoff,yoff] 
end else return, -1 
END
