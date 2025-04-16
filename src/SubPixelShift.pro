;-----------------------------------------------------------------
; subshift.pro
;
; (c)23.1.1998 by Andreas Reigber
;------------------------------------------------------------------
; Shift 2D complex array. Just like the normal shift, but with the 
; possibility of subpixel shifts, i.e. a=subshift(b,7.3,1.9)
;------------------------------------------------------------------
; This software has been released under the terms of the GNU Public
; license. See http://www.gnu.org/copyleft/gpl.html for details.
;------------------------------------------------------------------
function subshift,bild,xsh,ysh
	s=size(bild)
	xsize=s[1]
	ysize=s[2]
	sbild=bild
	if (floor(xsh) eq xsh) and (floor(ysh) eq ysh) then begin 
		;; print,'Keine Subpixeloffsets'
		sbild = shift(sbild,xsh,ysh)
	endif else begin
		phax  = -xsh*2*!pi/xsize
		phay  = -ysh*2*!pi/ysize
		freqx = shift((findgen(xsize)-xsize/2),xsize/2)
		freqy = shift((findgen(ysize)-ysize/2),ysize/2)
		zerax = fltarr(xsize)+1
		zeray = fltarr(ysize)+1
		shift_matrix = ((phax*freqx)#zeray)+(zerax#(phay*freqy))	
		sbild = fft(fft(sbild,-1) * exp(complex(0,shift_matrix)),1)
	endelse
	return, REAL_PART(sbild)
end
