;+
; NAME:
;    circletransform
;
; PURPOSE:
;    Performs a transform similar to a Hough transform
;    for detecting circular features in an image.
;
; CATEGORY:
;    Image analysis, feature detection
;
; CALLING SEQUENCE:
;    b = circletransform(a)
;
; INPUTS:
;    a: [nx,ny] gray-scale image data
;
; KEYWORD PARAMETERS:
;    range: maximum range over which a circle's center will be
;        sought.  Default: 100 pixels
;
;    noise: estimate for additive pixel noise.  Default: 1 [grey level]
;
;    smoothfactor: range over which to smooth input image before
;        computing gradient.  Default: 5 pixels
;        Setting this to 1 prevents smoothing.
;
;    deinterlace: if set to an odd number, then only perform
;        transform on odd field of an interlaced image.
;        If set to an even number, transform even field.
;
; OUTPUTS:
;    b: [nx,ny] circle transform.  Peaks correspond to estimated
;        centers of circular features in a.
;
; PROCEDURE:
;    Compute the gradient of the image.  The local gradient at each
;    pixel defines a line along which the center of a circle may
;    lie.  Cast votes for pixels along the line in the transformed
;    image.  The pixels in the transformed image with the most votes
;    correspond to the centers of circular features in the original
;    image.
;
; REFERENCE:
; F. C. Cheong, B. Sun, R. Dreyfus, J. Amato-Grill, K. Xiao, L. Dixon
; & D. G. Grier,
; Flow visualization and flow cytometry with holographic video
; microscopy, Optics Express 17, 13071-13079 (2009)
;
; EXAMPLE:
;    IDL> b = circletransform(a)
;
; MODIFICATION HISTORY:
; 10/07/2008 Written by David G. Grier, New York University.
; 01/26/2009 DGG Added DEINTERLACE keyword. Gracefully handle
;    case when original image has no features. Documentation cleanups.
; 02/03/2009 DGG Replaced THRESHOLD keyword with NOISE.
; 06/10/2010 DGG Documentation fixes.  Added COMPILE_OPT.
; 05/02/2012 DGG Updated keyword parsing.  Formatting.
; 06/24/2012 DGG Streamlined index range checking in inner loop
;    to improve efficiency.
; 07/16/2012 DGG IMPORTANT: Center results on pixels, not on vertices!
;    Use array_indices for clarity.
;
; Copyright (c) 2008-2012 David G. Grier
;
;
; UPDATES:
;    The most recent version of this program may be obtained from
;    http://physics.nyu.edu/grierlab/software.html
; 
; LICENSE:
;    This program is free software; you can redistribute it and/or
;    modify it under the terms of the GNU General Public License as
;    published by the Free Software Foundation; either version 2 of the
;    License, or (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;    General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;    02111-1307 USA
;
;    If the Internet and WWW are still functional when you are using
;    this, you should be able to access the GPL here: 
;    http://www.gnu.org/copyleft/gpl.html
;-

function circletransform, a_, $
                          range = range, $
                          noise = noise, $
                          smoothfactor = smoothfactor, $
                          deinterlace = deinterlace

COMPILE_OPT IDL2

if ~isa(range, /scalar, /number) then range = 100.
if ~isa(noise, /scalar, /number) then noise = 1.
if ~isa(smoothfactor, /scalar, /number) then smoothfactor = 5
dodeinterlace = isa(deinterlace, /scalar, /number)

sz = size(a_, /dimensions)
nx = sz[0]
ny = sz[1]

b = fltarr(nx, ny)

wx = smoothfactor
if dodeinterlace then begin
   n0 = deinterlace mod 2
   a = float(a_[*, n0:*:2])
   wy = wx / 2
endif else begin
   n0 = 0
   a =  float(a_)
   wy = wx
endelse

if smoothfactor gt 1 then $
   a = smooth(a, [wx, wy], /edge_truncate)

dx = rebin([-1., 0., 1.], 3, 3)

dadx = convol(a, dx, /center, /edge_truncate)
dady = convol(a, transpose(dx), /center, /edge_truncate)
if dodeinterlace then dady /= 2.

grada = sqrt(dadx^2 + dady^2)

w = where(grada gt 2.*noise, npts)

if npts le 0 then return, b

xy = array_indices(grada, w)
if dodeinterlace then xy[1,*] = 2.*xy[1,*] + n0
xy += 1.                       ; to center on pixels

costheta = dadx[w] / grada[w]
sintheta = dady[w] / grada[w]

r = findgen(2.*range + 1.) - range

for i = 0L, npts-1L do begin 
   x = round(xy[0,i] + r * costheta[i]) > 0 < nx-1
   y = round(xy[1,i] + r * sintheta[i]) > 0 < ny-1
   b[x, y] += 1. 
endfor

return, b
end


PRO meshgrid, x,y, retx, rety

nx=n_elements(x) & ny=n_elements(y)
if nx eq 0 then on_error,0 
if ny eq 0 then begin
y=x
ny=n_elements(y)
endif
x1=x#replicate(1,ny)
y1=y##replicate(1,nx)
retx=x1
rety=y1
end


;=================================================================================
;; FUNCTION CircleHoughLink,
;; https://www.cis.rit.edu/class/simg782.old/talkHough/HoughLecCircles.html
;=================================================================================
FUNCTION CircleHoughLink,xpts,ypts,THRESHOLD=thresh,NCIRCLES=ncircles,$
	XBINS=N,YBINS=M,HOUGH_MATRIX=A1,FINAL_MATRIX=A,H_MATRIX=H,$
	VERBOSE=verbose,RADIUS=radius,XMAX=xmax,$
	YMAX=ymax,NHD_RAD=nhd,NTHETA=ntheta
;+
;RESULT=CircleHoughLink(xpts,ypts,RADIUS=radius) finds circles in
;sets of points whose coordinates are given by xpts,ypts and whose
;radius matches the list of possible radii in the array RADIUS. The funcion
;uses the circle Hough transform (CHT) and is controlled by a number of keyword
;parameters.
;
;RESULT is an array of anonymous structures with the following tags
;and initial values:
;
;Index:-1L			Each line has a unique index [1,2,...,ncircles]
;Cx:0.0				X coordinate of center
;Cy:0.0				Y coordinate of center
;R:0.0				Radius of the circle
;Location:0L}		The location in the Hough Parameter matrix
;
;EXAMPLE
;Suppose that IMG is a binary image. An image B of boundary
;points that describe object boundaries can be found by erosion
;with a structuring element S. The circle descriptions of the
;boundaries can be found and plotted as an overlay on the
;image. If we assume a known radius R then use the following.
;
;S=REPLICATE(1,3,3)
;B=image AND NOT ERODE(IMG,S)
;imSiz=SIZE(B)
;WINDOW,/FREE,XSIZE=imSiz[1],YSIZE=imSiz[2],TITLE='Edges'
;circles=CircleHoughLink(x,y,RADIUS=R)
;ncircles=MAX(circles.Index) ;Find the number of lines found.
;theta=FLTARR(101)*!pi/50
;FOR k=0,circles-1 DO PLOTS,circles[k].Cx+circles[k].R*cos(theta),$
;	circles[k].Cy+circles[k].R*sin(theta)
;
;DESCRIPTION AND KEYWORDS
;The algorithm utilizes the parametric form of the description of
;a circle to search for the center. The radius is assumed to be
;given. The returned structure contains the centers and radius
;of each circle found.
;
;KEYWORDS: XBINS, YBINS, NTHETA, HOUGH_MATRIX
;The (Cx,Cy) cells correspond to a matrix A, which
;accumulates the count of the number of parametric curves in
;parametric space that pass through each cell. The number of
;cells can be set through the keywords XBINS and YBINS.
;The default values are XBINS=100, YBINS=100. The
;accumulated A matrix can be accessed via the keyword
;HOUGH_MATRIX. A[i,j] gives the number of circles that have
;Cx and Cy values that correspond to cell A[i,j].
;The maximum values may be given by the keywords XMAX and YMAX.
;The default is XMAX=YMAX=1000. The Hough bins are "filled" by tracing out
;a locus of points around each (x,y) point. The number of points
;in the circular locus in NTHETA (default 100).
;;
;KEYWORDS: THRESHOLD, ncircles
;The search for circles progresses by cycling through the A matrix
;and using a current maximum value to identify a parameter cell.
;The search continues until it finds NCIRCLES or there are no
;remaining cell counts that exceed THRESHOLD. The default value
;of NCIRCLES=1 and the default THRESHOLD=0.9*MAX(A). The actual
;number of circles returned is N_ELEMENTS(Result), which may be less
;than NCIRCLES.
;
;KEYWORDS: NHD_RAD, FINAL_MATRIX
;The search algorithm provides some parameters that facilitate
;weeding out unwanted circles. Each time a cell of A is used then
;that element is set to a value of -2 and the cells in a
;neighborhood of size NHD_RAD in each direction are set to -1. This
;marks the central cell and the neighborhood and prevents any A-cells
;in the neighborhood from being used. The default value is NHD_RAD=1.
;The cells are blocked out in the neighborhood in the same radius
;plane as the selected cell. Cells are not blocked out across
;radius planes.
;
;The FINAL_MATRIX keyword can be used to access the last parameter
;count matrix used by the algorithm just before it exited. This is
;useful in examining the parameter combinations that have
;contributed to the circles that were returned. A visual presentation
;can be constructed as in the code below.
;
;KEYWORD VERBOSE
;Setting VERBOSE=1 produces a printout that summarizes each circle as the
;search progresses. The default is VERBOSE=0.
;
;HISTORY
;H. Rhody, October, 1999 for Digital Image Processing class.
;-

;Set up defaults
IF KEYWORD_SET(ncircles) EQ 0 THEN ncircles=1
IF N_ELEMENTS(N) EQ 0 THEN N=100
IF N_ELEMENTS(M) EQ 0 THEN M=100
N=LONG(N) & M=LONG(M) & MN=M*N
IF KEYWORD_SET(ntheta) EQ 0 THEN ntheta=100
IF KEYWORD_SET(verbose) EQ 0 THEN verbose=0
;Set up the limits of the search in the X and Y directions.
IF KEYWORD_SET(xmax) EQ 0 THEN xmax=1000
IF KEYWORD_SET(ymax) EQ 0 THEN ymax=1000
;Construct the neighborhood description
IF KEYWORD_SET(nhd) EQ 0 THEN nhd=1
meshgrid,2*nhd+1,2*nhd+1,xNbrVec,yNbrVec
xNbrVec=xNbrVec-nhd
yNbrVec=yNbrVec-nhd

;If no list of radius values is given, then return with an error message
NR=N_ELEMENTS(radius)
IF NR LE 0 THEN BEGIN
	PRINT,'Function cirleHoughLink requires at least one value'
	PRINT,'for a radius function set via keyword RADIUS.'
	RETURN,-1
ENDIF ELSE BEGIN
;Set up the parameter count and point array matrices. NR is the
;number of radius values.
A=INTARR(N,M,NR)

;Define an array of structures to be filled in by the search.
;The index fields are initialized to -1 to facilitate finding
;the number of circles actually located should the search
;fall short.
;
;The anonymous structure has the tag names
;Index:-1L			Each line has a unique index [1,2,...,ncircles]
;Cx:0.0				X coordinate of center
;Cy:0.0				Y coordinate of center
;R:0.0				Radius of the circle
;Location:0L}		The location in the Hough Parameter matrix

circles=REPLICATE({Index:-1L,Cx:0.0,Cy:0.0,R:0.0,Location:0L},ncircles)

; If the lengths of the coordinate vectors are not equal,
; then use the smaller number of points.
NPTS=MIN([N_ELEMENTS(xpts),N_ELEMENTS(ypts)])
xpts=xpts[0:NPTS-1] & ypts=ypts[0:NPTS-1]

; Construct the theta vector of length N and the associated
; index vector.
theta=2*FINDGEN(ntheta)*!PI/(ntheta-1); Number of points in circle locus
radiusmax=MAX(radius)
deltaX=FLOAT(xmax+2*radiusmax)/N
deltaY=FLOAT(ymax+2*radiusmax)/M

IF KEYWORD_SET(VERBOSE) THEN $
PRINT,FORMAT=$
	'("Center Location Limits: [",f6.1,",",f6.1,"] deltaX=",g8.4," deltaY=",g8.4)',$
	[xmax+2*radiusmax,ymax+2*radiusmax,deltaX,deltaY]

; Construct the count in parameter space, for each point
; p=(x,y) provided in the function call.
FOR ir=0,NR-1 DO BEGIN
	Rcostheta=radius[ir]*cos(theta)
	Rsintheta=radius[ir]*sin(theta)
FOR p=0L,NPTS-1 DO BEGIN
	;Calculate the Cx and Cy coordinates for the circular
	;locus of points at radius[ir] R around each (x,y). Shift
	;each value by (R,R) so it does not go negative.
	ka=FIX((xpts[p]-Rcostheta + radiusmax)/deltaX)
	kb=FIX((ypts[p]-Rsintheta + radiusmax)/deltaY)
	ia=WHERE(ka GE 0 AND ka LT N AND kb GE 0 AND kb LT M)
	ka=ka[ia]
	kb=kb[ia]
	;Calculate the array index vector
	k=ka+kb*N + ir*MN	; Get the array index of each (a,b) pair
	A[k]=A[k]+1	; INCREMENT the counting matrix at each point.
ENDFOR
ENDFOR
;Preserve the original counting matrix to return if it is requested
;with the HOUGH_MATRIX keyword.
A1=A


circlesFound=0
IF KEYWORD_SET(thresh) EQ 0 THEN thresh=0.9*MAX(A)
REPEAT BEGIN
	HA=MAX(A,kq) & kq=kq[0]; Get the index kp of the peak
	ir=kq/MN ;Index of radius plane in A matrix
	kp=kq MOD MN
	xp=kp MOD N	;The horizontal coordinate of the peak
	yp=kp/N		;The vertical coordinate of the peak
	tnbrs=xp+xNbrVec ;The coordinates of the neighbors
	rnbrs=yp+yNbrVec
	;Check that the neighbors are within the array boundaries.
	inbrs=WHERE(tnbrs GE 0 AND tnbrs LT N AND rnbrs GE 0 AND rnbrs LT M)
	nbrs=tnbrs[inbrs]+N*rnbrs[inbrs]; Indices of valid neighbors
	;The neighbors will be removed later if the cell is accepted.

	;Remember the value at the peak then set that array value to
	;zero.
	peak=A[kq]
	A[kq]=0

	; Find the Cx and Cy for the Hough index kp.
	X1=(kp MOD N)*deltaX - radiusmax
	Y1=kp/N*deltaY - radiusmax

	circlesFound=circlesFound+1
	IF verbose GE 1 THEN BEGIN
	PRINT,' '
	PRINT,'===================================================='
	PRINT,'Circle Number ',circlesFound
	PRINT,'Radius=',radius[ir]
	PRINT,FORMAT='("Hough Index=",i7,"  Number of points=",i7)',[kp,peak]
	PRINT,FORMAT='("Center=[",f6.2,",",f6.2,"]")',[X1,Y1]
	ENDIF
;Keep the data in the structure.
	circles[circlesFound-1].Index=circlesFound
	circles[circlesFound-1].Cx=X1
	circles[circlesFound-1].Cy=Y1
	circles[circlesFound-1].R=radius[ir]
	circles[circlesFound-1].Location=kp
	;Set the neighborhood values to -1 and the point value
	;to -2 to mark them. These values will not be used
	;again in the search.
	A[nbrs]=-1
	A[kp]=-2

ENDREP UNTIL circlesFound GE ncircles OR peak LT thresh
RETURN,circles[0:circlesFound-1]
END
END 



;; circles=circleHoughLink(xpts,ypts,RADIUS=R,THRESHOLD=thresh,NCIRCLES=ncircles,XBINS=200,YBINS=200,XMAX=ImSz,YMAX=ImSz,HoughMatrix=A)



FUNCTION CircHough, x, y, RANGE=range, DX=dx, DY=dy, MAXX=maxx, maxy=maxy
  xc=r
 ;; 
END
