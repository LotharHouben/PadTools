; $Id: write_gif.pro,v 1.10 1999/02/16 20:21:04 slasica Exp $
;
; Copyright (c) 1992-1999, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited :-) oops
;
; Modified for animated GIF by lothar.houben@weizmann.ac.il in Nov. 2002
; the original code is still part of the idl distribution

PRO WRITE_ANIMATEDGIF, FILE, IMG, R, G, B, MULTIPLE=mult, CLOSE=close, DELAY=delay, LOOP=loop, ITERATIONS=iterations
;+
; NAME:
;	WRITE_ANIMATEDGIF
;
; PURPOSE:
;	Write an IDL image and color table vectors to a
;	GIF (graphics interchange format) file.
;       or ANIMATED GIF
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
;	WRITE_ANIMATEDGIF, File, Image  ;Write a given array.
;
;	WRITE_ANIMATEDGIF, File, Image, R, G, B  ;Write array with given color tables.
;
;
; INPUTS:
;	Image:	The 2D array to be output.
;
; OPTIONAL INPUT PARAMETERS:
;      R, G, B:	The Red, Green, and Blue color vectors to be written
;		with Image.
; Keyword Inputs:
;	CLOSE = if set, closes any open file if the MULTIPLE images
;		per file mode was used.  If this keyword is present,
;		nothing is written, and all other parameters are ignored.
;	MULTIPLE = if set, write files containing multiple images per
;		file.  Each call to WRITE_GIF writes the next image,
;		with the file remaining open between calls.  The File
;		parameter is ignored, but must be supplied,
;		after the first call.  When writing
;		the 2nd and subsequent images, R, G, and B are ignored.
;		All images written to a file must be the same size.
;       DELAY = if set, the give value will specify the delay time
;               before the single image is displayed in 1/100 s
;               by default, 1 s of delay is set
;       LOOP = if set, a NETSPAPE application extension is added
;                the animated gif will loop in programs that are aware of this
;                extension block
;       ITERATIONS = by default, the animated gif will loop forever if the LOOP 
;                keyword is set, it will loop the number of specified iterations if the 
;                ITERATION keyword is set to an unsigned integer between 1 and 65535
;
;
; OUTPUTS:
;	If R, G, B values are not provided, the last color table
;	established using LOADCT is saved. The table is padded to
;	256 entries. If LOADCT has never been called, we call it with
;	the gray scale entry.
;
;
; COMMON BLOCKS:
;	COLORS
;
; SIDE EFFECTS:
;	If R, G, and B aren't supplied and LOADCT hasn't been called yet,
;	this routine uses LOADCT to load the B/W tables.
;
; COMMON BLOCKS:
;	WRITE_ANIMATEDGIF_COMMON.
; RESTRICTIONS:
;	This routine only writes 8-bit deep GIF files of the standard
;	type: (non-interlaced, global colormap, 1 image, no local colormap)
;
;	The Graphics Interchange Format(c) is the Copyright property
;	of CompuServ Incorporated.  GIF(sm) is a Service Mark property of
;	CompuServ Incorporated.
;
; MODIFICATION HISTORY:
;	Written 9 June 1992, JWG.
;	Added MULTIPLE and CLOSE, Aug, 1996.
;       Animated GIF extensions written 25 Nov 2002
;-
;

COMMON WRITE_ANIMATEDGIF_COMMON, unit, width, height, position
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

; Check the arguments
ON_ERROR, 2			;Return to caller if error
n_params = N_PARAMS();

;; Fix case where passing through undefined r,g,b variables
;; SJL - 2/99
if ((n_params eq 5) and (N_ELEMENTS(r) eq 0)) then n_params = 2

; let user know about demo mode limitation.
; encode_gif is disabled in demo mode
if (LMGR(/DEMO)) then begin
    MESSAGE, 'Feature disabled for demo mode.'
    return
endif

if n_elements(unit) le 0 then unit = -1

if KEYWORD_SET(close) then begin
  if unit ge 0 then FREE_LUN, unit
  unit = -1
  return
  endif

IF ((n_params NE 2) AND (n_params NE 5))THEN $
  message, 'usage: WRITE_GIF, file, image, [r, g, b]'

; Is the image a 2-D array of bytes?

img_size	= SIZE(img)
IF img_size[0] NE 2 OR img_size[3] NE 1 THEN	$
	message, 'Image must be a byte matrix.'



if keyword_set(mult) and unit ge 0 then begin
  if width ne img_size[1] or height ne img_size[2] then $
	message,'Image size incompatible'
  point_lun, unit, position-1	;Back up before terminator mark
endif else begin		;First call
  width = img_size[1]
  height = img_size[2]

; If any color vectors are supplied, do they have right attributes ?
  IF (n_params EQ 2) THEN BEGIN
	IF (n_elements(r_curr) EQ 0) THEN LOADCT, 0	; Load B/W tables
	r	= r_curr
	g	= g_curr
	b	= b_curr
  ENDIF

  r_size = SIZE(r)
  g_size = SIZE(g)
  b_size = SIZE(b)
  IF ((r_size[0] + g_size[0] + b_size[0]) NE 3) THEN $
	message, "R, G, & B must all be 1D vectors."
  IF ((r_size[1] NE g_size[1]) OR (r_size[1] NE b_size[1]) ) THEN $
	message, "R, G, & B must all have the same length."

  ;	Pad color arrays

  clrmap = BYTARR(3,256)

  tbl_size		= r_size[1]-1
  clrmap[0,0:tbl_size]	= r
  clrmap[0,tbl_size:*]	= r[tbl_size]
  clrmap[1,0:tbl_size]	= g
  clrmap[1,tbl_size:*]	= g[tbl_size]
  clrmap[2,0:tbl_size]	= b
  clrmap[2,tbl_size:*]	= b[tbl_size]

  CATCH, Error_status
  IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% Write_AnimatedGIF: Error while attempting to access file "
      PrintToCon, "%               " + !ERR_STRING
      CATCH, /Cancel
      return
  END

  ; Write the result
  ; MACTYPE find me
  if (!version.os EQ 'MacOS') then begin
  OPENW, unit, file, /STREAM, /GET_LUN, MACTYPE = "GIFf"
  endif else begin
  OPENW, unit, file, /STREAM, /GET_LUN
  endelse
;;------------------------------------------
;; write the GiF signature and the Screen Descriptor
;;------------------------------------------

  hdr	=  { giffile, $		;Make the header
  magic:'GIF89a', 		$
  width_lo:0b, width_hi:0b,	$
  height_lo:0b, height_hi:0b,	$
  global_info: BYTE('F7'X),	$	; global map, 8 bits color
  background:0b, reserved:0b }		; 8 bits/pixel

  hdr.width_lo	= width AND 255
  hdr.width_hi	= width / 256
  hdr.height_lo	= height AND 255
  hdr.height_hi	= height / 256

  WRITEU, unit, hdr				;Write header

;;------------------------------------------
;; write the global colormap
;;------------------------------------------

  WRITEU, unit, clrmap				;Write color map

;;------------------------------------------
;; write the Application Extensions
;;------------------------------------------

  ;; modification, L.H. 10.10.06 
  ;; the Comment Block
  ;;
  ;; cmtblk={
  ;; }
  ;; WRITEU, unit, cmtblk

ehdr = {emagic: BYTE('21'X),  $ ; Byte 0x21 identifies the extension block
        elabel: BYTE('FF'X),  $ ; identifies the application extension type 
        bsize: BYTE('0B'X),            $ ; 11 bytes to follow
        ident: 'NETSCAPE',          $ ; application identifier
        auth: '2.0',       $ ; authentification
        dsize: BYTE('03'X),       $ ;  3 bytes to follow
        lflag: BYTE('01'X),            $ ; loop flag
        iter_lo: 0b,             $ ; number of iterations: 0 means infinite
        iter_hi: 0b,             $ ; number of iterations: 0 means infinite
        eend: 0b }              ; block end

IF KEYWORD_SET(loop) THEN BEGIN
;; write extension header
   IF NOT(KEYWORD_SET(iterations)) THEN iterations=0 
   ehdr.iter_lo = iterations AND 255
   ehdr.iter_hi = iterations/255

   WRITEU, unit, ehdr

END

endelse 				;Not Multiple

;;------------------------------------------
;; write the images
;;------------------------------------------

;;
;; now here's the modification
;; a control block for each image
;;
chdr = {cmagic: BYTE('21'X),  $ ; Byte 0x21 identifies control block
        clabel: BYTE('F9'X),  $ ; Byte 0xF9 graphics control label
        bsize: 4b,            $ ; 4 bytes to follow
        dmethod: 0b,          $ ; disposal method: do nothing
        dtime_lo: 100b,       $ ; delay time, unsigned int, number of 1/100s
        dtime_hi: 0b,       $ ; delay time, unsigned int, number of 1/100s
        tflag: 0b,            $ ; transpareny color, not set
        bend: 0b }              ; block end
if keyword_set(delay) then BEGIN
    chdr.dtime_lo = delay AND 255
    chdr.dtime_hi = delay/255
END
;;
;; image block
;;
ihdr	= {imagic: BYTE('2C'X),		$
           left:0,                      $
           top: 0,			$
	   width_lo:0b,                 $
           width_hi:0b,	                $
	   height_lo:0b,                $
           height_hi:0b,	        $
	   image_info:7b }
;; image data follows 
;; and terminator byte
ihdr.width_lo	= width AND 255
ihdr.width_hi	= width / 256
ihdr.height_lo	= height AND 255
ihdr.height_hi	= height / 256

WRITEU, unit, chdr
WRITEU, unit, ihdr
ENCODE_GIF, unit, img

if keyword_set(mult) then begin ;Multiple image mode?
  POINT_LUN, -unit, position	;Get the position
endif else begin		;Single image/file
  FREE_LUN, unit		; Close file and free unit
  unit = -1
endelse
END


;; Animated GIF:
;; 
;; GIF Header
;; Application extension
;; (
;;   Graphic Control extension
;;   Image Block
;; )
;; Trailer
