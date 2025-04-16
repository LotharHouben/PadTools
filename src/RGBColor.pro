FUNCTION RGBColor, red, green, blue
return, (LONG(blue)*256L+LONG(green))*256L+LONG(red)
END


FUNCTION RGBIndexColor, index, LIGHT=light
red=0L & blue=0L & green=0L
CASE (index MOD 3) OF
   0:BEGIN
      red= ((index*20) MOD 128)
      if keyword_set(light) then red=red+128
   END
   1: BEGIN
      blue= ((index*20) MOD 128)
      if keyword_set(light) then blue=blue+128
   END
   2: BEGIN
      green= ((index*20) MOD 128)
      if keyword_set(light) then green=green+128
   END
END
return, (LONG(blue)*256L+LONG(green))*256L+LONG(red)
END
