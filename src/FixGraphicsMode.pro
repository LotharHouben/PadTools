PRO SetGraphicsMode
  selFn=["GXclear","GXand","GXandReverse","GXcopy (Default)","GXandInverted","GXnoop","GXxor","GXor","GXnor","GXequiv","GXinvert","GXorReverse","GXcopyInverted","GXorInverted","GXnand","GXset"]
  res=XMChoiceDialog(selFn,"Choose Graphics Mode")
  IF (res EQ "") THEN BEGIN
     print, "% SI_AlignZLP: cancelled"
     return
  END
  CASE res OF
     "GXclear": DEVICE, SET_GRAPHICS_FUNCTION=0
     "GXand": DEVICE, SET_GRAPHICS_FUNCTION=1
     "GXandReverse": DEVICE, SET_GRAPHICS_FUNCTION=2
     "GXcopy (Default)": DEVICE, SET_GRAPHICS_FUNCTION=3
     "GXandInverted": DEVICE, SET_GRAPHICS_FUNCTION=4
     "GXnoop": DEVICE, SET_GRAPHICS_FUNCTION=5
     "GXxor": DEVICE, SET_GRAPHICS_FUNCTION=6
     "GXor": DEVICE, SET_GRAPHICS_FUNCTION=7
     "GXnor": DEVICE, SET_GRAPHICS_FUNCTION=8
     "GXequiv": DEVICE, SET_GRAPHICS_FUNCTION=9
     "GXinvert": DEVICE, SET_GRAPHICS_FUNCTION=10
     "GXorReverse": DEVICE, SET_GRAPHICS_FUNCTION=11
     "GXcopyInverted": DEVICE, SET_GRAPHICS_FUNCTION=12
     "GXorInverted": DEVICE, SET_GRAPHICS_FUNCTION=13
     "GXnand": DEVICE, SET_GRAPHICS_FUNCTION=14
     "GXset": DEVICE, SET_GRAPHICS_FUNCTION=15
     ELSE: DEVICE, SET_GRAPHICS_FUNCTION=3
  END
END
