Pro ImportDM, fn
  h=Read_DM3x(fn)
 ThrowImage,*(h["data"]), TITLE=fn, SAMP=[h["samplingx"],h["samplingy"],1], UNIT=[h["unitx"], h["unitx"],h["unitx"]] 
END
