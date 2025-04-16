PRO ReportStatistics
;;
;; Keywords:
;;
;; NOFIT: Take the front image and display the statistics in the console
;; 
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% ReportStatistics:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      CATCH, /Cancel
      return
   END
END

  ptr=GetRootP()
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% ReportStatistics: No data found."
     return
  END
  ptr=(*ptr).current
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "% ReportStatistics: Current 3D Data invalid."
     return
  END
  e=(*ptr).datap
  IF NOT(PTR_VALID(ptr)) THEN BEGIN
     printtocon, "%  ReportStatistics: Current 3D Data holds no data."
     return
  END
  N=SIZE(*(*e).data)
  IF (N[0] EQ 3) THEN BEGIN
     im=(*(*e).data)[*,*,(*e).slice]
  END ELSE BEGIN
     im=(*(*e).data)
  END
  ;;  mean, variance, skewness, and kurtosis o
  Printtocon, "% ReportStatistics: full frame (central quarter)"  
    Nim=SIZE(im)
    ;; line scan, coordinate preset values
    ;; a horizontal line of witdh 1 and a length of half the image size 
    X0 = FIX(Nim[1]/4) ;;; central quarter
    Y0 = FIX(Nim[2]/4)
    X1 = FIX(3*Nim[1]/4)
    Y1 = FIX(3*Nim[2]/4)
    m=Moment(im)
    mq=Moment(im[X0:X1,Y0:Y1])
    mmax=Max(im,MIN=mmin)
    mqmax=Max(im,MIN=mqmin)
    Printtocon, " Min      : "+MyString(mmin)+" ("+MyString(mqmin)+")"
    Printtocon, " Max      : "+MyString(mmax)+" ("+MyString(mqmax)+")"
    Printtocon, " Mean     : "+MyString(m[0])+" ("+MyString(mq[0])+")"
    Printtocon, " Median   : "+MyString(Median(im))+" ("+MyString(Median(im[X0:X1,Y0:Y1]))+")"
    Printtocon, " Sum      : "+MyString(Total(im))+" ("+MyString(Total(im[X0:X1,Y0:Y1]))+")"
    Printtocon, " Variance : "+MyString(m[1])+" ("+MyString(mq[1])+")"
    Printtocon, " StdDev   : "+MyString(Sqrt(m[1]))+" ("+MyString(Sqrt(mq[1]))+")"
    Printtocon, " Skewness : "+MyString(m[2])+" ("+MyString(mq[2])+")"
 END  
