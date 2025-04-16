PRO FilterNAN, DIRECTDATAP=directdatap
;;
;; Demo procedure that reports the statistics for the currently
;; highlighted data array in the console window
;; 
;; dereference all pointers to get to the data array in the currently
;; highlighted 3D data array list item
;; it is a good policy to check for undefined or NIL pointers 
IF NOT(keyword_set(directdatap)) THEN BEGIN
   ptr=GetRootP()
   IF (NOT(PTR_VALID(ptr))) THEN BEGIN
      ;; there is no root pointer
      printtocon, "%  ReportArrayStatistics: Root pointer is invalid." 
      return
   END
   p=(*ptr).current
   IF (NOT(PTR_VALID(p))) THEN BEGIN
      ;; there is no data in the list or nothing is highlighted 
      printtocon, "%  ReportArrayStatistics: Current data list pointer is invalid." 
      return
   END
   pp=(*p).datap
   IF (NOT(PTR_VALID(pp))) THEN BEGIN
      ;; there is no structure of type DATA
      printtocon, "% ReportArrayStatistics: Pointer to structure data in current data list element is invalid." 
      return
   END
   ppd=(*pp).data
END ELSE ppd=directdatap
IF (NOT(PTR_VALID(ppd))) THEN BEGIN
   ;; there is no data array
   printtocon, "% ReportArrayStatistics: Pointer to to data array is invalid." 
   return
END
;; now ppd is the pointer to the data array, *ppd is the data array
;; we calculate some statistical data and report it to the console
nonfin=Where(*ppd NE *ppd, count, COMPLEMENT=fin)
IF (count GT 0) THEN BEGIN
   finmean=Mean((*ppd)(fin))

;; printtocon also accepts string arrays, each of the elements will be
;; printed on a single line
   s=["% FilterNAN: "+(*pp).id, $
     "   Found "+MyString(count)+" non-finite values.", $
     "   Replacing array elements with the mean value of the complement data = " + MyString(finmean)+"."]
;; print the string array to the console
   printtocon, s
   (*ppd)[nonfin]=finmean    
END ELSE BEGIN
   s=["% FilterNAN: "+(*pp).id, $
      "   Found no non-finite values."]
   printtocon, s
END
END


