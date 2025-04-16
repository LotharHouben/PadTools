FUNCTION ImageEntropy, im
;;The entropy of a slice is computed as follows:
;;         - H, the histogram of the slice is computed, using 10000 bins from HISTMIN to HISTMAX.
;;         - H is scaled (divided) by the number of pixels in the slice
;;         - The entropy is then computed as the sum of H*log(H),
;;           summing over the 10000 elements in H
nobins=10000
pdf=HISTOGRAM(im, NBINS=nobins)
pdf=FLOAT(pdf)/SIZE(im,/N_ELEMENTS)
pdf=pdf(WHERE(pdf GT 0.))
return, -Total(pdf*alog(pdf))  
END
