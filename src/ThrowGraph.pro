
PRO ThrowGraph, l, XVALUES=xvalues, PS=ps, PC=pc, SN=sn, SS=ss, TITLE=title, XTITLE=xtitle, YTITLE=ytitle
;; l= list of 1D arrays
  ;; ps = list of symbol indices
  ;; pc = list of plot colors
  ;; sn = list of set names
  ;; ss = list of symbol sizes
  if NOT(keyword_set(title)) then title="XY Graph"
  if NOT(keyword_set(xtitle)) then xtitle="X"
  if NOT(keyword_set(ytitle)) then ytitle="Y"
  if NOT(keyword_set(ps)) then BEGIN
     ps=list()
     For i=0,l.Count()-1 DO ps.Add, 0
     symsz=0
  END
  if NOT(keyword_set(pc)) then BEGIN
     pc=list()
     For i=0,l.Count()-1 DO pc.Add, RGBColor((i*51) MOD 256,(i*19) MOD 256,(i*111) MOD 256)
  END
  if NOT(keyword_set(sn)) then BEGIN
     sn=list()
     For i=0,l.Count()-1 DO sn.Add, 'Set' + Mystring(i)
  END
  if NOT(keyword_set(ss)) then BEGIN
     ss=list()
     For i=0,l.Count()-1 DO ss.Add, 0.00001
  END
  mygraph=obj_new('XYGraph', title, XTITLE=xtitle, YTITLE=ytitle)
  if NOT(keyword_set(xvalues)) then xvalues=FINDGEN(N_Elements(l[0]))
  For i=0,l.count()-1 Do BEGIN
     mygraph->AddSet, sn[i], xvalues, l[i], PSYM=ps[i], SYMSIZE=ss[i], /LINE, COLOR=pc[i]
  END
  mygraph->UpdateDisplay
  newgraph=GetNewXYGraphContainer(title) 
  IF NOT(PTR_VALID(newgraph)) THEN BEGIN
     ErrMsg, "Could not create graph list item"
;; registration failed, delete graph object and return to calling level
     mygraph->TidyUp
     obj_destroy, mygraph
     return
  END
;; registration succesful, update graph object pointer in the new list item
  (*newgraph).pobj=mygraph
END


