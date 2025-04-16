PRO XYGraphExample
;;
;; Simple example that shows how to create a xy-plot object, 
;; how to add data sets and how to replot
;;
;; first create a new instance of a graph object XYGraph
;; parameters are the plot name (here "Example Plot" and the axes
;; titles, here "X-Axis", and "Y-Axis") 
  mygraph=obj_new('XYGraph', 'Example Plot', XTITLE='X-Axis', YTITLE='Y-Axis')
;; now register graph in Inspector graph list!
  newgraph=GetNewXYGraphContainer("Example Plot")
  IF NOT(PTR_VALID(newgraph)) THEN BEGIN
     ErrMsg, "Could not create graph list item"
     ;; registration failed, delete graph object and return to calling level 
     mygraph->TidyUp
     obj_destroy, mygraph
     return
  END
  ;; registration succesful, update graph object pointer in the new list 
  (*newgraph).pobj=mygraph
;; now we can use the graph
;; define X and Y arrays, let's calculate a sine curve
  XValues=(Float(FINDGEN(100))-50)*0.1
  YValues=0.5*SIN(XValues)
;; add the data set to the XYGraph object
;; you can give it an identifier name, here "Set 0"
;; PSYM is the type of symbols, see the IDL help for PLOT
;; SYMSIZE is the symbol size, see the IDL help for PLOT
;; if the keyword LINE is set, then the points will be connected by
;; lines
;; RGBColor defines the color in red, green, blue values ranging from 0-255
  mygraph->AddSet, 'Set 0', XValues, YValues, PSYM=2, SYMSIZE=1, /LINE, COLOR=RGBColor(20,100,100)
;; plot the data (first time) or update the plot on the screen
  mygraph->UpdateDisplay
;; add another set with a cosine
  YValues=COS(XValues)
  mygraph->AddSet, 'Set 1', XValues, YValues, PSYM=4, SYMSIZE=1, COLOR=RGBColor(200,0,20)
  mygraph->UpdateDisplay
;; now delete the sine 
  dummy=dialog_message("Deleting Set 0 ...")
  mygraph->DeleteSet, 'Set 0'
  mygraph->UpdateDisplay
;; modify 'Set 1' to display the square cosine 
  dummy=dialog_message("Calculating and displaying the square of Set 1 ...")
  YValues=YValues*YValues
  mygraph->ModifySet, 'Set 1', Y=YValues
  mygraph->UpdateDisplay
END
