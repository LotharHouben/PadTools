
Function XYGraph::Init, graphname, XTITLE=xtitle, YTITLE=ytitle, DEBUG=debug
self.name=graphname
self.psym=0
self.symsize=1
self.WindowSzX=384
self.WindowSzY=384
self.fgcolor=0L
self.bgcolor=13428479L
self.window=-1
self.line=0
IF keyword_set(xtitle) then self.xtitle=xtitle ELSE self.xtitle='X'
IF keyword_set(ytitle) then self.ytitle=ytitle ELSE self.ytitle='Y'
IF keyword_set(debug) then self.debug=debug ELSE self.debug=0
return, 1
END


pro XYGraph__define
 void={XYGraph,id:'xygraph',name:'undefined',xtitle:'X',ytitle:'Y',window:-1,fgcolor:6558720L,bgcolor:13428479L,psym:1,symsize:5,WindowSzX:384,WindowSzY:384,phash:Ptr_New(),debug:0,line:0}
 return 
end

PRO XYGraph::UpdateDisplay, XRANGE=xrange, YRANGE=yrange
  IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
     ErrMsg, "% XYGraph: no data sets in graph "+self.name+"."
     return
  END ELSE BEGIN
     if (self.window LT 0) THEN BEGIN
        currwin=!D.Window
        Window, /FREE,TITLE=self.name, XSIZE=self.WindowSzX, YSIZE=self.WindowSzY
        self.window = !D.Window
        WSET, currwin
     END
     if (self.window GE 0) THEN BEGIN
        ;; cycle through all data sets and plot them
        h=*(self.phash)
        setkeys=h->keys()
        Nsetkeys=N_Elements(setkeys)
        ;; autoscale
        IF NOT(keyword_set(xrange)) THEN BEGIN
           IF (Nsetkeys GE 1) THEN BEGIN
              s=h[setkeys[0]] & minx=MIN(*((*s).x),MAX=maxx) & xrange=[minx,maxx]
              For i=1,(NSetkeys-1) DO BEGIN
                 s=h[setkeys[i]] ;; pointer to structure
                 minx=MIN(*((*s).x),MAX=maxx)
                 IF (minx LT xrange[0]) THEN xrange[0]=minx
                 IF (maxx GT xrange[1]) THEN xrange[1]=maxx
              END
              xrange[0]-=0.05*(xrange[1]-xrange[0])
              xrange[1]+=0.05*(xrange[1]-xrange[0])
           END
        END
        IF NOT(keyword_set(yrange)) THEN BEGIN
           IF (Nsetkeys GE 1) THEN BEGIN
              s=h[setkeys[0]] & miny=MIN(*((*s).y),MAX=maxy) & yrange=[miny,maxy]
              For i=1,(NSetkeys-1) DO BEGIN
                 s=h[setkeys[i]] ;; pointer to structure
                 miny=MIN(*((*s).y),MAX=maxy)
                 IF (miny LT yrange[0]) THEN yrange[0]=miny
                 IF (maxy GT yrange[1]) THEN yrange[1]=maxy
              END
              yrange[0]-=0.05*(yrange[1]-yrange[0])
              yrange[1]+=0.05*(yrange[1]-yrange[0])
           END
        END 
        ;; plot sets
        currwin=!D.Window
        WSET, self.window
        IF (Nsetkeys GE 1) THEN BEGIN
           For i=0,(NSetkeys-1) DO BEGIN
              s=h[setkeys[i]] ;; pointer to structure
              if (i EQ 0) THEN BEGIN
                 ;; just to get the colors right
                 plot, *((*s).x), *((*s).y), Background=self.bgcolor, COLOR=self.fgcolor, XRANGE=xrange,YRANGE=yrange, PSYM=(*s).psym, SYMSIZE=(*s).symsize, XTITLE=self.xtitle, YTITLE=self.ytitle, /NODATA
                 
              END 
              IF ((*s).psym GT 0) THEN oplot, *((*s).x), *((*s).y), COLOR=(*s).color, PSYM=(*s).psym, SYMSIZE=(*s).symsize
              IF ((*s).line EQ 1) THEN oplot, *((*s).x), *((*s).y), COLOR=(*s).color, PSYM=0
           END
        END 
        WSET, currwin
     END  
  END 
END   
  
Pro XYGraph::AddSet, name, x, y, COLOR=color, PSYM=psym, SYMSIZE=symsize, Line=line, SORTKEY=sortkey
;; Create a new plot
;; create a display window for the current pointer
;; if redisplay is set, only existing windows will be reopened
;; 
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   h=hash()
   self.phash=PTR_NEW(h)
END 
;; add a hash entry
h=*(self.phash)
;; new anonymous structure for the set
IF NOT(keyword_set(color)) THEN color=self.fgcolor
IF NOT(keyword_set(psym)) THEN psym=self.psym
IF NOT(keyword_set(symsize)) THEN symsize=self.symsize
IF NOT(keyword_set(line)) THEN line=self.line
IF NOT(keyword_set(sortkey)) THEN sortkey=name
s=Create_Struct('name', name, 'x', PTR_NEW(x), 'y', PTR_NEW(y), 'color', color, 'psym', psym, 'symsize',symsize, 'line', line, 'sortkey', sortkey)
h[name]=PTR_NEW(s)
;;
return
END 

Pro XYGraph::ModifySet, name, X=x, Y=y, COLOR=color, PSYM=psym, SYMSIZE=symsize
;; Change set Settings
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "XYGraph: Request to modify set in graph"+self.name+" where there are no sets." 
   return
END 
;; add a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
;;
   IF keyword_set(color) THEN (*s).color=color
   IF keyword_set(psym) THEN (*s).psym=psym
   IF keyword_set(symsize) THEN (*s).symsize=symsize
   IF keyword_set(x) THEN BEGIN
      IF PTR_VALID((*s).x) THEN ptr_free, (*s).x
      (*s).x=PTR_NEW(x)
   END
   IF keyword_set(y) THEN BEGIN
      IF PTR_VALID((*s).y) THEN ptr_free, (*s).y
      (*s).y=PTR_NEW(y)
   END
END ELSE BEGIN
   ErrMsg, "XYGraph: Set "+name+" does not exist in graph"+self.name+"."
END
return
END  

Function XYGraph::GetSet, name, X=x, Y=y, POINTER=pointer
;; Get set Data
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "XYGraph: Request to return set in graph"+self.name+" where there are no sets." 
   return, 0
END
;; add a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
;;
   IF keyword_set(x) THEN BEGIN
      IF keyword_set(pointer) THEN x=(*s).x ELSE x=(*(*s).x)
   END
   IF keyword_set(y) THEN BEGIN
      IF keyword_set(pointer) THEN y=(*s).y ELSE y=(*(*s).y)
   END
END ELSE BEGIN
   ErrMsg, "XYGraph: Set "+name+" does not exist in graph"+self.name+"."
   return, 0
END
return, 1
END  

Pro XYGraph::DeleteSet, name
;;
;; Delete a set
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "XYGraph: Request to delete set in graph"+self.name+" where there are no sets." 
   return
END 
;; delete a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
   IF PTR_VALID(s) THEN BEGIN
      IF PTR_VALID((*s).x) THEN ptr_free, (*s).x
      IF PTR_VALID((*s).y) THEN ptr_free, (*s).y
   END
   h.Remove, name
END ELSE BEGIN
   ErrMsg, "XYGraph: Set "+name+" does not exit in graph"+self.name+"."
END
return
END  

FUNCTION XYGraph::NSets
;;
;; returns number of data sets
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set
   return, 0
END 
h=*(self.phash)
return, N_Elements(h->keys())
END  


Pro XYGraph::ExportData, LINEFEED=linefeed, SEPARATOR=separator
;;
;; Export data to file
;;
;; catch error if required
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% XYGraph::ExportData: An error occured" 
    Print, "%            " + !ERR_STRING
    ErrMsg, !ERR_STRING
    CATCH, /Cancel
    return
 END
;; are there data sets?
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "XYGraph::ExportData: Request to export data sets in graph"+self.name+" where there are no sets." 
   return
END 
printtocon, "% XYGraph::ExportData: Started graphics data set export."
;; get file name for export
swapend=0
crgrpd=1
type=0
filter="*.dat"
ftypes=PTR_NEW(["XY Data"])
files = rmd_pickfile(filter_in = filter, $
        path=GetWorkingDir(), $
        get_path = out_path,          $
        cancelled = cancelled,        $
        swapen = swapend,             $
        type = type,             $
        ftypes = ftypes,     $
        crgrp = crgrpd,             $
        /multiple, $
        /save)
if (cancelled EQ 1) then begin
   printtocon, "% XYGraph::ExportData: Data export cancelled."
   return
END
IF NOT(n_elements(files) GE 1) THEN BEGIN
   printtocon, "% XYGraph::ExportData: No file selected."
   return
END
SetWorkingDir, out_path
fname=files[0]
;;
openw, LUN, fname, ERROR = o_err, /GET_LUN
;;
if keyword_set(linefeed) THEN BEGIN
   CASE linefeed OF
      "msdos": BEGIN
         CR = string("15b)
         LF = string("12b)
         linefeed=CR+LF
      END
      ELSE:
   END
END
IF NOT(keyword_set(separator)) THEN separator=" "
IF NOT(keyword_set(linefeed)) THEN linefeed=""
;;
h=*(self.phash)
setkeys=h->keys()
Nsetkeys=N_Elements(setkeys)
;; order the keys before exporting
;; use the sortkey field
done = 0
size=Nsetkeys
while (NOT(done)) Do Begin
   done=1
   for i=0,size-2 DO BEGIN
      if ((*(h[setkeys[i]])).sortkey GT (*(h[setkeys[i+1]])).sortkey) THEN BEGIN
         done = 0
         ;; swap keys setkeys[i] and setkeys[i+1]
         tmp=setkeys[i]
         setkeys[i]=setkeys[i+1]
         setkeys[i+1]=tmp
      END
   END
   size=size-1
END
;;
For i=0,(NSetkeys-1) DO BEGIN
   s=h[setkeys[i]] ;; pointer to structure
   printtocon, "% XYGraph::DataExport: Writing data set "+(*s).name
   printf, lun, "# "+(*s).name+linefeed
   Ny=N_Elements(*(*s).y)
;;
   FOR j=0,(Ny-1) DO BEGIN
      outstr=STRTRIM(STRING((*(*s).x)[j]),2)+separator+STRTRIM(STRING((*(*s).y)[j]),2)+linefeed
      printf, lun, outstr
   END
   printf, lun, linefeed
END
;;
close, LUN
free_lun, LUN
printtocon, "% XYGraph::DataExport: Graph data sets written to "+fname
return
END   

PRO XYGraph::ExportDisplay, FORMAT=format, XRANGE=xrange, YRANGE=yrange, PSFONT=psfont
;;CATCH, Error_status
Error_status=0
IF (Error_status NE 0) THEN BEGIN
    Print, "% XYGraph::ExportData: An error occured" 
    Print, "%            " + !ERR_STRING
    ErrMsg, !ERR_STRING
    CATCH, /Cancel
    return
 END
printtocon, "% XYGraph::ExportDisplay: Started graphics export."
;; are there data sets?
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "XYGraph::ExportData: Request to export graph"+self.name+" where there are no sets." 
   return
END 
;; get file name for export
swapend=0
crgrpd=1
type=0
if not(keyword_set(format)) Then format="Postscript"
filter="*"
ftypes=PTR_NEW(["Postscript"])
files = rmd_pickfile(filter_in = filter, $
        path=GetWorkingDir(), $
        get_path = out_path,          $
        cancelled = cancelled,        $
        swapen = swapend,             $
        type = type,             $
        ftypes = ftypes,     $
        crgrp = crgrpd,             $
        /multiple, $
        /save)
if (cancelled EQ 1) then begin
   printtocon, "% XYGraph::ExportDisplay: Data export cancelled."
   return
END
IF NOT(n_elements(files) GE 1) THEN BEGIN
   printtocon, "% XYGraph::ExportDisplay: No file selected."
   return
END
SetWorkingDir, out_path
fname=files[0]
;;
entry_device = !D.NAME
CASE format OF
   "Postscript": BEGIN
      set_plot, 'PS'
   END
   ELSE: BEGIN
      ;; make postscript default
      set_plot, 'PS'
      format="Postscript" 
   END
END 
device, decomposed=1, /color
device, filename=fname
If (format EQ "Postscript") THEN BEGIN
   if NOT(keyword_set(psfont)) THEN psfont="Times" ;; psfont="Helvetica"
   DEVICE, SET_FONT=psfont, /TT_FONT
   xsize=9. & normxsize=9.
   currpfont=!p.font
   currpthick=!p.thick
   currxthick=!x.thick
   currythick=!y.thick
   currzthick=!z.thick
   !p.font = 1
   !p.thick = 2
   !x.thick = 2
   !y.thick = 2
   !z.thick = 2
   ;;
   symscale=1./FLOAT(!p.thick)
   ;;
   margin = 0.12
   wall = 0.03
   a = xsize/normxsize - (margin + wall)
   b = a ;; * 2d / (1 + sqrt(5))
   ysize = (margin + b + wall + b + wall)*normxsize
   ;;
   x1 = margin*normxsize/xsize
   x2 = x1 + a*normxsize/xsize
   xc = x2 + wall*normxsize/xsize
   y1 = margin*normxsize/ysize
   y2 = y1 + b*normxsize/ysize
   y3 = y2 + wall*normxsize/ysize
   y4 = y3 + b*normxsize/ysize
   yc = y4 + wall*normxsize/ysize
   ;;
   ticklen = 0.01
   xticklen = ticklen/b
   yticklen = ticklen/a
   ;;
   psfgcolor=0L
   ;;
   device, filename=fname, /encapsulated, xsize=xsize, ysize=ysize, /tt_font, set_font='Times', font_size=10
END
;;
;; cycle through all data sets and plot them
h=*(self.phash)
setkeys=h->keys()
Nsetkeys=N_Elements(setkeys)
;; autoscale
IF NOT(keyword_set(xrange)) THEN BEGIN
   IF (Nsetkeys GE 1) THEN BEGIN
      s=h[setkeys[0]] & minx=MIN(*((*s).x),MAX=maxx) & xrange=[minx,maxx]
      For i=1,(NSetkeys-1) DO BEGIN
         s=h[setkeys[i]] ;; pointer to structure
         minx=MIN(*((*s).x),MAX=maxx)
         IF (minx LT xrange[0]) THEN xrange[0]=minx
         IF (maxx GT xrange[1]) THEN xrange[1]=maxx
      END
      xrange[0]-=0.05*(xrange[1]-xrange[0])
      xrange[1]+=0.05*(xrange[1]-xrange[0])
   END
END
IF NOT(keyword_set(yrange)) THEN BEGIN
   IF (Nsetkeys GE 1) THEN BEGIN
      s=h[setkeys[0]] & miny=MIN(*((*s).y),MAX=maxy) & yrange=[miny,maxy]
      For i=1,(NSetkeys-1) DO BEGIN
         s=h[setkeys[i]] ;; pointer to structure
         miny=MIN(*((*s).y),MAX=maxy)
         IF (miny LT yrange[0]) THEN yrange[0]=miny
         IF (maxy GT yrange[1]) THEN yrange[1]=maxy
      END
      yrange[0]-=0.05*(yrange[1]-yrange[0])
      yrange[1]+=0.05*(yrange[1]-yrange[0])
   END
END 
;; plot sets
IF (Nsetkeys GE 1) THEN BEGIN
   For i=0,(NSetkeys-1) DO BEGIN
      s=h[setkeys[i]] ;; pointer to structure
      if (i EQ 0) THEN BEGIN
         ;; just to get the colors right: /nodata
            plot, *((*s).x), *((*s).y), /nodata, /noerase, position=[x1,y1,x2,y2], xminor=1, yminor=1, xticklen=xticklen, yticklen=yticklen, Background=self.bgcolor, COLOR=psfgcolor, XRANGE=xrange, YRANGE=yrange, XTITLE=self.xtitle, YTITLE=self.ytitle
      END 
      IF ((*s).psym GT 0) THEN oplot, *((*s).x), *((*s).y), COLOR=(*s).color, PSYM=(*s).psym, SYMSIZE=(*s).symsize*symscale
      IF ((*s).line EQ 1) THEN oplot, *((*s).x), *((*s).y), COLOR=(*s).color, PSYM=0
   END
END  
device, /close_file 
   !p.font=currpfont
   !p.thick=currpthick
   !x.thick=currxthick
   !y.thick=currythick
   !z.thick=currzthick
set_plot, entry_device
printtocon, "% XYGraph::ExportDisplay: Graphics written to "+fname
END     


Pro XYGraph::CloseWindow
;;
;; Delete window
;;
;; catch error if required
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    Print, "% XYGraph::CloseWindow: Error when closing window with index " + MyString(self.window)
    Print, "%            " + !ERR_STRING
    ErrMsg, !ERR_STRING
    self.window=-1
    CATCH, /Cancel
    return
 END
currwin=!D.Window
IF (self.window GT 0) THEN BEGIN
   WDelete, self.window
   self.window=-1
END
;;!D.Window=currwin
return
END   


Pro XYGraph::TidyUp
;-- free memory allocated to pointer when destroying object
  IF Ptr_Valid(self.phash) THEN BEGIN
     h=*(self.phash)
     setkeys=h.keys()
     Nsetkeys=N_Elements(setkeys)
     For i=0, (NSetkeys-1) DO BEGIN
        s=h[setkeys[i]]
        IF PTR_VALID(s) THEN BEGIN
           IF PTR_VALID((*s).x) THEN PTR_FREE, (*s).x
           IF PTR_VALID((*s).y) THEN PTR_FREE, (*s).y
           PTR_FREE, s
        END
     END
     PTR_FREE, self.phash
  END
  IF (self.window GE 0) THEN BEGIN
     WDelete, self.window
     self.window=-1
  END
  return
end 


function XYGraph::cleanup
;-- free memory allocated to pointer when destroying object
  IF Ptr_Valid(self.phash) THEN BEGIN
     h=*(self.phash)
     setkeys=h.keys()
     Nsetkeys=N_Elements(setkeys)
     For i=0, (NSetkeys-1) DO BEGIN
        s=h[setkeys[i]]
        IF PTR_VALID(s) THEN BEGIN
           IF PTR_VALID((*s).x) THEN PTR_FREE, (*s).x
           IF PTR_VALID((*s).y) THEN PTR_FREE, (*s).y
           PTR_FREE, s
        END
     END
     PTR_FREE, self.phash
  END
  IF (self.window GE 0) THEN BEGIN
     WDelete, self.window
  END
  return, 1
end 

;; Example:
;; --------
;; requires RGBColor.pro
;;
PRO Graph_Example
;;
;; Simple example that shows how to create a xy-plot object, 
;; how to add data sets and how to replot
;;
;; first create a new instance of a graph object XYGraph
;; parameters are the plot name (here "Example Plot" and the axes
;; titles, here "X-Axis", and "Y-Axis") 
mygraph=obj_new('XYGraph', 'Example Plot', XTITLE='X-Axis', YTITLE='Y-Axis')
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
mygraph->AddSet, 'Set 0', XValues, YValues, PSYM=0, SYMSIZE=0, /LINE, COLOR=RGBColor(20,100,100)
;; plot the data (first time) or update the plot on the screen
mygraph->UpdateDisplay
;; add another set with a cosine
YValues=COS(XValues)
mygraph->AddSet, 'Set 1', XValues, YValues, PSYM=4, SYMSIZE=0.1, COLOR=RGBColor(200,0,20)
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
;;
dummy=dialog_message("Deleting XYGraph object ...")
mygraph->TidyUp
Help, /HEAP
obj_destroy, mygraph
Help, /HEAP
END
