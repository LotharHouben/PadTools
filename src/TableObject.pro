;; +--------------------------------------------------
;; TableObject
;; +--------------------------------------------------
;;
;; an object to store and manipulate data tables
;;
;; (c) L. Houben, Forschungszentrum Juelich GmbH 2014
;; 
;; DEPENDENCIES: rmd_pickfile.pro, general.pro
;;
;; +-------------------------------------------------- 

Function DTable::Init, tablename, ncolumns, NLayers=nlayers, Columnlabels=columnlabels, DEBUG=debug, DESCRIPTOR=descriptor
;; table layers can have a variable number of columns and rows
;; column indices are stored in a list
;; by default there is one layer or nlayers with ncolumns columns
self.tablename=tablename
self.ncolumns=ncolumns
self.nrows=0
self.separator=" "
;; by default we have one layer
self.nlayers=1
IF keyword_set(nlayers) then BEGIN
   IF (nlayers GE 1) then self.nlayers=nlayers
END ELSE nlayers=1
self.pcoltitle=PTR_NEW(STRARR(nlayers*ncolumns))
self.pcol=PTR_NEW(OBJARR(nlayers*ncolumns))
for j=1,nlayers DO BEGIN
 i0=(j-1)*ncolumns
 for i=1,ncolumns DO BEGIN
   (*(self.pcol))[i0+i-1]=List() ;; initialize column (i-1) to empty list
   (*(self.pcoltitle))[i0+i-1]="Column "+STRCOMPRESS(STRING(i-1, /PRINT), /REMOVE_ALL)                          ;; Initialize column title
END

IF keyword_set(columnlabels) then BEGIN
   ntitles=N_Elements(columnlabels)
   IF (ntitles GE ncolumns) THEN BEGIN
      (*(self.pcoltitle))[i0:(i0+ncolumns-1)]=columnlabels[0:(ncolumns-1)]
   END ELSE BEGIN
      (*(self.pcoltitle))[i0:(i0+ntitles-1)]=columnlabels[0:(ntitles-1)]
   END
END 
END
self.currentlayer=1 ;; currentlayer counts from 1, 0 means invalid
IF keyword_set(descriptor) THEN self.pdescriptor=PTR_NEW(descriptor) ELSE self.pdescriptor=PTR_NEW([""])
self.playernames=PTR_NEW(STRARR(nlayers))
for i=1,nlayers DO (*(self.playernames))[i-1]='Layer '+STRCOMPRESS(STRING(i, /PRINT), /REMOVE_ALL)
IF keyword_set(debug) then self.debug=debug ELSE self.debug=0
self.sellistid=[0L,0L]
return, 1
END 




Function DTable::GetTableName
return, self.tablename
END

Pro DTable::AddRow, rowlist, LAYER=layer
;; Add a new row to the current layer
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::AddRow .. No column definition."
END 
IF keyword_set(layer) THEN BEGIN
   i0=self.ncolumns*(layer-1)
END ELSE BEGIN
   ;; take currently selected layer
   i0=self.currentlayer-1 ;; currentlayer counts from 1, array indices from 0
END
IF (i0 LT 0) AND (i0 GE self.nlayers) THEN BEGIN
   print, "% DTable::AddRow: Invalid layer number ("+STRCOMPRESS(i0)+").";
   return
END
;; check row size
if (rowlist.Count() EQ self.ncolumns) THEN BEGIN 
   h=*(self.pcol)
   for i=1,self.ncolumns DO BEGIN
      h[i0+i-1].Add, rowlist[i-1]
   END 
END ELSE BEGIN
   ;; undefined ?
   print, "DTable::AddRow .. column count mismatch."
END 
return
END    

Pro DTable::SetDescriptor, s
  IF PTR_VALID(self.pdescriptor) THEN BEGIN
     PTR_FREE, self.pdescriptor
  END
 self.pdescriptor=PTR_NEW(s)
END

FUNCTION DTable::GetPdescriptor
 return, self.pdescriptor
END

Pro DTable::SetLayerNames, s, LAYER=layer
  IF keyword_set(layer) THEN BEGIN
     IF ((layer GE 1) AND (layer LE self.nlayers)) THEN (*(self.playernames))[layer-1]=s
     return
  END
  IF (N_Elements(s) EQ N_Elements(*(self.playernames))) THEN BEGIN
     PTR_FREE, self.playernames
     self.playernames=PTR_NEW(s)
  END ELSE BEGIN
     print, "DTable::SetLayerNames: Array size mismatch."
  END
END

FUNCTION DTable::GetLayerName
 return, (*(self.playernames))[self.currentlayer-1]
END

FUNCTION DTable::GetPLayerNames
 return, self.playernames
END

Function DTable::NColumns
;; return number of rows
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::NColumns .. No column definition."
   return, -1
END 
;; check row size
return, self.ncolumns
END


Function DTable::NRows
;; return number of rows
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::NRows .. No column definition."
   return, -1
END 
;; check row size
h=*(self.pcol)
return, h[0].Count()
END

Pro DTable::SetColumnLabels, s
  IF (N_Elements(s) EQ self.ncolumns) THEN BEGIN
     For i=1,self.nlayers DO BEGIN
        i0=(i-1)*self.ncolumns & i1=i0+self.ncolumns-1
        (*(self.pcoltitle))[i0:i1]=s
     END
  END ELSE BEGIN
     print, "DTable::SetColumnLabels: Array size mismatch."
  END
END

Function DTable::GetColumnLabels
;; return pointer to column labels
return, self.pcoltitle
END


Function DTable::GetLayer
;; return number of currently selected table layer
IF NOT((self.currentlayer GE 1) AND (self.currentlayer LE self.nlayers)) THEN self.currentlayer=1
return, self.currentlayer
END

PRO DTable::SetLayer, layer
;; set number of currently selected table layer
  if ((layer GE 1) AND (layer LE self.nlayers)) THEN self.currentlayer=layer ELSE BEGIN
     print, "DTable::SetLayer: Layer out of range."
     self.currentlayer=1
  END
END

Function DTable::NLayers
;; return number of layers in the table 
return, self.nlayers
END

Function DTable::GetColumnData, columnlabel, LAYER=layer
;; return a table column for the label columnlabel 
IF keyword_set(layer) then BEGIN
   IF NOT((layer GE 1) AND (layer LE self.nlayers)) THEN BEGIN
      print, "% DTable::GetColumnArray: Invalid layer."
      return, !NULL
   END
END ELSE BEGIN
   layer=self.currentlayer
END
;; 
i0=(layer-1)*self.ncolumns
i1=i0+self.ncolumns-1
pos=WHERE((*(self.pcoltitle))[i0:i1] EQ  columnlabel)
IF (N_Elements(pos) GT 1) THEN pos=pos[0] ;; only first hit
If NOT((pos GE 0) AND (pos LT self.ncolumns)) THEN BEGIN
   print, "% DTable::GetColumnArray: Invalid column label."
   return, !NULL
END
h=*(self.pcol)
i0=i0+pos
return,h[i0].ToArray()
END

FUNCTION DTable::GetListId
return, self.sellistid
END


PRO DTable::ClearPlotWidgetIds
  ;; put your cleanup stuff for the GUI here
  self.sellistid=[0L,0L]
END


PRO DTable_plot_cleanup, base
return
END

; Event-handler routine for DTable_widget
PRO Dtable_plot_events, ev
  WIDGET_CONTROL, ev.id, GET_UVALUE=uv
  CASE uv OF
     "list1": BEGIN
     END
     "list2": BEGIN
     END
     "plot": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        xcol=Widget_INFO((pobj->GetListID())[0], /LIST_SELECT)
        ycol=Widget_INFO((pobj->GetListID())[1], /LIST_SELECT)
        IF (N_Elements(ycol) GE 1) THEN BEGIN
           ;; create graph
           ylabels=(*(pobj->GetColumnlabels()))[ycol]
           YValues=pobj->GetColumnData(ylabels[0])
           ;; list counts columns from 0
           XVALUES=FLOAT(INDGEN(N_ELEMENTS(YValues))) & xlabel='Row Index'
           IF ((xcol[0]) LT pobj->NColumns()) THEN BEGIN
              xlabel=(*(pobj->GetColumnlabels()))[xcol[0]]
              XValues=pobj->GetColumnData(xlabel)
           END
           ;; now we have data for the first set
           ytitle=ylabels[0] & For i=2,N_Elements(ylabels) DO ytitle=ytitle+", "+ylabels(i-1)
           mygraph=obj_new('XYGraph', pobj->GetTableName()+"::"+pobj->GetLayerName(), XTITLE=xlabel, YTITLE=ytitle)
           IF OBJ_VALID(mygraph) THEN BEGIN
              mygraph->AddSet, ylabels[0], XValues, YValues, PSYM=2, SYMSIZE=1, /LINE, COLOR=RGBIndexColor(0,/LIGHT)
              For i=2,N_Elements(ylabels) DO BEGIN
                 YValues=pobj->GetColumnData(ylabels[i-1])
                 mygraph->AddSet, ylabels[i-1], XValues, YValues, PSYM=2, SYMSIZE=1, /LINE, COLOR=RGBIndexColor(i-1,/LIGHT)              
              END
              ;; add graph to graph container
              newgraph=GetNewXYGraphContainer(pobj->GetTableName()+"::"+pobj->GetLayerName())
              IF NOT(PTR_VALID(newgraph)) THEN BEGIN
                 ;; an error occurred, delete the object
                 ErrMsg, "Could not create graph list item"
                 mygraph->TidyUp
                 obj_destroy, mygraph
              END ELSE BEGIN
                 (*newgraph).pobj=mygraph
                 mygraph->UpdateDisplay
              END
           END
      END
        ;;
        WIDGET_CONTROL, ev.TOP, /DESTROY
        pobj->ClearPlotWidgetIds
     END
     "quit": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        WIDGET_CONTROL, ev.TOP, /DESTROY
        pobj->ClearPlotWidgetIds
     END
     "help": dummy=Dialog_Message(["Table Plot Dialog"," ","Choose your plot columns. You can select multipe columns for the independent variable."," "], /INFORMATION)
     ELSE:
  END
  Widget_Control, /Clear_Events 
END


FUNCTION DTable::PlotData, COLUMNS=columns, LAYER=layer
  IF NOT(keyword_set(layer)) THEN BEGIN
     layer=self.currentlayer
  END
  IF NOT(keyword_set(columns)) THEN BEGIN
     ;; present a widget to determine columns to plot
     i0=(layer-1)*self.ncolumns & i1=i0+self.ncolumns-1
     sellist1=[(*(self.pcoltitle))[i0:i1],"Row Index"]
     sellist2=(*(self.pcoltitle))[i0:i1]
     max_strlen=22
     for i=1,N_Elements(sellist1) DO BEGIN
        t_strlen = strlen(sellist1[i-1])
        if (t_strlen GT max_strlen) then max_strlen=t_strlen
     END
     maxwidth = max_strlen * !d.x_ch_size + 6 ; ... + 6 for padding
     base=WIDGET_BASE(Group_Leader=self.tlb, TITLE='Plot XY Graph',/COLUMN)
     row1=WIDGET_BASE(base, /ROW)
     col11=WIDGET_BASE(row1, /COLUMN)
     col12=WIDGET_BASE(row1, /COLUMN)
     label1=WIDGET_LABEL(col11, VALUE='Independent Variable')
     label2=WIDGET_LABEL(col12, VALUE='Dependent Variable')
     list1=WIDGET_LIST(col11, VALUE=sellist1, UVALUE='list1', XSIZE=max_strlen, YSIZE=5)
     list2=WIDGET_LIST(col12, VALUE=sellist2, /MULTIPLE, UVALUE='list2', XSIZE=max_strlen, YSIZE=5)
     self.sellistid=[list1,list2]
     row2=WIDGET_BASE(base, /ROW)
     plot = WIDGET_BUTTON( row2, VALUE = " Plot ", UVALUE='plot')
     help = WIDGET_BUTTON( row2, VALUE = " Help ", UVALUE='help')
     quit = WIDGET_BUTTON( row2, VALUE = " Cancel ", UVALUE='quit')
                                ; Realize the widgets.
     WIDGET_CONTROL, base, /REALIZE
     WIDGET_CONTROL, base, SET_UVALUE=self
     XMANAGER, 'DTable::PlotData', base, ClEANUP='DTable_plot_cleanup', EVENT_HANDLER='DTable_plot_events'
  END ELSE BEGIN

  END
  ;; create the graph
  graph=obj_new()
  return, graph
  ;; 
END

FUNCTION DTable::SelectColumnLabels, Text=text, LAYER=layer
  NCols=N_Elements(text)
  IF NOT(keyword_set(layer)) THEN BEGIN
     layer=self.currentlayer
  END
  ;; present a widget to determine columns to plot
  i0=(layer-1)*self.ncolumns & i1=i0+self.ncolumns-1
  sellist=(*(self.pcoltitle))[i0:i1]
  max_strlen=22
  for i=1,N_Elements(sellist) DO BEGIN
     t_strlen = strlen(sellist[i-1])
     if (t_strlen GT max_strlen) then max_strlen=t_strlen
  END
  maxwidth = max_strlen * !d.x_ch_size + 6 ; ... + 6 for padding
  base=WIDGET_BASE(TITLE='Select Table Columns',/COLUMN)
  row1=WIDGET_BASE(base, /ROW)
  col=LONARR(NCols) 
  dlabel=LONARR(NCols) 
  dlist=LONARR(NCols) 
  for k=0, NCols-1 DO BEGIN
     col[k]=WIDGET_BASE(row1, /COLUMN)
     dlabel[k]=WIDGET_LABEL(col[k], VALUE=Text[k])
     dlist[k]=WIDGET_LIST(col[k], VALUE=sellist, UVALUE="list"+MyString(dlist[k]), XSIZE=max_strlen, YSIZE=5)
  END
  row2=WIDGET_BASE(base, /ROW)
  select = WIDGET_BUTTON( row2, VALUE = " Select ", UVALUE='select')
  help = WIDGET_BUTTON( row2, VALUE = " Help ", UVALUE='help')
  cancel = WIDGET_BUTTON( row2, VALUE = " Cancel ", UVALUE='quit')
                                ; Realize the widgets.
  WIDGET_CONTROL, base, /REALIZE
  quit=0
  REPEAT BEGIN

     ev = WIDGET_Event(base)
     CASE ev.id of
        select:  BEGIN
           result=1
           quit=1
        END
        help: BEGIN
           
        END
        cancel: BEGIN
           quit=1
           result=0
        END
        ELSE:
     ENDCASE 
  ENDREP  UNTIL (quit EQ 1)
  ret=STRARR(NCols)
  IF (result EQ 1) THEN BEGIN
     for k=0, NCols-1 DO BEGIN
        ;; read selected
        selected=WIDGET_INFO(dlist[k], /LIST_SELECT)
        IF (selected GE 0) THEN  ret[k]=sellist[selected]
     END
  END
  Widget_Control, base, /DESTROY
  return, ret
  ;; 
END 


PRO DTable::FromStringArray, pdatastr, LAYER=layer
  ;; converts the data table from a string array
  ;; writes the data into layer table when layer is set 
  ;; otherwise it writes a full table 
  tabledim=SIZE(*pdatastr,/DIMENSIONS)
  IF (SIZE(tabledim, /N_Elements) LT 2) THEN BEGIN
     ;; only one row? tabledim needs to have 2 elements
     tabledim=[tabledim,1]
  END
  ;; create a vector of structures for widget_table
   i0=1 & i1=N_Elements(h)
   IF keyword_set(layer) THEN BEGIN
      IF ((layer GE 1) and (layer LE self.nlayers)) THEN BEGIN
         i0=(layer-1)*self.ncolumns+1 & i1=i0+self.ncolumns-1
      END
   END
   h=*(self.pcol)
   nrows=h[i0].Count()
   ncolumns=i1-i0+1
   IF (nrows NE tabledim[1]) THEN BEGIN
       print, "% DTable::FromStringArray: Row mismatch."
       return
   END
   IF (ncolumns NE tabledim[0]) THEN BEGIN
       print, "% DTable::FromStringArray: Column mismatch."
       return
   END
   ;;
   for j=0,(nrows-1) Do BEGIN ;; row counter
      for i=i0,i1 DO BEGIN ;; column counter
         l=h[i-1] ;; column list
         l[j]=FIX((*pdatastr)[i-i0,j],TYPE=SIZE(l[j], /TYPE)) ;; convert data type
         ;; print, "wrote col "+STRING(i)+" row " +STRING(j)+" val "+STRING(l[j]) + " type "+ STRING(SIZE(l[j], /TYPE)) + " from string " + (*pdatastr)[i-i0,j]
      END
   END 
return
END    

FUNCTION AddXMLCell, value
  s="<ss:Cell>"
  type=SIZE(value, /TYPE)
  Case type OF
     0: BEGIN
        ;; undefined
        s+='<ss:Data ss:Type="String">'
        s+='!NULL'
        s+='</ss:Data>'
     END
     1: BEGIN
        ;; Byte
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     2: BEGIN
        ;; 16bit integer
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     12: BEGIN
        ;; 16bit unsigned integer
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     3: BEGIN
        ;; 32bit signed integer
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     13: BEGIN
        ;; 32bit unsigned integer
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     14: BEGIN
        ;; 64bit signed integer
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'

     END
     15: BEGIN
        ;; 64bit unsigned integer
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     4: BEGIN
        ;; 32bit float
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     5: BEGIN
        ;; 64bit float
        s+='<ss:Data ss:Type="Number">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
      END
     6: BEGIN
        ;; complex ... not yet correct
        s+='<ss:Data ss:Type="String">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     9: BEGIN
        ;; complex ... not yet correct
        s+='<ss:Data ss:Type="String">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     7: BEGIN
        ;; string
        s+='<ss:Data ss:Type="String">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     8: BEGIN
        ;; structure
        s+='<ss:Data ss:Type="String">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     10: BEGIN
        ;; Pointer
        s+='<ss:Data ss:Type="String">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     11: BEGIN
        ;; Object Refrence
        s+='<ss:Data ss:Type="String">'
        s+=CleanSTRING(value)
        s+='</ss:Data>'
     END
     ELSE: BEGIN
        s+='<ss:Data ss:Type="String">'
        s+='!NULL'
        s+='</ss:Data>'
     END
  END
  s+="</ss:Cell>"
  return, s
END 


PRO DTable::ToXMLWorksheet, lines, layer
  ;; converts the data table to a string array
  ;; returns a nil pointer when the data array is empty
  ;; otherwise it returns a pointer to the string array 
res=PTR_NEW()
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::Edit .. No column definition."
   return
END 
IF (self.ncolumns GT 0) THEN BEGIN
;; create a vector of structures for widget_table
   i0=1 & i1=self.ncolumns
   IF ((layer GE 1) and (layer LE self.nlayers)) THEN BEGIN
      i0=(layer-1)*self.ncolumns+1 & i1=i0+self.ncolumns-1
   END
   h=*(self.pcol)
   ;; 
   layername=(*(self.playernames))[layer-1]
   IF (STRLEN(layername) GT 32) THEN BEGIN
      ;; if it is a filename then we may be successful by cutting the
      ;; path name off  
      try=FILE_BASENAME(layername)
      IF (STRLEN(try) GT 32) THEN Begin
         indexlabel="L"+STRCOMPRESS(STRING(layer),/REMOVE_ALL)+" "
         indexlabelen=strlen(indexlabel)
         reslen=32-indexlabelen
         try=+indexlabel+STRMID(try,reslen)
      END
      layername=try
   END
   lines.Add, '<ss:Worksheet ss:Name="'+layername+'">'
   ;; lines.Add, '<ss:Table ss:ExpandedColumnCount="'+STRCOMPRESS(STRING(i1-i0+1))+ '"' + $
   ;;   ' ss:ExpandedRowCount="'+STRCOMPRESS(STRING(h[i0-1].Count()-1))+'"'+
   ;;	 ' x:FullColumns="1" x:FullRows="1">'
   ;; ExpandedColumnCount, this attribute specifies the total number of columns in this table. If specified, this attribute must be in sync with the table. Columns indices in the table should begin at 1 and go to ExpandedColumnCount.
   lines.Add, '<ss:Table>'
   lines.Add, '<ss:Row ss:StyleID="1">'
   for i=i0,i1 DO BEGIN         ;;     add header row 
      lines.Add, AddXMLCell((*(self.pcoltitle))[i-1]) 
   END
   lines.Add, '</ss:Row>'
   
   for j=0,(h[i0-1].Count()-1) Do BEGIN ;; row counter
      lines.Add, '<ss:Row>'
      for i=i0,i1 DO BEGIN               ;; column counter, starting from 1
         ;; print, "Column: ", i
         col=h[i-1] ;; the column values
         ;; print, col
         lines.Add, AddXMLCell(col[j])
      END
      lines.Add, '</ss:Row>'
   END 
   lines.Add, "</ss:Table>"
   lines.Add, "</ss:Worksheet>"
END
;;
return
END    



FUNCTION DTABLE::XML, Layer=layer
lines=list()
;; prepare the xml header
lines.add, '<?xml version="1.0"?>'
lines.add, '<ss:Workbook xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet">'
lines.add, '<ss:Styles>'
lines.add, '<ss:Style ss:ID="1">'
lines.add, '<ss:Font ss:Bold="1"/>'
lines.add, '</ss:Style>'
lines.add, '</ss:Styles>'
IF keyword_set(layer) THEN BEGIN
 self->ToXMLWorksheet, lines, layer
END ELSE BEGIN
   for i=1,self.nlayers DO BEGIN
      self->ToXMLWorksheet, lines, i
   END
END
;; prepare the xml footer
lines.add, '</ss:Workbook>'
return, lines
END

FUNCTION DTABLE::BlockData, Layer=layer
  lines=list()
  IF keyword_set(layer) THEN BEGIN
     ;; prepare the header line
     i0=(layer-1)*self.ncolumns ;; first column of layer layer
     s=""
     for i=1,self.ncolumns DO BEGIN ;; column counter
        s+=(*(self.pcoltitle))[i0+i-1]+ self.separator
     END
     lines.Add, s
   ;;
     pdata=self->ToStringArray(LAYER=layer)
     dim=SIZE((*pdata),/DIMENSIONS)
     for i=0,dim[1]-1 DO BEGIN
        s=""
        for j=0,dim[0]-1 DO BEGIN
           s=s+" "+(*pdata)[j,i]
        END
        lines.Add, s
     END
END ELSE BEGIN
   for k=1,self.nlayers DO BEGIN
      pdata=self->ToStringArray(LAYER=k)
     dim=SIZE(*pdata,/Dimensions)
     for i=0,dim[1]-1 DO BEGIN
        s=""
        for j=0,dim[0]-1 DO BEGIN
           s=s+" "+(*pdata)[j,i]
        END
        lines.Add, s
     END
   END
END
;; prepare the xml footer
return, lines
END 


Pro DTable::Export, Format=format, LAYER=layer, ALL=all, FILE=file, NONINTERACTIVE=noninteractive
;; export data
;; by default it exports a free-formatted output
;; each line of the output is a row 
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::AddRow .. No column definition."
END
;; declare file type list here and hand it over to XFileDialog!
   tlist=PTR_NEW(['XML', $
                  'CSV', $
                  'SYLK', $
                  'X Y1 Y2 ... YN Blockdata' $
                 ])

   ftype=3 & swp=0 & grp=0 & savefilter='*'
   IF (ftype LT 0) THEN ftype=0
   IF (ftype GE N_ELEMENTS(*tlist)) THEN ftype=0
IF keyword_set(file) AND NOT(keyword_set(noninteractive)) THEN BEGIN
;; declare file type list here and hand it over to XFileDialog!
   cancelled=0
   wdir="."
   files = rmd_pickfile(title="TableObject::Export", $
                              filter_in = savefilter,              $
                              filter_out = savefilter,              $
                              path = wdir,          $
                              get_path = wdir,          $
                              cancelled = cancelled,        $
                              swapen = swp,             $
                              type = ftype,             $
                              ftypes = tlist,     $
                              crgrp = 0,             $
                              /save)
      if (cancelled EQ 1) then begin
              printtocon, '% TableObject::Export: User cancelled file dialog!'
              return
      END
      if (files[0] EQ '') THEN BEGIN
         printtocon, '% TableObject::Export: User cancelled file dialog!'
         return
      END
      fn=files[0]
   END ELSE fn=file
BEGIN
      
      lun=-1
      IF (fn EQ "STDOUT") THEN BEGIN
         lun=-1
         PrintToCon, "% DTable::Export: Data export to STDOUT" 
      END ELSE BEGIN
         if (LMGR(/DEMO)) then begin
            MESSAGE, 'Feature disabled for demo mode.'
            XConsole_PopState
            return
         END
         
         PrintToCon, "% DTable::Export: Data export to "+fn 
         GET_LUN, lun
         OPENW, lun, fn
      END   
      ftype=(*tlist)[ftype]
      CASE ftype OF
         "XML": BEGIN
            if keyword_set(all) then BEGIN
               lines=self->XML()
            END ELSE BEGIN
               lines=self->XML(LAYER=self.currentlayer)
            END
            ;; write lines to file
            for i=0,lines.Count()-1 Do printf, lun, lines[i]

         END
         'X Y1 Y2 ... YN Blockdata': BEGIN
            if keyword_set(all) then BEGIN
               lines=self->BlockData()
            END ELSE BEGIN
               lines=self->Blockdata(LAYER=self.currentlayer)
            END
            ;; write lines to file
            for i=0,lines.Count()-1 Do printf, lun, lines[i]
         END
         ELSE: BEGIN
         END
      END
      IF (lun GE 0) THEN BEGIN
         FREE_LUN, lun
      END 
      return
   END 
s=""
IF keyword_set(layer) THEN BEGIN
   layer0=layer-1 & layer1=layer-1
END ELSE BEGIN
   IF keyword_set(all) THEN BEGIN
      layer0=0 & layer1=self.nlayers-1
   END ELSE BEGIN
      layer0=self.currentlayer-1 & layer1=self.currentlayer-1      
   END
END
IF (layer0 LT 0) AND (layer0 GE self.nlayers) THEN BEGIN
   print, "% DTable::Export: Invalid layer number."
   return
END
FOR layer=layer0,layer1 DO BEGIN
   ;; output title
   i0=layer*self.ncolumns ;; first column of layer layer
   s=""
   for i=1,self.ncolumns DO BEGIN ;; column counter
      s+=(*(self.pcoltitle))[i0+i-1]+ self.separator
   END
   print, s
   ;; 
   h=*(self.pcol)
   for j=0,(h[i0].Count()-1) Do BEGIN ;; row counter
      row=list()
      for i=1,self.ncolumns DO BEGIN ;; column counter
         l=h[i0+i-1]
         row.Add, l[j]
      END
      IF keyword_set(format) THEN BEGIN
         print, FORMAT=format, row
      END ELSE BEGIN
         s=""
         for i=1,self.ncolumns DO BEGIN
            s=s + self.separator + (STRING(row[i-1], /PRINT))
         END
         print, s
      END 
      row.Remove, /ALL
   END
;;
END   
;;
return
END    

FUNCTION DTable::ToList, LAYER=layer, SKIPINDEXCOL=skipindexcol
;; export layer data to list of rows
;; each line of the output is a row
;; KEYWORDS:
;; LAYER: the spreadsheet layer to convert, first layer index is 1   
;; SKIPINDEXCOL: skip first column which usually contans the row index
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::ToList .. No column definition."
END
 IF NOT(keyword_set(layer)) THEN layer=self.currentlayer-1 
 IF (layer LT 0) AND (layer GE self.nlayers) THEN BEGIN
   print, "% DTable::ToList: Invalid layer number."
   return, 0
END
  outlist=list()
   ;; output title
   i0=layer*self.ncolumns ;; first column of layer layer
   s=""
   for i=1,self.ncolumns DO BEGIN ;; column counter
      s+=(*(self.pcoltitle))[i0+i-1]+ self.separator
   END
   print, s
   ;; 
   h=*(self.pcol)
   if keyword_set(skipindexcol) THEN BEGIN
      tmp=FLTARR(self.ncolumns-1)
      firstcol=1
   END ELSE BEGIN
      tmp=FLTARR(self.ncolumns)
      firstcol=0
   END
   for j=0,(h[i0].Count()-1) Do BEGIN ;; row counter
      for i=0, (self.ncolumns-1-firstcol) DO BEGIN
         ;; construct row vector
         tmp[i]=(h[i+i0+firstcol])[j]
      END
      outlist->add, tmp ;; row array
   END

;;
return, outlist
END    


FUNCTION DTable::ToStringArray, LAYER=layer
  ;; converts the data table to a string array
  ;; returns a nil pointer when the data array is empty
  ;; otherwise it returns a pointer to the string array 
res=PTR_NEW()
IF NOT(Ptr_Valid(self.pcol)) THEN BEGIN
   ;; undefined ?
   print, "DTable::Edit .. No column definition."
   return, res
END 
IF (self.ncolumns GT 0) THEN BEGIN
;; create a vector of structures for widget_table
   i0=1 & i1=self.ncolumns
   IF keyword_set(layer) THEN BEGIN
      IF ((layer GE 1) and (layer LE self.nlayers)) THEN BEGIN
         i0=(layer-1)*self.ncolumns+1 & i1=i0+self.ncolumns-1
      END
   END
   h=*(self.pcol)
   dataarray=PTR_NEW(STRARR(i1-i0+1,h[i0].Count()))
   for i=i0,i1 DO (*dataarray)[i-i0,*]=(h[i-1]).ToArray(TYPE=7)
   return, dataarray
END
;;
return, res
END    




PRO DTable::ClearWidgetIds
  ;; put your cleanup stuff for the GUI here
  self.tlb=0L
  self.tableid=0L
END

PRO DTable::Cleanup_Widgets
  ;; put your cleanup stuff for the GUI here
END

FUNCTION DTable::GetTableId
return, self.tableid
END

FUNCTION DTable::GetTableBase
return, self.tablebase
END


PRO Dtable_widget_cleanup, tbl
  ;; trick xmanager to call a cleanup procedure
END



; Event-handler routine for DTable_widget
PRO Dtable_widget_events, ev
  print, ev
  WIDGET_CONTROL, ev.id, GET_UVALUE=uv
  CASE uv OF
     "exportcurrent": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        pobj->Export, LAYER=pobj->GetLayer(), /FILE
     END
     "exportall": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        pobj->Export, /ALL, /FILE
     END
     "plotlayer": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        graph=pobj->PlotData()
     END
     "layer": BEGIN
        ;; changing layer view
        ;; get current layer data from table and return it to the
        ;; object
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        WIDGET_CONTROL, pobj->GetTableId(), GET_VALUE=data
        pdata=PTR_NEW(data)
        IF PTR_VALID(pdata) THEN BEGIN
           pobj->FromStringArray, pdata, LAYER=pobj->GetLayer()
           PTR_FREE, pdata
        END
        ;;
        ;; get layer data from object and display it in widget
        Widget_CONTROL, ev.id, Get_Value=value
        layer=ev.index+1
        pobj->SetLayer, layer
        pdata=pobj->ToStringArray(LAYER=pobj->GetLayer())
        ;; set widget table display
        IF PTR_VALID(pdata) THEN BEGIN
           WIDGET_CONTROL, pobj->GetTableId(), SET_VALUE=(*pdata)
           PTR_FREE, pdata
        END
     END
     "quit": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        WIDGET_CONTROL, ev.TOP, /DESTROY
        pobj->ClearWidgetIds
     END
     "tableinfo": BEGIN
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        dummy=Dialog_Message(*(pobj->GetPdescriptor()), /INFORMATION)
     END
     "save changes": BEGIN
        ;; rewrite all object data from the table
        ;; changing layer view
        ;; get current layer data from table and return it to the
        ;; object
        WIDGET_CONTROL, ev.top, GET_UVALUE=pobj
        WIDGET_CONTROL, pobj->GetTableId(), GET_VALUE=data
        pdata=PTR_NEW(data)
        IF PTR_VALID(pdata) THEN BEGIN
           pobj->FromStringArray, pdata, LAYER=pobj->GetLayer()
           PTR_FREE, pdata
        END
        ;;
        ;; get layer data from object and display it in widget
        Widget_CONTROL, ev.id, Get_Value=value
        layer=1
        IF IN("INDEX",Tag_names(ev)) THEN layer=ev.index+1
        pobj->SetLayer, layer
        pdata=pobj->ToStringArray(LAYER=pobj->GetLayer())
        ;; set widget table display
        IF PTR_VALID(pdata) THEN BEGIN
           WIDGET_CONTROL, pobj->GetTableId(), SET_VALUE=(*pdata)
           PTR_FREE, pdata
        END
        
     END
     "help": dummy=Dialog_Message(["Table Editor"," ","Choose table layers from the 'Layer' menu. 'Information'->'Show Descriptor' will show the table descriptor.","Make changes to a cell value permanent by pressing return or selecting another cell."," "], /INFORMATION)
     ELSE:
  END
  Widget_Control, /Clear_Events 
END


PRO DTable::TidyUp
; destroy widget, if still open
print, "Cleaning up ", self.tablename
if (self.tlb GT 0) THEN BEGIN
   ;; ask to save data?
   widget_control,self.tlb,/destroy
   self->ClearWidgetIds
END
;-- free memory allocated to pointer when destroying object
  IF Ptr_Valid(self.pcol) THEN BEGIN
     h=*(self.pcol) ;; array of column lists
     For i=1, N_Elements(h) DO BEGIN
        l=h[i-1]
        IF (l.Count() GT 0) THEN BEGIN
           l.REMOVE, /ALL 
        END
     END
     PTR_FREE, self.pcol
  END
 IF Ptr_Valid(self.pcoltitle) THEN BEGIN
    PTR_FREE, self.pcol
 END
 IF Ptr_Valid(self.pdescriptor) THEN BEGIN
    PTR_FREE, self.pdescriptor
 END
 IF Ptr_Valid(self.playernames) THEN BEGIN
    PTR_FREE, self.playernames
 END
 return
end  

function DTable::cleanup
  self->TidyUp
  return, 1
END

; Widget creation routine.
PRO DTable::WidgetEditor, BLOCK=block
; To make sure the table looks nice on all platforms,
; set all column widths to the width of the longest string
; that can be a header.
   max_strlen=10
   IF (PTR_VALID(self.pcoltitle)) THEN BEGIN
      for i=1,N_Elements(*(self.pcoltitle)) DO BEGIN
         t_strlen = strlen((*(self.pcoltitle))[i-1])
         if (t_strlen GT max_strlen) then max_strlen=t_strlen
      END
   END 
  maxwidth = max_strlen * !d.x_ch_size + 6   ; ... + 6 for padding
; Create base widget, two tables (column- and row-major,
  ; respectively), and 'Quit' button.
  tlb = WIDGET_BASE(TITLE=self.tablename,/COLUMN, MBAR=bar)
  self.tlb=tlb
  menu3 = WIDGET_BUTTON(bar, VALUE='Table Editor', UVALUE='help')
  menu1 = WIDGET_BUTTON(bar, VALUE='Plot', /MENU)
  menu11 = WIDGET_BUTTON(menu1, VALUE=' Plot Layer Data', UVALUE='plotlayer')
  menu2 = WIDGET_BUTTON(bar, VALUE='Export', /MENU)
  menu21 = WIDGET_BUTTON(menu2, VALUE=' Current Layer ', UVALUE='exportcurrent')
  menu21 = WIDGET_BUTTON(menu2, VALUE=' All Layers ', UVALUE='exportall')
  menu31 = WIDGET_BUTTON(menu3, VALUE=' Save Changes ', UVALUE='save changes')
  menu31 = WIDGET_BUTTON(menu3, VALUE=' Help ', UVALUE='help')
  menu32 = WIDGET_BUTTON(menu3, VALUE=' Close ', UVALUE='quit')
  layer=self->GetLayer()
  pdata=self->ToStringArray(LAYER=layer)
  IF NOT(PTR_VALID(pdata)) THEN BEGIN
     print, "Table_widget_example: Invalid table data."
  END
  tableid = WIDGET_TABLE(tlb, VALUE=(*pdata), ROW_LABELS='', COLUMN_LABELS=*(self.pcoltitle), /RESIZEABLE_COLUMNS, /EDITABLE,  BACKGROUND_COLOR=[220,220,220], /NO_ROW_HEADERS, UVALUE="table", ALIGNMENT=1, COLUMN_WIDTHS=maxwidth, SCR_YSIZE=300)
  self.tableid=tableid
  layerselrow=WIDGET_BASE(tlb, /ROW)
  layerlabel=Widget_label(layerselrow, VALUE='Layer: ')
  layerbuttonid=WIDGET_COMBOBOX(layerselrow, VALUE=(*self.playernames), UVALUE="layer",/DYNAMIC_RESIZE, /FLAT)
  dummy=WIDGET_TEXT(tlb, VALUE=*(self.pdescriptor), YSize=3,FRAME=0, EDITABLE=0, /WRAP)
  ;; brow=WIDGET_BASE(tlb, /ROW)
  ;; b_quit = WIDGET_BUTTON(brow, VALUE='  Quit  ', UVALUE='quit')
  ;; b_help = WIDGET_BUTTON(brow, VALUE='  Help  ', UVALUE='help')
  ;; b_descr = WIDGET_BUTTON(brow, VALUE='Table Descriptor', UVALUE='tableinfo')
  ;;
                                ; create structure of parameters that
                                ; can be picked up by the event
                                ; handler


  HELP, self
  ; Realize the widgets.
  WIDGET_CONTROL, tlb, /REALIZE
  WIDGET_CONTROL, tlb, SET_UVALUE=self
  
  if keyword_set(block) THEN BEGIN
     XMANAGER, 'DTable::WidgetEditor', self.tlb, ClEANUP='DTable_widget_cleanup', EVENT_HANDLER='DTable_widget_events'
  END ELSE BEGIN
     XMANAGER, 'DTable::WidgetEditor', self.tlb, /NO_BLOCK, ClEANUP='DTable_widget_cleanup', EVENT_HANDLER='DTable_widget_events'
  END
END


pro DTable__define
 void={DTable,name:'DTable',tablename:'undefined', nlayers:1, playernames:PTR_NEW(), currentlayer:0, $
              ncolumns:0,nrows:0,separator:' ',fgcolor:6558720L,bgcolor:13428479L,pcol:Ptr_New(), $
              pcoltitle:Ptr_New(),debug:0,pdescriptor:PTR_NEW(),sellistid:[0L,0L],tlb:0L,tableid:0L}
 return 
end 

Function DTable_Example
  mytable=obj_new('DTable', 'Example Table', 3, NLAYERS=2)
  mytable->SetColumnLabels, ["index","label","value"]
  mytable->SetLayerNames, ["Table Layer A", "Table Layer B"]
  mytable->SetDescriptor, ["Example table","Table with two layers."] 
;; layer 0
  mytable->AddRow, list(1,"row-1",1.23456)
  mytable->AddRow, list(2,"row-2",2.23456E5)
  mytable->AddRow, list(3,"row-3",3.23456E10)
  mytable->AddRow, list(4,"row-4",4.23456)
  ;; layer 1
  mytable->AddRow, list(11,"layer 2, row 1",22.), LAYER=2
  mytable->AddRow, list(22,"layer 2, row 2",33.), LAYER=2
  mytable->AddRow, list(33,"layer 2, row 3",44.), LAYER=2
  mytable->AddRow, list(44,"layer 2, row 4",55.), LAYER=2
;;
  mytable->SetLayer, 1
  mytable->Export, FORMAT='(1I4, 1A20, 1E16.7)'
  mytable->SetLayer, 2
  mytable->Export, FORMAT='(1I4, 1A20, 1E16.7)'
  ;; mytable->Export, FORMAT='(1I4,",", 1A7,",", 1E16.7)'
  mytable->WidgetEditor
  ;; mytable->Export, /File
  return, mytable
END 


Function TableImport2DArray, a, TITLE=title, COLUMNTITLES=columntitles, SKIPINDEX=skipindex
  mytable=OBJ_NEW()
 IF (N_Elements(title) EQ 0) THEN title='Table'
 ndims=SIZE(a,/N_DIMENSIONS)
 dim= SIZE(a,/DIMENSIONS)
 CASE ndims OF
    1: BEGIN
       nlayers=1
       if keyword_set(skipindex) THEN ncolums=1 ELSE ncolumns=2
       mytable=obj_new('DTable', 'Example Table', ncolumns, NLAYERS=nlayers, COLUMNLABELS=columntitles)
       For i=1,nrows DO BEGIN
          if keyword_set(skipindex) THEN mytable->AddRow, list([a[i-1]],/EXTRACT) ELSE mytable->AddRow, list([i,a[i-1]],/EXTRACT)
       END
    END
    2: BEGIN
       nlayers=1
       if keyword_set(skipindex) THEN ncolumns=dim[1] ELSE ncolumns=dim[1]+1
       nrows=dim[0]
       mytable=obj_new('DTable', 'Example Table', ncolumns, NLAYERS=nlayers, COLUMNLABELS=columntitles)
       For i=1,nrows DO BEGIN
          if keyword_set(skipindex) THEN mytable->AddRow, list([REFORM(a[i-1,*])],/EXTRACT) ELSE mytable->AddRow, list([i,REFORM(a[i-1,*])],/EXTRACT)
       END
    END
    3: BEGIN
       nlayers=dims[2]
       ncolumns=dims[1]+1
       mytable=obj_new('DTable', 'Example Table', ncolumns, NLAYERS=nlayers, COLUMNLABELS=columntitles)
    END
    ELSE:
 END
 return, mytable
end  


FUNCTION TableImport2DList, l, TITLE=title, COLUMNTITLES=columntitles, SKIPINDEX=skipindex
;; import layer data from to list of rows
;; each row is a list
;; 
;; KEYWORDS:
;; LAYER: the spreadsheet layer, first layer index is 1   
;; SKIPHEADREROW: skip first column which usually contans the row
;; index
  IF (N_Elements(title) EQ 0) THEN title='Table'
  nlayers=1
  IF keyword_set(columntitles) THEN ncolumns=N_Elements(columntitles) ELSE BEGIN
     row=l[0]
     ncolumns=row.Count() 
     columntitles=STRARR(row.Count())
  END
  ;; how to use NewTable:
;; a table with 5 columns and 3 layers
  mytable=obj_new('DTable', title, ncolumns, NLAYERS=1)
  mytable->SetColumnLabels, columntitles
; mytable->SetDescriptor, ["Demonstration table.","The table has 5 columns ", "and 3 layers"] 
;; select layer number 2 of 3
  mytable->Setlayer, 1
  IF (l.Count() GT 0) THEN BEGIN
     For jj=0,l.count()-1 DO BEGIN
       mytable->AddRow, l[jj]
     END
  END
return, mytable
END    

FUNCTION ReadTableFromBlockData, Header=header, NOARRAY=noarray
;; reads ascii block data from a file and returns table object
;;
;; 
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% ReadTableFromBlockData: Error while attempting to read data "
    PrintToCon, "%                " + !ERR_STRING
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    return, 0
 END
p=0
tlist=PTR_NEW(["X Y1 Y2 ... Block Data","CSV data"])
type=0
result=rmd_pickfile(  $
       filter_in=datafilter, $
       filter_out=datafilter, $
       path = GetWorkingDir(),          $
       get_path = out_path,          $
       title = "Read Ascii Block Data",          $
;;       crgrp = savealldata,          $
       cancelled = cancelled,        $
       type = type,        $
       ftypes=tlist, $
       /open)

PTR_FREE, tlist
ap=PTR_NEW()
IF NOT(cancelled) THEN BEGIN
   SetWorkingDir, out_path
   fn=result[0]
   CASE type OF
      1: separator=","
      ELSE: separator=" "
   END
   header=["empty"]
   IF keyword_set(noarray) THEN BEGIN
      a=ReadBlockDataToList(fn, HEADER=header, SEPARATOR=separator)
      return, TableImport2DList(a, TITLE=fn, COLUMNTITLES=header, /SKIPINDEX)
   END ELSE BEGIN
      a=ReadBlockDataToList(fn, /TOARRAY, HEADER=header, SEPARATOR=separator,/TOFLOAT)
      return, TableImport2DArray(a, TITLE=fn, COLUMNTITLES=header, /SKIPINDEX)
   END
END
return, 0
END 


