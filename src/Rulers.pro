PRO plot_ruler, x, y, ORIENTATION=orientation, COLOR=plcolor, SCREENSCOPE=screenscope, SCREENSIZE=screensize, RULERSCOPE=rulerscope, PSTYLE=pstyle, SINGLE=single
;; plot a ruler
;;print, "plot-ruler: orientation = ", orientation
;;print, "plot-ruler: x, y = ", x, ", ", y, " (in)"
IF (orientation EQ 0) THEN BEGIN
   ;; horizontal ruler
   ;; check with boundary values along vertical direction
   ;; we try to keep the width
   ;; check that the width fits into the scope
   IF (y GT (rulerscope[3]-rulerscope[2])) THEN y=(rulerscope[3]-rulerscope[2])
   ;; x must be greater than 0.5*y plus rulerscope[2]
   IF (x LT (0.5*y+rulerscope[2])) THEN x=0.5*y+rulerscope[2]
   ;; x must be smaller than rulerscope[3] - 0.5*y
   IF (x GT (rulerscope[3]-0.5*y)) THEN x=rulerscope[3]-0.5*y
   ;; x is mid position for vertical direction
   y0=FIX(((x-y*0.5)-screenscope[2])/(screenscope[3]-screenscope[2])*(screensize[1]-1))
   y1=FIX(((x+y*0.5)-screenscope[2])/(screenscope[3]-screenscope[2])*(screensize[1]-1))
   ym=FIX((x-screenscope[2])/(screenscope[3]-screenscope[2])*(screensize[1]-1))
   ;; check with boundary values along vertical direction  
   x0=((rulerscope[0]-screenscope[0])/(screenscope[1]-screenscope[0]))*(screensize[0]-1) 
   x1=((rulerscope[1]-screenscope[0])/(screenscope[1]-screenscope[0]))*(screensize[0]-1) 
   ;;
   if NOT(keyword_set(single)) THEN BEGIN
      plots, [x0,x1], [y0,y0],/device, COLOR=plcolor, LINESTYLE=pstyle
      plots, [x0,x1], [y1,y1],/device, COLOR=plcolor, LINESTYLE=pstyle
      plots, [x0,x1], [ym,ym],/device, COLOR=plcolor, LINESTYLE=1
   END ELSE plots, [x0,x1], [ym,ym],/device, COLOR=plcolor, LINESTYLE=pstyle
END
IF (orientation EQ 1) THEN BEGIN
   ;; horizontal ruler
   ;; check with boundary values along vertical direction
   ;; we try to keep the width
   ;; check that the width fits into the scope
   IF (y GT (rulerscope[1]-rulerscope[0])) THEN y=(rulerscope[1]-rulerscope[0])
   ;; position x must be greater than 0.5*y plus rulerscope[2]
   IF (x LT (0.5*y+rulerscope[0])) THEN x=0.5*y+rulerscope[0]
   ;; x must be smaller than rulerscope[1] - 0.5*y 
   IF (x GT (rulerscope[1]-0.5*y)) THEN x=rulerscope[1]-0.5*y
   ;; x is mid position for vertical direction
   x0=FIX(((x-y*0.5)-screenscope[0])/(screenscope[1]-screenscope[0])*(screensize[0]-1))
   x1=FIX(((x+y*0.5)-screenscope[0])/(screenscope[1]-screenscope[0])*(screensize[0]-1))
   xm=FIX((x-screenscope[0])/(screenscope[1]-screenscope[0])*(screensize[0]-1))
   ;; check with boundary values along horizontal direction  
   y0=((rulerscope[2]-screenscope[2])/(screenscope[3]-screenscope[2]))*(screensize[1]-1) 
   y1=((rulerscope[3]-screenscope[2])/(screenscope[3]-screenscope[2]))*(screensize[1]-1) 
   ;;
   if NOT(keyword_set(single)) THEN BEGIN
      plots, [x0,x0], [y0,y1],/device, COLOR=plcolor, LINESTYLE=pstyle
      plots, [x1,x1], [y0,y1],/device, COLOR=plcolor, LINESTYLE=pstyle
      plots, [xm,xm], [y0,y1],/device, COLOR=plcolor, LINESTYLE=1
   END ELSE plots, [xm,xm], [y0,y1],/device, COLOR=plcolor, LINESTYLE=pstyle
;;STOP
   END
;;print, "plot-ruler: x, y = ", x, ", ", y, " (out)"
END

Function Ruler::Init, SCREENSCOPE=screenscope, WDOW=wdow
;; screensope is the absolute size for the object displayed, it can be
;; nm, pix, apples
self.name="ruler"
self.pstyle=0
self.fgcolor=0L
self.bgcolor=13428479L
self.window=-1
if not(keyword_set(wdow)) THEN BEGIN
   self.window=!D.WINDOW
END ELSE self.window=wdow
IF (self.window LT 0) THEN BEGIN
       print, "% Ruler::Init: invalid window id."
       return, 0
END
IF (self.window NE !D.WINDOW) THEN BEGIN
   tmp=!D.WINDOW
   WSET, self.window
   self.screensize=[!D.X_VSIZE,!D.Y_VSIZE]
   WSET, tmp
END ELSE BEGIN
   self.screensize=[!D.X_VSIZE,!D.Y_VSIZE]
END
self.plotmode=9
self.orientation=0 ;; horizontal
IF keyword_set(screenscope) THEN BEGIN
   ;; window scope in absolute pixel numbers
   self.screenscope=screenscope
END ELSE BEGIN
   ;; ruler coordinates relative to view frame
   ;; 
   self.screenscope=[0.,100.,0.,100.]
END
IF keyword_set(debug) then self.debug=debug ELSE self.debug=0
return, 1
END

pro Ruler__define
 void={Ruler,id:'ruler',name:'undefined',window:-1,fgcolor:6558720L,bgcolor:13428479L,pstyle:0,phash:Ptr_New(),debug:0,plotmode:9, orientation:0, screenscope:[0.,100.,0.,100.], screensize:[100,100], single:0}
 return 
end

Pro Ruler::AddRuler, name, x, y, ORIENTATION=orientation, COLOR=color, PSTYLE=pstyle, SORTKEY=sortkey, RULERSCOPE=rulerscope, SINGLE=single
;; Create a new ruler
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
IF NOT(keyword_set(pstyle)) THEN pstyle=self.pstyle
IF NOT(keyword_set(orientation)) THEN orientation=self.orientation
IF NOT(keyword_set(rulerscope)) THEN scope=self.scope
IF NOT(keyword_set(sortkey)) THEN sortkey=name
IF NOT(keyword_set(single)) THEN single=self.single
s=Create_Struct('name', name, 'x', x, 'y', y, 'color', color, 'pstyle', pstyle, 'sortkey', sortkey, 'orientation', orientation, 'rulerscope', rulerscope, 'hidden', 0B, 'single', single)
h[name]=PTR_NEW(s)
;; plot ruler
tmp=!D.WINDOW
IF (self.window NE !D.WINDOW) THEN BEGIN
   WSET, self.window
END
      ;;
device, get_graphics_function=gmode0        ; store the current graphics mode.
device, set_graphics_function=self.plotmode ; XOR mode.
;; plot ruler
x=s.x & y=s.y
plot_ruler, x, y, ORIENTATION=s.orientation, COLOR=s.color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=s.rulerscope, PSTYLE=s.pstyle, SINGLE=single
;; reset old window and graphics mode
s.x=x & s.y=y 
s.hidden=0B
device, set_graphics_function=gmode0  
WSET, tmp
;;
return
END 

Pro Ruler::ModifyRuler, name, X=xnew, Y=ynew, COLOR=color, PSTYLE=pstyle
;; Change set Settings
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "Ruler: Request to modify set in graph"+self.name+" where there are no sets." 
   return
END 
;; add a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
;;
   IF PTR_VALID(s) THEN BEGIN
      
      ;; plot ruler
      tmp=!D.WINDOW
      IF (self.window NE !D.WINDOW) THEN BEGIN
         WSET, self.window
      END
      ;;
      device, get_graphics_function=gmode0     ; store the current graphics mode.
      device, set_graphics_function=self.plotmode ; XOR mode.
      ;; overplot old ruler
      IF ((*s).hidden EQ 0) THEN BEGIN 
         plot_ruler, (*s).x, (*s).y, ORIENTATION=(*s).orientation, COLOR=(*s).color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=(*s).rulerscope, PSTYLE=(*s).pstyle, SINGLE=(*s).single
      END
      IF keyword_set(color) THEN (*s).color=color
      IF keyword_set(pstyle) THEN (*s).pstyle=pstyle
      IF NOT(keyword_set(xnew)) THEN x=(*s).x ELSE x=xnew
      IF NOT(keyword_set(ynew)) THEN y=(*s).y ELSE y=ynew
      ;;print, "modify ruler: x, y = ", x, ", ", y, " (in)"
      plot_ruler, x, y, ORIENTATION=(*s ).orientation, COLOR=(*s).color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=(*s).rulerscope, PSTYLE=(*s).pstyle, SINGLE=(*s).single
      (*s).hidden=0B
      ;;print, "modify ruler: x, y = ", x, ", ", y, " (out)"
      (*s).x=x & (*s).y=y
      (*s).hidden=0B
      ;; reset old window and graphics mode
      device, set_graphics_function=gmode0  
      WSET, tmp
   END ELSE BEGIN
      ErrMsg, "Ruler: Ruler data structure "+name+" does not exist for ruler "+ name +" in "+self.name+"."
   END
END ELSE BEGIN
   ErrMsg, "Ruler: Ruler "+name+" does not exist in "+self.name+"."
END
return
END 

Pro Ruler::HideRuler, name
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
   IF (PTR_VALID(s)) THEN BEGIN
      IF ((*s).hidden EQ 0) THEN BEGIN
      
         ;; plot ruler
         tmp=!D.WINDOW
         IF (self.window NE !D.WINDOW) THEN BEGIN
            WSET, self.window
         END
         ;;
         device, get_graphics_function=gmode0     ; store the current graphics mode.
         device, set_graphics_function=self.plotmode ; XOR mode.
         ;; overplot old ruler
         plot_ruler, (*s).x, (*s).y, ORIENTATION=(*s).orientation, COLOR=(*s).color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=(*s).rulerscope, PSTYLE=(*s).pstyle, SINGLE=(*s).single
         (*s).hidden=1B
         ;; reset old window and graphics mode
         device, set_graphics_function=gmode0  
         WSET, tmp
      END 
   END ELSE BEGIN
      ErrMsg, "Ruler: Ruler data structure "+name+" does not exist for ruler "+ name +" in "+self.name+"."
   END
END ELSE BEGIN
   ErrMsg, "Ruler: Ruler "+name+" does not exist in "+self.name+"."
END
return
END 

Pro Ruler::ShowRuler, name
;; Change set Settings
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "Ruler: Request to modify set in graph"+self.name+" where there are no sets." 
   return
END 
;; add a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
;;
   IF (PTR_VALID(s)) THEN BEGIN
      IF ((*s).hidden EQ 1) THEN BEGIN
      
         ;; plot ruler
         tmp=!D.WINDOW
         IF (self.window NE !D.WINDOW) THEN BEGIN
            WSET, self.window
         END
         ;;
         device, get_graphics_function=gmode0     ; store the current graphics mode.
         device, set_graphics_function=self.plotmode ; XOR mode.
         ;; overplot old ruler
         plot_ruler, (*s).x, (*s).y, ORIENTATION=(*s).orientation, COLOR=(*s).color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=(*s).rulerscope, PSTYLE=(*s).pstyle, SINGLE=(*s).single
         (*s).hidden=0B
         ;; reset old window and graphics mode
         device, set_graphics_function=gmode0  
         WSET, tmp
      END
   END ELSE BEGIN
      ErrMsg, "Ruler: Ruler data structure "+name+" does not exist for ruler "+ name +" in "+self.name+"."
   END
END ELSE BEGIN
   ErrMsg, "Ruler: Ruler "+name+" does not exist in "+self.name+"."
END
return
END 

Function Ruler::GetRuler, name, X=x, Y=y
result=0
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "XYGraph: Request to modify set in graph"+self.name+" where there are no sets." 
   return, 0
END 
;; add a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
;;
   IF PTR_VALID(s) THEN BEGIN
      result=1
      x=(*s).x & y=(*s).y
END ELSE BEGIN
      ErrMsg, "Ruler: Ruler data structure "+name+" does not exist for ruler "+ name +" in "+self.name+"."
   END
END  ELSE BEGIN
   ErrMsg, "Ruler: Ruler "+name+" does not exist in "+self.name+"."
END 

return, result
END 

Pro Ruler::DeleteRuler, name
;;
;; Delete a set
;;
IF NOT(Ptr_Valid(self.phash)) THEN BEGIN
   ;; no data set, create new hash
   ErrMsg, "Ruler: Request to delete ruler in "+self.name+" where there are no rulers." 
   return
END 
;; delete a hash entry
h=*(self.phash)
;; catch error when key does not exist
if (h.HasKey(name)) THEN BEGIN
   s=h[name] ;; pointer to structure
   IF PTR_VALID(s) THEN BEGIN
      ;; plot ruler
      tmp=!D.WINDOW
      IF (self.window NE !D.WINDOW) THEN BEGIN
         WSET, self.window
      END
      ;;
      device, get_graphics_function=gmode0        ; store the current graphics mode.
      device, set_graphics_function=self.plotmode ; XOR mode.
      ;; overplot old ruler
      plot_ruler, (*s).x, (*s).y, ORIENTATION=(*s).orientation, COLOR=(*s).color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=(*s).rulerscope, PSTYLE=(*s).pstyle, SINGLE=(*s).single
      ;; reset old window and graphics mode
      device, set_graphics_function=gmode0  
      WSET, tmp
      ptr_free, s
   END
   h.Remove, name
END ELSE BEGIN
   ErrMsg, "Ruler: Ruler "+name+" does not exit in "+self.name+"."
END
return
END  

function Ruler::cleanup
;-- free memory allocated to pointer when destroying object
  IF Ptr_Valid(self.phash) THEN BEGIN
     h=*(self.phash)
     setkeys=h.keys()
     Nsetkeys=N_Elements(setkeys)
     For i=0, (NSetkeys-1) DO BEGIN
        s=h[setkeys[i]]
        IF PTR_VALID(s) THEN BEGIN
;; plot ruler
           tmp=!D.WINDOW
           IF (self.window NE !D.WINDOW) THEN BEGIN
              WSET, self.window
           END
           ;;
           device, get_graphics_function=gmode0   ; store the current graphics mode.
           device, set_graphics_function=self.plotmode ; XOR mode.
           ;; overplot old ruler
           plot_ruler, (*s).x, (*s).y, ORIENTATION=(*s).orientation, COLOR=(*s).color, SCREENSCOPE=self.screenscope, SCREENSIZE=self.screensize, RULERSCOPE=(*s).rulerscope, PSTYLE=(*s).pstyle, SINGLE=(*s).single
           ;; reset old window and graphics mode
           device, set_graphics_function=gmode0  
           WSET, tmp
           PTR_FREE, s
        END 
     END
     PTR_FREE, self.phash
  END
  return, 1
end 


;; Example:
;; --------
;; requires RGBColor.pro
;;
PRO Ruler_Example
;;
;; Simple example that shows how to create a ruler object, 
;; how to add rulers how to replot
;;
;;wdow=2
;;DimX=512 & DimY=400
;;Window, wdow, XSIZE=DimX, YSize=DimY
;; first create a new instance of a ruler object
;; parameters are the screen scope in arbitrary units 
;; (here for instance for a screen binning of 2)
;; and the window number 
;;myrulers=obj_new('Ruler', SCREENSCOPE=[0.,1023.,0,799], WDOW=wdow)
myrulers=obj_new('Ruler', SCREENSCOPE=[0.,1023.,0,799])
;; add a ruler to the ruler object
;; you have to give it an identifier name, here "Ruler 0"
;; x is the mid-position
;; dx the width
;; ORIENTATION: 0-horizontal, 1-vertical
;; PSTYLE is the plot style, 0-solid, 1-dotted, 2-dashed, 3-DashDot,
;; 4-DashDotDotDot, 5-Long Dashes
;; RGBColor defines the color in red, green, blue values ranging from
;; 0-255
;; RULERSCOPE defines the plotrange in absolute coordinates
x=400 & dx=300
;;
myrulers->AddRuler, 'Ruler 0', x, dx, ORIENTATION=1, PSTYLE=0, COLOR=RGBColor(20,100,100), RULERSCOPE=[50.,712.,0.,799]
;;print, "Set: ruler range  ", x-dx*0.5, " - ", x+dx*0.5
rx=0 & ry=0
IF myrulers->GetRuler('Ruler 0', X=rx, Y=ry) THEN BEGIN
   print, "Read: ruler range  ", rx-ry*0.5, " - ", rx+ry*0.5
END ELSE BEGIN
   print, "Read: ruler range read failure "
END
;;
;; plot the data (first time) or update the plot on the screen
dummy=dialog_message("Click to continue ...")

;; modify 'Set 1' to display the square cosine 
x=950 & dx=300
print, "modified ruler range: ", x-dx*0.5, " - ", x+dx*0.5
myrulers->ModifyRuler, 'Ruler 0', X=x, Y=dx
print, "Set: ruler range  ", x-dx*0.5, " - ", x+dx*0.5
IF myrulers->GetRuler('Ruler 0', X=rx, Y=ry) THEN BEGIN
   print, "Read: ruler range  ", rx-ry*0.5, " - ", rx+ry*0.5
END ELSE BEGIN
   print, "Read: ruler range read failure "
END

;;
dummy=dialog_message("Deleting ruler ...")
myrulers->DeleteRuler, 'Ruler 0'
dummy=dialog_message("Deleting ruler object ...")
;; Help, /HEAP
obj_destroy, myrulers
;; Help, /HEAP
END


