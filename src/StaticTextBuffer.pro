Function StaticTextBuffer::Init, buffername, MAXLINES=maxlines, DEBUG=debug
  self.name=buffername
  self.separator=String(10B)+String(13B)
  self.nlines=0L
  self.ptext=PTR_NEW()
  IF keyword_set(debug) then self.debug=debug ELSE self.debug=0
  return, 1
END


pro StaticTextBuffer__define
 void={StaticTextBuffer,id:'statictextbuffer',name:'undefined',nlines:0L,separator:'',ptext:Ptr_New(),debug:0}
 return 
end

PRO StaticTextBuffer::Add, s, index
;; add textlines at line index index
;; get current string
  t=!NULL
  IF PTR_Valid(self.ptext) THEN BEGIN
     t=*(self.ptext)
     PTR_FREE, self.ptext
  END
  IF (index GT 0) THEN BEGIN
     IF (t EQ !NULL) THEN BEGIN
        ;; no previous text
        x=STRARR(index+1)
        x[index]=s
     END ELSE BEGIN
        x=strsplit(t,self.separator,/EXTRACT)
        n=SIZE(x,/N_ELEMENTS)
        IF (index LT n) THEN BEGIN
           ;; we have to replace the line
           x[index]=s
        END ELSE BEGIN
           y=x
           x=STRARR(index+1)
           x[index]=s
           if (n GE 1) THEN x[0:n-1]=y
        END
     END
     t=""
     self.nlines=N_Elements(x)
     For i=0,self.nlines-1 DO BEGIN
        if (i GT 0) THEN t=t+self.separator+x[i] ELSE t=x[i]
     END
     self.ptext=PTR_NEW(t)
     self.nlines=N_Elements(x)
  END ELSE BEGIN
        ;; index <0 : append
     END
  return
END   

function StaticTextBuffer::GetNumberOfLines
;-- free memory allocated to pointer when destroying object                 ;
  return, self.nlines
end

function StaticTextBuffer::ToStringArray
;-- free memory allocated to pointer when destroying object
  p=self.ptext
  x=[] ;; set as null: (x EQ !NULL) is 1
  IF PTR_VALID(p) THEN BEGIN
     t=(*p)
     x=[]
     IF (t NE !NULL) THEN x=strsplit(t,self.separator,/EXTRACT)
  END
  return, x
end



function StaticTextBuffer::ToString 
; convert to a single string
; default choice of separator in CR+LF
;-- STRING(10B)+STRING(13B)
  p=self.ptext
  IF PTR_VALID(p) THEN return, *p ELSE return, !NULL
end

PRO StaticTextBuffer::FromString, s
; convert to a single string
; default choice of separator in CR+LF
;-- STRING(10B)+STRING(13B)
  IF PTR_VALID(self.ptext) THEN BEGIN
     ;; delete current string
     PTR_FREE, self.ptext
  END
  IF ((s EQ "") OR (s EQ !NULL)) THEN self.ptext=PTR_NEW() ELSE self.ptext=PTR_NEW(s)
  return
end


PRO StaticTextBuffer::flush
;-- free memory allocated to pointer when destroying object
  p=self.ptext                  ;
  If Ptr_Valid(p) THEN BEGIN
     PTR_FREE, p
  END
end 


function StaticTextBuffer::cleanup
;-- free memory allocated to pointer when destroying object
  p=self.ptext ;
  While Ptr_Valid(p) DO BEGIN
     PTR_FREE, p ;; Free string pointer
  END
  return, 1
end 


Pro StaticTextbufferTest
  ;;HELP, /HEAP
  HELP, /MEMORY
  print, "creating buffer ..."
  print, "four lines ..."
  t=obj_new('StaticTextBuffer', 'Console')
  t->Add, "Line 1", 0
  t->Add, "Line 2", 1
  t->Add, "Line 3", 2
  t->Add, "Line 4", 3
  print, t->ToStringArray()
  print, "changing two lines ..."
  t->Add, "Line 5", 0
  t->Add, "Line 6", 1
  print, t->ToStringArray()
  print, "flushing lines ..."
  t->flush
  print, t->ToStringArray()
  print, "changing two lines more ..."
  t->Add, "Line 7", 0
  t->Add, "Line 8", 1
  print, t->ToStringArray()
  print, "destroying buffer ..."
  obj_destroy, t
  ;; HELP, /HEAP
  HELP, /MEMORY
END

