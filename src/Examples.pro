

PRO DotStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% RotateVol: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="Dot(32,32,48)"
  pp=DataList_CreateElement(ptr, name)
  NDim=128
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% Dot: failed to create data array" 
     return
  END
  (*pp).data=ppd
  (*ppd)[32,32,48]=100
  (*pp).slice=48
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END


PRO LineStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% RotateVol: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="Line(32,32,*)"
  pp=DataList_CreateElement(ptr, name)
  NDim=128
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% LineStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  (*ppd)[32,32,*]=100
  (*pp).slice=48
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END

FUNCTION RadiusVol, r
;;
;; returns a quadratic voulume containing in each pixel the distance
;; to the central pixel  
;; 
Radius = fltarr(2*r+1,2*r+1,2*r+1)
;; 
i0=r & j0=r & k0=r
FOR i=0,r DO BEGIN
   FOR j=0,r DO BEGIN
      FOR k=0,r DO BEGIN
         d=SQRT(i*i+j*j+k*k)
         Radius(i0+i,j0+j,k0+k)=d
         Radius(i0+i,j0+j,k0-k)=d
         Radius(i0+i,j0-j,k0+k)=d
         Radius(i0-i,j0+j,k0+k)=d
         Radius(i0+i,j0-j,k0-k)=d
         Radius(i0-i,j0-j,k0+k)=d
         Radius(i0-i,j0+j,k0-k)=d
         Radius(i0-i,j0-j,k0-k)=d
      END
   END
END
return, Radius
END


PRO SphereStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% RotateVol: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="Sphere(32,32,32;5)-Shifted(-10,-10,-5)"
  pp=DataList_CreateElement(ptr, name)
  radius=50
  Sp=RadiusVol(radius)
  B=WHERE((Sp GT (radius*0.7)) AND (Sp LT (radius*0.9)))
  C=WHERE((Sp GT (radius*0.2)) AND (Sp LT (radius*0.4)))
  Sp(*,*,*)=0
  Sp(B)=100
  Sp(C)=50
  NDim=256
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% LineStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/2)-radius & i1=i0+2*radius
  (*ppd)[i0:i1,i0:i1,i0:i1]=Sp
  (*ppd)=Shift((*ppd),-10,-10,-5)
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END

Pro SimpleSphereStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% SimpleSphereStack: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="SimpleSphere(32,32,32;5)-shifted(-6,-10,5)"
  pp=DataList_CreateElement(ptr, name)
  radius=50
  Sp=RadiusVol(radius)
  B=WHERE((Sp GT (radius*0.4)) AND (Sp LT (radius*0.9)))
  ;;C=WHERE((Sp GT (radius*0.2)) AND (Sp LT (radius*0.4)))
  Sp(*,*,*)=0
  Sp(B)=100
  ;;Sp(C)=50
  NDim=192
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% SimpleSphereStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/2)-radius & i1=i0+2*radius
  (*ppd)[i0:i1,i0:i1,i0:i1]=Sp
  (*ppd)=Shift((*ppd),-6,-10,5)
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END

Pro HalfDomeStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% HalfDomeStack: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="HalfDome,shifted(-6,-10,5)"
  pp=DataList_CreateElement(ptr, name)
  radius=50
  Sp=RadiusVol(radius)
  B=WHERE((Sp GT (radius*0.4)) AND (Sp LT (radius*0.9)))
  ;;C=WHERE((Sp GT (radius*0.2)) AND (Sp LT (radius*0.4)))
  Sp(*,*,*)=0
  Sp(B)=100
  ;;Sp(C)=50
  NDim=192
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% HalfDomeStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/2)-radius & i1=i0+2*radius
  iz0=Fix(NDim/2)
  (*ppd)[i0:i1,i0:i1,i0:i1]=Sp
  (*ppd)[i0:i1,i0:i1,i0:iz0]=0.
  (*ppd)=Shift((*ppd),-6,-10,5)
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END

Pro CappedSphere
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% CappedSphere: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="CappedSphere,shifted(-6,-10,5)"
  pp=DataList_CreateElement(ptr, name)
  radius=50
  Sp=RadiusVol(radius)
  B=WHERE((Sp GT (radius*0.5)) AND (Sp LT (radius*0.9)))
  ;;C=WHERE((Sp GT (radius*0.2)) AND (Sp LT (radius*0.4)))
  Sp(*,*,*)=0
  Sp(B)=100
  ;;Sp(C)=50
  NDim=192
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% CappedSphere: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/2)-radius & i1=i0+2*radius
  iz0=Fix(NDim/2)-radius*0.4
  (*ppd)[i0:i1,i0:i1,i0:i1]=Sp
  (*ppd)[i0:i1,i0:i1,i0:iz0]=0.
  (*ppd)=Shift((*ppd),-6,-10,5)
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END


FUNCTION CylinderVol, r, l
;;
;; returns a quadratic volume containing in each pixel the distance
;; to the central line  
;; 
Cylinder = fltarr(2*r+1,2*l+1,2*r+1)
;; 
i0=r & j0=l & k0=r
FOR i=0,r DO BEGIN ;; x
   FOR j=0,l DO BEGIN  ;; y
      FOR k=0,r DO BEGIN ;; z
         d=SQRT(i*i+k*k)
         Cylinder(i0+i,j0+j,k0+k)=d
         Cylinder(i0+i,j0+j,k0-k)=d
         Cylinder(i0+i,j0-j,k0+k)=d
         Cylinder(i0-i,j0+j,k0+k)=d
         Cylinder(i0+i,j0-j,k0-k)=d
         Cylinder(i0-i,j0-j,k0+k)=d
         Cylinder(i0-i,j0+j,k0-k)=d
         Cylinder(i0-i,j0-j,k0-k)=d
      END
   END
END
return, Cylinder
END


PRO CylinderStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% CylinderStack: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="Cylinder(32,32,32;5)-Shifted(-10,-10,-5)"
  pp=DataList_CreateElement(ptr, name)
  radius=50
  halflength=100
  Sp=CylinderVol(radius,halflength)
  B=WHERE((Sp GT (radius*0.7)) AND (Sp LT (radius*0.9)))
  C=WHERE((Sp GT (radius*0.2)) AND (Sp LT (radius*0.4)))
  Sp(*,*,*)=0
  Sp(B)=100
  Sp(C)=50
  NDim=256
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% LineStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/2)-radius & i1=i0+2*radius
  j0=Fix(NDim/2)-halflength & j1=j0+2*halflength
  (*ppd)[i0:i1,j0:j1,i0:i1]=Sp
  (*ppd)=Shift((*ppd),-10,-10,-5)
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END

PRO RandomSpheres
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% RandomSpheres: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  NDim=512
  minpix=5
  maxpix=15
  minsig=50
  maxsig=100
  num=200
  a=STRARR(2,6)
  a[0,0]="Dimension (pix)                 "
  a[1,0]=MyString(NDim)
  a[0,1]="Minumum radius (pix)            "
  a[1,1]=MyString(minpix)
  a[0,2]="Maximum radius (pix)            "
  a[1,2]=MyString(maxpix)
  a[0,3]="Minimum density (integer value) "
  a[1,3]=MyString(minsig)
  a[0,4]="Maximum density (integer value) "
  a[1,4]=MyString(maxsig)
  a[0,5]="Number of spheres               "
  a[1,5]=MyString(num)
  IF (XSimpleDataField(a, TITLE="Parameters") EQ 1) THEN BEGIN
      NDim=Fix(a[1,0])
      minpix=Fix(a[1,1])
      maxpix=Fix(a[1,2])
      minsig=Fix(a[1,3])
      maxsig=Fix(a[1,4])
      num=Fix(a[1,5])
   END ELSE BEGIN
      return
   END
  name="RandomSpheres"
  pp=DataList_CreateElement(ptr, name)
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% RandomSpheres: failed to create data array" 
     return
  END
  count=0
  seed=LONG(SYSTIME(/SECONDS))
  ;; STOP
  WHILE (count LT num) DO BEGIN
     radius=FIX(randomu(seed)*(maxpix-minpix)+minpix)  ;; calculate random radius
     Sp=RadiusVol(radius)
     B=WHERE(Sp LT (radius))
     Sp(*,*,*)=0
     signal=FIX(randomu(seed)*(maxsig-minsig)+minsig)  ;; calculate random radius;; add random signal
     Sp(B)=signal
     ;; calculate random position
     posx=FIX(randomu(seed)*(NDim-2*radius-2))
     posy=FIX(randomu(seed)*(NDim-2*radius-2))
     posz=FIX(randomu(seed)*(NDim-2*radius-2))
     x0=posx & x1=x0+2*radius 
     y0=posy & y1=y0+2*radius
     z0=posz & z1=z0+2*radius
     ;; position volume Sp inside the 3D volume 
     ;; STOP
     (*ppd)[x0:x1,y0:y1,z0:z1]+=Sp
     count=count+1
  END
  (*pp).data=ppd
  (*pp).slice=NDim/2
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END



PRO HexagonStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% HexagonStack: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="HexagonStack"
  pp=DataList_CreateElement(ptr, name)
  r=96
  halflength=100
  EINS=INTARR(4*r+1) 
  EINS(*)=1
  Grad=INDGEN(4*r+1)-2*r
  A=Transpose(EINS)##Grad
  B=Transpose(A)
  res=A 
  res(*,*)=100
  alpha=30./180.*!DPI
  cosa=cos(alpha) & sina=sin(alpha)
  plen=cosa*r
  ind=WHERE((cosa*A+sina*B) GT plen)
  res(ind)=0
  ind=WHERE((cosa*A-sina*B) GT plen)
  res(ind)=0
  ind=WHERE((-cosa*A+sina*B) GT plen)
  res(ind)=0
  ind=WHERE((-cosa*A-sina*B) GT plen)
  res(ind)=0
  ind=WHERE(B GT plen)
  res(ind)=0
  ind=WHERE(B LT -plen)
  res(ind)=0 
  NDim=4*r
  res=res[0:(NDim-1),0:(NDim-1)]
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=1
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% HexagonStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/6) & i1=Fix(NDim-NDim/6)
  FOR i=i0,i1 DO BEGIN
     (*ppd)[i,*,*]=res
  END
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END

PRO OctagonStack
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% OctagonStack: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  name="OctagonStack"
  pp=DataList_CreateElement(ptr, name)
  r=96
  halflength=100
  EINS=INTARR(4*r+1) 
  EINS(*)=1
  Grad=INDGEN(4*r+1)-2*r
  A=Transpose(EINS)##Grad
  B=Transpose(A)
  res=A 
  res(*,*)=100
  ind=WHERE((A+B) GT r)
  res(ind)=0
  ind=WHERE((-A+B) GT r)
  res(ind)=0
  ind=WHERE(ABS((-A+B)) GT r)
  res(ind)=0
  ind=WHERE((A+B) LT -r) 
  res(ind)=0
  ind=WHERE(A GT 0.707*r)
  res(ind)=0
  ind=WHERE(A LT -0.707*r)
  res(ind)=0
  ind=WHERE(B GT 0.707*r)
  res(ind)=0
  ind=WHERE(B LT -0.707*r)
  res(ind)=0 
  NDim=4*r
  res=res[0:(NDim-1),0:(NDim-1)]
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=1
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% OctagonStack: failed to create data array" 
     return
  END
  (*pp).data=ppd
  i0=Fix(NDim/6) & i1=Fix(NDim-NDim/6)
  FOR i=i0,i1 DO BEGIN
     (*ppd)[i,*,*]=res
  END
  (*pp).slice=32
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END


PRO PSFSphere
  ptr=GetRootP()
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     print, "% RandomSpheres: Root pointer is invalid" 
     return
  END
  ;;Create a new stack
  NDim=512
  minpix=5
  maxpix=15
  minsig=50
  maxsig=100
  num=200
  a=STRARR(2,2)
  a[0,0]="Dimension (pix)  "
  a[1,0]=MyString(NDim)
  a[0,1]="Radius (pix)     "
  a[1,1]=MyString(minpix)
  IF (XSimpleDataField(a, TITLE="Parameters") EQ 1) THEN BEGIN
      NDim=Fix(a[1,0])
      minpix=Fix(a[1,1])
   END ELSE BEGIN
      return
   END
  name="PSFSphere"
  pp=DataList_CreateElement(ptr, name)
  (*pp).SzX=NDim & (*pp).SzY=NDim & (*pp).SzZ=NDim
  (*pp).type=2
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% RandomSpheres: failed to create data array" 
     return
  END
  radius=FIX(minpix)
  Sp=RadiusVol(radius)
  B=WHERE(Sp LT (radius))
  Sp(*,*,*)=0
  signal=1.
  Sp(B)=signal
  ;; calculate random position
  posx=FIX(NDim/2.)
  posy=FIX(NDim/2.)
  posz=FIX(NDim/2.)
  x0=posx-radius & x1=x0+2*radius 
  y0=posy-radius & y1=y0+2*radius
  z0=posz-radius & z1=z0+2*radius
  ;; position volume Sp inside the 3D volume 
  ;; STOP
  (*ppd)[x0:x1,y0:y1,z0:z1]+=Sp
  (*pp).data=ppd
  (*pp).slice=NDim/2
  (*pp).zcoord=2
  (*pp).contrastmode="auto"
  CreateWindow
  TVDisplay  
  Update_XTabControl
END
