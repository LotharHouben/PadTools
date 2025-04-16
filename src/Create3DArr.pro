PRO Create3DArr, DimX, DimY, DimZ, type, identifier
;;
;; demonstrate the creation of a new data stack
;;
;; the procedure creates a new pointer structure and a data array 
;; parameters: DimX, DimY, DimZ = integer:array dimensions
;;             type = integer: data type
;;             identifier = string: the name of the data stack 
;;
;; example: Create3DArr, 512, 512, 128, 4, "a floating point array"
;; 
  ptr=GetRootP() ;; get the root pointer of the list of all data stacks
  IF (NOT(PTR_VALID(ptr))) THEN BEGIN
     ;; there is no root
     printtocon, "% Create_Stack: Root pointer is invalid" 
     return
  END
  ;; Create a new container for the data stack, the pointer to this
  ;; container - pp - will be appended to the existing list and appear
  ;; automatically in the 'Data Inspector' 
  pp=DataList_CreateElement(ptr, identifier)
  ;; set the dimensions of the data array
  (*pp).SzX=DimX & (*pp).SzY=DimY & (*pp).SzZ=DimZ
  ;; set the data type, here: float
  (*pp).type=4
  ;; allocate memory and get pointer ppd to the array
  ppd=(Data_GetEmptyArrayP(pp))
  IF NOT(PTR_VALID(ppd)) THEN BEGIN
     print, "% Create_Stack: failed to create data array" 
     return
  END
  ;; strore array pointer in data container
  (*pp).data=ppd
  ;; here you can do something with the array, here is an example how
  ;; to index 
  (*ppd)[(*pp).SzX/2,(*pp).SzY/2,*]=3.1415927
  ;; set the viewing direction to Y
  (*pp).zcoord=2
  ;; choose center slice for display
  (*pp).slice=FIX((*pp).SzY/2)
  ;; leave contrastmode as 'auto'
  (*pp).contrastmode="auto"
  ;; create the disply window
  CreateWindow
  ;; display the array slice in the window
  TVDisplay  
  ;; update the GUI to show your data container and the container info
  Update_XTabControl
END
