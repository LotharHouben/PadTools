

FUNCTION NodeRecursion, parentname, oNodeIterator
  FORWARD_FUNCTION NodeRecursion
  ;; a new child
  res=1
  oNode = oNodeIterator->nextNode()
  WHILE OBJ_VALID( oNode ) DO BEGIN
    
     IF oNODE->HasChildNodes() THEN BEGIN
        ;; more children, step further down
        ;; but only if it is not text!
        IF (oNode->GetNodeType() NE 3) THEN BEGIN
           tmp=parentname
           print, "before: ", parentname
           Help, oNode.GetNodeType()
           parentname= parentname+"."+ oNode.GetNodeName()
           ;; IDL passes by reference while it should be by value for the recursion
           res = NodeRecursion(parentname, oNodeIterator)
           parentname=tmp
           print, "after: ", parentname
           STOP
           return, parentname
        END ELSE BEGIN
           ;; print the text 
           ;; no child, read value
           PRINT, "parentname : "+ parentname
           PRINT, "nodename: "+oNode->getNodeName()
           PRINT, "value: "
           STOP
        END
     END ELSE BEGIN
        ;; no child, read value
     END
     STOP
     oNode = oNodeIterator->nextNode()
  END 
  return, parentname
END



;+
; Use the IDL DOM parser to parse an XML file.
;-

;+
; Routine to recursively parse the DOM tree.
;
; :Params:
;    node : in, required, type=object
;       DOM tree node
;    names : in, out, required, type=strarr
;       planet names encountered so far
;-
pro xml_parse_rec, node, names, phash
  compile_opt strictarr
  tag = node->getNodeName()
  ;; print, "tag, node type=", tag, " ", node->GetNodeType()
  if (obj_isa(node, 'IDLffXMLDOMElement')) then begin
     vchild = node->GetFirstChild()
     if (vchild) THEN BEGIN
        ;; print, strlowcase(node->getTagName())+"="+strlowcase(vchild->GetNodeValue())
        (*phash)[node->getTagName()]=vchild->GetNodeValue()
    if (strlowcase(node->getTagName()) eq 'pix_x') then begin
      name = node->getAttribute('NAME')
      names = n_elements(names) eq 0L ? name : [names, name]
    endif
 endif else begin
     ;; not a child? Try attributes
    t=node.GetAttributes()
  ;; timestamp: node type is 1 
    if (t) then BEGIN
       (*phash)[node->getTagName()]=hash()
       For i=1,t->GetLength() DO BEGIN
          ta=t->Item(i-1)
          if (obj_isa(ta, 'IDLffXMLDOMAttr')) then begin
             ;; print, "Attribute ", strlowcase(ta->getName()), "=", strlowcase(ta->getValue())
             ((*phash)[node->getTagName()])[ta->getName()]=ta->getValue()
          END
       END
    END
    end
 endif  

  child = node->getFirstChild()
  while (obj_valid(child)) do begin
    xml_parse_rec, child, names, phash
    child = child->getNextSibling()
 endwhile 
end   




    

FUNCTION ReadXMLToHash, fname
  ; Example of using the IDL DOM parser to parse an EMPAD XML file.
  domEMPAD = obj_new('IDLffXMLDOMDocument', FILENAME=fname)
  phash=PTR_NEW(hash())
  xml_parse_rec, domEMPAD, names, phash
  obj_destroy, domEMPAD
  result=*phash
  PTR_FREE, phash
  return, result
END 
