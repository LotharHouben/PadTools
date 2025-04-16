

;; 
;; Ugly code for ugly data formats! 
;; 


     

FUNCTION AnalyseDM3Structure, LUN, i, swapdescr, sizeofencoded, dsize, DEBUG=debug, VERSION4=version4
;;
;; purpose: analyses a structure descriptor in a TagType field
;;          of a Digital Micrograph 3/4 coded data file
;; evaluates the datasize dsize of the structure in the TagData field
;; 
;;
err=1 & success=0
if keyword_set(version4) THEN along=0LL ELSE along=0L
if KEYWORD_SET(debug) then PrintToLog, "          is structure"
readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
if KEYWORD_SET(debug) then PrintToLog, "          structure name length=" +MyString(along)
readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
nf=along
if KEYWORD_SET(debug) then PrintToLog, "          number of fields =" + MyString(nf)
while (nf GT 0) DO BEGIN
    readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
    if KEYWORD_SET(debug) then PrintToLog, "          field name length=" + MyString(along)
    readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
    if KEYWORD_SET(debug) then PrintToLog, "          field type=" +MyString(along)
    if (along GT 14) THEN BEGIN
        PrintToLog, '% AnalyseDM3Structure: fatal - unknown type, cannot determine data size'
        return, err
    END
    dsize=dsize+sizeofencoded[along]
    nf=nf-1
END
return, success
END





FUNCTION Read_DM3x_TagGroup, LUN, swapdescr, sizeofencoded, nestlevel, fullkeyname, counter, KEYS=keys, VERBOSE=verbose, DEBUG=debug, VERSION4=version4
;; purpose: analyses a complete TagGroup structure
;;          of a Digital Micrograph 3 coded data file
;; LUN       = logical unit number or the data file
;; swapdescr = 0 or 1, 1 if descriptor bytes must be swapped
;; sizeofencoded = array of long integers, the data size in bytes
;;                 for the encoded data types
;; nestlevel = nesting level of this routine, which is called
;;             recursively
;; 

FORWARD_FUNCTION Read_DM3x_TagGroup
nestlevel=nestlevel+1
oldcounter=counter
counter=0 ;; to number non-labelled (empty label) tag entrys
;debug=1
;verbose=1
oldfullkeyname=fullkeyname


err=1 & success=0
abyte=0B
along32=0L
along64=0LL
ashort=0S
nentry=0L
;;
;; start reading tag group header
;;
;; TagGroup format
;; Byte:  Type:           Value:   Comment:
;; 0      8bit char       0 or 1   is the group sorted?
;; 1      8bit char       0 or 1   is the group open?
;; 2-5    32bit integer   N        number of tags entries in the group
;;        (DM4: 64bit integer)
;; 6-...                           TagEntry
if KEYWORD_SET(debug) then BEGIN 
  PrintToLog, "+++ start of tag group, nesting level " + MyString(nestlevel)
  pos=0LL
  POINT_LUN, -LUN, pos
  PrintToLog, "file position= " + MyString(pos)
END

readu, LUN, abyte
if KEYWORD_SET(debug) then PrintToLog, "TagGroup - sorted=" + MyString(abyte)
readu, LUN, abyte
if KEYWORD_SET(debug) then PrintToLog, "TagGroup - group open=" + MyString(abyte)
if keyword_set(version4) THEN BEGIN
   along=along64
END ELSE BEGIN
   along=along32
END
readu, LUN, along
IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
nentry=along
if KEYWORD_SET(debug) then PrintToLog, "TagGroup - number of tag entries=" + MyString(along)

;;
;; proceed with tag entries
;;
;; TagEntry format
;; Byte:  Type:           Value:      Comment:
;; 0      8bit char       14h or 15h  14h => A TagGroup will follow (recursion!) 
;;                                    15h => TagType and Tag Data will follow
;; 1-2    16bit integer   N           length of tag label in bytes, 
;;                                    Warning: label length 0 is
;;                                    allowed. This is complete non-sense:
;;                                    how do you find a tag labelled
;;                                    the empty string in text search?    
;; 3-N+2  char*N                      tag label
;; (DM4: N+3:  64bit integer          tag length in bytes, jump from
;;                                    here (N+12) to get to the next
;;                                    tag entry!
;; N+3-...                            TagType and TagData or TagGroup
;; (DM4: N+12 - ...)
;;
tag='entry'
WHILE  (nentry GT 0) DO BEGIN
   ;; STOP
     ;; begin with entry analysis
    
    if KEYWORD_SET(debug) then $
       PrintToLog, "+++ start of tag entry " + MyString(nestlevel) + "." + MyString(nentry)
    ;; ---------------------------------------------------------------------------------------------
    ;; read tag identifier byte
    ;; 14h = 20      tag directory
    ;; 15h = 21      tag
    ;; 00            end of file
    ;; ---------------------------------------------------------------------------------------------
    readu, LUN, abyte
    tag='entry'
    nentry=nentry-1
    CASE abyte OF
        20: tag='group' ;; a Group will follow, recursion
        21: tag='type'  ;; Data will follow
        ELSE: ;; entry? let's try!
     ENDCASE

    if KEYWORD_SET(debug) then BEGIN
       CASE abyte OF
          20: PrintToLog, "tag is tag directory" ;; a Group will follow, recursion
          21: PrintToLog, "tag is a data tag"  ;; Data will follow
          ELSE:           ;; entry? let's try!
       ENDCASE
    END
    ;; ---------------------------------------------------------------------------------------------
    ;; read the name, if it is empty then number the entry
    ;; tag label name length l: 16bit int 
    ;; tage label: l bytes
    ;; ---------------------------------------------------------------------------------------------
    readu, LUN, ashort & IF (swapdescr EQ 1) then ashort=SWAP_ENDIAN(ashort)
    if KEYWORD_SET(debug) then PrintToLog, "tag label length=" +MyString(ashort)
    IF (ashort GT 0) THEN BEGIN
        label=BYTARR(ashort)
        readu, LUN, label
        label=STRING(label)
    END ELSE BEGIN
       label = MyString(nentry)
    END

    fullkeyname=oldfullkeyname+'.'+label
    if KEYWORD_SET(debug) then PrintToLog, "tag label=" + MyString(fullkeyname)
    ;; PrintToLog, "tag label=" + MyString(fullkeyname)

    if keyword_set(version4) then BEGIN
       readu, LUN, along64 & IF (swapdescr EQ 1) then along64=SWAP_ENDIAN(along64)
       if KEYWORD_SET(debug) then PrintToLog, "tag size in bytes=" + MyString(along64)
    END
    ;; ---------------------------------------------------------------------------------------------
    ;; try to match a certain tag with a key
    ;; ---------------------------------------------------------------------------------------------
    keyindex=-1
    IF KEYWORD_SET(keys) THEN BEGIN
        ;; search for label in keylist
        l=N_ELEMENTS(keys)
        while (l GT 0) DO BEGIN
           ;; printtocon, keys[l-1].name + " <-> " +MyString(fullkeyname)
            if (keys[l-1].name EQ MyString(fullkeyname)) THEN BEGIN

;;            if (keys[l-1].name EQ MyString(label)) THEN BEGIN
                if (keys[l-1].written EQ 0) THEN BEGIN
                    keyindex=l-1
                    if keyword_set(debug) THEN printtocon, '% Read_DM3x_TagGroup: found Tag labelled ' + MyString(fullkeyname)
                    l=0
                END
            END
            l=l-1
        END
    END 
    ;; some debugging
    ;; IF (label EQ 'Indicated Magnification') then STOP
    ;;
    ;; ---------------------------------------------------------------------------------------------
    ;; analyse a tag with data (15h)
    ;; 4 x 8bit int: %%%%
    ;; 32bit (64bit) integer: number n of info integers following
    ;; n*32bit (64bit) integer: info integers
    ;; ---------------------------------------------------------------------------------------------
    If (tag EQ 'type') THEN  BEGIN ;; data follows immediately starting with '%%%%'

        readu, LUN, along32

        IF (along32 NE 623191333) THEN BEGIN ;; 623191333 = 25252525hex = %%%%
            printtocon, '% Read_DM3x_TagGroup: unexpected result - did not find %%%% in TagType for tag '+MyString(label)
            return, err
        END

        dsize=0LL   ;; the size of the data block
        if keyword_set(version4) THEN BEGIN
           along=along64
        END ELSE BEGIN
           along=along32
        END
        ;; read number of info integers
        readu, LUN, along & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
        j=along
        if KEYWORD_SET(debug) then BEGIN
           if keyword_set(version4) THEN BEGIN
              PrintToLog, "number of 64bit integers describing the encoded types=" + MyString(j)
           END ELSE BEGIN
              PrintToLog, "number of 32bit integers describing the encoded types=" + MyString(j)
           END
        END
        minorindex=j*0 ;; counts the encoded items, match type of j
        i=j*0+1 ;; match type of i and j
        while (i LE j) DO BEGIN

            minorindex=minorindex+1;
            
            readu, LUN, along & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
            if KEYWORD_SET(debug) then PrintToLog, "TagType - encoded type #" + MyString(i) +"=" +MyString(along)
            atype=along

            CASE along OF
                15: BEGIN ;; STRUCTURE!
                    result=AnalyseDM3Structure(LUN, i, swapdescr, sizeofencoded, dsize, DEBUG=debug, VERSION4=version4)
                    if (result EQ err) THEN return, err
                END      
                18: BEGIN             ;; STRING
                    if KEYWORD_SET(debug) then PrintToLog, "          is a string"
                    readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
                    if KEYWORD_SET(debug) then PrintToLog, "          string length=" + MyString(along)
                    dsize=dsize+2*along
                END
                20: BEGIN        ;; ARRAY
                    if KEYWORD_SET(debug) then PrintToLog, "          is an array"
                    ;;
                    ;; read array data type in field 2
                    ;;
                    readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
                    atype=along
                    ;; if KEYWORD_SET(debug) then PrintToLog, "          array type=" +MyString(atype)

                    if (atype Gt 10) THEN BEGIN
                        ;; holy shit, Gatan people must be annoyed by
                        ;; their own programs
                        CASE atype OF
                            15: BEGIN ;; a structure, determine structure size
                                elength=0
                                result=AnalyseDM3Structure(LUN, i, swapdescr, sizeofencoded, $
                                                           elength, DEBUG=debug, VERSION4=version4)
                                if (result EQ err) THEN return, err
                            END
                            18: BEGIN
                                printtocon, " array of strings - not implemented yet"
                                return, err
                            END
                            20: BEGIN
                                printtocon, " array of arrays - not implemented yet"
                                return, err
                            END
                            ELSE:
                        ENDCASE 
                    END ELSE BEGIN
                        elength=sizeofencoded[atype]
                    END  

                    ;; read the array length from field 3
                    readu, LUN, along & i=i+1 & IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
                    if KEYWORD_SET(debug) then PrintToLog, "          array length=" +MyString(along)
                    alength=along
                    
                    IF (keyindex GE 0) THEN BEGIN 
                        IF (keys[keyindex].minor EQ minorindex) THEN BEGIN
                            keys[keyindex].enc=atype ;; array encoded type
                            keys[keyindex].elemlen=elength ;; length of a single element  
                            keys[keyindex].len=alength ;; number of array elements
                            keys[keyindex].pos=dsize
                            keys[keyindex].written=1
                            keys[keyindex].type=20
                        END
                    END
                    dsize=dsize+elength*alength                    
                    if KEYWORD_SET(debug) then BEGIN
                        PrintToLog, "          array encoded type         =" + MyString(atype)
                        PrintToLog, "          length of a single element =" + MyString(elength)
                        PrintToLog, "          number of elements         =" +MyString(alength)
                        PrintToLog, "          array length in bytes      =" +MyString(dsize)
                     END
                END 
                ELSE: BEGIN ;; a type other than array, structure or string, i.e a simple type

                    if (keyindex GE 0) THEN BEGIN
                      ;;
                      IF (keys[keyindex].minor EQ minorindex) THEN BEGIN
                        keys[keyindex].enc=along ;; array encoded type
                        keys[keyindex].elemlen=sizeofencoded[along] ;; length of a single element  
                        keys[keyindex].len=1 ;; number of array elements
                        keys[keyindex].pos=dsize
                        keys[keyindex].written=1
                        keys[keyindex].type=along
                     END
                   END
                    dsize=dsize+sizeofencoded[along]                    
                END
            END
            i=i+1 
        ENDWHILE 
        
        ;; now the data of dsize bytes should follow at exactly at this position
        ;; skip data if label 

        if KEYWORD_SET(debug) then PrintToLog, "TagData"
        fpos=0LL
        ;; POINT_LUN, LUN, fpos
        POINT_LUN, -LUN, fpos

        ;; store data position in key structure
        if (keyindex GE 0) THEN BEGIN
            keys[keyindex].pos=keys[keyindex].pos+fpos
        END
        
        ;; skip data
            
        if KEYWORD_SET(debug) then PrintToLog, "skipping from byte " + MyString(fpos)+ " to byte " + MyString((fpos+dsize))
        
        fpos=fpos+dsize
        POINT_LUN, LUN, fpos
        
    END  
    IF (tag EQ 'group') THEN BEGIN
        ;;
        if (keyindex GE 0) THEN BEGIN
            ;; the parser expected TagType and TagData
            ;; but the structure forks into
            ;; another TagGroup
            ;; The only thing we can do is to complain
            ;; and to store the current LUN position in the keylist
            ;; together with an invalid data type
            ;;if keyword_set(debug) THEN printtocon, "% Read_DM3x_TagGroup: oops, TagEntry " + MyString(keys[keyindex].name) + " forks into TagGroup" 
            ;;if keyword_set(debug) THEN printtocon, "%                     before providing data, will store LUN position in key stucture"
            keys[keyindex].enc=-1
            fpos=0LL
            POINT_LUN, -LUN, fpos
            keys[keyindex].pos=fpos
            keys[keyindex].written=1
        END
        ;;

        result=Read_DM3x_TagGroup(LUN, swapdescr, sizeofencoded, nestlevel, fullkeyname, counter, KEYS=keys, VERBOSE=verbose, DEBUG=debug, VERSION4=version4)

        if (result EQ err) THEN return, err
    END
 END  



nestlevel=nestlevel-1 ;; because variables are always passed by reference
counter=oldcounter
fullkeyname=oldfullkeyname
return, success
END  



Function GetTagData, LUN, taginfo, tagvalue
  IF (taginfo.len EQ 1) THEN BEGIN
     POINT_LUN, LUN, taginfo.pos
     CASE taginfo.type OF
        1: tagvalue=1B
        2: tagvalue=1
        3: tagvalue=1L
        4: tagvalue=1U
        5: tagvalue=1UL
        6: tagvalue=1.0
        7: tagvalue=1.0D
        8: tagvalue=1B
        9: tagvalue=1B
        11: tagvalue=1LL
        12: tagvalue=Complex(1.,0.)
        13: tagvalue=DComplex(1.,0.)
        18: BEGIN
           ;; unicode string 16bit/char
           tagvalue=STRARR(2)
        END
        23: tagvalue=1L
        ELSE: return, 0
     END
     readu, LUN, tagvalue
     return, 1
  END ELSE BEGIN
     POINT_LUN, LUN, taginfo.pos
     CASE taginfo.type OF
        1: tagvalue=BYTARR(tagvalue.len)
        2: tagvalue=INTARR(tagvalue.len)
        3: tagvalue=LONARR(tagvalue.len)
        4: tagvalue=UINTARR(tagvalue.len)
        5: tagvalue=LONARR(tagvalue.len)
        6: tagvalue=FLTARR(tagvalue.len)
        7: tagvalue=DBLARR(tagvalue.len)
        8: tagvalue=BYTARR(tagvalue.len)
        9: tagvalue=BYTARR(tagvalue.len)
        11: tagvalue=LON64ARR(tagvalue.len)
        12: tagvalue=ComplexArr(tagvalue.len)
        13: tagvalue=DComplexArr(tagvalue.len)
        18: BEGIN
           ;; unicode string 16bit/char
           tagvalue=STRARR(2*tagvalue.len)
        END
        23: tagvalue=LONARR(tagvalue.len)
        ELSE: return, 0
     END
     readu, LUN, tagvalue
     return, 1
  END
  return, 0
END
     
FUNCTION Read_DM3x, fname, VERBOSE=verbose, DEBUG=debug, FLIP=flip,  QUIET=quiet
;;
;; PURPOSE: read GATAN Digital Micrograph 3.x format
;;
;; KEYWORDS:  verbose - geschwaetzig mode
;;            debug   - even more verbose
;;            flip    - flip image
;;                      DM has (0,0) in the upper left, IDL in the
;;                      lower left
;;
;; FORMAT Description
;; (starting point for reading is http://www.stanford.edu/~jefferies/ImageJ/DM3Format.gj.html)
;; 
;; The DM3 format is a tag hierarchy format. 
;; There are no fixed lengths and each file has to be
;; parsed in time consuming search for the image data.
;; In straight words: a crappy format! 
;;
;; The tag hierarchy is preceeded by a 3 times 32bit (12 byte)
;; integer header:
;; 
;; Byte:  Type:           Value:   Comment:
;; 0-3    32 bit integer  3        DM Version
;; 4-7    32 bit integer  N        N=filesize in bytes - 16 
;;                                 (why minus 16? why at all?)
;; 8-11   32 bit integer  0 or 1   tag data byte order (different from
;;                                 descriptor if equal to 1) 
;;
;; The 12 byte header is followed by a tag hierarchy
;; starting with the top-level TagGroup and consisting of 
;;  
;; TagGroups, TagEntrys, TagTypes and TagData 
;;  
;; The tag hierarchy is best represented by a tree structure:
;;
;;                         TagGroup
;;                             |
;;    --------------------------------------------------------------
;;    |                       |                                    |
;; TagEntry 1             TagEntry 2        ...               TagEntry n
;;    |                       |                                    |
;; TagType                 TagGroup                               ...
;;    |                       |
;; TagData      ---------------------------------
;;              |              |                |
;;           TagEntry 1'    TagEntry 2' ...   TagEntry n'
;;              |              |                |
;;           TagGroup       TagType            ...
;;              |              |
;;         -------------    TagData
;;         |   | ...   |
;; 
;;             ...
;;
;; Version 3
;; 
;; TagGroup format
;; Byte:  Type:           Value:   Comment:
;; 0      8bit char       0 or 1   is the group sorted?
;; 1      8bit char       0 or 1   is the group open?
;; 2-5    32bit integer   N        number of tags in the group
;; 6-...                           TagEntry
;;
;; TagEntry format
;; Byte:  Type:           Value:      Comment:
;; 0      8bit char       14h or 15h  14h => A TagGroup will follow (recursion!) 
;;                                    15h => TagType and Tag Data will follow
;; 1-2    16bit integer   N           length of tag label in bytes, 
;;                                    Warning: label length 0 is
;;                                    allowed. This is complete non-sense:
;;                                    how do you find a tag labelled
;;                                    the empty string in text search?    
;; 3-N+2  char*N                      tag label
;; N+3-...                            TagType and TagData or TagGroup 
;; 
;; TagType format
;; Byte:  Type:           Value:      Comment:
;; 0-3    char*4          25252525h   equals '%%%%'
;; 4-7    32bit integer   j=1,2,3...  number of 32bit integers for the list of encoded types
;;                                    that follows in the data block
;;                                    integer, float, double ...  ->
;;                                    adds 1 (type)
;;                                    string -> adds 2 (string
;;                                    identifier=18 and
;;                                    number of characters)
;;                                    array -> adds 3 (array
;;                                    identifier=20, data type, array length
;;                                    data)
;;                                    structure -> adds 1+2*f if the stucture has
;;                                    f fields (structure
;;                                    identifier=15, then f times
;;                                    field name length and field type)
;; 8+j*4-1 32bit integer*j            List of encoded types per datum
;;                                    (see below) 
;; 8+j*4-...                          Tag Data
;;
;;
;; 
;; Encoded Types:
;; id:     type:
;; 2       signed 16bit integer
;; 3       signed 32bit integer
;; 4       unsigned 16bit integer
;; 5       unsigned 32bit integer
;; 6       32bit floating point number
;; 7       64bit floating point number
;; 8       8bit char (boolean)
;; 9       8bit char
;; 10      octet 
;; 15      struct
;; 18      string (size: 2*length because of 2 byte unicode encoding)
;; 20      array  (size: array length * size of array type)
;; 23      signed 32bit integer
;;
;; for structs, strings and arrays additional fields are needed for
;; the definition as declared above
;;
;; NOTES: Tag Descriptors are stored in big-endian order 
;;        (I do not rely on this information)
;;        Tag data is stored in the machines byte ordering  
;;        (I do not rely on this information)
;;        (sigh ...)
;;        
;; ALGORITHM: examines recursively a TagGroup Tree
;;            remembers TagData file positions for key labels
;;            while skipping over the TagData
;;            
;; KEY LABELS:
;; Data - the image data array, may occur several times for the image
;;        data and e.g. a pixmap
;;        (important: newer DM versions (3.4+ ?) contain an additional 
;;         key label "Image List" which contains -- no, not the number of 
;;         images -- the number of data arrays)
;;        the algorithm assumes that the biggest data array contains
;;        the image(s) (images because it can also be a 3d data cube!)
;; Dimensions - image dimensions (caution: TagEntry forks into
;;              TagGroup, not TagType and TagData! A foreseable future
;;              bug. Wonder when this changes and leads to
;;              incompatibility between Digital Micrograph versions. ) 
;;              (there is one Dimensions field for each data array)
;; DataType - image data data type id, the same as for DM 2.5
;;            see 'encoded types' above
;;
IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% Read_DM3x:        Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    return, hash()
 END
END
;;
err=1
h=hash()
success=0
;; data types that the parser knows  
KnownDataType=[1, 2, 3, 4, 5, 6, 7, 9, 10,12,13,23,11,14]
;; byte sizes for tag data types 02h-0bh
sizeofencoded=[0L,0L,2L,4L,2L,4L,4L,8L,1L,1L,1L,8L,8L,16L,1L]
DataKey=["Unknown", $ ;; 0 
         "Signed Integer (8bit)", $ ;; 1 
         "Signed Integer (16bit)", $ ;; 2
         "Signed Integer (32bit)", $ ;; 3
         "Unsigned Integer (16bit)", $ ;; 4
         "Unsigned Integer (32bit)", $ ;; 5
         "Floating Point (32bit)", $ ;; 6
         "Floating Point (64bit)", $ ;; 7
         "Char (boolean 8bit)",$ ;; 8
         "Char (8bit)",$ ;; 9
         "Octet", $  ;; 10
         "Integer (64bit)", $  ;; 11
         "Floating Point (Complex, 64bit)", $ ;; 12
         "Floating Point (Complex, 128bit)", $ ;; 13
         "Unknown", $  ;; 14
         "Structure", $ ;; 15
         "Unknown", $  ;; 16
         "Unknown", $ ;; 17
         "Unicode String (16bit/Char)", $ ;; 18
         "Unknown", $ ;; 19
         "Array", $  ;; 20
         "Unknown", $ ;; 21
         "Unknown", $  ;; 22
         "Signed Integer (32bit)",$ ;; 23
         "Unknown", $ ;; 24
         "Unknown", $  ;; 25
         "Unknown", $  ;; 26
         "Unknown", $  ;; 27
         "Unknown", $  ;; 28
         "Unknown", $  ;; 29
         "Unknown"]  ;; 30

;; get LUN and open file
comment=fname
openr, LUN, fname, ERROR = o_err, /GET_LUN
if (o_err NE 0 ) then begin
   printtocon, "% Read_DM3x: error while trying to open file " +fname
   printtocon, "             " + !ERR_STRING
    return, err
endif


;; need to swap endian in tag descriptors? 
swapdescr=0

;; ---------------------------------------
;; read header data
;; DM3: 12 bytes
;; DM4: 16 bytes
;; ---------------------------------------
fail=1B
;; try to find version 3 identifier first
dmformat=0L
nbytes=0L ;; for DM3
databyteorder=0L
POINT_LUN, LUN, 0 ;; rewind
readu, LUN, dmformat
readu, LUN, nbytes
readu, LUN, databyteorder

;; check version, return if version does not match 3 or 4
version=3 
IF (dmformat NE version) THEN BEGIN
    swapdescr=1
    dmformat=SWAP_ENDIAN(dmformat)
END
IF (dmformat NE version) THEN BEGIN
   IF NOT(keyword_set(quiet)) THEN printtocon, "% Read_DM3x: DM3 identifier missing."
END ELSE BEGIN
   fail=0B
   version4=0
END 
  
;; check for version4
if (fail EQ 1B) THEN BEGIN
dmformat=0L
nbytes=0LL ;; for DM 4
databyteorder=0L
POINT_LUN, LUN, 0 ;; rewind
readu, LUN, dmformat
readu, LUN, nbytes
readu, LUN, databyteorder
;; check version, return if version does not match 3 or 4
version=4
IF (dmformat NE version) THEN BEGIN
    swapdescr=1
    dmformat=SWAP_ENDIAN(dmformat)
END 
IF (dmformat NE version) THEN BEGIN
   IF NOT(keyword_set(quiet)) THEN BEGIN
      printtocon, "% Read_DM3x: DM4 identifier missing. "
   END
   END ELSE BEGIN
   fail=0B
   version4=1
END
END   

;; 
if (fail EQ 1B) THEN BEGIN
    printtocon, "% Read_DM3x: Failed reading " +comment +"."
   close, LUN
   free_lun, LUN
   return, err
END

;; report version 
IF NOT(keyword_set(quiet)) THEN BEGIN
    printtocon, '% Read_DM3x: Filename: ' + comment
    printtocon, '% Read_DM3x: GATAN Digital Micrograph Version ' + MyString(dmformat)
 END

If (swapdescr EQ 1) THEN BEGIN
    databyteorder=SWAP_ENDIAN(databyteorder)
    nbytes=SWAP_ENDIAN(nbytes)
END  


;; tags
;; root.ImageList.0.ImageTags.FocalSeries.ExposureTime
;; root.ImageList.0.ImageTags.FocalSeries.FocusIncrement
;; root.ImageList.0.ImageTags.FocalSeries.FocusSteps
;; root.ImageList.0.ImageTags.FocalSeries.TotalAcquisitionTime
;; root.ImageList.0.ImageTags.MicroscopeInfo.IndicatedMagnification
;;
;; now start the iteration
;; first search for the tag with name ImageList
;; and return the number of Data fields (called NImages here; however,
;; it is not the number of images, do not get confused as i did) 


NImages=0
;; use internal keys to memorize tag positions and tag descriptions
keys=REPLICATE({KeyStruct, name:'root.ImageList', minor:1, written:0, enc:0L, len:0LL, elemlen:0L, pos:0LL, type:0L},1)
nestlevel=0
fullkeyname='root'
counter=0
result=Read_DM3x_TagGroup(LUN, swapdescr, sizeofencoded, nestlevel, fullkeyname, counter, KEYS=keys, VERBOSE=verbose, DEBUG=debug,VERSION4=version4)
IF (result EQ success) THEN BEGIN
;;    printtocon, "% Read_DM3x: found ImageList tag entry"
    if keyword_set(debug) THEN printtocon, "                   ImageList group tag at byte " + MyString(keys[0].pos)
    ;; jump to the position of the tag group
    POINT_LUN, LUN, keys[0].pos
    ;; read the tag group data to get the number of images
    abyte=0B
    along32=0L
    along64=0LL
    ashort=0S
    nentry=0L
    
    readu, LUN, abyte
    readu, LUN, abyte
    if keyword_set(version4) THEN along=along64 ELSE along=along32
    readu, LUN, along
    IF (swapdescr EQ 1) then along=SWAP_ENDIAN(along)
    NImages=along
    IF NOT(keyword_set(quiet)) THEN BEGIN
       printtocon, "% Read_DM3x: number of images in the file =" + MyString(NImages)
    END
    ;; step back again
    POINT_LUN, LUN, keys[0].pos
    ;; decode the Imagelist tag
    NSearchtags=35
    NKeys=NImages*NSearchtags
    keys=REPLICATE({KeyStruct, name:'Data', minor:1, written:0, enc:0L, len:0LL, elemlen:0L, pos:0LL, type:0L},NKeys)
    i=0
    c=0
    ;; TAG Definition - What to look for
    While (i LT NKeys) DO BEGIN 
        keys[i+0].name='root.ImageList.'+MyString(c)+'.ImageData.Data'
        keys[i+1].name='root.ImageList.'+MyString(c)+'.ImageData.DataType'
        keys[i+2].name='root.ImageList.'+MyString(c)+'.ImageData.Dimensions.0'
        keys[i+3].name='root.ImageList.'+MyString(c)+'.ImageData.Dimensions.1'
        keys[i+4].name='root.ImageList.'+MyString(c)+'.ImageData.PixelDepth'
        keys[i+5].name='root.ImageList.'+MyString(c)+'.ImageTags.MicroscopeInfo.IndicatedMagnification'
;;         root.ImageList.0.ImageTags.MicroscopeInfo.IndicatedMagnification
        keys[i+6].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.0.Scale'
        keys[i+7].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.1.Scale'
        keys[i+8].name='root.ImageList.'+MyString(c)+'.ImageTags.MicroscopeInfo.Voltage'
        keys[i+9].name='root.ImageList.'+MyString(c)+'.ImageTags.DataBar.AcquisitionDate' 
        keys[i+10].name='root.ImageList.'+MyString(c)+'.ImageTags.DataBar.AcquisitionTime'
        ;; for 3D arrays!
        keys[i+11].name='root.ImageList.'+MyString(c)+'.ImageData.Dimensions.2' 
        keys[i+12].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.2.Scale'
        keys[i+13].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.ExposureTime'
        keys[i+14].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.FocusIncrement'
        keys[i+15].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.FocusSteps'
        keys[i+16].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.TotalAcquisitionTime'
        ;; for scale units
        keys[i+17].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.0.Units'
        keys[i+18].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.1.Units'
        keys[i+19].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.2.Units'
        ;; for EFTEM
        keys[i+20].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.ExposureTime'
        keys[i+21].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.DT Increment'
        keys[i+22].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.DT Steps'
        keys[i+23].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.DT Voltage'
        ;; for camera details
        keys[i+24].name='root.ImageList.'+MyString(c)+'.ImageTags.Acquisition.Device.CCD.PixelSize(um)'
        keys[i+25].name='root.ImageList.'+MyString(c)+'.ImageTags.Acquisition.Device.Name'
        keys[i+26].name='root.ImageList.'+MyString(c)+'.ImageTags.DataBar.ExposureTime(s)'
        ;; for EELS details
        keys[i+27].name='root.ImageList.'+MyString(c)+'.ImageTags.EELSSpectrometer.Drifttubevoltage(V)'
        keys[i+28].name='root.ImageList.'+MyString(c)+'.ImageTags.EELSSpectrometer.Energyloss(eV)'
        keys[i+29].name='root.ImageList.'+MyString(c)+'.ImageTags.EELSSpectrometer.HToffset(V)'
        keys[i+30].name='root.ImageList.'+MyString(c)+'.ImageTags.EELSSpectrometer.Prismoffset(V)'
        keys[i+31].name='root.ImageList.'+MyString(c)+'.ImageTags.EELSSpectrometer.Slitwidth(eV)'
        ;;Pixel Size (um)
        keys[i+32].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.0.Origin'
        keys[i+33].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.1.Origin'
        keys[i+34].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.2.Origin'
        i=i+NSearchtags
        c=c+1
    END 
    ;; STOP
    ;; start parsing tags from th top level TagGroup
    ;; read tag start
    
    nestlevel=0
    fullkeyname='root.ImageList'
    counter=0
    result=Read_DM3x_TagGroup(LUN, swapdescr, sizeofencoded, nestlevel, fullkeyname, counter, KEYS=keys, VERBOSE=verbose, DEBUG=debug, VERSION4=version4)
    IF (result EQ success) THEN BEGIN
    END ELSE BEGIN
        printtocon, "% Read_DM3x: fatal parser error" + MyString(type)
        close, LUN
        free_lun, LUN
        return, err
    END   
END ELSE BEGIN
    printtocon, "% Read_DM3x: fatal error - did not find ImageList tag"
    close, LUN
    free_lun, LUN
    return, err
END  

;; find the right Data index
;; do not get tricked by the pixmap
;; find the maximum size data array
cmp=0
maxsize=0
i=1
While (i LT NImages) DO BEGIN 
    ;; cycle through image descriptor keys
    i=i+1 & cmp=cmp+4 
    if (keys[maxsize].written EQ 1) THEN BEGIN
        if (keys[cmp].written EQ 1) THEN BEGIN
            if ((keys[cmp].len*keys[cmp].elemlen) GE  (keys[maxsize].len*keys[maxsize].elemlen)) THEN BEGIN
                maxsize=cmp 
            END   
        END    
    END     
    
END 
   
realdata=0 & if (keys[maxsize].written EQ 1) THEN realdata=maxsize


;; 

IF (realdata LT 0) THEN BEGIN
    printtocon, "% Read_DM3x: fatal error - Data tag not found "+ MyString(type)
    close, LUN
    free_lun, LUN
    return, err
 END  ELSE BEGIN
;;    printtocon, "% Read_DM3x: found "+MyString(NImages) +"images!"
;;    printtocon, "%            the maximum size image is hidden in image number "+MyString(realdata)+"!"
 END






;; get the data type
;; and check for tag data endian order
;;
along=0L
POINT_LUN, LUN, keys[realdata*NSearchtags+1].pos
readu, LUN, along

swapendian=0


dtype=along

IF NOT(IN(dtype, KnownDataType)) THEN BEGIN
  swapendian=1
  dtype=SWAP_ENDIAN(dtype)
END 

IF NOT(IN(dtype, KnownDataType)) THEN BEGIN
    printtocon, "% Read_DM3x: fatal error - invalid data type " + MyString(dtype)
    close, LUN
    free_lun, LUN
    return, err
END 


;; for debugging
;; for i=0,NKeys do PrintToLog, MyString(keys[i])


arrsize=keys[realdata].len*keys[realdata].elemlen ;; image size in bytes
ipos=keys[realdata].pos                           ;; file pos
;; printtocon, "% Read_DM3x: Image file position, size ="+ MyString(ipos)+ ", "+ MyString((*p).Sz) 
;; now get the image dimensions
;;
;; This is a rather inconvenient hack, because the Dimensions TagEntry 
;; forks into a TagGroup, so we have to start from the TagGroup again
;; to find our data for the Dimensions. 
;;
DimX=ULONG(0L) & DimY=ULONG(0L) & DimZ=ULONG(0L)
xsamp=FLOAT(0.0) & ysamp=FLOAT(0.0) & zsamp=FLOAT(0.0)
;;
j=0
; Is the array three-dimensional?
if (keys[realdata*NSearchtags+11].pos NE 0) THEN BEGIN
   NDim=3
   ;; somehow: dimension.2 = X 
   ;; somehow: dimension.1 = Y
   ;; somehow: dimension.0 = Z  
   POINT_LUN, LUN, keys[realdata*NSearchtags+2].pos  ;; Dimension.0
   readu, LUN, DimZ
   if (swapendian EQ 1) THEN DimZ=SWAP_ENDIAN(DimZ) 
   POINT_LUN, LUN, keys[realdata*NSearchtags+3].pos  ;; Dimension.1
   readu, LUN, DimY
   if (swapendian EQ 1) THEN DimY=SWAP_ENDIAN(DimY) 
   POINT_LUN, LUN, keys[realdata*NSearchtags+11].pos  ;; Dimension.2
   readu, LUN, DimX
   if (swapendian EQ 1) THEN DimX=SWAP_ENDIAN(DimX)
   ;; now the sampling data
   IF (keys[j*NSearchtags+6].len GE 1) THEN BEGIN
      POINT_LUN, LUN, keys[j*NSearchtags+6].pos ;; Dimension.0.Scale
      readu, LUN, zsamp
      if (swapendian EQ 1) THEN zsamp=SWAP_ENDIAN(zsamp)
      POINT_LUN, LUN, keys[j*NSearchtags+7].pos  ;; Dimension.1.Scale
      readu, LUN, ysamp
      if (swapendian EQ 1) THEN ysamp=SWAP_ENDIAN(ysamp)
      POINT_LUN, LUN, keys[j*NSearchtags+12].pos  ;; Dimension.2.Scale
      readu, LUN, xsamp
      if (swapendian EQ 1) THEN xsamp=SWAP_ENDIAN(xsamp)
      ;;
   END
END ELSE BEGIN
;; try with 2 dimensions
   NDim=2
   DimZ=1
   ;; somehow: dimension.1 = X
   ;; somehow: dimension.0 = Y  
   DimY=ULONG(0L)
   POINT_LUN, LUN, keys[realdata*NSearchtags+2].pos
   readu, LUN, DimY
   if (swapendian EQ 1) THEN DimY=SWAP_ENDIAN(DimY) 
   DimX=ULONG(0L)
   POINT_LUN, LUN, keys[realdata*NSearchtags+3].pos
   readu, LUN, DimX
   if (swapendian EQ 1) THEN DimX=SWAP_ENDIAN(DimX) 
   ;; now the sampling data
   zsamp=1.
   IF (keys[j*NSearchtags+6].len GE 1) THEN BEGIN
      POINT_LUN, LUN, keys[j*NSearchtags+6].pos ;; Dimension.0.Scale
      readu, LUN, xsamp
      if (swapendian EQ 1) THEN xsamp=SWAP_ENDIAN(xsamp)
      POINT_LUN, LUN, keys[j*NSearchtags+7].pos  ;; Dimension.1.Scale
      readu, LUN, ysamp
      if (swapendian EQ 1) THEN ysamp=SWAP_ENDIAN(ysamp)
      ;;
   END
END

;;
xdim=FIX(DimX) & ydim=DimY
;;
;; array length
alength=DimX*DimY*DimZ*keys[realdata].elemlen
;;
if NOT((xdim GE 1)  AND (ydim GE 1) AND (DimZ GE 1)) THEN BEGIN
   if ((xdim EQ 0)  AND (ydim GE 1) AND (DimZ EQ 1 )) THEN BEGIN
      s="% Read_DM3x: Detected spectrum data. "
      printtocon, s
      ;; err=Read_DM3_Spectra(fname, LUN, keys, NSearchtags, dmformat, databyteorder, swapdescr, swapendian, VERBOSE=verbose, DEBUG=debug)
      close, LUN
      free_lun, LUN
      return, hash()  ;; return error for image loading, needed for cleaning up p properly
   END ELSE BEGIN
   s="% Read_DM3x: Invalid image dimension - x="+ MyString(xdim)+ ", y="+ MyString(ydim)
   if (NDim EQ 3) THEN s=s+ ", z=" + MyString(DimZ)
    printtocon, s
    close, LUN
    free_lun, LUN
    return, hash()
 END 
END 



;; get the magnification for the image realdata
;;
;; printtocon, '% Read_DM3x: Magnification'
;; seems to be stored in image 0
magnification='not found'
j=0
IF (keys[j*NSearchtags+5].len EQ 1) THEN BEGIN
   IF (keys[j*NSearchtags+5].type EQ 7) THEN tmp=DOUBLE(0.0) ELSE tmp=FLOAT(0.0)
   POINT_LUN, LUN, keys[j*NSearchtags+5].pos
   readu, LUN, tmp
   if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
   magnification=MyString(tmp)
;;   printtocon, '%            '+ MyString(afloat)
END ELSE BEGIN
;;   printtocon, '%            info not found'
END 



;; get the voltage for the images
;;
;;printtocon, '% Read_DM3x: Microscope Voltage'
voltage=0
j=0
;; seems to be stored in image 0
IF (keys[j*NSearchtags+8].len GE 1) THEN BEGIN
IF (keys[j*NSearchtags+8].type EQ 7) THEN tmp=DOUBLE(0.0) ELSE tmp=FLOAT(0.0)
   POINT_LUN, LUN, keys[j*NSearchtags+8].pos
   readu, LUN, tmp
   if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
   voltage=tmp
;;   printtocon, '%            ' + MyString(afloat)
END ELSE BEGIN
;;   printtocon, '%            info not found'
END 


;; get the acquisition date
;;
;;printtocon, '% Read_DM3x: Acquisition Date'
j=0
;;STOP
acqdate=['','','']
IF (keys[j*NSearchtags+9].len GE 1) THEN BEGIN
   auint=UINT(0)
   POINT_LUN, LUN, keys[j*NSearchtags+9].pos

   ;; day= acqdate[1], month=acqdate[0], year=acqdate[2]
   ind=0
   FOR k=1,(keys[j*NSearchtags+9].len) DO BEGIN
      readu, LUN, auint
      if (swapendian EQ 1) THEN auint=SWAP_ENDIAN(auint)
      if (auint EQ 47) THEN BEGIN
         if (ind LT 2) THEN ind=ind+1
      END ELSE BEGIN
         acqdate[ind]=acqdate[ind]+String(Byte(auint))
      END  
   END 
;;   printtocon, '%           ' + acqdate
END  ELSE BEGIN
;;   printtocon, '%            info not found'
END    

;; get the acquisition time
;;
;;printtocon, '% Read_DM3x: Acquisition Time'
j=0
   acqtime=''
IF (keys[j*NSearchtags+10].len GE 1) THEN BEGIN
   auint=UINT(0)
   POINT_LUN, LUN, keys[j*NSearchtags+10].pos

   FOR k=1,(keys[j*NSearchtags+10].len) DO BEGIN
      readu, LUN, auint
      if (swapendian EQ 1) THEN auint=SWAP_ENDIAN(auint)
      if (auint EQ 47) THEN BEGIN
         acqtime=acqtime+":"
      END ELSE BEGIN
      acqtime=acqtime+String(Byte(auint))
      END
   END
END ELSE BEGIN
;;   printtocon, '%            info not found'
END 

;;printtocon, '% Read_DM3x: Device Name'
;; j=0
    device=''
 IF (keys[j*NSearchtags+25].len GE 1) THEN BEGIN
    auint=UINT(0)
    POINT_LUN, LUN, keys[j*NSearchtags+25].pos

    FOR k=1,(keys[j*NSearchtags+25].len) DO BEGIN
       readu, LUN, auint
       if (swapendian EQ 1) THEN auint=SWAP_ENDIAN(auint)
       device=device+String(Byte(auint))
    END
 END ELSE BEGIN
    ;;   printtocon, '%            info not found'
END 

 

; print some information
if KEYWORD_SET(debug) then begin
;;      	printtocon, '% Read_DM3x: Filename: ' + (*p).comment

        IF swapendian THEN se='yes' else se='no'
        printtocon, "%            Swapping Data Endian = " + se 
        IF swapdescr THEN se='yes' else se='no'
        printtocon, "%            Swapping Tag Descriptor Endian = " + se 
    printtocon, "% Read_DM3x: key structure data is"
    printtocon, "             name    - " + MyString(keys[realdata].name)
    printtocon, "             minor   - " + MyString(keys[realdata].minor)
    printtocon, "             written - " + MyString(keys[realdata].written)
    printtocon, "             type    - " + MyString(keys[realdata].type)    ;; data type (20=array)
    printtocon, "             enc     - " + MyString(keys[realdata].enc)     ;; simple data type
    printtocon, "             len     - " + MyString(keys[realdata].len)     ;; array length
    printtocon, "             elemlen - " + MyString(keys[realdata].elemlen) ;; length of single element in Byte
    printtocon, "             pos     - " + MyString(keys[realdata].pos)     ;; Byte pos of data 
END




;; determine data type
case dtype of
;;    9 : datatype=1
;;    1 : datatype=2
;;    7 : datatype=3
;;    2 : datatype=4
;;    12 : datatype=5
;;    3 : datatype=6
;;    13 : datatype=9
;;    4 : datatype=12
;;    5 : datatype=13
;;    11 : datatype=14
;;    10 : datatype=2 ;; not sure here!
    9 : datatype=1
    1 : datatype=2
    7 : datatype=3
    2 : datatype=4
    12 : datatype=5
    3 : datatype=6
    13 : datatype=9
    4 : datatype=12 ;; not sure here!
    5 : datatype=13
    11 : datatype=13
    10 : datatype=12
    6 : datatype=1  
    
    else: BEGIN
        printtocon, "% Read_DM3x: invalid or unsupported data type " + MyString(dtype)
        close, LUN
        free_lun, LUN
        return, err
    END
endcase
;; STOP
IF NOT(keyword_set(quiet)) THEN BEGIN
   printtocon, '% Read_DM3x: Data Array Found at Byte: '+ MyString(keys[realdata].pos)
   printtocon, '%            Array Dimensions: ' + MyString(NDim)
   IF (NDim EQ 2) THEN printtocon, '%            Array format: '+ MyString(xdim)+ "x"+ MyString(ydim)
   IF (NDim EQ 3) THEN printtocon, '%            Array format: '+ MyString(xdim)+ "x"+ MyString(ydim)+"x"+MyString(DimZ)
   printtocon, '%            Array Size in Bytes: ' + MyString(arrsize)
   printtocon, '%            DM Array Data Type: ' + MyString(dtype)+"->"+DataKey[datatype]
END

;; for scale units
    ;;    keys[i+17].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.0.Unit'
    ;;    keys[i+18].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.1.Unit'
    ;;    keys[i+19].name='root.ImageList.'+MyString(c)+'.ImageData.Calibrations.Dimension.2.Unit'
    ;;
    j=0
    units='' & samp=xsamp
    IF (keys[j*NSearchtags+17].len GE 1) THEN BEGIN
       tmp=0U
       POINT_LUN, LUN, keys[j*NSearchtags+17].pos ;; '.ImageData.Calibrations.Dimension.0.Unit'
       ;; it is an array of keys[j*NSearchtags+17].len unsigned 16bit
       ;; integers (UTF-16?)
      
       FOR ii=1, keys[j*NSearchtags+17].len DO BEGIN
          readu, LUN, tmp ;; read unsigned int
          if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
          units=units+string(tmp,FORM='(Z04)') ;; built hex string
                                         
       END
      
       CASE units OF
          '003F': BEGIN
             ;; units='Angstrom'
             ;; printtocon, '%    Calibration Units    - '+units
             units="nm"
             samp=samp*0.1
          END
          '006E006D': BEGIN
             units = 'nm'
            ;; printtocon, '%    Calibration Units    - '+units
          END
          '00B5006D': BEGIN
             units = 'um'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          '0031002F003F': BEGIN
             ;; units='1/Angstrom'
             ;; printtocon, '%    Calibration Units    - '+units
             units='1/nm'
             samp=samp*10.
          END
          '0031002F006E006D': BEGIN
             units='1/nm'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          '0031002F00B5006D': BEGIN
             units='1/um'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          ELSE: units=""
       END
    END
    xsamp=samp & xunit=units
    units='' & samp=ysamp
    IF (keys[j*NSearchtags+18].len GE 1) THEN BEGIN
       tmp=0U
       POINT_LUN, LUN, keys[j*NSearchtags+18].pos ;; '.ImageData.Calibrations.Dimension.1.Unit'
       ;; it is an array of keys[j*NSearchtags+17].len unsigned 16bit
       ;; integers (UTF-16?)
      
       FOR ii=1, keys[j*NSearchtags+18].len DO BEGIN
          readu, LUN, tmp ;; read unsigned int
          if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
          units=units+string(tmp,FORM='(Z04)') ;; built hex string
                                         
       END
      
       CASE units OF
          '003F': BEGIN
             ;; units='Angstrom'
             ;; printtocon, '%    Calibration Units    - '+units
             units="nm"
             samp=samp*0.1
          END
          '006E006D': BEGIN
             units = 'nm'
            ;; printtocon, '%    Calibration Units    - '+units
          END
          '00B5006D': BEGIN
             units = 'um'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          '0031002F003F': BEGIN
             ;; units='1/Angstrom'
             ;; printtocon, '%    Calibration Units    - '+units
             units='1/nm'
             samp=samp*10.
          END
          '0031002F006E006D': BEGIN
             units='1/nm'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          '0031002F00B5006D': BEGIN
             units='1/um'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          ELSE: units=""
       END
    END
    ysamp=samp & yunit=units
    units='' & samp=zsamp
    
    IF (keys[j*NSearchtags+19].len GE 1) THEN BEGIN
       tmp=0U
       POINT_LUN, LUN, keys[j*NSearchtags+19].pos ;; '.ImageData.Calibrations.Dimension.0.Unit'
       ;; it is an array of keys[j*NSearchtags+19].len unsigned 16bit
       ;; integers (UTF-16?)
      
       FOR ii=1, keys[j*NSearchtags+17].len DO BEGIN
          readu, LUN, tmp ;; read unsigned int
          if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
          units=units+string(tmp,FORM='(Z04)') ;; built hex string
                                         
       END
      
       CASE units OF
          '003F': BEGIN
             ;; units='Angstrom'
             ;; printtocon, '%    Calibration Units    - '+units
             units="nm"
             samp=samp*0.1
          END
          '006E006D': BEGIN
             units = 'nm'
            ;; printtocon, '%    Calibration Units    - '+units
          END
          '00B5006D': BEGIN
             units = 'um'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          '0031002F003F': BEGIN
             ;; units='1/Angstrom'
             ;; printtocon, '%    Calibration Units    - '+units
             units='1/nm'
             samp=samp*10.
          END
          '0031002F006E006D': BEGIN
             units='1/nm'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          '0031002F00B5006D': BEGIN
             units='1/um'
             ;; printtocon, '%    Calibration Units    - '+units
          END
          ELSE: units=""
       END
    END
    zsamp=samp & zunit=units
   ;; STOP

    tagvalue=0
    xoffs=0. & yoffs=0. & zoffs=0.
    IF GetTagData(LUN,keys[28],tagvalue) then begin
       zoffs=tagvalue ;; energy loss
       IF GetTagData(LUN,keys[31],tagvalue) then zsamp=tagvalue ;; slit width
       zunit="eV"
    end

    exposure=0. 
    IF GetTagData(LUN,keys[26],tagvalue) then begin
       exposure=tagvalue ;; exposure time
    end

;; check for image size and data size consistency


;;if (alength EQ keys[realdata].len) THEN BEGIN
IF (NDim EQ 2) THEN BEGIN
;; a single image
   NIm=1
   h["nimages"]=NIm
   SplitPath, fname, path, fn
   h["filename"]=fn
   h["path"]=path
   h["SzX"]=xdim
   h["SzY"]=ydim 
   h["type"]=datatype
   h["Sz"]=arrsize
   h["voltage"]=voltage
   h["device"]=device
   h["maglabel"]=magnification
   h["exposure"]=exposure
   h["samplingx"]=xsamp
   h["samplingy"]=ysamp
   h["unitx"]=xunit
   h["unity"]=yunit
;; convert dtype to generic data type and allocate memory 
    case datatype of
        1 : BEGIN
            h["data"]=PTR_NEW(BYTARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, BYTARR(h["SzX"], h["SzY"]), ipos)	
        END
        2 : BEGIN
            h["data"]=PTR_NEW(INTARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, INTARR(h["SzX"],h["SzY"]), ipos)	
        END
        3 : BEGIN
            h["data"]=PTR_NEW(LONARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, LONARR(h["SzX"],h["SzY"]), ipos)	
        END
        4 : BEGIN
            h["data"]=PTR_NEW(FLTARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, FLTARR(h["SzX"],h["SzY"]), ipos)	
        END
        5 : BEGIN
            h["data"]=PTR_NEW(DBLARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, DBLARR(h["SzX"],h["SzY"]), ipos)	
        END
        6 : BEGIN
            h["data"]=PTR_NEW(COMPLEXARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, COMPLEXARR(h["SzX"],h["SzY"]), ipos)	
        END
        9 : BEGIN
            h["data"]=PTR_NEW(DCOMPLEXARR(h["SzX"],h["SzY"]))
            i_data = ASSOC(LUN, DCOMPLEXARR(h["SzX"],h["SzY"]), ipos)	
        END
        12: BEGIN
             h["data"]=PTR_NEW(UINTARR(h["SzX"],h["SzY"]))
             i_data = ASSOC(LUN, UINTARR(h["SzX"],h["SzY"]), ipos)	
         END
        13: BEGIN
             h["data"]=PTR_NEW(ULONARR(h["SzX"],h["SzY"]))
             i_data = ASSOC(LUN, ULONARR(h["SzX"],h["SzY"]), ipos)	
         END
        else: BEGIN
            printtocon, "% Read_DM3x: invalid or unsupported data type " + MyString(type)
            close, LUN
            free_lun, LUN
            return, err
        END
    endcase

;transfer data from file to memory
    if (swapendian EQ 1) then BEGIN
        *(h["data"]) = SWAP_ENDIAN(i_data(0))
    ENDIF ELSE BEGIN
        *(h["data"]) = i_data(0)
    ENDELSE

    IF keyword_set(flip) THEN BEGIN
       *(h["data"])=REVERSE((*(*p).im), 2)
    END

 END ;; 2D array 

DimVec=[DimX,DimY,DimZ]
SampVec=[xsamp,ysamp,zsamp]
UnitVec=[xunit,yunit,zunit]
 
if (NDim EQ 3) THEN BEGIN
 printtocon, ["% ReadDigMGraph: 3D data import not yet supported in PadTools."]
END          




; print some information
if KEYWORD_SET(verbose) then begin
    printtocon, '% Read_DM3x: ' + comment
;;    printtocon, '% Read_DM3x: Image Data Found at Byte: ' +  MyString(keys[realdata].pos)
    printtocon, '%    Data Type           - ' + type_spec(datatype)
    IF (SIZE(DimVec, /N_Elements) EQ 3) THEN BEGIN
       printtocon, '%    Dimensions          - ' + MyString(DimVec[0]) + "x" + MyString(DimVec[1]) + "x" + MyString(DimVec[2]) 
    END ELSE BEGIN
       printtocon, '%    Dimensions          - ' + MyString(DimVec[0]) + "x" + MyString(DimVec[1])
    END
    printtocon, '%    Sampling            - ' + MyString(SampVec[0]) + " x " + MyString(SampVec[1])
    UnitStr=xunit+" x "+yunit
    IF (NDim GT 2) THEN UnitStr=UnitStr+" x "+zunit
     printtocon, '%    Calibration Units   - '+ UnitStr
;;    printtocon, '%            Number of Images:         ' + MyString(NIm)
;;    printtocon, '%            Array Size in Bytes:      ' + MyString(keys[realdata].len)
    printtocon, '%    Magnification Label - ' + magnification
    printtocon, '%    Voltage             - ' + MyString(voltage)
    printtocon, '%    Acq. Date & Time    - ' + acqdate[1] + '.' + acqdate[0] + '.'+acqdate[2] + ' ' + acqtime
    printtocon, '%    Device              - ' + device
;;  Focal Series Parameters    
    j=0
    IF (keys[j*NSearchtags+14].len GE 1) THEN BEGIN
       tmp=0.0
       POINT_LUN, LUN, keys[j*NSearchtags+14].pos ;; ImageTags.FocalSeries.FocusIncrement
       readu, LUN, tmp
       if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
       printtocon, '%    Focal Series Focus Increment (nm)       - '+MyString(tmp) 
    END
    IF (keys[j*NSearchtags+15].len GE 1) THEN BEGIN
       tmp=0
       POINT_LUN, LUN, keys[j*NSearchtags+15].pos ;; ImageTags.FocalSeries.FocusSteps
       readu, LUN, tmp
       if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
       printtocon, '%    Focal Series Number of Steps            - '+MyString(tmp) 
    END
    IF (keys[j*NSearchtags+13].len GE 1) THEN BEGIN
       tmp=0.0
       POINT_LUN, LUN, keys[j*NSearchtags+13].pos ;; .ImageTags.FocalSeries.ExposureTime
       readu, LUN, tmp
       if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
       printtocon, '%    Focal Series Exposure Time (s)          - '+MyString(tmp) 
    END
    IF (keys[j*NSearchtags+16].len GE 1) THEN BEGIN
       tmp=0.0
       POINT_LUN, LUN, keys[j*NSearchtags+16].pos ;; .ImageTags.FocalSeries.ExposureTime
       readu, LUN, tmp
       if (swapendian EQ 1) THEN tmp=SWAP_ENDIAN(tmp)
       printtocon, '%    Focal Series Total Acquisition Time (s) - '+MyString(tmp) 
    END
    
      ;; keys[i+14].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.FocusIncrement'
    ;; keys[i+15].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.FocusSteps'
    ;; keys[i+16].name='root.ImageList.'+MyString(c)+'.ImageTags.FocalSeries.TotalAcquisitionTime'
   
    ;; for EFTEM
    ;;    keys[i+20].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.ExposureTime'
    ;;    keys[i+21].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.DT Increment'
    ;;    keys[i+22].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.DT Steps'
    ;;    keys[i+23].name='root.ImageList.'+MyString(c)+'.ImageTags.DT Series.DT Voltage'
;;    IF swapendian THEN se='yes' else se='no'
;;    printtocon, "%            Swap Data Endian:         " + se 
;;    IF swapdescr THEN se='yes' else se='no'
;;    printtocon, "%            Swap Tag Descr. Endian:   " + se 
    ;;
    ;; output for the data loaded
    ;;IF keyword_set(slices) OR keyword_set(binning) THEN BEGIN
    ;;   IF (SIZE(DimVec, /N_Elements) EQ 3) THEN BEGIN
    ;;      printtocon, '%    Imported Slices     - ' + MyString(u0) + "-" + MyString(u1)
    ;;      printtocon, '%    Binning             - ' + MyString(b0) + "x" + MyString(b1) + "x" + MyString(b2)
    ;;      printtocon, '%    Dimensions (binned) - ' + MyString((*p).SzX) + "x" + MyString((*p).SzY) + "x" + MyString((*p).count) 
    ;;END ELSE BEGIN
    ;;   printtocon, '%    Dimensions (binned) - ' + MyString((*p).SzX) + "x" + MyString((*p).SzY)
    ;;END
    ;;END
END  




;close file
close, LUN
free_lun, LUN


if (result EQ err) THEN BEGIN
    printtocon, "% Read_DM3x: error, probably image dimensions inconsistent - SzX*SzY " + MyString(alength) + ", array len="+ MyString(keys[realdata].len)    
    return, hash() 
END ELSE return, h
END     
     
     
     
;; ++-------------------- Documentation
;;
;; Calibration units
;; +++ start of tag entry 7.1
;;tag is a data tag
;;tag label length=5
;;tag label=root.ImageList.1.ImageData.Calibrations.Dimension.1.Units
;;number of 32bit integers describing the encoded types=3
;;TagType - encoded type #1=20
;;          is an array
;;          array length=0
;;          array encoded type         =4
;;          length of a single element =2
;;          number of elements         =0
;;          array length in bytes      =0
;;TagData
;;skipping from byte 4987 to byte 4987


