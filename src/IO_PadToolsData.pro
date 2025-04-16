FUNCTION Write_ptd, d, file
  ;; file = file name
  ;; ending should be ptd
  IF NOT(GetDebugFlag()) THEN BEGIN
     CATCH, Error_status
     IF (Error_status NE 0) THEN BEGIN
        Print, "% Write_ptd:  Fatal error "
        Print, "%   Error status  - " + STRING(error_status)
        Print, "%   Error message - " + !ERROR_STATE.MSG
        CATCH, /Cancel
        ErrMsg, !ERROR_STATE.MSG
        return, 0
     END
  END

  IF (FileIsWriteable(file,NONINTERACTIVE=noninteractive) NE 1) THEN BEGIN
     printtocon, "% Write_ptd: file "+ file + " is not writeable"
     return, 0
  END
  IF (NOT(PTR_VALID(d))) THEN BEGIN
     print, "% Write_ptd: Current image list pointer is invalid."
     return, 0
  END
  p=(*d).datap
  IF (NOT(PTR_VALID(p))) THEN BEGIN
     print, "% Write_ptd: Current data pointer is invalid."
     return, 0
  END
  ;; we first create a file identifier, since all communication with
  ;; the file is done via the file identifier.
  ;;STOP
  ;; Version identifier, structure to write into File
  ;; creator={creator:'PadTools',version:'1.8'}
  metadata=GetHDF5ImageDescriptor(p)
  ArraySz=SIZE(*(*p).data)
  type={type:'2D_SLICE_SEQUENCE',ndimensions:ArraySz[0],dimensions:ArraySZ[1:ArraySz[0]],id:'array',datatype:FIX((*p).type),nbytes:ArraySz[-1], rawdata:''}
  e=(*p).extra
  IF OBJ_VALID((e)) THEN BEGIN ;; it is a 4D data retrieve subset
     metadata.ispaddata=1B
     if metadata.ISSUBSET THEN BEGIN ;; should have subset
        subset=1
        IF (e->GetSubset(SUBSET=subset) EQ 1) THEN BEGIN
           ;; data is now in subset
        END ELSE metadata.issubset=0B        
     END
  END ELSE metadata.ispaddata=0B
  ;; update filename  
  ;; we split off the file path for relative folder references
  ;; the header and subset info will be written to a file with the suffix .ptd
  ;; the data array will be written to an ordinary MRC file
  Splitpath, file, datapath, datafilename
  metadata.name=datafilename
  metadata.pathinfo=datapath
  ;; split suffix from filename
  SplitFileName, datafilename, basefilename
  type.rawdata=basefilename+".mrc"
  ;; 
  ;; 
  ;; create hash and save to json
   metahash=orderedhash()
   ;; metahash["creator"]=creator
   metahash["Creator"]="PadTools"
   metahash["Version"]=STRING(GetVersion(/ASFLOAT), FORMAT='(F3.1)')
   metahash["Metadata"]=metadata
   metahash["Data"]=type
   if OBJ_VALID((*p).Note) THEN metahash["Notes"]=(*p).Note->ToStringArray() 
   if metadata.ISSUBSET THEN metahash["Subset"]=subset
   ;;STOP
   openw, LUN, file, ERROR = o_err, /GET_LUN
   if (o_err NE 0 ) then begin
	printtocon, "% Write_PTD: error while trying to open file " + file+ ":" + !ERR_STRING
        ErrMsg,  "% Write_PTD: error while trying to open file " + file+ ":" + !ERR_STRING
	return, 0
     endif
   writeu, LUN, json_serialize(metahash)
   close, LUN
   free_lun, LUN
   printtocon, "% Write_PTD: exported header data"
   printtocon, "%            format   = ptd "
   printtocon, "%            path     = "+datapath
   printtocon, "%            filename = "+file
   ;; printtocon, "%              mrc data = "+type.rawdata
   SaveBinData, datapath+Path_Sep()+type.rawdata, FORMAT="mrc"
   return, 1
END 


PRO Read_ptd, file
;;
;; EMPAD Object will be created upon return
;; file: filename
;; 
IF NOT(GetDebugFlag()) THEN BEGIN

CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% Read_ptd5:        Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
 END
END
;
;; A minimal metadata hash has the following keys:
;;
;; {"Creator":"PadTools",
;; "Version":"1.9",
;; "Metadata":{"NAME":"acquisition_19-mysubset.ptd",
;;             "DIMENSION":[128,130,16384],
;;             "ISPADDATA":1,
;;             "ISSUBSET:0,
;;             "SCANSIZE":[128,128]}
;; "Data":{"TYPE":"2D_SLICE_SEQUENCE",
;;         "NDIMENSIONS":3,
;;         "DIMENSIONS":[128,130,1093],
;;         "ID":"array",
;;         "DATATYPE":4,
;;         "NBYTES":18187520,
;;         "RAWDATA":"acquisition_19-mysubset.mrc"}
;; }



err=1                  ;; return err code 1 by default
success=0
isempadtools=0B
minversion=1.9
isvalidversion=0B
;; read json
PrintToCon, "% Read_ptd: "+file
PrintToCon, "%     Parsing json file ..."
s=read_json(file)
metahash=json_parse(s,/toarray,/FOLD_CASE)
if Not(STRCOMPRESS(metahash["Creator"]) EQ 'PadTools') THEN BEGIN
   PrintToCon, "% Read_ptd: File is not a PadTools ptd header file."
   return
END
IF NOT((FLOAT(metahash["Version"]) GE minversion)) THEN BEGIN
   PrintToCon, "% Read_ptd: File is not a PadTools ptd header file in JSON format."
   return
END
;; check that data file exists
;; get current directory
Splitpath, file, datapath, datafilename
mrcfile=datapath+Path_Sep()+(metahash["data"])["RAWDATA"]
filecheck=FILE_TEST(mrcfile)
IF NOT(filecheck) THEN BEGIN
   PrintToCon, "% Read_ptd: Could not find raw data file"
   PrintToCon, "%         "+mrcfile
   return
END
;; prepare entry and load data ... we can load the mrc file and then ass the meta data
err=1
ReadMRCData, mrcfile, ERROR=err
IF err THEN BEGIN
   PrintToCon, "% Read_ptd: Error reading raw data file"
   PrintToCon, "%         "+mrcfile
   return
END
;; now handle all header data
d=GetCurrentP()
(*d).name=file
p=(*d).datap
    ;; file data in struc, image container in *p
(*p).id=(metahash['metadata'])['NAME']
(*p).SzX=LONG(((metahash['metadata'])['DIMENSION'])[0])
(*p).SzY=LONG(((metahash['metadata'])['DIMENSION'])[1])
(*p).SzZ=LONG(((metahash['metadata'])['DIMENSION'])[2])
if (metahash['metadata']).HasKey('SAMPLING') THEN BEGIN
   (*p).xsamp=Float(((metahash['metadata'])['SAMPLING'])[0])
   (*p).ysamp=Float(((metahash['metadata'])['SAMPLING'])[1])
   (*p).zsamp=Float(((metahash['metadata'])['SAMPLING'])[2])
END
if (metahash['metadata']).HasKey('UNIT') THEN BEGIN
   (*p).xunit=((metahash['metadata'])['UNIT'])[0] 
   (*p).yunit=((metahash['metadata'])['UNIT'])[1] 
   (*p).zunit=((metahash['metadata'])['UNIT'])[2]
END 
if (metahash['metadata']).HasKey('VOLTAGE') THEN (*p).voltage=(metahash['metadata'])['VOLTAGE']
;; handle notes
IF ((metahash['metadata']).HASKEY('NOTE') AND OBJ_VALID((*p).note)) THEN (*p).note->FromString,(metahash['metadata'])['NOTE']
;;
;; STOP
IF  (metahash['metadata'])['ISPADDATA'] THEN BEGIN
   IF  (metahash['metadata'])['ISSUBSET'] THEN BEGIN    
      IF metahash.HASKEY('SUBSET') THEN empaddata=obj_new('EMPADObj',file, Subset=metahash['SUBSET'])
   END ELSE empaddata=obj_new('EMPADObj',file)
   IF OBJ_VALID(empaddata) THEN BEGIN
      empaddata.SetScanSize, ((metahash['metadata'])['SCANSIZE'])[0],((metahash['metadata'])['SCANSIZE'])[1]
      if (metahash['metadata']).HasKey('DETECTORSIZE') THEN BEGIN
         empaddata.SetFrameSize, ((metahash['metadata'])['DETECTORSIZE'])[0],((metahash['metadata'])['DETECTORSIZE'])[1]
      END ELSE BEGIN
         empaddata.SetFrameSize, (*p).SzX, (*p).SzY
      END  
      if (metahash['metadata']).HasKey('DETECTORSAMPLING') THEN $
         empaddata.SetDetectorSampling,  FLOAT(((metahash['metadata'])['DETECTORSAMPLING'])[0]),FLOAT(((metahash['metadata'])['DETECTORSAMPLING'])[1])
      if (metahash['metadata']).HasKey('SCANSAMPLING') THEN $
         empaddata.SetScanSampling,  FLOAT(((metahash['metadata'])['SCANSAMPLING'])[0]),FLOAT(((metahash['metadata'])['SCANSAMPLING'])[1])
      empaddata.SetDataPointer, (*p).data
      (*p).extra=empaddata
   END  
END  
Update_XTabControl
END    
