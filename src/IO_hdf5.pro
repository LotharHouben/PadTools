FUNCTION GetHDF5ImageDescriptor, p, JSON=json
   
  struct={name:"",pathinfo:"",source:"",dimension:LonArr(3),sampling:FLTARR(3),offset:FLTARR(3),unit:STRARR(3),device:"",voltage:0.0,wavelength:0.0,exposure:0.0,note:"",scansize:LONARR(2),detectorsize:LONARR(2),scansampling:FLTARR(3),detectorsampling:FLTARR(3),ispaddata:0B,issubset:0B}
  ;; avoid pointers here, they do not work when writing the h5 descriptor
  
  
  IF PTR_VALID(p) THEN BEGIN
     struct.name=(*p).id
     struct.source=(*p).id
     struct.dimension[0]=(*p).SzX
     struct.dimension[1]=(*p).SzY
     struct.dimension[2]=(*p).SzZ
     struct.sampling[0]=(*p).xsamp
     struct.sampling[1]=(*p).ysamp
     struct.sampling[2]=(*p).zsamp
     ;;struct.offset[0]=(*p).xoffs
     ;;struct.offset[1]=(*p).yoffs
     ;;struct.offset[2]=(*p).zoffs
     struct.unit[0]=(*p).xunit
     struct.unit[1]=(*p).yunit
     struct.unit[2]=(*p).zunit
     ;;struct.device=(*p).device
     struct.voltage=(*p).voltage
     ;;struct.wavelength=(*p).lambda
     ;;struct.exposure=(*p).exposure
     struct.note=""
     IF (OBJ_VALID((*p).note)) THEN Begin
        s=(*p).note->ToString()
        IF (s NE !NULL) THEN struct.note=s
     END
     ;; add info from empad object
     e=(*p).extra
     IF OBJ_VALID((e)) THEN Begin
        struct.ispaddata=1B
        struct.detectorsize=[e->GetFrameSize(/X),e->GetFrameSize(/Y)]
        struct.scansize=[e->GetScanSize(/X),e->GetScanSize(/Y)]
        struct.scansampling=[e->GetScanSampling(/X),e->GetScanSampling(/Y)]
        struct.detectorsampling=[e->GetDetectorSampling(/X),e->GetDetectorSampling(/Y)]
        IF (e->GetSubset(SUBSET=subset) EQ 1) THEN BEGIN
           struct.issubset=1
           ;; struct.subset=PTR_NEW(subset)
        END
     END
  END
  if keyword_set(json) THEN return, JSON_SERIALIZE(struct) else return, struct
END
  

FUNCTION Write_hdf5, d, file, NONINTERACTIVE=noninteractive
  ;; file = file name
  ;; ending should be h5
  IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     Print, "% DataWrite_hdf5:  Fatal error "
     Print, "%   Error status  - " + STRING(error_status)
     Print, "%   Error message - " + !ERROR_STATE.MSG
     CATCH, /Cancel
     ErrMsg, !ERROR_STATE.MSG
     IF (N_Elements(fid) GT 0) THEN H5F_CLOSE,fid
     return, 0
  END
END
 IF (FileIsWriteable(file,NONINTERACTIVE=noninteractive) NE 1) THEN BEGIN
    printtocon, "% Write_hdf5: file "+ file + " is not writeable"
    return, 0
 END
 IF (NOT(PTR_VALID(d))) THEN BEGIN
    print, "% Write_hdf5: Current image list pointer is invalid."
    return, 0
 END
 p=(*d).datap
 IF (NOT(PTR_VALID(p))) THEN BEGIN
    print, "% Write_hdf5: Current data pointer is invalid."
    return, 0
 END
 ;; we first create a file identifier, since all communication with
 ;; the file is done via the file identifier.
 ;;STOP
 ;; Version identifier, structure to write into File
 
 fid = H5F_CREATE(file)
 ;;
       ;; + ---- write creator field --- +
 ;;
 creator={creator:'empadTools',version:'0.4'}
 datatype_ID = H5T_IDL_CREATE(creator)
 ;; create a simple (or, in this case, not so simple) data
 ;; space. As an argument, we require the dimensions of the
 ;; dataspace. In this case, with a single structure variable,
 ;; the dimension is 1.
 dataspace_ID = H5S_CREATE_SIMPLE(1)
 dataset_ID = H5D_CREATE(fid, 'CREATOR', datatype_ID, dataspace_ID)
 H5D_WRITE,dataset_id, creator
 ;; close all open identifiers
 H5D_CLOSE,dataset_id
 H5S_CLOSE,dataspace_id
 H5T_CLOSE,datatype_id
 ;;
;; create data
  CASE 1 OF 
    ((*p).SzZ EQ 1): BEGIN
       ;;
       ;; + ---- write data type field --- +
       ;;
       type={type:"single",dimensions:2,id:'image',datatype:(*p).type} ;; a single data array, could be a table or graph as well,
       ;; we store the data type in IDL convention, this is required
       ;; to retrieve complex data (type  6 and 9)
       datatype_ID = H5T_IDL_CREATE(type)
       ;; create a simple (or, in this case, not so simple) data
       ;; space. As an argument, we require the dimensions of the
       ;; dataspace. In this case, with a single structure variable,
       ;; the dimension is 1.
       dataspace_ID = H5S_CREATE_SIMPLE(1)
       dataset_ID = H5D_CREATE(fid, 'CONTENT', datatype_ID, dataspace_ID)
       H5D_WRITE,dataset_id, type
       ;; close all open identifiers
       H5D_CLOSE,dataset_id
       H5S_CLOSE,dataspace_id
       H5T_CLOSE,datatype_id
       ;;
       ;; + ---- write descriptor field --- +
       ;;
       s=GetHDF5ImageDescriptor(p)
       datatype_ID = H5T_IDL_CREATE(s)
       ;; create a simple (or, in this case, not so simple) data
       ;; space. As an argument, we require the dimensions of the
       ;; dataspace. In this case, with a single structure variable,
       ;; the dimension is 1.
       dataspace_ID = H5S_CREATE_SIMPLE(1)
       dataset_ID = H5D_CREATE(fid, 'DESCRIPTOR', datatype_ID, dataspace_ID)
       H5D_WRITE,dataset_id, s
       ;; close all open identifiers
       H5D_CLOSE,dataset_id
       H5S_CLOSE,dataspace_id
       H5T_CLOSE,datatype_id
       ;;
       ;; + ---- write data field --- +
       ;;
       ;; get data type and space, needed to create the dataset
       IF (((*p).type EQ 6) OR ((*p).type EQ 9)) THEN BEGIN
          ;; image is complex, hdf5 does not know complex data
          ;; store as real and imaginary part
          datatype_id = H5T_IDL_CREATE(Real_Part((*(*p).data))) ;; does this work?
          dataspace_id = H5S_CREATE_SIMPLE(size((*(*p).data),/DIMENSIONS))
          dataset_id = H5D_CREATE(fid,'DATAr',datatype_id,dataspace_id)
          H5D_WRITE,dataset_id, Real_Part(*(*p).data)
          ;; create dataset in the output file
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          dataspace_id = H5S_CREATE_SIMPLE(size((*(*p).data),/DIMENSIONS))
          dataset_id = H5D_CREATE(fid,'DATAi',datatype_id,dataspace_id)
          H5D_WRITE,dataset_id, Imaginary(*(*p).data)
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          H5T_CLOSE,datatype_id
       END ELSE BEGIN
          datatype_id = H5T_IDL_CREATE((*(*p).data))
          dataspace_id = H5S_CREATE_SIMPLE(size((*(*p).data),/DIMENSIONS))
          ;; create dataset in the output file
          dataset_id = H5D_CREATE(fid,'DATA',datatype_id,dataspace_id)
          H5D_WRITE,dataset_id, (*(*p).data)
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          H5T_CLOSE,datatype_id
          ;; write data to dataset
;; close all open identifiers
       END
       printtocon, "% Write_hdf5: Wrote empadTools image with descriptor data."
       ;;       
    END
    ((*p).SzZ GT 1): BEGIN
       NIm=(*p).SzZ ;;number of images
       
       ;; + ---- write data type field --- +
       ;;
       type={type:'2D_SLICE_SEQUENCE',dimensions:3,id:'array',datatype:FIX((*p).type)} ;; multiple two-dimensional data arrays, in the form of a three-dimensional array
       datatype_ID = H5T_IDL_CREATE(type)
       ;; create a simple (or, in this case, not so simple) data
       ;; space. As an argument, we require the dimensions of the
       ;; dataspace. In this case, with a single structure variable,
       ;; the dimension is 1.
       dataspace_ID = H5S_CREATE_SIMPLE(1)
       dataset_ID = H5D_CREATE(fid, 'CONTENT', datatype_ID, dataspace_ID)
       H5D_WRITE,dataset_id, type
       ;; close all open identifiers
       H5D_CLOSE,dataset_id
       H5S_CLOSE,dataspace_id
       H5T_CLOSE,datatype_id
       ;;
       ;; + ---- create a DATASET  field --- +
       group_id = H5G_CREATE(fid,'DATASET')
       ;; + ---- write group descriptor field --- +
       ;;
       s=GetHDF5ImageDescriptor(p)
       datatype_ID = H5T_IDL_CREATE(s)
       ;; create a simple (or, in this case, not so simple) data
       ;; space. As an argument, we require the dimensions of the
       ;; dataspace. In this case, with a single structure variable,
       ;; the dimension is 1.
       dataspace_ID = H5S_CREATE_SIMPLE(1)
       dataset_ID = H5D_CREATE(group_id, 'DESCRIPTOR', datatype_ID, dataspace_ID)
       H5D_WRITE,dataset_id, s
       ;; close all open identifiers
       H5D_CLOSE,dataset_id
       H5S_CLOSE,dataspace_id
       H5T_CLOSE,datatype_id
       ;;
       ;; data, create leaf _DATA_ within DATASET 
       ;; _DATA_ wil contain the 4D STEM array data
       image_id = H5G_CREATE(group_id,"_DATA_")
       ;;
       ;; + ---- write data field --- +
       ;; + ---- write descriptor field --- +
       ;;
       s={dataset:"Array",dimension:LONARR(3),type:FIX((*p).type)}
       s.dimension=SIZE(*(*p).data,/DIMENSIONS)
       ;;STOP
       datatype_ID = H5T_IDL_CREATE(s)
       ;; create a simple (or, in this case, not so simple) data
       ;; space. As an argument, we require the dimensions of the
       ;; dataspace. In this case, with a single structure variable,
       ;; the dimension is 1.
       dataspace_ID = H5S_CREATE_SIMPLE(1)
       dataset_ID = H5D_CREATE(image_id, 'DESCRIPTOR', datatype_ID, dataspace_ID)
       H5D_WRITE,dataset_id, s
       ;; close all open identifiers
       H5D_CLOSE,dataset_id
       H5S_CLOSE,dataspace_id
       H5T_CLOSE,datatype_id
       ;;
       IF (((*p).type EQ 6) OR ((*p).type EQ 9)) THEN BEGIN
          datatype_id = H5T_IDL_CREATE(REAL_PART(*((*p).data)))
          dim=size(*(*p).data,/DIMENSIONS)
          dataspace_id = H5S_CREATE_SIMPLE(dim)
          ;; create dataset in the output file
          dataset_id = H5D_CREATE(image_id,'DATAr',datatype_id,dataspace_id)
          ;; write data to dataset
          H5D_WRITE,dataset_id, REAL_PART(*((*p).data))
;; close all open identifiers
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          dataspace_id = H5S_CREATE_SIMPLE(dim)
          ;; create dataset in the output file
          dataset_id = H5D_CREATE(image_id,'DATAi',datatype_id,dataspace_id)
          ;; write data to dataset
          H5D_WRITE,dataset_id, IMAGINARY(*((*p).data))
;; close all open identifiers
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          H5T_CLOSE,datatype_id
       END ELSE BEGIN
          datatype_id = H5T_IDL_CREATE(*((*p).data))
          dataspace_id = H5S_CREATE_SIMPLE(size(*((*p).data),/DIMENSIONS))
          ;; create dataset in the output file
          dataset_id = H5D_CREATE(image_id,'DATA',datatype_id,dataspace_id)
          ;; write data to dataset
          H5D_WRITE,dataset_id, *((*p).data)
;; close all open identifiers
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          H5T_CLOSE,datatype_id
       END 
       ;;
       H5G_CLOSE, image_id
       ;;
       ;; subset data, create leaf _SUBSET_ within DATASET 
       ;; _SUBSET_ will contain the indices to the 4D STEM array positions
       subset=1
       e=(*p).extra
       IF OBJ_VALID((e)) THEN Begin ;; data is an EMPAD Object
          image_id = H5G_CREATE(group_id,"EMPADDATA")
          s={setid:"EMPAD",scan_size_x:e.GetScanSize(/X),scan_size_y:e.GetScanSize(/Y),frame_size_x:e.GetFrameSize(/X),frame_size_y:e.GetFrameSize(/Y),det_samp_x:e.GetDetectorSampling(/X),det_samp_y:e.GetDetectorSampling(/Y),scan_samp_x:e.GetScanSampling(/X),scan_samp_y:e.GetScanSampling(/Y)}
          datatype_ID = H5T_IDL_CREATE(s)
          dataspace_ID = H5S_CREATE_SIMPLE(1)
          dataset_ID = H5D_CREATE(image_id, 'DESCRIPTOR', datatype_ID, dataspace_ID)
          H5D_WRITE,dataset_id, s
          H5D_CLOSE,dataset_id
          H5S_CLOSE,dataspace_id
          H5T_CLOSE,datatype_id
          H5G_CLOSE, image_id
          IF (e->GetSubset(SUBSET=subset) EQ 1) THEN BEGIN
             image_id = H5G_CREATE(group_id,"_SUBSET_")
             s={subset:"INDICES",dimension:N_ELEMENTS(subset),type:5}
             datatype_ID = H5T_IDL_CREATE(s)
             dataspace_ID = H5S_CREATE_SIMPLE(1)
             dataset_ID = H5D_CREATE(image_id, 'DESCRIPTOR', datatype_ID, dataspace_ID)
             H5D_WRITE,dataset_id, s
             H5D_CLOSE,dataset_id
             H5S_CLOSE,dataspace_id
             H5T_CLOSE,datatype_id
       
             datatype_id = H5T_IDL_CREATE(subset)
             dataspace_id = H5S_CREATE_SIMPLE(size(subset,/DIMENSIONS))
             dataset_id = H5D_CREATE(image_id,'DATA',datatype_id,dataspace_id)
             H5D_WRITE,dataset_id, subset
             H5D_CLOSE,dataset_id
             H5S_CLOSE,dataspace_id
             H5T_CLOSE,datatype_id
             H5G_CLOSE, image_id
          END 
       END 

       H5G_CLOSE, group_id
       printtocon, "% Write_hdf5: Wrote empadTools data with descriptors."
    END
    ELSE:
 END
  H5F_CLOSE,fid
  printtocon, "% Write_hdf5: Closing "+file+"."
  return, 1
END


PRO SetImageProperties_hdf5, p, struc, IMTOOLS=imtools, EMPADTOOLS=empadtools
  ;; Check if structure is valid
  CATCH, Error_status
  IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% SetImageProperties_hdf5: Field variable error. "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
    return
 END
  IF keyword_set(iMtools) THEN BEGIN
     CASE (*p).id OF
        "image": BEGIN
           (*p).comment=struc.name
           (*p).path=struc.pathinfo
           (*p).SzX=struc.dimension[0] & (*p).SzY=struc.dimension[1]
;; sampling data; given in m, transform into nm
           (*p).xsamp=struc.sampling[0] & (*p).ysamp=struc.sampling[1] & (*p).zsamp=struc.sampling[2]
           (*p).xoffs=struc.offset[0] & (*p).yoffs=struc.offset[1] & (*p).zoffs=struc.offset[2]
           (*p).xunit=struc.unit[0] & (*p).yunit=struc.unit[1] & (*p).zunit=struc.unit[2]
           IF OBJ_VALID((*p).note) THEN (*p).note->FromString, struc.note
           (*p).voltage=struc.voltage
           (*p).lambda=struc.wavelength
           (*p).device=struc.device
           (*p).exposure=struc.exposure
        END
        "group": BEGIN
           
           (*p).comment=struc.name
           grp=(*p).group
           IF PTR_VALID(grp) THEN BEGIN
              ;; (*p).path=struc.pathinfo
              (*grp).comment=struc.name
              (*grp).gn=struc.name
              (*grp).SzX=struc.dimension[0] & (*grp).SzY=struc.dimension[1] &  (*grp).count=struc.dimension[2]
;; sampling data; given in m, transform into nm
              (*grp).xsamp=struc.sampling[0] & (*grp).ysamp=struc.sampling[1] & (*grp).zsamp=struc.sampling[2]
              (*grp).xoffs=struc.offset[0] & (*grp).yoffs=struc.offset[1] & (*grp).zoffs=struc.offset[2]
              (*grp).xunit=struc.unit[0] & (*grp).yunit=struc.unit[1] & (*grp).zunit=struc.unit[2]
              IF OBJ_VALID((*grp).note) THEN BEGIN
                 (*grp).note->FromString, struc.note
              END
              (*grp).voltage=struc.voltage
              (*grp).lambda=struc.wavelength
              (*grp).device=struc.device
              (*grp).exposure=struc.exposure
           END
        END
        ELSE:
     END  
  END
  IF keyword_set(empadtools) THEN BEGIN
     ;; file data in struc, image container in *p
     (*p).id=struc.name
     (*p).SzX=struc.dimension[0]
     (*p).SzY=struc.dimension[1]
     (*p).SzZ=struc.dimension[2]
     (*p).xsamp=struc.sampling[0]
     (*p).ysamp=struc.sampling[1]
     (*p).zsamp=struc.sampling[2]
     ;;struct.offset[0]=(*p).xoffs
     ;;struct.offset[1]=(*p).yoffs
     ;;struct.offset[2]=(*p).zoffs
     (*p).xunit=struc.unit[0]
     (*p).yunit=struc.unit[1]
     (*p).zunit=struc.unit[2]
     ;;struct.device=(*p).device
     (*p).voltage=struc.voltage
     ;;struct.wavelength=(*p).lambda
     ;;struct.exposure=(*p).exposure
     IF OBJ_VALID((*p).note) THEN (*p).note->FromString, struc.note
     ;; TODO: import extra field
  END
END 

PRO Read_hdf5, fname, VERBOSE=verbose, DEBUG=debug
;;
;; EMPAD Object will be created upon return
;; fname: filename
;; 
IF NOT(GetDebugFlag()) THEN BEGIN

CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% Read_hdf5:        Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
 END
END
;
err=1                  ;; return err code 1 by default
success=0
isempadtools=0B
versions=['0.4','0.5']
isvalidversion=0B

PrintToCon, "% Read_hdf5: "+fname
PrintToCon, "%     Parsing file ..."
s = H5_PARSE(fname, /READ_DATA)
PrintToCon, "%     Reading data ..."
;; creator is empadtools?  s.CREATOR._DATA.CREATOR = empadTools
;; version is 0.4? s.CREATOR._DATA.VERSION = 0.4
IF tag_exist(s,"CREATOR") THEN BEGIN
   field=s.CREATOR._DATA
   IF (tag_exist(field,"CREATOR") AND tag_exist(field,"VERSION")) THEN BEGIN
      IF NOT(field.CREATOR EQ "empadTools") THEN BEGIN
         Printtocon, "% Read_hdf5: File is not recognised as empadTools hdf5 file."
      END ELSE BEGIN
         isempadTools=1B
         IF NOT(In(field.version,versions)) THEN BEGIN
            Printtocon, "% Read_hdf5: File version is not recognised as known empadTools hdf5 file version."
         END ELSE BEGIN
            isvalidversion=1B
         END
      END
   END
END

IF (isempadtools AND isvalidversion) THEN BEGIN
   field=s.CONTENT._DATA
   IF NOT(tag_exist(field,"DATATYPE")) THEN BEGIN
      ;; old versions, no complex datatype identifier
      struct_add_field, field, 'datatype', 0
   END
   If (field.type EQ 'single') THEN BEGIN
      ;; a single array input
      IF (field.id EQ 'image') THEN BEGIN
         IF (field.dimensions EQ 2) THEN BEGIN
            ;; a single 2D image
            ;; read data, this should not fail
            ;; XXXXXXXXXXXXXXXXXXX p=PGenericImage()
            p=DataList_CreateElement(GetRootP(), fname)
            IF NOT(PTR_VALID(p)) THEN BEGIN
               printtocon, "% Read_hdf5: Could not allocate memory for image container."
               return
            END
            ;; 
                       ;; check whether data is complex
            IF ((field.datatype EQ 6) OR (field.datatype EQ 9)) THEN BEGIN
               (*p).type=field.datatype
               dim=SIZE(s.DATAr._DATA, /DIMENSION)
               (*p).data=PTR_NEW(make_array(DIMENSION=dim,TYPE=field.datatype))
               IF (field.datatype EQ 6) THEN (*(*p).data)=COMPLEX(s.DATAr._DATA,s.DATAi._DATA)
               IF (field.datatype EQ 9) THEN (*(*p).data)=DCOMPLEX(s.DATAr._DATA,s.DATAi._DATA)
            END ELSE  BEGIN
               (*p).data=PTR_NEW(s.DATA._DATA)
               (*p).type=SIZE(*(*p).data,/TYPE)
               dim=SIZE(s.DATA._DATA, /DIMENSION)
            END
            (*p).SzX=dim[0]
            (*p).SzY=dim[1]
            (*p).SzZ=1
            (*p).zcoord=3
            (*p).slice=0
            bin=1
            (*p).BINNING=bin
            (*p).BINNINGX=bin
            (*p).BINNINGY=bin
            ;;
            ;; read descriptor
            SetImageProperties_hdf5, p, s.DESCRIPTOR._DATA, /EMPADTOOLS
            ;;(*p).comment=s._NAME
            IF (success EQ 0) then printtocon, "% Read_hdf5: Read empadTools image with descriptor data."            
            return
         END
      END
   END
   If (field.type EQ '2D_SLICE_SEQUENCE') THEN BEGIN
      IF (field.id EQ 'array') THEN BEGIN
         IF (field.dimensions EQ 3) THEN BEGIN
            ;; an array of 2D slices
            ;; read data, this should not fail
            ;; XXXXXXXXXXXXXXXXXXX p=PGenericImage()
            IF NOT(tag_exist(s,"DATASET")) THEN BEGIN
               ;; old versions, no complex datatype identifier
               printtocon, "% Read_hdf5: Missing DATASET group identifier."
               return
            END
            datafield=s.DATASET._DATA_
            datadescr=s.DATASET._DATA_.DESCRIPTOR._DATA
            p=DataList_CreateElement(GetRootP(), fname)
            IF NOT(PTR_VALID(p)) THEN BEGIN
               printtocon, "% Read_hdf5: Could not allocate memory for image container."
               return
            END ;;
;; data array is in datafield.DATA._DATA
;; (or in datafield.DATAr._DATA and datafield.DATAi._DATA if the data is complex)
            
            IF ((datadescr.TYPE EQ 6) OR (datadescr.TYPE EQ 9)) THEN BEGIN
               (*p).type=SIZE(datafield.DATA._DATA, /TYPE)
               dim=SIZE(datafield.DATAr._DATA, /DIMENSION)
               (*p).data=PTR_NEW(make_array(DIMENSION=dim,TYPE=(*p).type))
               IF ((*p).type EQ 6) THEN (*(*p).data)=COMPLEX(datafield.DATAr._DATA,s.DATAi._DATA)
               IF ((*p).type EQ 9) THEN (*(*p).data)=DCOMPLEX(datafield.DATAr._DATA,s.DATAi._DATA)
            END ELSE  BEGIN
               (*p).data=PTR_NEW(datafield.DATA._DATA)
               (*p).type=SIZE(*(*p).data,/TYPE)
               dim=SIZE(datafield.DATA._DATA, /DIMENSION)
            END
            
            ;;
            SetImageProperties_hdf5, p, s.DATASET.DESCRIPTOR._DATA, /EMPADTOOLS
            ;; now check if it is empad data
            IF (tag_exist(s.DATASET,"EMPADDATA")) THEN BEGIN
               field=s.DATASET.EMPADDATA.DESCRIPTOR._DATA
               IF (field.SETID EQ "EMPAD") THEN BEGIN
                  IF tag_exist(s.DATASET,"_SUBSET_") THEN BEGIN
                     ;; get indices from file
                     ds=s.DATASET._SUBSET_.DESCRIPTOR._DATA
                     IF NOT((ds.SUBSET EQ "INDICES") AND (ds.TYPE EQ 5)) THEN BEGIN
                        printtocon, "% Read_hdf5: Error reading SUBSET information."
                        return
                     END
                     empaddata=obj_new('EMPADObj',fname, Subset=s.DATASET._SUBSET_.DATA._DATA)
                  END ELSE empaddata=obj_new('EMPADObj',fname)
                  empaddata.SetScanSize, field.SCAN_SIZE_X, field.SCAN_SIZE_Y
                  empaddata.SetFrameSize, field.FRAME_SIZE_X, field.FRAME_SIZE_Y
                  empaddata.SetDetectorSampling,  field.DET_SAMP_X, field.DET_SAMP_Y
                  empaddata.SetScanSampling, field.SCAN_SAMP_X, field.SCAN_SAMP_Y
                  ;; map data pointer
                  empaddata.SetDataPointer, (*p).data
                  ;;empaddata.SetDataPointer, (*(*p).datap).data 
                  (*p).extra=empaddata
                  
               END               
            END
         END  
      END  
   END
   PrintToCon, "%     done."
   Update_XTabControl
END   
return
END  

