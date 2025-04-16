;; IMPORTANT:
;;
;; The DECTRIS Eiger format uses hdf5 compression with a Lz4 algorithm
;; and bitshuffling. HDF uses a plugin mechanism to use various
;; compression filters. You will need to install the plugin filters
;; alongside with the HDF5 programslibraries to make them work.
;;
;; On macos:
;;
;;   install from mac ports
;;
;;   sudo port install HDF5
;;   Sudo port install HDF5-External-Filter-Plugins
;; 
;;   HDF5-External-Filter-Plugins has the following notes:
;;    To enable, use (with H5Pset_filter()):
;;     filter_id = 307   for bzip2
;;     filter_id = 32004 for lz4
;;     filter_id = 32008 for bitshuffle
;;    
;;     To enable use from Matlab, or other tools expecting the default HDF5 plugin path, create the
;;    required symbolic link:
;;      mkdir -p /usr/local/hdf5/lib
;;      ln -s /opt/local/lib/hdf5 /usr/local/hdf5/lib/plugin
;;
;;      set ennvironment variable for plugin path
;;      export HDF5_PLUGIN_PATH=/usr/local/hdf5/lib
;;
;;  NOT FINALIZED ;;; testing: h5repack -v -l /entry/data/data:CONTI STEM_Test_Lines5_data_000001.h5 test.h5
;;
;; uncompress with h5tools (this seems to work):
;;
;; h5repack -v STEM_Test_Lines5_data_000001.h5  out_file.h5 -f UD=32008 -f UD:32004
;;
;; Notes:
;;
;; ARINA file is missing information
;; - in /entry/data/
;;   - add /entry/data/series_type - to identify the type of
;;     experiment, set it to a string such as
;;     TIME_SERIES, RASTER_SCAN 
;;
;; - in /entry/instrument/detector/
;;   - in /entry/instrument/detector/detector_distance - set it to the
;;           camera length in m or mm
;;
;; - add a group /entry/instrument/microscope
;;    - add attribute /entry/instrument/microscope/mode
;;      (such as TEM, STEM)
;;    - add attribute /entry/instrument/microscope/submode
;;      (such as microprobe, nanoprobe, EFTEM)
;;    - add attribute /entry/instrument/microscope/magnification
;;    - add attribute /entry/instrument/microscope/cameralength
;;    - add attribute /entry/instrument/microscope/x_scan_size
;;      (number of scan pixels in x-direction)
;;    - add attribute /entry/instrument/microscope/y_scan_size
;;      (number of scan pixels in y-direction)
;;    - add attribute /entry/instrument/microscope/x_fow_size
;;      (field of view in x-direction in nm)
;;    - add attribute /entry/instrument/microscope/y_fow_size
;;      (field of view in y-direction in nm)
;;
;;
;; file info obtained with h5dump
;;
;; 1) master file
;;
;;
;; 2) data file
;;
;; h5dump -pH STEM_Test_Lines5_data_000001.h5
;; HDF5 "STEM_Test_Lines5_data_000001.h5" {
;; GROUP "/" {
;;    GROUP "entry" {
;;       ATTRIBUTE "NX_class" {
;;          DATATYPE  H5T_STRING {
;;             STRSIZE 8;
;;             STRPAD H5T_STR_NULLTERM;
;;             CSET H5T_CSET_ASCII;
;;             CTYPE H5T_C_S1;
;;          }
;;          DATASPACE  SCALAR
;;       }
;;       GROUP "data" {
;;          ATTRIBUTE "NX_class" {
;;             DATATYPE  H5T_STRING {
;;                STRSIZE 7;
;;                STRPAD H5T_STR_NULLTERM;
;;                CSET H5T_CSET_ASCII;
;;                CTYPE H5T_C_S1;
;;             }
;;             DATASPACE  SCALAR
;;          }
;;          DATASET "data" {
;;             DATATYPE  H5T_STD_U16LE
;;             DATASPACE  SIMPLE { ( 32768, 192, 192 ) / ( H5S_UNLIMITED, 192, 192 ) }
;;             STORAGE_LAYOUT {
;;                CHUNKED ( 1, 192, 192 )
;;                SIZE 249432610 (9.686:1 COMPRESSION)
;;             }
;;             FILTERS {
;;                USER_DEFINED_FILTER {
;;                   FILTER_ID 32008
;;                   COMMENT bitshuffle; see https://github.com/kiyo-masui/bitshuffle
;;                   PARAMS { 0 3 2 0 2 }
;;                }
;;             }
;;             FILLVALUE {
;;                FILL_TIME H5D_FILL_TIME_IFSET
;;                VALUE  65535
;;             }
;;             ALLOCATION_TIME {
;;                H5D_ALLOC_TIME_INCR
;;             }
;;             ATTRIBUTE "image_nr_high" {
;;                DATATYPE  H5T_STD_U64LE
;;                DATASPACE  SCALAR
;;             }
;;             ATTRIBUTE "image_nr_low" {
;;                DATATYPE  H5T_STD_U64LE
;;                DATASPACE  SCALAR
;;             }
;;          }
;;       }
;;    }
;; }
;; }


FUNCTION ReadMRCDataToArray, fname
 CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadMRCDataToArray:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return, 0
 END
    ;;
     ;; open file to read header
     ;;
     openr, LUN, fname, ERROR = o_err, /GET_LUN
     if (o_err NE 0 ) then begin
	printtocon, ["% ReadMRCData: Error while trying to open file " + fname,!ERR_STRING]
        ErrMsg, "% ReadMRCData: Error while trying to open file " + fname+ ":" + !ERR_STRING
	return, 0
     endif 
     printtocon, "% "
     printtocon, "% ReadMRCData: Reading " + fname + "."
     x=0L & y=0L & z=0L
     swap=0   ;; need to swap the bytes?
     offset=1024       ;; starting byte for image data, regular, non-extended header
     KnownDataType=[1,2,3,4,6,9] ;; data type id's known
     ;; 9 is a fake unsigned byte data type
     header=LONARR(56)  ;; read 224 bytes ....
     readu, LUN, header
     ;; check for id and autodetect endian type
;; (you can ignore byte 0 for this and use id byte 1)
;; 0x197=407
     IF (NOT(IN(header[3],KnownDataType))) THEN BEGIN
        ;; could it be that we have to swap bytes?
        header=SWAP_ENDIAN(header)
        IF ((IN(header[3],KnownDataType))) THEN BEGIN
           printtocon, "% ReadMRCData: swapping bytes"
           swap=1
        END ELSE BEGIN
           printtocon, "% ReadMRCData: Error - wrong file format or data type"
           close, LUN
           free_lun, LUN
           return, 0
        END
     END     
     x=LONG(header[0]) & y= LONG(header[1]) & z=LONG(header[2])
     CASE header[3] OF
        9: BEGIN
           ;; unsigned byte
           type=1
           printtocon, "% ReadMRCData: warning - reading signed byte" 
        END
        1: BEGIN
           ;; signed integer 16 bit
           type=2
           ;;
        END
        2: BEGIN
           ;; floating point 32 bit
           type=4
           ;;
        END
        3: BEGIN
           ;; complex 16 bit integer
           printtocon, "% ReadMRCData: error - cannot read 16 bit complex integers"
           ErrMsg,  "% ReadMRCData: error - cannot read 16 bit complex integers"
           return, 0
           ;;
        END
        4: BEGIN
           ;; complex floating point 32 bit 
           type=6
           ;;
        END
        6: BEGIN
           ;; 16 bit unsigned integer
           type=12
           ;;
        END
        ELSE: BEGIN
           printtocon, "% ReadMRCData: error - unknown data type "+MyString(header[3])
           ErrMsg, "% ReadMRCData: error - unknown data type "+MyString(header[3])
           return, 0
        END
     ENDCASE
     extendedheadersize=0
     extendedheadersize=header[23]
     printtocon, "% ReadMRCData: DimX=" + MyString(header[0]) + " DimY="+MyString(header[1])+ " DimZ="+MyString(header[2]) + " type="+MyString(type)
     printtocon, "%              first data byte at " + MyString((offset+extendedheadersize))
     ;; check labels
     labels=BYTARR(800) ;; 
     readu, LUN, labels     
     nlabelsused=header[55]
     printtocon, "% "
     printtocon, "% ReadMRCData: Header Labels."
     IF (nlabelsused GE 1) THEN BEGIN
        for k=1, nlabelsused DO BEGIN
           printtocon, "%   "+STRING(labels((k-1)*80:k*80-1))
        END
     END
     IF keyword_set(fei) THEN BEGIN
        ;; check for FEI copyright string
        ;; 'Fei Company (C)' should appear at byte E0
        copyrightstr=STRING(labels(0:14))
        if NOT(copyrightstr EQ "Fei Company (C)") THEN BEGIN
        ;; check that the header size matches 128 Bytes times 1024
        ;; IF (extendedheadersize NE 131072) THEN BEGIN
           printtocon, "% ReadMRCData: FEI copyright string not found, no FEI header?" 
        END ELSE BEGIN
           feiheader=FLTARR(32,1024)
           readu, LUN, feiheader
           printtocon, "% "
           printtocon, "% ReadMRCData: FEI header information."
           
           IF (z LT 1024) THEN BEGIN
              tmp=(feiheader(13,0:z-1)) 
              tmp=tmp(UNIQ(tmp))/1000
              PRINTTOCON, "%   High Tension (kV)       : "+ MyString(tmp)
              tmp=(feiheader(12,0:z-1)) 
              tmp=tmp(UNIQ(tmp))
              PRINTTOCON, "%   Magnification           : "+ MyString(tmp)
              tmp=(feiheader(14,0:z-1)) 
              tmp=tmp(UNIQ(tmp))
              PRINTTOCON, "%   Binning                 : "+ MyString(tmp)
              tmp=(feiheader(11,0:z-1)) 
              tmp=tmp(UNIQ(tmp))*1E9
              PRINTTOCON, "%   Sampling (nm/pix)       : "+ MyString(tmp)
              tmp=(feiheader(10,0:z-1)) 
              tmp=tmp(UNIQ(tmp))
              PRINTTOCON, "%   Tilt axis rotation (deg): "+ MyString(tmp)
              printtocon, "% " 
              PRINTTOCON, "% ReadMRC: FEI single image information."
              printtocon, "%   image alpha(deg) beta(deg) defocus(nm) exptime(s) stage x(um) y(um) z(um) image shift x(um) y(um)"
              FMT='(F10.4,:," ")'
              ;; units should be m
              for k=0,(z-1) DO BEGIN
                 s=STRING(FORMAT='(I4,:," ")',k)+ $
                   STRING(FORMAT=FMT, feiheader[0,k])+ $
                   STRING(FORMAT=FMT, feiheader[1,k])+ $
                   STRING(FORMAT=FMT, feiheader[7,k]*1E9)+ $
                   STRING(FORMAT=FMT, feiheader[8,k])+ $
                   STRING(FORMAT=FMT, feiheader[2,k]*1E6)+ $
                   STRING(FORMAT=FMT, feiheader[3,k]*1E6)+ $
                   STRING(FORMAT=FMT, feiheader[4,k]*1E6)+ $
                   STRING(FORMAT=FMT, feiheader[5,k]*1E6)+ $
                   STRING(FORMAT=FMT, feiheader[6,k]*1E6)
                   PRINTTOCON, s 
                END
              ;;
           END
        END
     END  
     close, LUN
     free_lun, LUN
     return, Read3DData(fname, (offset+extendedheadersize), x, y, z, type, SWAPENDIAN=swap)
END 



FUNCTION GetDectrisHDF5ImageMasterDescriptor, fn
;; read Dectris _master file
;;
;; structure:
;;
;; - entry (group)
;;     | - data (group)
;;          | - "xxxx" (attribute - data link)
;;          | - "xxxx" (attribute - data link)
;;          | -  ...
;;     | - instrument (group)
;;          | - detector (group)
;;                | - detectorSpecific (group)
;;                        | - x_pixels_in_detector (attribute)
;;                        | - y_pixels_in_detector (attribute)
;;                        | - photon_energy (attribute)
;;                        | - ...
;;                | - description (atrribute)
  
  IF NOT(GetDebugFlag()) THEN BEGIN

CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% GetDectrisHDF5ImageMasterDescriptor:        Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    CATCH, /Cancel
 END
END
  struct={pdatafiles:PTR_NEW(),pathinfo:"",dimension:LonArr(3),sampling:FLTARR(3),offset:FLTARR(3),unit:STRARR(3),device:"",voltage:0.0,wavelength:0.0,exposure:0.0,note:"",scansize:LONARR(2),detectorsize:LONARR(2),scansampling:FLTARR(3),detectorsampling:FLTARR(3),issubset:0B,pixelmaskcorrection:0B,bitdepthfile:16}
  ;;
err=1                  ;; return err code 1 by default
success=0
isDECTRIS=0B
;; versions=0['1.4']
isvalidversion=1B
  PrintToCon, "% GetDectrisHDF5ImageMasterDescriptor: "+fn
  ; PrintToCon, "%     Parsing "+fn
  IF NOT(H5F_IS_HDF5(fn)) THEN BEGIN
     PrintToCon, "% Error: file format is not HDF5"
     return, ""
  END
  fileID = H5F_OPEN(fn)
  group='entry'
  gID=h5g_open(fileID, group) ;; id of top group 'entry'
  dgID=H5G_Open(gID,'data') 
  Nattr = H5A_GET_NUM_ATTRS(dgID)
  If (Nattr GE 1) THEN BEGIN
     aID = H5A_OPEN_IDX(dgID, 1) ;; get the link to the data - file/directory, e.g. STEM_Test_Lines5_data_000001.h5:///entry/data/data
     aName = H5A_READ(aID)       ;; attribute name
     ;; ### the code in the next 3 lines fails, reason unclear
     ;; objInfo = h5g_get_objinfo(Dgid, aName) ;;
     ;; STOP
     ;;Fileno=objInfo.FileNO
     ;;
     filelist=lh_h5_dump_group(dgID, level=0,  /objects) 
     ;;PrintToCon, "%   H5 Attribute 'data'"
     ;;PrintToCon, "%      Name  - "+ aName
     ;; linkval=h5g_get_linkval(ggID,aName) ;; can't get the
     ;; link value in IDL, User-defined link values are not supported
     ;; let's construct the link name from the master input filename
     ;; - strip off 'master.h5'
     PrintToCon, "%      Number of files  - "+ MyString(filelist.Count())
     startind=0L ;; these values are in the linked files, don't retrieve
     endind=0L
     ;; STOP
     l=list()
     For kk=1,filelist.Count() Do Begin
        linkval=fn.replace("master",(filelist[kk-1])('name'))
        ;; Get image_nr_high, image_nr_low
        l.Add, list(kk, linkval,startind,endind)
     END
     struct.pdatafiles=PTR_NEW(l)
     ;; data should be found in 
     H5a_CLOSE, aID
  END
  h5g_close, dgID ;; data group
  ;;
  ;; get detector info
  dgID=H5G_Open(gID,'instrument')
  ;; get info: l=lh_h5_dump_group(dgID, level=0,  /objects)
  detgID=H5G_Open(dgID,'detector')
  dID=H5D_OPEN(detgID,'description')
  Struct.device=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detgID,'countrate_correction_applied')
  ratecorrection=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detgID,'pixel_mask_applied')
  struct.pixelmaskcorrection=(H5D_Read(dID) EQ 1)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detgID,'sensor_material')
  sensor=MyString(H5D_Read(dID))
  H5D_CLOSE, dID
  ;;dID=H5D_OPEN(detgID,'sensor_thickness')
  ;;sensor=sensor+" "+MyString(H5D_Read(dID))
  ;;H5D_CLOSE, dID
  dID=H5D_OPEN(detgID,'bit_depth_readout')
  bitdepth=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detgID,'bit_depth_image')
  struct.bitdepthfile=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detgID,'count_time')
  Struct.exposure=H5D_Read(dID)
  H5D_CLOSE, dID
  ;; access detector specific data
  detspecgID=H5G_OPEN(detgID,'detectorSpecific')
  dID=H5D_OPEN(detspecgID,'nimages')
  struct.scansize[0]=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detspecgID,'ntrigger')
  struct.scansize[1]=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detspecgID,'photon_energy')
  struct.voltage=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detspecgID,'x_pixels_in_detector')
  struct.detectorsize[0]=Long(H5D_Read(dID))
  H5D_CLOSE, dID
  dID=H5D_OPEN(detspecgID,'y_pixels_in_detector')
  struct.detectorsize[1]=LONG(H5D_Read(dID))
  H5D_CLOSE, dID
  dID=H5D_OPEN(detspecgID,'data_collection_date')
  date=H5D_Read(dID)
  H5D_CLOSE, dID
  dID=H5D_OPEN(detspecgID,'compression')
  compression=H5D_Read(dID)
  H5D_CLOSE, dID
  h5g_close, detspecgID ;; detector specific group
  h5g_close, detgID     ;; detector group
  h5g_close, gID        ;; entry group
  h5f_close, fileID
  filelist=*(struct.pdatafiles)
  If (filelist.Count() LT 1) THEN BEGIN
    printtocon, "% GetDectrisHDF5ImageMasterDescriptor: Error, no data files??. Check HDF file. "
    return, -1 
  END
  PrintToCon, "%  Data file(s)         :  "+ (filelist[0])[1]
  for j=2,filelist.Count()  DO PrintToCon, "%                          "+ (filelist[j-1])[1]
  Printtocon, "%  Date                 :  "+ date
  PrintToCon, "%  Device               :  "+ struct.device
  PrintToCon, "%  Sensor               :  "+ sensor
  PrintToCon, "%  Frame Size           :  "+ MyString(struct.detectorsize[0]) +" x "+ MyString(struct.detectorsize[1])
  PrintToCon, "%  Scan Size            :  "+ MyString(struct.scansize[0]) +" x "+ MyString(struct.scansize[1])
  PrintToCon, "%  Voltage              :  "+ MyString(struct.voltage)
  PrintToCon, "%  Dwell Time           :  "+ MyString(struct.exposure)
  PrintToCon, "%  Bit Depth, readout   :  "+ MyString(bitdepth)
  PrintToCon, "%  Bit Depth, file      :  "+MyString(struct.bitdepthfile)
  PrintToCon, "%  Data compression     :  "+ MyString(compression)
  PrintToCon, "%  Count rate correction:  " + MyString(ratecorrection)
  PrintToCon, "%  Pixel mask correction:  " + MyString(struct.pixelmaskcorrection)
  return, struct
END 




PRO ReadDectrisData, fname, SWAPENDIAN=swapendian, NONINTERACTIVE=noninteractive, MAGNIFICATIONOVERWRITE=magnificationoverwrite, BIN=bin, READMRC=readmrc
 If NOT(GetDebugFlag()) THEN BEGIN
  CATCH, Error_status
 IF (Error_status NE 0) THEN BEGIN
     PrintToCon, "% ReadDectrisData:  Fatal error "
     PrintToCon, "%   Error status  - " + STRING(error_status)
     PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
     ErrMsg, !ERROR_STATE.MSG
     CATCH, /Cancel
     return
 END
END
 
 SplitPath, fname, path, filename
 suffix=STRSPLIT(filename, '.',/EXTRACT)
 len=SIZE(suffix,/N_ELEMENTS)
 suffix=suffix[len-1]
 sx=256 & sy= 256
 instrumentname="FEI Tecnai F20"
 voltage=200000.
 cameralength=0.2863
 qsamplingx=1.0
 qsamplingy=1.0
 samplingx=1.0
 samplingy=1.0
 unit='nm'
 qunit='1/nm'
 magnification=57000.
 fraction=1.0
 detx=192L & dety=192L
 binfact=1
 if keyword_set(bin) THEN binfact=bin
 ;;
 minv=[0]
 maxv=[5]
 mybar=obj_new('ProgressBar', MINV=minv,MAXV=maxv,TEXT=["      Loading Dectris File     "], STATUS="Processing master file ... ", FRAMESTYLE=2)
 ;;
 IF (strupcase(suffix) EQ 'H5') THEN BEGIN
    mybar->Set, 0, 0, TEXT=""
    mybar->SetStatus, "Reading master descriptor ..."
    h=GETDECTRISHDF5IMAGEMASTERDESCRIPTOR(fname)
    ;;
    ;; try data file to get dimensions properly
    printtocon, "% ReadDectrisData: Error retrieving magnification calibration for "+STRING(voltage)+"->"+STRING(magnification)+  "."  
    ;;
    ;;mybar->Set, 0, 1, TEXT=""
    ;;mybar->SetStatus, "Retrieving data dimensions ..."
    ;;s = H5_PARSE(h.name, /READ_DATA)
    ;;S_=s.ENTRY
    ;;s__=s_.DATA
    ;;s___=s__.DATA
    ;;datadim=s___._DIMENSIONS
    ;;datatype=s___._DATATYPE
    ;;STOP
    mybar->Set, 0, 1, TEXT=""
    mybar->SetStatus, "Processing meta data ..."
    ;;
    ;; struct={name:PTR_NEW,pathinfo:"",dimension:LonArr(3),sampling:FLTARR(3),offset:FLTARR(3),unit:STRARR(3),device:"",voltage:0.0,wavelength:0.0,exposure:0.0,note:"",scansize:LONARR(2),detectorsize:LONARR(2),scansampling:FLTARR(3),detectorsampling:FLTARR(3),issubset:0B}
    voltage=h.voltage
    sx=h.scansize[0]
    sy=h.scansize[1]
    detx=h.detectorsize[0]
    dety=h.detectorsize[1]
    qsamplingx=h.detectorsampling[0]
    qsamplingy=h.detectorsampling[1]
    samplingx=h.scansampling[0]
    samplingy=h.scansampling[1]
    rawdata=*(h.pdatafiles)
    TotFrames=LONG(h.scansize[0])*LONG(h.scansize[1]) ;; initial guess of totsize
 END
 ;; create parameter list for the user dialogue
 s=list()
 s.Add, {value:voltage,label:"Voltage ",newrow:0B}
 s.Add, {value:cameralength,label:"Camera length (m)",newrow:1B}
 s.Add, {value:magnification,label:"Magnification ",newrow:1B} 
 s.Add, {value:sx,label:"Scan size x, y",newrow:1B}
 s.Add, {value:sy,label:"",newrow:0B}
 s.Add, {value:fraction,label:"Fraction",newrow:0B}
 s.Add, {value:samplingx,label:"Scan sampling rate x, y",newrow:1B}
 s.Add, {value:samplingy,label:"",newrow:0B}
 s.Add, {value:unit,label:"Unit",newrow:0B}
 s.Add, {value:qsamplingx,label:"Detector sampling rate qx, qy",newrow:1B}
 s.Add, {value:qsamplingy,label:"",newrow:0B}
 s.Add, {value:qunit,label:"Unit",newrow:0B}
 s.Add, {value:binfact,label:"Binning factor for spatial dimensions upon loading",newrow:1B}
 c=list()
 c.Add, {value:0B,label:"Load decompressed data files as .mrc",newrow:0B}
 c.Add, {value:0B,label:"Use internal magnification calibration",newrow:1B}
 c.Add, {value:0B,label:"Use internal camera length calibration",newrow:1B}
 IF NOT(keyword_set(noninteractive)) THEN BEGIN
    IF NOT(XMDataChoiceField(s, c, TITLE="Edit Properties") EQ 1) THEN BEGIN
       printtocon, "% ReadDectrisData: Aborting."
       return
    END
 END ELSE BEGIN
    IF keyword_set(magnification_overwrite) THEN BEGIN
       c[1].value=1B
       s[2].value=magnification_overwrite
    END
 END
 voltage=s[0].value
 sx=s[3].value
 sy=s[4].value
 samplingx=s[6].value
 samplingy=s[7].value
 unit=s[8].value
 qsamplingx=s[9].value
 qsamplingy=s[10].value
 qunit=s[11].value
 cameralength=s[1].value
 magnification=s[2].value
 fraction=s[5].value
 binfact=FIX(s[12].value)
 if (c[0].value EQ 1) THEN readmrc=1
 ;;
 ;; Reading calibration data
 ;;
 mybar->Set, 0, 2, TEXT=""
 mybar->SetStatus, "Retrieving calibration data ..."
 if ((c[1].value EQ 1) OR (c[2].value EQ 1)) THEN Begin
    printtocon, "% ReadDectrisData: Accessing internal calibration table."
    mycal=GetCalibrationObj()
    IF OBJ_VALID(mycal) THEN BEGIN
       if (c[1].value EQ 1) THEN BEGIN
          fov=mycal->QuerySelectedCalibration(VALUE=float(magnification))
          IF (fov NE !NULL) THEN BEGIN
             samplingx=fraction*fov/sx*binfact ;;; missing fraction!
             samplingy=samplingx
             printtocon, "% ReadDectrisData: Setting magnification calibration"
             printtocon, "%    Sampling (x, y) = ("  +MyString(samplingx)+","+MyString(samplingy)+  ")"
             printtocon, "%    Field of view (fov_x, fov_y) = ("  +MyString(sx*samplingx)+","+MyString(sy*samplingy)+  ")"

          END  ELSE BEGIN
             printtocon, "% ReadDectrisData: Error retrieving magnification calibration."
             samplingx=1.
             samplingy=1.
          END
       END
       ;;
       if (c[2].value EQ 1) THEN BEGIN
          label=STRCOMPRESS(STRING(cameralength*1000),/REMOVE_ALL)
          ;; calqsampx=mycallist->Query(INSTRUMENT=instrumentname,DEVICE=h.device,MODE="STEM",CATEGORY="Cameralength",SECTION=voltage,LABEL=cameralength*1000,BIN=1.,DIMX=detx)
          fov=mycal->QuerySelectedCalibration(VALUE=float(magnification), /DIFFRACTION)
          IF (fov NE !NULL) THEN BEGIN
             qsamplingx=fov/detx
             qsamplingy=calqsampxqsamplingy
             printtocon, "% ReadDectrisData: Setting diffraction calibration."
             printtocon, "%    Sampling (qx, qy) = ("  +MyString(qsamplingx)+","+MyString(qsamplingy)+  ")"
             printtocon, "%    Field of view (fov_x, fov_y) = ("  +MyString(detx*qsamplingx)+","+MyString(dety*qsamplingy)+  ")"
          END  ELSE BEGIN
             printtocon, "% ReadDectrisData: Error retrieving camera length calibration."
             qsamplingx=1.
             qsamplingy=1.
          END
       END
       ;; STOP
    END ELSE BEGIN
       printtocon, "% ReadDectrisData: Calibration object not initialized."  
    END
 END
 
 ;;
 printtocon, "% "
 printtocon, "% ReadDectrisData: Reading data from datafiles."

 ;; Creating Dectris Object
 empaddata=obj_new('EMPADObj',fname,FRAMEX=detx,FRAMEY=dety)
 ;;
 IF (binfact GT 1) THEN empaddata.SetScanSize, (sx/binfact), (sy/binfact) ELSE empaddata.SetScanSize, sx, sy
 empaddata.SetScanSampling, samplingx*FLOAT(binfact), samplingy*float(binfact)
 empaddata.SetDetectorSampling, qsamplingx, qsamplingy
;; open dataset(s)
;; two stages: first get the start and end frame indices, then the
;; data

 val=LONG(2)^h.bitdepthfile-1
 ;; for pixelmask correction
 if not(keyword_set(readmrc)) then BEGIN
     TotFrames=0L
    ;; the following code opens the hdf5 data file for decompression
     For i=1,rawdata.Count() Do BEGIN
        s = H5_PARSE((rawdata[i-1])[1])
        s_=s.ENTRY
        s__=s_.DATA
        s___=s__.DATA
        s____=s___.IMAGE_NR_LOW
        (rawdata[i-1])[2]=s____._DATA
        s____=s___.IMAGE_NR_HIGH
        (rawdata[i-1])[3]=s____._DATA
        ;;datadim=s___._DIMENSIONS
        ;;datatype=s___._DATATYPE
        TotFrames=(rawdata[i-1])[3]  ;; TotFrames, hi-frame entry in last file
     END

  printtocon, "%  - Total number of frames: "+MyString(TotFrames)
  IF (binfact GT 1) THEN printtocon, "%  - Total number of frames (binned): "+MyString(TotFrames/binfact/binfact)
  detx=(h.detectorsize)[0]
  dety=(h.detectorsize)[1]
  IF (binfact GT 1) THEN cdata=UINTARR(detx,dety,TotFrames/binfact) ELSE cdata=UINTARR(detx,dety,TotFrames)
 ;; binning is in two stages: first along x, meaning that it is a simple rebinning along the 1D sequance
 ;; binning along y: later it requires reshuffling.
  For i=1,rawdata.Count() Do BEGIN
      printtocon, "%  "+(rawdata[i-1])[1]
      printtocon, "%  - frame range: "+MySTRING((rawdata[i-1])[2])+":"+MyString((rawdata[i-1])[3])
      i0=(rawdata[i-1])[2]-1 & i1=(rawdata[i-1])[3]-1
      if GetDebugFlag() THEN BEGIN
         printtocon, "% DEGBUG: Opening HDF handle for file #"+MyString(i) 
         printtocon, "% DEGBUG: Memavail="
      END
     fileID=H5F_OPEN((rawdata[i-1])[1])
     Gid=h5g_open(fileID, 'entry')
     dgID=h5g_open(gID, 'data')
     ds=H5D_open(dgID,'data') 
     result = H5D_GET_SPACE(ds)
     ;; H5S_SELECT_ELEMENTS, Dataspace_id, Coordinates, /RESET
     type = H5D_GET_TYPE(ds)
     stsize=H5D_GET_STORAGE_SIZE(ds)
     mybar->Set, 0, 4, TEXT="Deflating"
     mybar->SetStatus, "Deflating file "+MyString(i)+"/"+ MyString(rawdata.Count())+"..."
     IF (binfact GT 1)  THEN BEGIN
        A=TEMPORARY(mg_h5_getdata_getvariable(fileID, "/entry/data/data"))
        ;; printtocon, "%  - debug: Size(A)= "+STRING(SIZE(A))
        if (h.pixelmaskcorrection EQ 1) THEN BEGIN
           mybar->Set, 0, 5, TEXT=""
           mybar->SetStatus, "Fixing pixel mask ..."
           B=WHERE(A EQ val, count)
           IF (count GE 1) THEN A[B]=0
        END 
        mybar->Set, 0, 5, TEXT=""
        mybar->SetStatus, "Binning stage 1 ("+MyString(i)+"/"+ MyString(rawdata.Count())+") ..."
        i0bin=i0/binfact & i1bin=(((i1-i0)+1)/binfact)+i0bin-1
        printtocon, "%  - frame range (binned): "+MySTRING(i0bin)+":"+MyString(i1bin)
        cdata[*,*,i0bin:i1bin]=Rebin(binfact*A, detx, dety,(i1bin-i0bin+1),/SAMPLE)
     END ELSE BEGIN
        cdata[*,*,i0:i1]=mg_h5_getdata_getvariable(fileID, "/entry/data/data")
        ;; Fix pixelmask correction
        if (h.pixelmaskcorrection EQ 1) THEN BEGIN
           mybar->Set, 0, 5, TEXT=""
           mybar->SetStatus, "Fixing pixel mask ..."
           B=WHERE(cdata[*,*,i0:i1] EQ val, count)
           IF (count GE 1) THEN cdata[B]=0
        END
     END
     H5G_close, dgID
     H5G_close, gID
     H5F_close, fileID
  END
END ELSE BEGIN
   ;; Read from mrc
   ;; the total number of frames
   printtocon, "%  - Total number of frames: "+MyString(TotFrames)
   IF (binfact GT 1) THEN printtocon, "%  - Total number of frames (binned): "+MyString(TotFrames/binfact/binfact)
   detx=(h.detectorsize)[0]
   dety=(h.detectorsize)[1]
   IF (binfact GT 1) THEN cdata=UINTARR(detx,dety,TotFrames/binfact) ELSE cdata=UINTARR(detx,dety,TotFrames)
    ;; the following code opens previously uncompressed  mrc data files
   printtocon, "% ReadDectrisData: Reading data from mrc datafiles."
   i0=0L
     For i=1,rawdata.Count() Do BEGIN
        fn=(rawdata[i-1])[1]
        fname=fn.replace('.h5','.mrc')
        mybar->Set, 0, 4, TEXT="Importing"
        mybar->SetStatus, "Importing mrc data file "+MyString(i)+"/"+ MyString(rawdata.Count())+"..."
        ;; get number of frames
        A=(ReadMRCDataToArray(fname))
        sz=SIZE(*A)
        if (h.pixelmaskcorrection EQ 1) THEN BEGIN
           mybar->Set, 0, 5, TEXT=""
           mybar->SetStatus, "Fixing pixel mask ..."
           B=WHERE((*A) EQ val, count)
           IF (count GE 1) THEN (*A)[B]=0
        END   
        i1=i0+sz[3]-1
        IF (binfact GT 1)  THEN BEGIN
           mybar->Set, 0, 5, TEXT=""
           mybar->SetStatus, "Binning stage 1 ("+MyString(i)+"/"+ MyString(rawdata.Count())+") ..."
           i0bin=i0/binfact & i1bin=(((i1-i0)+1)/binfact)+i0bin-1
           printtocon, "%  frame range         : "+MySTRING(i0)+":"+MyString(i1)
           printtocon, "%  frame range (binned): "+MySTRING(i0bin)+":"+MyString(i1bin)
           cdata[*,*,i0bin:i1bin]=Rebin(binfact*(*A), detx, dety,(i1bin-i0bin+1),/SAMPLE)
     END  ELSE BEGIN
           printtocon, "%  frame range         : "+MySTRING(i0)+":"+MyString(i1)
           cdata[*,*,i0:i1]=*A
        END
        i0=i1+1
     END
  END  
     ;;
  ;;
  ;; STOP
  IF (binfact GT 1) THEN BEGIN
     ncols=(sx/binfact)
     nrows=(sy/binfact)
     ;; printtocon, "% debug: -  target columns/rows "+ MyString(nrows) + ", "+  MyString(ncols)
     ;; printtocon, "% debug: Size(cdata)= "+STRING(SIZE(cdata))
     mybar->ReInit, 0, 0, nrows
     mybar->SetStatus, "Binning stage 2 ..."
     ;; here we add rows in cdata over the last dimension
     ;; bdata=UINTARR(detx,dety,TotFrames/binfact/binfact)
     For j0=0,nrows-1 DO BEGIN
        ;; loop over rows
        ;; we want to add sequential rows,
        ;; their indices in the one-dimensional scheme are
        ;; (j0*binfact+0)*ncols:((j0*binfact+1)*ncols-1)
        ;; (j0*binfact+1)*ncols:((j0*binfact+2)*ncols-1)
        ;; ....
        ;; (j0*binfact+i)*ncols:((j0*binfact+i+1)*ncols-1) wher j0=row in binned frame, i=0 ..binfact
        mybar->Set, 0, j0+1, TEXT=STRING(j0)
        i0=j0*binfact*ncols ;; starting row element for binning, not that the target row is j0 and the starting element is j0*ncols
        cdata[*,*,(j0*ncols):((j0+1)*ncols-1)]=cdata[*,*,i0:(i0+ncols-1)]
        ;;printtocon, "% debug: -  row index "+ MyString(i0) + " -> "+  MyString((j0*ncols)) +" in binned scheme"
        For i=2,binfact Do Begin
           ;; printtocon, "%        adding frame index range "+ MyString((i0+(i-1)*ncols)) + " -> "+ MyString((i0+i*ncols-1)) +" to binned row"
           ;;printtocon, "%                              at "+ MyString((j0*ncols)) + " -> "+ MyString(((j0+1)*ncols-1))
           cdata[*,*,(j0*ncols):((j0+1)*ncols-1)]+=cdata[*,*,(i0+(i-1)*ncols):(i0+i*ncols-1)]
        END 
     END
  cdata=cdata[*,*,0:(TotFrames/binfact/binfact-1)]
  END
  ;;
  mybar->ReInit, 0, 0, 5, TEXT=""
  mybar->Set, 0, 5, TEXT=""
  mybar->SetStatus, "Finalizing ..."
  ptr=GetRootP() ;; datalistpointer
  names=DataList_GetNames(ptr)
  Selected=WHERE(names EQ fname)
  IF (Selected EQ -1) THEN BEGIN
     e=DataList_CreateElement(ptr, fname)
  END ELSE BEGIN
     i=2
     REPEAT BEGIN
       altname=fname+ "" + MyString(i)
       Selected=WHERE(names EQ altname)
       i=i+1
     END UNTIL (Selected EQ -1)
     e=DataList_CreateElement(ptr, altname)
  END     
;; data array is in data
            dtype=SIZE(cdata,/TYPE)
            IF ((dtype EQ 6) OR (dtype EQ 9)) THEN BEGIN
               ;; complex data, probably needs a fix, not yet
               ;; supported
               ;; code should be like in Read_hdf in IO_hdf.pro
               (*e).type=dtype
               (*e).data=PTR_NEW(cdata)
               printtocon, "% Read_DECTRIS: Complex data, not yet supported."
             END ELSE  BEGIN
               (*e).data=PTR_NEW(cdata)
               (*e).type=SIZE(cdata,/TYPE)
               dim=SIZE(cdata, /DIMENSION)
            END
  empaddata.SetDataPointer, (*e).data
  IF PTR_VALID(e) THEN BEGIN
     (*e).data=empaddata.GetDataPointer()
     IF (PTR_VALID((*e).data)) THEN BEGIN
        bin=0.5
        (*e).id=empaddata.GetSetName()
        (*e).SzX=empaddata.GetFrameSize(/X)
        (*e).SzY=empaddata.GetFrameSize(/Y)
        ;; (*e).SzZ=empaddata.GetScanSize(/X)*empaddata.GetScanSize(/Y)
        (*e).SzZ=Long(dim[2])
        (*e).xsamp=qsamplingx
        (*e).ysamp=qsamplingy
        (*e).xunit=qunit
        (*e).yunit=qunit
        (*e).voltage=voltage
        (*e).id=fname
        (*e).type=4
        (*e).slice=FIX((*e).SzZ/2)
        (*e).zcoord=3
        (*e).extra=empaddata
        (*e).contrastmode="auto"
        (*e).contrastsubmode="minmax" 
        (*e).contrastroi="diff"
        (*e).binning=bin
        (*e).binningx=bin
        (*e).binningy=bin
        CreateWindow, BIN=bin
        TVDisplay
        Update_XTabControl
     End
  END
  obj_destroy, mybar
END



