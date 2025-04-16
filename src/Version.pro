FUNCTION GetApplicationName
return, 'PadTools'
END

FUNCTION GetApplicationCode
  appcode='6442DA3'
  return, appcode
END

FUNCTION GetApplicationVersion
  return, '2.1.1'
END

FUNCTION GetApplicationDate
  return, "March 2025"
END

Function GetVersion, ASFLOAT=asfloat
  majorversion=2.1
  minorversion=1
  if keyword_set(asfloat) THEN return, majorversion
return, "v2.1.1 build 250318"
END

PRO News
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% NEWS:    Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ErrMsg, !ERROR_STATE.MSG
    CATCH, /Cancel
    return
END   
s=[" ","         +++ PadTools NEWS +++        ", "", $
   "          Version: "+GetVersion(), $
   "          (c) L. Houben          ", $
   "          Mailto: lothar.houben@weizmann.ac.il          ", $
   "", $
   "          2.1.1 - Bug fix in line profile tools.", $
   "          2.1.0 - Rewritten line profile tools. The tool now creates a stack holing the diffraction frames", $
   "                  along a line. Analysis->uDiffraction->ROIDiffraction->Line Profile",$
   "          2.0.1 - Pixel correction for Dectris max_uint16 values with automatic detection on ten random",$
   "                  frames under Transformations->Corrections-> Dectris->maxuint16.", $
   "          2.0.0 - Major revision of the calibration handling. A calibration table is selected from a set  ", $
   "                  of tables that are defined in the json calibration data file in the home directory. The ",$
   "                  table is selected under Tools->Calibration->Select Calibration. It is the active table",$
   "                  until another table is chosen. Use Tools->Calibrate->Calibrate Data to apply the",$
   "                  calibration data to the active data set in the Data Inspector. The active calibration", $
   "                  table is also applied when loading specifc data formats.", $
   "          1.9.0 - Revision of the file format for PadTools. HDF5 files are suffering from slow", $
   "                  performance in particular when large files are loaded. The new format for PadTools ", $
   "                  writes data to a standard MRC file and a header file in JSON format. The MRC file ", $
   "                  is created automatically when saving to PadTools Data. Choose the header file name", $
   "                  when writing data to disk. The default suffix is .ptd. When loading the data make ", $
   "                  sure that the MRC file is in the same directory as the header file. Load the header ", $
   "                  file, it has a reference to the MRC data file. Keep header and MRC file together.", $
   "          1.8.2 - Bug fixes for sliding FFT routine, added Hanning window and padding options.", $
   "          1.8.1 - Added Digital Micrograph file import under File -> Open, for single image data.", $
   "                - Added a sliding FFT routine to create a false 4D STEM data set from FFTs under ", $
   "                  Transformations -> Image Transformations -> Sliding FFT", $
   "          1.8.0 - Added mrc data file import for Dectris data.", $
   "                  This hack is to bypass disfunctional standard HDF plugin filters on various platforms.", $
   "                  Loading starts with the master file *_master.h5, data is loaded from uncomressed",$
   "                  *_data_0000??.mrc files instead of compressed .h5 data.",$
   "                  Decompress data files with external utilities to provide the mrc files.", $
   "                - Bug fix for slice selection in peak detection procedure.", $
   "                - Subtraction of rotational average in diffraction frames for PAD data. Choose ", $
   "                  Transformations -> Corrections -> Subtract Rotational Average", $
   "          1.7.9 - Added shear distortion correction in the Cluster Analysis from a peak search list.", $
   "          1.7.8 - Added elliptical distortion correction in the Cluster Analysis from a peak search list.", $
   "          1.7.7 - The peak frame center refinement in the Cluster Analysis from a peak search list", $
   "                  can now be restricted to the peak closest to the nominal frame center. The ", $
   "                  default behavior is the refinement to the center of the strongest peak, which ", $
   "                  may fail in case of strong dynamic scattering. ", $
   "          1.7.6 - Added fit constraints to peak refinement in the peak search procedures.", $
   "                  Added improved filter options for the peak search procedures.", $
   "          1.7.5 - Added export of peakdata per diffraction frame into a json string.", $
   "                  The peaklist can be used as input for automated orientation mapping.", $
   "                  Analysis -> uDiffraction -> Peak Search and Analysis -> Export Frame Preak Data.", $
   "                  Added a routibe to merge single frames of identical dimension to a stack. The function", $
   "                  Merge Frames to Stack is accessible with the context menu in the Data Insepctor. ", $
   "          1.7.4 - Fixed bugs occuring when creating peak orientation maps.", $
   "          1.7.3 - Added virtual objective aperture under Analysis -> Virtual Detector.", $
   "          1.7.2 - Fixed the angular argument output of the Peak table evaluation to radians units.", $
   "          1.7.1 - Peak table evaluation:Template correlation map. Analyzes scale and rotation correlation with respect", $
   "                  to a reference pattern frame by frame.", $
   "                  Analysis -> Phase and Orientation Map -> Template Scale-Rotation Correlation Map.", $ 
   "          1.7.0 - Peak table evaluation:", $
   "                  - Debye Scherrer Plot under Analysis -> uDiffraction ->  Peak Search and Analysis -> Debye Scherrer Plot", $
   "                  - Powder Pattern Plot under Analysis -> uDiffraction ->  Peak Search and Analysis -> Powder Pattern Plot", $
   "                  The current selection in the Tables tab has to be a table output after peak search.", $
   "                  The table can be loaded from a file containing the peak data, saved after a Peak Search.", $   
   "          1.6.3 - Bug fixed in orientation map creation after Peak Search.", $
   "          1.6.2 - Modified algorithm for spatial averaging.", $
   "                  Transformation by formula expression under Transformations -> Transformation by Expression. ",$
   "          1.6.1 - Bug fix in type selection in raw data import, 2D and 3D.", $
   "          1.6.0 - Revision of circular color map coding, use of color tables is supported now to replace  HSV if selected.", $
   "                  See Tools -> Visualisation. Use cyclic color maps here. Among the standard color maps PRISM will work.", $
   "          1.5.3 - Bug fixes for for rotation correction on PAD data. Nicer statistics output for NBED shift correction.", $
   "          1.5.2 - Bug fixes for segment mask creation. Fixes issues with shifted masks and single segment creation", $
   "          1.5.1 - Updated strain mapping routines.", $
   "          1.5.0 - Added spatial binning during the loading stage to reduce memory greed for big DECTRIS data.", $
   "          1.4.7 - Fixed a bug in the stack duplicate function.", $
   "                  Re-arranged PAD meta data context menu in the Data Inspector, ", $
   "                  and added new functions to load and save meta data from and to JSON files. ", $
   "          1.4.6 - Fixed a bug that happened when reading DECTRIS metadata.", $
   "                  Fixed a bug around window scaling in ROI Diffraction.", $
   "                  Fix for log scale display in ROI Diffraction.", $
   "                  Added calibration data for Arina detector.", $
   "                  Changed binning determination for the time a window is created.", $
   "                  Fixed a bug for the Sparse Event Diffraction routine, it wasn't invoked from the UI.", $
   "          1.4.5 - Angular Map for color legend display for circular color tables, such  ", $
   "                  as hsv or the preceptual color tables CET-C.., Tools -> Visualisation -> ",$
   "                  Angular Map (Color Wheel).", $
   "          1.4.4 - Simulation of sparse mode diffraction data, Transformation -> Simulation ->  ", $
   "                  Sparse Event Diffraction.", $
   "                  Cropping function for PAD data sets, Transformation -> Volume Transformations -> ", $
   "                  Crop Diffraction Frames.", $
   "          1.4.3 - Fixed a bug in MRC reading procedure, long integer data type for ", $
   "                  large data set dimension are suported now. ", $
   "                  Segment filter mask for diffraction slices. See Analysis->Filter.", $ 
   "          1.4.2 - Unique auto-identification for Dectris/PadTools hdf5 files. ", $
   "          1.4.1 - Import large set of images in tif, png, jpeg .. 1 channel images. ", $
   "                  File -> Import Data -> Import Large Set", $
   "          1.4.0 - Bug fixes for auto contrast in Macros.pro. ", $
   "          1.3.1 - Bug fixes for contrast manipulation and contrast inspector dialogue. ", $
   "                  Bug fixes for crashing peak search user upon user interrupt. ", $
   "          1.3.0 - Modifications to the peak serach: Includes median filtering now, displays the filtered images in live mode. ", $
   "                  Includes orientation analysis based on the strongest reflection now as well.", $
   "          1.2.9 - Bug fix for magnification overwrite for EMPAD data.", $
   "          1.2.8 - Bug fix for camera length and magnification overwrite for EMPAD data.", $
   "                  Center-of-mass map output for Peak Mapping.", $
   "          1.2.7 - Upscaling option for diffraction frames. Transformations -> Volume Transformations ", $
   "                  -> Upscaling Diffraction Frames.", $
   "          1.2.6 - Bug fix in segment detector calculation. The bug appeared for non-square detector arrays.", $
   "          1.2.5 - Allowing for anisotropic sampling with virtual detectors. Adjust the pixel", $
   "                  radii of the detectors according to the detector sampling in x-direction.", $
   "          1.2.4 - Bug fixes. ", $
   "          1.2.3 - Magnification and Camera Length overwrite in Directory recursion macros", $
   "                  New scheme: Prepare a directory recursion file list and  table with magnification and", $
   "                    Camera length data under Macros -> Directory Recursion -> Prepare File List. Save this", $
   "                    table to a text file, the reload the file when starting the macro.", $
   "          1.2.2 - Magnification and Camera Length overwrite in Directory recursion macros", $
   "                  Directory recursion macro for BF, ADF and ROI diffraction.", $
   "          1.2.1 - Improved region of interest difffraction feature ", $
   "                  - added scale bar option ", $
   "                  - added log transformation option ", $
   "                  - added rotation compensation option ", $
   "                  - added autosave to file option ", $
   "          1.2.0 - Bug fixes for distortion correction. ", $
   "                  Multiple data file reading for DECTRIS Eiger format. ", $
   "                  Convert data stack to PAD Object (3D stack to 3D with shape information)", $
   "                  Reshaping option for spatial pixel array under Transformations->VolumeTransformations", $
   "                  Correction for real pixel display when using markers.", $
   "                  Simple diffraction shift under Transformations -> Corrections -> Pick center.", $
   "          1.1.0 - Directory recursion macros for ", $
   "                  - ADF and BF images.", $
      "               - Nanodiffraction Peak search.", $
   "          1.0.1 - Bug fix for spatial binning.", $
   "                  Added Threshold cut-off under Transformations -> Corrections -> Threshold Cut-Off. ", $
   "          1.0.0 - Import for Dectris hdf5 data.", $
   "                  Added hot pixel correction under Transformations-> Corrections -> Hot Pixel.", $
   "          0.9.8 - Bug fix in calibration list object.", $
   "          0.9.7 - Shear distortion correction in radial profile export.", $
   "          0.9.6 - Create a binary mask from an image. Transformations -> Mask or Filter -> Create Binary Mask from Image.", $
   "          0.9.5 - ROI Diffraction now creates a cumulative binary mask image.", $
   "                  Binary mask cropping, under Transformations -> Cropping -> Binary Mask Cropping", $ 
   "          0.9.4 - Radial Profiles are now included in the Menu, under Transformations -> Coordinate Transformations -> Radial Profile.", $ 
   "          0.9.3 - Contrast Inspector remains active during the uDiffraction ROI mapping. Same for the Circular Marker.", $ 
   "          0.9.2 - Rewritten routines for azimuthal average (radial plot). The routines avoid any interpolation and", $
   "                  the correction for elliptical distortion is included. See DistortionCorrection.pro.Yet to be included in Menu.", $
   "          0.9.1 - Sequence average slider. Sequential averaging in animation and animation export.", $
   "          0.9.0 - Peak search in diffraction data reviewed. Positions and distances to the transmitted beam are refined", $
   "                  through a gaussian peak fit of the centroid. Analysis ->  uDiffraction -> Peak Search.", $
   "                  Reviewed fluctuation figure: the program now returns the variance after normalizing the total annular ", $
   "                  patch intensity to 1.", $
   "                  Added entropy map as a measure of diffraction fluctuation, entropy is lowest for ordered material.", $
   "          0.8.0 - Adaptations for idl 8.8.1 and Apple Silicon.", $
   "          0.7.3 - Added spatial binning under Transformations -> Sptial Binning.", $
   "          0.7.2 - Orientation map based on angular correlation over a ring segment. Analysis -> Orientation Maps -> ", $
   "                  Correlation Orientation map. (NOT FUNCTIONAL)", $
   "          0.7.1 - Added peak-to-peak separation measurement under Analysis -> Measure -> Peak-to-Peak Distances.", $
   "          0.7.0 - Added polar to rectangular coordinate transform for azimuthal averaging.", $
   "                  Available under Transformations -> Coordinate Transforms -> Polare to Cartesian", $
   "                  The routine works for EMPAD data sets and for ROI Diffraction images.", $
   "                  Added peak search under Analysis ->  uDiffraction -> Peak Search.", $
   "          0.6.5 - Added contrast scaling options in the Contrast Inspector.", $
   "          0.6.4 - Added orientation map function under Analysis -> Weighted Detectors.", $
   "                  The routine returns the tilt of the diffraction intensities (COM)", $
   "                  on a detection ring in magnitude and direction.", $
   "          0.6.3 - Bug fix for endless loop in ROI selection.", $
   "                  Corection for the rotation of a NBED with respect to an EMPAD real space frame, Tools -> EMPAD Fix for Diffraction Rotation.", $
   "          0.6.2 - Bug fix in live update of the ROI diffraction.", $
   "          0.6.1 - Diffraction disc centering under Transformations -> Corrections -> Ronchigram shift.", $
   "                  Menues reorganized.", $
   "                  Macro processing for automation.", $
   "                  Angular intensity fluctuation mapping under Analysis -> uDiffraction -> Fluctuation and Order ", $
   "                  lets you create a map of the azimuth angle of Friedel pairs of reflections. For colormap display ", $
   "                  map can be coded in color-indices 1-255 to represent the angle between 0 and 360/m where m is the", $
   "                  symmetry.", $
   "          0.6.0 - Live update feature for ROI diffraction.", $
   "          0.5.1 - Series import added. The metadata in the xml files is incomplete for time series. You need ", $
   "                  to add the camera length and high-tension information!", $
   "                  Series subranges can be extracted with Series -> Slice.", $
   "          0.5.0 - HDF import and export completed. The HDF format works for single images, data stacks,", $
   "                  EMPAD data and EMPAD subset data.", $
   "                - Automatic type suggestion for file import.", $
   "          0.4.3 - Repeated readout for ROI uDiffraction without re-opening of the UI dialog window.", $
   "                - Radial and angular fluctuation maps under uDiffraction -> Fluctuation and Order.",$
   "                - Beam stop mask under Corrections->Beamstop.",$
   "                - Sequence of circular annular detectors under Virtual Detectors.", $
   "          0.4.2 - Load mrc data files with EMPAD frame sizes as EMPAD data.", $
   "          0.4.1 - Bug fix in calibration data object. Calibration data should load fine from internal DB.", $
   "                  Added font selection for scale bar label export in png in the preferences file under [Font].", $
   "                  Possible options are 'helvetica', 'helvetica*bold', 'times*bold', 'Helvetica*Bold*Italic' etc.", $
   "                  Fixed bug in display export that left the graphics display object on screen after expeort. ", $ 
   "          0.4.0 - Preliminary reverse index histogram routines implemented. The plot the signal of multiple detectors", $
   "                  in a multidimensional scatter plot. These can be used to extract regions of thresholded signal and ", $
   "                  locate their spatial origin. You'll find it under Virtual Detectors ->  Segmentation. ", $
   "          0.3.0 - Major overhaul of color table support. Multiple table files are now supported.", $
   "                  You can edit, append, insert, delete entries in a color table file. Color tables ", $
   "                  can now be imported as RGB columns from .csv files. Multiple .csv files can be ", $
   "                  imported into the current list or into a new colortable file. New colortable  ", $
   "                  files are provided, containing for instance matplotlib and Mathematica colortables,", $
   "                  colortables for the color blind, perceptual colors and cyclic color maps.", $
   "                  Pseudocolor tables are now memorized and the last table used will be loaded upon ", $
   "                  program start. ", $
   "          0.2.3 - Corrections to the COM algorithm. Corrections to the diffraction shift corrction. ", $
   "                  Fixed a bug in the ROI diffraction routine.", $
   "          0.2.2 - Added calibration table support. Calibrations are saved in a common file in your home directory or", $
   "                  for system-wide access in the installation directory. The name of the calibration file is ", $
   "                  MicroscopeCalibrationDB.pref. See the empadTools documentation for a description of the format. ", $
   "                - Fixed reading capability from xml descriptor in Virtual Machine. ", $
   "                - Fixed missing setup dialog for the desktop directory. ", $
   "          0.2.1 - Added Angular Segment Detectors under Virtual detectors.", $
   "                  Added Diffraction Shift Correction under Corrections.", $
   "          0.2.0 - Reorganized menues.", $
   "                  Centre of Mass calculation.", $
   "                  Region of interest picking for accumulated diffraction data.", $
   "          0.1.1 - Fixed a bug in the virtual DF analysis related to Empad rows 129, 130.", $
   "          0.1.0 - Scale bar implementation. Use right mouse click on a data set and choose 'Dislay Scale Bar' from the menu.", $
   "                - Display export to png fixed (Known issue: contrast scaling bug). ", $
   "                - Import of xml descriptor: choose xml dscriptor inseat of raw data file in the file import dialog.", $
   "                  This will help you to read further parameters such as the sampling rate. You can still load a ", $
    "                 raw data file in case no xml file is available or the file information in the xml file is incorrect.", $
   "                - Alignment to maximum only under uDiff -> Align Empad Frame Stack.", $
   "          0.0.1 - First GUI implementation.", $
  ""]
res=Dialog_Info(s, /INFO)
END
