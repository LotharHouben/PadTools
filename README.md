# PadTools - IDL program to process 4D STEM data 

L. Houben, Weizmann Institute of Science  
lothar.houben@weizmann.ac.il  



PadTools is a GUI-based program for 4D STEM electron microscopy data


<bf>
<bf>
    


## Installation

System Requirements

The compiled version of PadTools requires the presence of the IDL virtual machine v8.8 or higher. The IDL virtual machine itself is freely available from the IDL website. IDL is a cross-plattform application and can be installed on the following operating systems: MAC OS X, Windows, PC- Unix (LinuX) and Unix. 

Download and Installation of PadTools

Copy PadTools from the release folder. Copy the entire release/PadTools folder into an appropriate installation directory. On Windows this is preferably the system-wide ‘Program Files’ folder, on Mac OS X the system-wide ‘Applications’ folder.

On other Unix variants there are a few more steps to take: copy the PadTools folder to an installation directory of your choice (e.g. ‘/usr/local’ and Create a script named "PadTools" containing the following line: 
idl -vm=/INSTDIR/PadTools/PadTools.sav 
where INSTDIR is the installation path. 
Make the script executable ( ́sudo chmod a+x PadTools ́) and copy it to a binary directory, e.g. ‘/usr/ local/bin’. 


PadTools stores parameters in a preference file named PadTools.pref. E.g. recently visited directories are stored in this file. On a multiuser system each user can have his own preference file, located in the HOME directory. On unix systems, the HOME directory is defined and the value, i.e. the actual directory, can be retrieved from the $HOME environment variable. On Windows, this environment variable does not exist by default. Please follow the steps below to set your $HOME environment variable under Windows. 
Once you have a $HOME directory set, copy the system-wide preferences file, which is located in the installation directory of PadTools, to the HOME directory and restart PadTools. It should launch without an error message. The user preference file will be updated upon closing the program, the file needs to be writeable. 
Microscope and detector calibration data are stored in a file MicroscopeCalibrationDB.json. Copy the default file from the PadTools installation directory in your HOME directory. MicroscopeCalibrationDB.json contains JSON-encoded information for the calibration data. Currently there is no user interface for editing the data, please use a standard JSON editor to edit the file according to your system calibrations. 


## Program Start

On Windows systems: Double-click on the file PadTools.sav. 
On Mac OS X: Double-click on the supplied application icon. You can drag the icon onto the dock to keep it in the dock. 
On Other Unix systems: first check the DISPLAY environment variable and the access permissions to the client X-server, then type ́PadTools ́ on the command line.


## Documentation

Please refer to the documentation in the doc/ folder


## COMPILING FROM SOURCES

PadToos can be compiled from an idl shell using the command @batch.idl

see under LICENSES below for information about modules that need to be downloaded from external sources 


## LICENSES

PadTools is free software distributed under the MIT license (see LICENSE.txt)


PadTools requires external modules to compile from source, place the modules into the source tree: 

- MPFIT IDL package written by Craig Markwardt, http://cow.physics.wisc.edu/~craigm/idl/fitqa.html#image

  required modules: mpfit.pro, mpfitfun.pro, mpfit2dfun.pro, mpcurvefit.pro, mpfit2dpeak.pro 

- hdf5 routines in Michael Galloy's mglib, Copyright (c) 2007-2010, Michael Galloy <mgalloy@idldev.com>, 
   released under a BSD-type license. See https://github.com/mgalloy/mglib/blob/master/LICENSE

- modules for peak/particle finding (feature.pro, bpass.pro), These modules are license under GPLv3, https://www.gnu.org/licenses/gpl-3.0.html, please obtain them from https://github.com/davidgrier/features

- a file dialog rm_pickfile.pro by Robert M. Dimeo,, modified in lh_pickfile, from the header of rm_pickfile: 

" The software in this file is written by an employee of
  National Institute of Standards and Technology
  as part of the DAVE software project.

  The DAVE software package is not subject to copyright
  protection and is in the public domain. It should be
  considered as an experimental neutron scattering data
  reduction, visualization, and analysis system. As such,
  the authors assume no responsibility whatsoever for its
  use, and make no guarantees, expressed or implied,
  about its quality, reliability, or any other
  characteristic. The use of certain trade names or commercial
  products does not imply any endorsement of a particular
  product, nor does it imply that the named product is
  necessarily the best product for the stated purpose.
  We would appreciate acknowledgment if the DAVE software
  is used or if the code in this file is included in another
  product."

- colormap dialogs from the Coyote Library by D.J. Fanning, https://github.com/idl-coyote/coyote, licensed under the BSD Open Source License:

;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2012, by Fanning Software Consulting, Inc. All rights reserved.           ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
