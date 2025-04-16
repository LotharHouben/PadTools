FUNCTION CycleWorkingDir
COMMON WDIR, dirlist
Nlist=N_Elements(dirlist)
m=1
dir='.'
REPEAT BEGIN
    tmp=dirlist[0]
    For k=0,(Nlist-2) DO dirlist[k]=dirlist[k+1]
    dirlist[Nlist-1]=tmp
;; check whether dirlist[0] exists
    if file_test(dirlist[0],/directory) then BEGIN
        return, dirlist[0]
    END ELSE BEGIN
        printtocon, "% CycleWorkingDir: warning, the directory "+dirlist[0]+" does not exist"
    END
    m=m+1
END UNTIL (m GT Nlist)
return, dir
END

; $Id: rmd_pickfile.pro,v 1.1 2005/02/15 21:11:53 rdimeo Exp $
; ************************************************ ;
;
; NAME:
;  RMD_PICKFILE
;
; PURPOSE:
;  File selection dialog that provides additional
;  functionality beyond that provided by DIALOG_PICKFILE
;  and CW_FILESEL.  Note that this cannot be used for writing
;  files.  This is a modal widget.  The return value is a
;  scalar array or string array of fully path-qualified filenames.
;
;  User single-clicks on a file to select it.  User double-clicks
;  on a directory to go into it.
;
; CATEGORY:
;  DAVE, objects, widgets, file utilities
;
; CALLING SEQUENCE:
;  files = RMD_PICKFILE( GROUP_LEADER = group_leader,  $
;                        FILTER_IN = filter_in,        $
;                        FILTER_OUT = filter_out,      $
;                        MULTIPLE = multiple,          $
;                        PATH = path,                  $
;                        GET_PATH = get_path,          $
;                        TITLE = title,                $
;                        TYPELIST = typelist,          $
;                        CANCELLED = cancelled         )
;
; KEYWORDS:
;  GROUP_LEADER:  group leader for the widget (optional input)
;  FILTER_IN:Set this keyword to a string value or an array of
;           strings specifying the file types to be displayed
;           in the file list. This keyword is used to reduce
;           the number of files displayed in the file list.
;           (optional input)
;  FILTER_OUT:
;           On output, if the OPEN button is pressed, the final
;           filter (possibly determined by the user) is returned.
;           (optional output)
;  MULTIPLE: Set this keyword so that the user can accept multiple
;            files. (optional input)
;  PATH:     Set this keyword to a scalar string specifying the
;            directory in which to perform the file selection.
;            (optional input)
;  GET_PATH: Set this keyword to a named variable in which the
;            path of the selection is returned. (optional output)
;  TITLE:    Name to be displayed at the top of the dialog
;            (optional input)
;  TYPELIST: PTR to an array with file types, appears as a droplist
;  TYPE:     currently selected index in typelist 
;  SWAPEN:   Swap Endian Button (only for /OPEN)
;  SUFFIXIDENT:   Identify type by suffix 
;  CRGRP:    Create Group button
;            (/OPEN -- create a group)
;            (/SAVE -- save all images in sequence)
;  OPEN:     present a file open dialog
;  SAVE:     present a save dialog
;  DIREC:    use dialog for directory selection
;  CANCELLED:Output keyword that indicates if the user pressed the
;            "CANCEL" button.
;
; AUTHOR:
;   Robert M. Dimeo, Ph.D.
;   NIST Center for Neutron Research
;   100 Bureau Drive
;   Gaithersburg, MD 20899
;   Phone: (301) 975-8135
;   E-mail: robert.dimeo@nist.gov
;   http://www.ncnr.nist.gov/staff/dimeo
;
; COMMON BLOCKS:
;  None
;
; REQUIRED PROGRAMS:
;  None
;
; REQUIREMENTS:
;  IDL 6.1 and higher
;
; EXAMPLE USAGE:
;  See the code appended to the end of this listing named TEST_PF.
;
; MODIFICATION HISTORY:
;  -Written 02/10/05 (RMD)
;  -Replaced list widget with a tree widget (02/12/05)
;  -Made some cosmetic improvements in widget layout (02/15/05)
;  -Replaced the FILTER keyword with FILTER_IN and FILTER_OUT
;   to make it clear that one is input and one is output (02/15/05)
;  -Added a checkbox to list the files in reverse alphabetical
;   order (02/15/05)
;  -Added drive selection ability using a droplist widget (02/15/05)
;
; LICENSE:
;  The software in this file is written by an employee of
;  National Institute of Standards and Technology
;  as part of the DAVE software project.
;
;  The DAVE software package is not subject to copyright
;  protection and is in the public domain. It should be
;  considered as an experimental neutron scattering data
;  reduction, visualization, and analysis system. As such,
;  the authors assume no responsibility whatsoever for its
;  use, and make no guarantees, expressed or implied,
;  about its quality, reliability, or any other
;  characteristic. The use of certain trade names or commercial
;  products does not imply any endorsement of a particular
;  product, nor does it imply that the named product is
;  necessarily the best product for the stated purpose.
;  We would appreciate acknowledgment if the DAVE software
;  is used or if the code in this file is included in another
;  product.
;
; ************************************************ ;
pro rmd_filesel_cleanup,tlb
compile_opt hidden,idl2
end
; ************************************************ ;
pro rmd_filesel::cleanup
compile_opt hidden,idl2
ptr_free,self.file_ptr
ptr_free,self.dir_contents_ptr,self.dir_ptr
(self.filesellist).Remove, /ALL
end
; ************************************************ ;
pro rmd_filesel::get_property,   filter_in = filter_in,     $
                                 filter_out = filter_out,   $
                                 path = path,               $
                                 files = files,             $
                                 cancel = cancel,            $
                                 swapen = swapen,    $
                                 suffixident = suffixident, $
                                 crgrp = crgrp,              $
                                 type = type
compile_opt hidden,idl2
files = (self.filesellist).ToArray()
;; add path
if n_elements(files) eq 0 then files = '' ELSE files=self.path+Path_Sep()+files
;;STOP
cancel = self.cancel
filter_in = self.filter_in
filter_out = self.filter_out
path = self.path
swapen = self.swapen
suffixident = self.suffixident
crgrp = self.crgrp
type = self.type
end
; ************************************************ ;
function rmd_filesel::return_sel_file_names,files
compile_opt hidden,idl2
; First determine the selection(s) in the list widget.
; The names of valid files are returned in the output
; parameter FILES.

file_id = widget_info(self.tlb,find_by_uname = 'FILENAME')
widget_control,file_id,get_value = text_contents
;;STOP
;;files = strsplit(text_contents,STRING(13b),/extract)
;; L.H.: The comma here is not the biggest problem
;; if you have a filename containing a special character like , or [],
;; then file_info later on will return an error!
;; behavior depends on save field
;; save means one file ... we need to read it from the dialog because
;; it may have been edited
;; STOP
IF (self.save EQ 1) THEN BEGIN
   files = strsplit(text_contents,',',/extract)
   (self.filesellist).Remove, /ALL
   (self.filesellist).Add, files, /EXTRACT
END ELSE BEGIN
   files = (self.filesellist).ToArray()
END
if n_elements(files) eq 0 then return,0B
return,1B
end
; ************************************************ ;
pro rmd_filesel::tree_event,event
compile_opt hidden,idl2
if event.type eq 0 then begin
   case event.clicks of
   1: begin ; one button click
         tree_id = widget_info(event.top,find_by_uname = 'FILE_TREE')
         sel = widget_info(tree_id,/tree_select)
         nsel = n_elements(sel)
         counter = 0L
         if n_elements(*self.file_ptr) eq 0 then return
         for i = 0,nsel-1 do begin
            widget_control,sel[i],get_value = value
            ok = where(*self.file_ptr eq value,count)
            if count gt 0 then begin
               if counter eq 0L then begin
                  files = value
               endif else begin
                  files = [files,value]
               endelse
               counter++
            endif
         endfor

         if n_elements(files) gt 0 then begin
            file_id = widget_info(event.top,find_by_uname = 'FILENAME')
;;            widget_control,file_id,set_value =
;;            strjoin(files,STRING(13b))
            ;; files contains the list of selected files
            ;; store them in in the list
            (self.filesellist).Remove, /ALL
            widget_control,file_id,set_value = strjoin(files,',')
            (self.filesellist).Add, files, /EXTRACT
            ;; files where chosen
            if (self.suffixident) THEN BEGIN
               ;; take the first entry to decide which type it is
               test=(self.filesellist)[0]
               SplitFileName, test, testbase, testext
               type_id=Filetypes(/READ,MATCHEXT=testext,FULL=test)
               ftypes_dlid = widget_info(event.top,find_by_uname = 'FTYPES')
               widget_control, ftypes_dlid, SET_DROPLIST_SELECT = type_id.INDEX
               self.type = widget_info(ftypes_dlid, /DROPLIST_SELECT)
;; STOP
            END
         endif
         ;; STOP
      end
   2: begin ; two button clicks
         ; If user has clicked on a single directory then go down into it
         tree_id = widget_info(event.top,find_by_uname = 'FILE_TREE')
         sel = widget_info(tree_id,/tree_select)
         child_id = widget_info(tree_id,/child)
         if event.id eq child_id then begin
            self->up_dir,event
            return
         endif
         nsel = n_elements(sel)
         if nsel ne 1 then return
         widget_control,event.id,get_value = value
         ; Is this a directory?
         cd,current = this_dir
         if self->is_top() then begin
         ; Remove the path separator on Windows
            if strupcase(!version.os_family) eq 'WINDOWS' then begin
               dir_length = strlen(this_dir)
               this_dir = strmid(this_dir,0,strlen(this_dir)-1)
            endif
         endif
         new_path = this_dir+path_sep()+value
         if ~file_test(new_path,/directory) then return
         ; Remove the final path separator
         path_len = strlen(new_path)
         new_path = strmid(new_path,0,path_len-1)
         self.path = new_path
         cd,self.path
         path_id = widget_info(event.top,find_by_uname = 'PATH_LIST')
         widget_control,path_id,set_value = self.path
         ret = self->update_dir_contents()
         id = widget_info(self.tlb,find_by_uname = 'FILENAME')
         widget_control,id,set_value = ''
      end
   else:
   endcase
endif
end
; ************************************************ ;
pro rmd_filesel::quit,event
compile_opt hidden,idl2
widget_control,event.id,get_value = value
action = strupcase(value)
case action of
'   CANCEL   ': $
;'CANCEL':   $
   begin
      self.cancel = 1B
      *self.file_ptr = ''
      self.filter_out = self.orig_filter
      cd,self.orig_directory
   end
else: $
   begin
      self.cancel = 0B
      ok = self->return_sel_file_names(files)
      ;; STOP error occurs before, files is not correct anymore when ,
      ;; is the separator
      if (ok and (n_elements(files) ne 0)) then begin
         n = n_elements(files)
         files_out = strarr(n)
         for i = 0,n-1 do begin
            files_out[i] = self.path+path_sep()+files[i]
         endfor
         ; Are these valid files?
         counter = -1L
         ; STOP
         for i = 0,n-1 do begin
             addfile=0B
            if (self.save EQ 1) then begin
                ;; filename must not exist, only the directory
                isdir=file_test(self.path,/DIRECTORY)
                if isdir then begin
                    addfile=1B
                end 
                isdir=file_test(files_out[i],/DIRECTORY)
                if isdir then begin
                    ;; cannot save to an existing directory name
                    addfile=0B
                end 
            end else begin
                ;; file must exist
                if file_test(files_out[i]) then begin
                    
                    isdir=file_test(files_out[i],/DIRECTORY)
                    ; print, files_out[i] + " isdir=" + STRING(isdir)
                    ;; it exists, but is it a directory or a file
                    
                    ;; STOP
                    if (self.direc EQ 1) THEN BEGIN
                        ;; should be a directory
                        if isdir then addfile=1B 
                    END ELSE BEGIN
                        ;; should be a file
                        if NOT(isdir) THEN addfile=1B 
                    END
                    
                endif
            end     
            if addfile then begin
                if counter lt 0L then begin
                    ;; print, " adding first file=" + files_out[i]
                    files = files_out[i]
                    counter++
                end else begin 
                    ;; print, " adding another file=" + files_out[i]
                    files = [files,files_out[i]]
                    counter++
                end 
            end 
         endfor
         ;if (self.direc EQ 0) THEN BEGIN
         ;    n = n_elements(files)
         ;    files_out = strarr(n)
         ;    for i = 0,n-1 do begin
         ;        files_out[i] = self.path+path_sep()+files[i]
         ;    endfor
             ;; STOP
         ; END
         ; print, "counter=", counter
         if counter lt 0L then begin
             retfiles=''
;              self.cancel=1B
         end ELSE begin 
             retfiles=files[0:counter]
         end
         *self.file_ptr = retfiles
      endif else begin
         *self.file_ptr = ''
      endelse
   end
endcase
widget_control,self.tlb,/destroy
end
; ************************************************ ;
function rmd_filesel::is_top
compile_opt hidden,idl2
case strupcase(!version.os_family) of
'WINDOWS':  result = (strmid(self.path,strlen(self.path)-2) eq ':\')
else: result = (self.path eq '/')
endcase
return,result
end
; ************************************************ ;
function rmd_filesel::update_dir_contents
compile_opt hidden,idl2
id = widget_info(self.tlb,find_by_uname = 'DIR_LIST')
dirs = file_search('*',/test_directory)
ndirs = n_elements(dirs)
if (ndirs eq 1) and (dirs[0] eq '') then ndirs = 0
if ndirs gt 0 then begin
   output = dirs + path_sep()
   dir_sort = sort(strupcase(output))
   output = output[dir_sort]
   dir_out = output
   ptr_free,self.dir_ptr
   self.dir_ptr = ptr_new(/allocate_heap)
   *self.dir_ptr = output
endif
filter_id = widget_info(self.tlb,find_by_uname = 'FILTER')
widget_control,filter_id,get_value = val
;; file_search only for directories if direc is set
IF (self.direc EQ 0) THEN  all_results = file_search(val) else all_results = file_search(val, /TEST_DIRECTORY)
; Which ones in result are directories?
n = n_elements(all_results)
count = 0L
for i = 0,n-1 do begin
   dir_math = where(dirs eq all_results[i],true)
   if ~true then begin
      if count eq 0L then $
         files = all_results[i] else $
         files = [files,all_results[i]]
      count++
   endif
endfor
ptr_free,self.file_ptr
self.file_ptr = ptr_new(/allocate_heap)
if n_elements(files) gt 0 then begin
   file_sort = sort(strupcase(files))
   if self.order eq (-1) then file_sort = reverse(file_sort)
   if ndirs gt 0 then begin
      output = [output,files[file_sort]]
   endif else begin
      output = files[file_sort]
   endelse
   *self.file_ptr = files[file_sort]
endif
if n_elements(output) gt 0 then $
   *self.dir_contents_ptr = output

; Destroy the first child of the tree widget and remake it to
; eliminate the leaves easily.
tree_id = widget_info(self.tlb,find_by_uname = 'FILE_TREE')
child = widget_info(tree_id,/child)
if widget_info(child,/valid_id) then $
   widget_control,child,/destroy
cd,current = this_dir

current_dir = widget_tree(tree_id,value = this_dir,/folder,      $
              /expanded,uvalue = {object:self,method:'tree_event'}  )

; Now build up the tree widget
ndirs = n_elements(dir_out)
WIDGET_CONTROL, current_dir, UPDATE=0  ;; switching of screen update does not seem to work anymore,161022
for i = 0,ndirs-1 do begin
   crgrp = widget_tree(current_dir,value = dir_out[i],/folder,       $
            /expanded,uvalue = {object:self,method:'tree_event'})
endfor
;;WIDGET_CONTROL, current_dir, SET_TREE_VISIBLE=1
if (n_elements(*self.file_ptr) eq 1) then begin
  if (*self.file_ptr)[0] eq '' then $
  nfiles = 0 else nfiles = 1
endif else begin
   nfiles = n_elements(*self.file_ptr)
endelse
WIDGET_CONTROL,  current_dir, SET_TREE_VISIBLE=0
for i = 0,nfiles-1 do begin
    ;; this loop causes the flickering on slow computers!
   leaf1 = widget_tree(current_dir, value = (*self.file_ptr)[i],  $
   uvalue = {object:self,method:'tree_event'})
endfor
WIDGET_CONTROL,  current_dir, SET_TREE_VISIBLE=1
WIDGET_CONTROL,  current_dir, UPDATE=1
return,1B
end
; ************************************************ ;
pro rmd_filesel::filter_event,event
compile_opt hidden,idl2
widget_control,event.id,get_value = filter
self.filter_out = filter
ret = self->update_dir_contents()
end
; ************************************************ ;
pro rmd_filesel::up_dir,event
compile_opt hidden,idl2
if self->is_top() then begin
   ; can't go up any further!
endif else begin
   ; we can go up another level here
   cd,'..'
   cd,current = this_dir
   self.path = this_dir
   id = widget_info(event.top,find_by_uname = 'PATH_LIST')
   widget_control,id,set_value = self.path
   cd,self.path
   ret = self->update_dir_contents()
   id = widget_info(self.tlb,find_by_uname = 'FILENAME')
   widget_control,id,set_value = ''
endelse

end
; ************************************************ ;
pro rmd_filesel::mk_dir,event
compile_opt hidden,idl2
;; get current director
cd,current = this_dir
;; get directory name
result=GetText(GROUP_LEADER=self.group_leader,TITLE="Create Directory")
IF (result NE '') THEN BEGIN
;; create directory
FILE_MKDIR, this_dir+PATH_SEP()+result, /NOEXPAND_PATH
ret = self->update_dir_contents()
;; update directory content   
END
END
; ************************************************ ;
pro rmd_filesel::help,event
  compile_opt hidden,idl2
;; result=ShowHelp(self.help,GROUP_LEADER=self.group_leader,TITLE="Help")
  IF PTR_VALID(self.help) THEN dummy= Dialog_Message(*(self.help), /INFORMATION) ELSE dummy= Dialog_Message('No help available.', /INFORMATION)
END  
; ************************************************ ;
; ************************************************ ;
pro rmd_filesel::desktop_dir,event
compile_opt hidden,idl2
if self->is_top() then begin
   ; can't go up any further!
endif else begin
   ; we can go up another level here
    this_dir=GetDesktopDir()
    SplitPath, this_dir, direct, filename
    dirwrite=FILE_TEST(direct, /DIRECTORY, /WRITE)
    IF (dirwrite EQ 0) THEN BEGIN
        dummy= Dialog_Info("Error: directory "+direct+" does not exist or is not writeable!", /INFO)
        this_dir="."
    END
    cd, this_dir
    self.path = this_dir
    id = widget_info(event.top,find_by_uname = 'PATH_LIST')
   widget_control,id,set_value = self.path
   cd,self.path
   ret = self->update_dir_contents()
   id = widget_info(self.tlb,find_by_uname = 'FILENAME')
   widget_control,id,set_value = ''
endelse
end

pro rmd_filesel::home_dir,event
compile_opt hidden,idl2
if self->is_top() then begin
   ; can't go up any further!
endif else begin
   ; we can go up another level here
    this_dir=ProgramRootDir()
    SplitPath, this_dir, direct, filename
    dirwrite=FILE_TEST(direct, /DIRECTORY, /WRITE)
    IF (dirwrite EQ 0) THEN BEGIN
        dummy= Dialog_Info("Error: directory "+direct+" does not exist or is not writeable!", /INFO)
        this_dir="."
    END
    cd, this_dir
    self.path = this_dir
    id = widget_info(event.top,find_by_uname = 'PATH_LIST')
   widget_control,id,set_value = self.path
   cd,self.path
   ret = self->update_dir_contents()
   id = widget_info(self.tlb,find_by_uname = 'FILENAME')
   widget_control,id,set_value = ''
endelse
end
; ************************************************ ;
pro rmd_filesel::cycle_dir,event
compile_opt hidden,idl2
if self->is_top() then begin
   ; can't go up any further!
endif else begin
   ; we can go up another level here
    this_dir=CycleWorkingDir()
    SplitPath, this_dir, direct, filename
    dirwrite=FILE_TEST(direct, /DIRECTORY, /WRITE)
    IF (dirwrite EQ 0) THEN BEGIN
        dummy= Dialog_Info("Error: directory "+direct+" does not exist or is not writeable!", /INFO)
        this_dir="."
    END
    cd, this_dir
    self.path = this_dir
    id = widget_info(event.top,find_by_uname = 'PATH_LIST')
   widget_control,id,set_value = self.path
   cd,self.path
   ret = self->update_dir_contents()
   id = widget_info(self.tlb,find_by_uname = 'FILENAME')
   widget_control,id,set_value = ''
endelse

end

; ************************************************ ;
pro rmd_filesel::do_nothing,event
compile_opt hidden,idl2

end
; ************************************************ ;
pro rmd_filesel::change_drive,event
compile_opt hidden,idl2
catch,the_error
if the_error ne 0 then begin
   catch,/cancel
   void = dialog_message(dialog_parent = event.top,!error_state.msg)
   return
endif
index = widget_info(event.id,/droplist_select)
drives = get_drive_list()
new_drive = drives[index]
cd,new_drive
self.path = new_drive
ret = self->update_dir_contents()
; Update the directory path text field
path_id = widget_info(event.top,find_by_uname = 'PATH_LIST')
widget_control,path_id,set_value = self.path
catch,/cancel
end
; ************************************************ ;
pro rmd_filesel::toggle_order,event
compile_opt hidden,idl2
setting = widget_info(event.id,/button_set)
if setting then self.order = (-1) else self.order = 1
ret = self->update_dir_contents()
end
; ************************************************ ;
pro rmd_filesel::swap_endian,event
compile_opt hidden,idl2
setting = widget_info(event.id,/button_set)
if setting then self.swapen = 1 else self.swapen = 0
end
; ************************************************ ;
pro rmd_filesel::suffix_ident,event
compile_opt hidden,idl2
setting = widget_info(event.id,/button_set)
if setting then self.suffixident = 1 else self.suffixident = 0
end
; ************************************************ ;
pro rmd_filesel::ftypes,event
compile_opt hidden,idl2
self.type = widget_info(event.id, /DROPLIST_SELECT)
end

; ************************************************ ;
pro rmd_filesel::crgrp,event
compile_opt hidden,idl2
setting = widget_info(event.id, /button_set)
if setting then self.crgrp = 1 else self.crgrp = 0
end
; ************************************************ ;
pro loop_rmd_filesel_events, base
compile_opt hidden,idl2
loop=1
repeat begin
event = WIDGET_Event(base, /NoWait)
IF (event.id GT 0) THEN BEGIN
    widget_control,event.id,get_uvalue = cmd
    call_method,cmd.method,cmd.object,event
    IF (cmd.method EQ "quit") THEN loop=0
END
end until (loop EQ 0)
end
; ************************************************ ;

; ************************************************ ;
pro rmd_filesel_events,event
compile_opt hidden,idl2
widget_control,event.id,get_uvalue = cmd
;; print, "% rmd_filesel_events: cmd.method=", cmd.method
call_method,cmd.method,cmd.object,event
end
; ************************************************ ;
function rmd_filesel::build_widget
compile_opt hidden,idl2
device,get_screen_size = ss
;;
if widget_info(self.group_leader,/valid_id) then BEGIN
   modal = 1B
   floating=1B
END Else BEGIN
   modal = 0B
   floating=0B
END
self.tlb = widget_base(group_leader = self.group_leader, $
   /col,title = self.title,modal = modal,/tlb_frame_attr, FLOATING=floating) ;; , DISPLAY_NAME=GetDisplayName()

void = widget_label(self.tlb,value = 'Directory')
row1 = widget_base(self.tlb,/row)

case strupcase(!version.os_family) of
'WINDOWS':  $
   begin
      drive_vol = 'Drive: '
      drives = get_drive_list()
      drive_loc = (where(strupcase(strmid(self.path,0,strpos(self.path,'\')+1)) $
         eq strupcase(drives)) > 0)[0]
   end
else: drive_vol = ''
endcase
if drive_vol ne '' then begin
   drive_id = widget_droplist(row1,value = drives, $
      uvalue = {object:self,method:'change_drive'})
   widget_control,drive_id,set_droplist_select = drive_loc
endif


bitmap_filename = filepath('up1lvl.bmp', $
   subdirectory = ['resource','bitmaps'])
up_dir = widget_button(row1,value = bitmap_filename, $
   uvalue = {object:self,method:'up_dir'},/bitmap, $
   tooltip = 'Go up a directory')
xsize = 40 & ysize = 20
path_id = widget_text(row1,xsize = xsize, value = self.path, $
   uname = 'PATH_LIST',/editable,                        $
   uvalue = {object:self,method:'do_nothing'})

row2 = widget_base(self.tlb,/row,/align_left)

bitmap_filename = GetBaseDir()+Path_Sep()+'bitmaps'+Path_Sep()+'Desktop.bmp'
up_dir = widget_button(row2,value = bitmap_filename, $
   uvalue = {object:self,method:'desktop_dir'},/bitmap, $
   tooltip = 'Go to desktop directory')

bitmap_filename = GetBaseDir()+Path_Sep()+'bitmaps'+Path_Sep()+'cycle.bmp'
up_dir = widget_button(row2,value = bitmap_filename, $
   uvalue = {object:self,method:'cycle_dir'},/bitmap, $
   tooltip = 'Cycle through recently used directories')

bitmap_filename = GetBaseDir()+Path_Sep()+'bitmaps'+Path_Sep()+'home.bmp'
up_dir = widget_button(row2,value = bitmap_filename, $
   uvalue = {object:self,method:'home_dir'},/bitmap, $
   tooltip = 'iMtools home')

;bitmap_filename = GetBaseDir()+Path_Sep()+'bitmaps'+Path_Sep()+'mkdir.bmp'
;up_dir = widget_button(row2,value = bitmap_filename, $
;   uvalue = {object:self,method:'mk_dir'},/bitmap, $
;   tooltip = 'create new directory')

label1 = widget_label(row2,value = 'Filter:')
filter_id = widget_text(row2,value = self.filter_in, xsize=10,$
   uvalue = {object:self,method:'filter_event'},/editable,$
   uname = 'FILTER')
nonex_base = widget_base(row2,/nonexclusive)
void = widget_button(nonex_base,value = 'Reverse order', $
   uvalue = {object:self,method:'toggle_order'})


tree_base = widget_base(self.tlb,/col)
tree_xsize = fix(ss[0]/3.)
file_tree = widget_tree(tree_base, multiple = self.multiple, $
   uvalue = {object:self,method:'do_nothing'},              $
   xsize = tree_xsize, ysize=300, uname = 'FILE_TREE')

;;



;;row2a = widget_base(self.tlb,/row,/align_left)
;;label2 = widget_label(row2a,value = 'Selection:')
;;filename_id = widget_text(row2a,value = '',uname = 'FILENAME',     $
;;   uvalue = {object:self,method:'do_nothing'},xsize = 26,   $
;;                          /editable)

row2a = widget_base(self.tlb,/row,/align_left)
label2 = widget_label(row2a,value = 'Selection:')
filename_id = widget_text(row2a,value = '',uname = 'FILENAME',     $
   uvalue = {object:self,method:'do_nothing'},xsize = 26,   $
                          /editable)

if (self.direc EQ 0) THEN begin

   
row20=widget_base(self.tlb,/row,/align_left)
void = WIDGET_DROPLIST(row20,TITLE="File Type: ",uname = 'FTYPES', VALUE=(*self.ftypes),uvalue = {object:self,method:'ftypes'})
WIDGET_CONTROL, void, SET_DROPLIST_SELECT=self.type

IF (self.open EQ 1) THEN BEGIN 
nonex_base = widget_base(row20,/row,/nonexclusive)
   void = widget_button(nonex_base,value = 'Suggest Type', $
   uvalue = {object:self,method:'suffix_ident'})
if (self.suffixident EQ 1) THEN WIDGET_CONTROL, void, /SET_BUTTON 

row2_ = widget_base(self.tlb,/row,/align_left)
nonex_base = widget_base(row2_,/column,/nonexclusive)
;;void = widget_button(nonex_base,value = 'Group Multiple Images', $
;;   uvalue = {object:self,method:'crgrp'})
;;if (self.crgrp EQ 1) THEN WIDGET_CONTROL, void, /SET_BUTTON 
void = widget_button(nonex_base,value = 'Swap Endian', $
                     uvalue = {object:self,method:'swap_endian'})
if (self.swapen EQ 1) THEN WIDGET_CONTROL, void, /SET_BUTTON

END 
IF (self.save EQ 1) THEN BEGIN 
;; row2_ = widget_base(self.tlb,/row,/align_left)
;; nonex_base = widget_base(row2_,/row,/nonexclusive)
;; void = widget_button(nonex_base,value = 'Save All Images', $
;;    uvalue = {object:self,method:'crgrp'})
;; if (self.crgrp EQ 1) THEN WIDGET_CONTROL, void, /SET_BUTTON 
END
END ;; self.direc EQ 0

row3 = widget_base(self.tlb,/row,/align_center)
buttsz=40
CASE 1 OF 
(self.save EQ 1): val='    Save     ' 
(self.direc EQ 1): val='    Choose     ' 
ELSE: val='    Open     '
ENDCASE 
void = widget_button(row3,ysize=buttsz,value = val,     $
                     uvalue = {object:self,method:'quit'}       )
void = widget_button(row3,ysize=buttsz,value = '   Help   ',     $
   uvalue = {object:self,method:'help'}       )
void = widget_button(row3,ysize=buttsz,value = '   Cancel   ',   $
   uvalue = {object:self,method:'quit'}       )


;; widpos=PlaceWidget(self.tlb, POSKEY=WidgetPos("DialogWin"))
;;widget_control,self.tlb,/realize, XOFFSET=widpos[0], YOFFSET=widpos[1]
widget_control,self.tlb,/realize
dir_geom = widget_info(up_dir,/geom)
tlb_geom = widget_info(self.tlb,/geom)
path_geom = widget_info(path_id,/geom)
label1_geom = widget_info(label1,/geom,units = 0)
label2_geom = widget_info(label2,/geom,units = 0)
nonex_geom = widget_info(nonex_base,/geom,units = 0)
if n_elements(drive_id) ne 0 then $
  drive_geom = widget_info(drive_id,/geom,units = 0)
;;widget_control,filter_id,scr_xsize = tree_xsize-label1_geom.xsize-nonex_geom.xsize
;; widget_control,filter_id
widget_control,filename_id,scr_xsize = tree_xsize-label2_geom.xsize
if (self.direc EQ 1) THEN BEGIN
   widget_control,filter_id, EDITABLE=1
   widget_control,filename_id, EDITABLE=0, MAP=0
   widget_control,label2, MAP=0
   ;; hide filename_id
END
if n_elements(drive_id) ne 0 then $
   widget_control,path_id,scr_xsize = tree_xsize-dir_geom.xsize-drive_geom.xsize $
   else $
   widget_control,path_id,scr_xsize = tree_xsize-dir_geom.xsize
widget_control,self.tlb,set_uvalue = self
ret = self->update_dir_contents()
reg_name = 'rmd_filesel'
;;
;; STOP
;;loop_rmd_filesel_events, self.tlb
;;
xmanager,reg_name,self.tlb,event_handler = 'rmd_filesel_events',cleanup = 'rmd_filesel_cleanup'
return,1B
end
; ************************************************ ;
function rmd_filesel::init,   group_leader = group_leader,  $
                              filter_in = filter_in,        $
                              multiple = multiple,          $
                              path = path,                  $
                              title = title,                 $
                              swapen=swapen,         $
                              suffixident = suffixident, $
                              crgrp = crgrp,                 $
                              open = open,                 $
                              save = save,                 $
                              help = hlp,                 $
                              ftypes = ftypes,                 $
                              direc = direc,                 $
                              type = type   
compile_opt hidden,idl2
cd, current = current_dir
self.orig_directory = current_dir
self.cancel = 0B
self.title = (n_elements(title) eq 0) ? 'Select file(s)':title
if n_elements(group_leader) eq 0 then group_leader = 0L
self.group_leader = group_leader
if n_elements(filter_in) ne 0 then self.filter_in = filter_in else $
   self.filter_in = '*'
self.filter_out = self.filter_in
self.orig_filter = self.filter_in
self.file_ptr = ptr_new(/allocate_heap)
self.dir_ptr = ptr_new(/allocate_heap)
self.tlb = 0L
self.open=open
self.save=save
self.type=type
self.suffixident=suffixident
self.swapen=swapen
self.crgrp=crgrp
self.ftypes=ftypes
self.direc=direc

if n_elements(multiple) eq 0 then self.multiple = 0B else $
   self.multiple = multiple
if n_elements(path) eq 0 then begin
        cd,current = path
     endif
if n_elements(hlp) ne 0 then self.help = hlp
self.dir_contents_ptr = ptr_new(/allocate_heap)

self.path = path
cd,self.path
self.order = 1                  ; alphabetical order
self.filesellist=list()
return,1B
end
; ************************************************ ;
pro rmd_filesel__define
compile_opt hidden,idl2
void =   {  rmd_filesel,                  $
            filename:'',                  $
            multiple:0B,                  $
            path:'',                      $
            filter_in:'',                 $
            filter_out:'',                $
            file_ptr:ptr_new(),           $
            dir_contents_ptr:ptr_new(),   $
            dir_ptr:ptr_new(),            $
            tlb:0L,                       $
            cancel:0B,                    $
            orig_directory:'',            $
            orig_filter:'',               $
            order:0,                      $
            title:'',                     $
            swapen:0,                 $
            suffixident:0, $
            crgrp:0,                      $
            type:0,                       $
            open:1,                       $
            save:0,                       $
            help:ptr_new(),               $
            direc:0,                       $
            ftypes:ptr_new(),                       $
            filesellist:list(), $
            group_leader:0L               }

end
; ************************************************ ;
function rmd_pickfile,  group_leader = group_leader,  $
                        filter_in = filter_in,        $
                        filter_out = filter_out,      $
                        multiple = multiple,          $
                        direc = direc,          $
                        path = path,                  $
                        get_path = get_path,          $
                        cancelled = cancelled,        $
                        swapen = swapen,       $
                        suffixident = suffixident, $
                        crgrp = crgrp,                 $
                        type = type,                   $
                        ftypes = ftypes,           $
                        help = hlp,           $
                        save = save,                   $
                        open = open,                   $
                        title = title
compile_opt hidden,idl2

IF NOT(GetDebugFlag()) THEN BEGIN
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
   PrintToCon, "% rmd_pickfile: Fatal error "
   PrintToCon, "%               Error status  - " +  STRING(error_status)
   PrintToCon, "%               Error message - " + !ERROR_STATE.MSG
   CATCH, /Cancel
   return, ''
END
END
if n_elements(group_leader) eq 0 then group_leader = 0L
if n_elements(filter_in) eq 0 then filter_in = '*'
if n_elements(multiple) eq 0 then multiple = 0B
if n_elements(path) gt 0 then begin
    ;; check for existence
    dirtest=FILE_TEST(path, /DIRECTORY)
    IF (dirtest EQ 1) THEN BEGIN
        cd, path
    END ELSE BEGIN
        PrintToCon, "% rmd_pickfile: path "+path+" does not exist"
        PrintToCon, "%               falling back to program root directory"
        path=ProgramRootDir()
    END
endif
if n_elements(swapen) eq 0 then swapen = 0
if n_elements(suffixident) eq 0 then suffixident = 0
if n_elements(direc) eq 0 then direc = 0
if n_elements(crgrp) eq 0 then crgrp = 0
if n_elements(type) eq 0 then type = 0
if n_elements(ftypes) eq 0 then ftypes = PTR_NEW(["no data types declared"])
tmptitle="Read Image(s)"
if n_elements(save) eq 0 then save = 0 ELSE tmptitle="Save Image(s)"
if n_elements(hlp) ne 0 then help=hlp
if n_elements(open) eq 0 then BEGIN
    IF (save EQ 1) THEN open = 0 ELSE BEGIN 
        open = 1
    END
END

this_title = (n_elements(title) eq 0) ? tmptitle : title

obj_ref = obj_new('rmd_filesel',                $
               group_leader = group_leader,     $
               filter_in = filter_in,           $
               multiple = multiple,             $
               title = this_title,              $
               save = save,                     $
               open = open,                     $
                  swapen = swapen,                 $
                  suffixident = suffixident, $
               crgrp = crgrp,                   $
               type = type,                     $
                  ftypes = ftypes,                 $
                  help = help,                     $
               direc = direc,                 $
               path = path                      )

ret = obj_ref->build_widget()
obj_ref->get_property,  files = files,          $
                        path = get_path,        $
                        cancel = cancelled,     $
                        swapen = swapen,       $
                        suffixident = suffixident, $
                        crgrp = crgrp,                 $
                        type = type,                   $
                        filter_out = filter_out
obj_destroy,obj_ref

return,files
end
; ************************************************ ;
; ************************************************ ;
; BEGIN TEST PROGRAM NAMED TEST_PF
; ************************************************ ;
; ************************************************ ;
;pro test_pf_event,event
;case widget_info(event.id,/uname) of
;'QUIT':  widget_control,event.top,/destroy
;'OPEN':  $
;   begin
;      swapend=0
;      crgrpd=1
;      type=2
;      filter="*"
;      ftypes=PTR_NEW(["Tvips","Digital Micrograph","FUJI IP"])
;      files = rmd_pickfile(   group_leader = event.top,     $
;                              filter_in = filter,              $
;                              get_path = out_path,          $
;                              cancelled = cancelled,        $
;                              swapen = swapend,             $
;                              type = type,             $
;                              ftypes = ftypes,     $
;                              crgrp = crgrpd,             $
;                              /multiple, $
;                              /open)
;      if NOT(cancelled EQ 1) then begin
;          case n_elements(files) of
;              0: print, 'No files selected'
;              1: print, files
;              else: print, files
;          endcase
;          print, "Swap Endian = ", swapend
;          print, "Create Group = ", crgrpd
;          print, "Type = ", type
;          print, "Filter = ", filter
;      endif ELSE print, 'cancelled'
;   end
;'SAVE':  $
;   begin
;      swapend=0
;      crgrpd=1
;      type=2
;      filter="*"
;      ftypes=PTR_NEW(["Tvips","Digital Micrograph","FUJI IP"])
;      files = rmd_pickfile(   group_leader = event.top,     $
;                              filter_in = filter,              $
;                              get_path = out_path,          $
;                              cancelled = cancelled,        $
;                              swapen = swapend,             $
;                              type = type,             $
;                              ftypes = ftypes,     $
;                              crgrp = crgrpd,             $
;                              /multiple, $
;                              /save)
;      if NOT(cancelled EQ 1) then begin
;          case n_elements(files) of
;              0: print, 'No files selected'
;              1: print, files
;              else: print, files
;          endcase
;          print, "Swap Endian = ", swapend
;          print, "Create Group = ", crgrpd
;          print, "Type = ", type
;          print, "Filter = ", filter
;      endif ELSE print, 'cancelled'
;   end
;'DIREC':  $
;   begin
;      swapend=0
;      crgrpd=1
;      type=2
;      filter="*"
;      ftypes=PTR_NEW(["Tvips","Digital Micrograph","FUJI IP"])
;      files = rmd_pickfile(   group_leader = event.top,     $
;                              filter_in = filter,              $
;                              get_path = out_path,          $
;                              cancelled = cancelled,        $
;                              swapen = swapend,             $
;                              type = type,             $
;                              ftypes = ftypes,     $
;                              crgrp = crgrpd,             $
;                              /multiple, $
;                              /direc)
;      if NOT(cancelled EQ 1) then begin
;          case n_elements(files) of
;              0: print, 'No files selected'
;              1: print, files
;              else: print, files
;          endcase
;          print, "Swap Endian = ", swapend
;          print, "Create Group = ", crgrpd
;          print, "Type = ", type
;          print, "Filter = ", filter
;      endif ELSE print, 'cancelled'
;   end
;'DATA':  $
;   begin
;      swapend=0
;      crgrpd=1
;      type=2
;      filter="*"
;      ftypes=PTR_NEW(["Tvips","Digital Micrograph","FUJI IP"])
;      files = rmd_pickfile(   group_leader = event.top,     $
;                              filter_in = filter,              $
;                              get_path = out_path,          $
;                              cancelled = cancelled,        $
;                              swapen = swapend,             $
;                              type = type,             $
;                              ftypes = ftypes,     $
;                              crgrp = crgrpd,             $
;                              /multiple, $
;                              TITLE="Save Data Set(s)", $
;                              /save)
;      if NOT(cancelled EQ 1) then begin
;          case n_elements(files) of
;             0: print, 'No files selected'
;              1: print, files
;              else: print, files
;          endcase
;          print, "Swap Endian = ", swapend
;          print, "Create Group = ", crgrpd
;          print, "Type = ", type
;          print, "Filter = ", filter
;      endif ELSE print, 'cancelled'
;   end
;else:
;endcase
;end
; ************************************************ ;
;pro test_pf
; This is just a simple test program to see that the
; dialog behaves as expected.
;tlb = widget_base(/col,title = 'Test program',/tlb_frame_attr)
;void = widget_button(tlb,value = 'Open Image(s)',uname = 'OPEN')
;void = widget_button(tlb,value = 'Save Image(s)',uname = 'SAVE')
;void = widget_button(tlb,value = 'Save Data Set(s)',uname = 'DATA')
;void = widget_button(tlb,value = 'Choose Directory',uname = 'DIREC')
;void = widget_button(tlb,value = 'Quit',uname = 'QUIT')
;widget_control,tlb,/realize
;xmanager,'test_pf',tlb,/no_block
;end
; ************************************************ ;
