;+
; NAME:
;       XCOLORS
;
; PURPOSE:
;
;       The purpose of this routine is to interactively change color tables
;       in a manner similar to XLOADCT. No common blocks are used so
;       multiple copies of XCOLORS can be on the display at the same
;       time (if each has a different TITLE). XCOLORS has the ability
;       to notify a widget event handler, an object method, or an IDL
;       procedure if and when a new color table has been loaded. Brewer
;       color tables can also be accessed from this program, if the file
;       fsc_brewer.tbl can be found somewhere in your IDL path.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Widgets, Object, Command line.
;
; CALLING SEQUENCE:
;
;       XCOLORS
;
; INPUTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;
;       BLOCK: If this keyword is set, the program will try to block the
;          IDL command line. Note that this is only possible if no other
;          widget program is currently blocking the IDL command line. It
;          is much more reliable to make XCOLORS a modal widget (see the MODAL
;          keyword), although this can generally only be done when XCOLORS
;          is called from another widget program.
;
;       BREWER: Set this keyword if you wish to use the Brewer Colors, as explained
;           in this reference: http://www.dfanning.com/color_tips/brewer.html. The
;           Brewer color table file, fsc_brewer.tbl must be found somewhere in your
;           IDL path for this option to be available. The file can be downloaded here.
;
;                      http://www.dfanning.com/programs/fsc_brewer.tbl
;             
;           Note that if this file is found, the Brewer colors are automatically added
;           to the program as an option. In this case, the BREWER keyword just makes sure
;           this is the initial user choice.
;
;       BOTTOM: The lowest color index of the colors to be changed.
;
;       COLORINFO: This output keyword will return either a pointer to
;          a color information structure (if the program is called in
;          a non-modal fashion) or a color information structure (if the program
;          is called in modal or blocking fashion). The color information
;          structure is an anonymous structure defined like this:
;
;             struct = { R: BytArr(!D.Table_Size), $ ; The current R color vector.
;                        G: BytArr(!D.Table_Size), $ ; The current G color vector.
;                        B: BytArr(!D.Table_Size), $ ; The current B color vector.
;                        NAME: "", $                 ; The name of the current color table.
;                        INDEX: 0, $                 ; The index number of the current color table.
;                        TYPE: "" }                  ; The type of color table (e.g, BREWER or IDL).
;                        
;          If a pointer to the structure is obtained, you will be responsible
;          for freeing it to prevent memory leakage:
;
;             XColors, ColorInfo=colorInfoPtr
;             Print, "Color Table Name: ", (*colorInfoPtr).Name
;             Ptr_Free, colorInfoPtr
;
;          Note that that Name field will be "Unknown" and the Index field will
;          be -1 until a color table is actually selected by the user. You are
;          responsible for checking this value before you use it.
;
;          When called in modal or blocking fashion, you don't have to worry about freeing
;          the pointer, since no pointer is involved:
;
;             XColors, /Block, ColorInfo=colorInfoData
;             Help, colorInfoData, /Structure
;             Print, "Color Table Name: ", colorInfoData.Name
;
;       DATA: This keyword can be set to any valid IDL variable. If
;          the variable is defined, the specified object method or notify
;          procedure will be passed this variable via a DATA keyword. This
;          keyword is defined primarily so that Notify Procedures are compatible
;          with the XLOADCT way of passing data. It is not strictly required,
;          since the _EXTRA keyword inheritance mechanism will allow passing
;          of *any* keyword parameter defined for the object or procedure that is
;          to be notified.
;
;       DRAG: Set this keyword if you want colors loaded as you drag
;          the sliders. Default is to update colors only when you release
;          the sliders. Use of this keyword is greatly discouraged.
;
;       _EXTRA: This keyword inheritance mechanism will pick up and
;          pass along to any method or procedure to be notified and keywords
;          that are defined for that procedure. Note that you should be sure
;          that keywords are spelled correctly. Any mis-spelled keyword will
;          be ignored.
;
;       FILE: A string variable pointing to a file that holds the
;          color tables to load. The normal colors1.tbl file is used by default.
;
;       GROUP_LEADER: The group leader for this program. When the group
;          leader is destroyed, this program will be destroyed.
;
;       INDEX: The index of the color table to start up. If provided, a color
;           table of this index number is loaded prior to display. Otherwise,
;           the current color table is used. Set this keyword if you wish
;           to have the index number of the event structure correct when
;           the user CANCELs out of the progam.
;
;       MODAL: Set this keyword (along with the GROUP_LEADER keyword) to
;          make the XCOLORS dialog a modal widget dialog. Note that NO
;          other events can occur until the XCOLORS program is destroyed
;          when in modal mode.
;
;       NCOLORS: This is the number of colors to load when a color table
;          is selected.
;
;       NOSLIDERS: If this keyword is set, the color stretch and color gamma
;          sliders are not displayed. This would be appropriate, for example,
;          for programs that just load pre-defined color tables.
;
;       NOTIFYID: A 2-column by n-row array that contains the IDs of widgets
;          that should be notified when XCOLORS loads a color table. The first
;          column of the array is the widgets that should be notified. The
;          second column contains IDs of widgets that are at the top of the
;          hierarchy in which the corresponding widgets in the first column
;          are located. (The purpose of the top widget IDs is to make it
;          possible for the widget in the first column to get the "info"
;          structure of the widget program.) An XCOLORS_LOAD event will be
;          sent to the widget identified in the first column. The event
;          structure is defined like this:
;
;          event = {XCOLORS_LOAD, ID:0L, TOP:0L, HANDLER:0L, $
;             R:BytArr(!D.TABLE_SIZE < 256), G:BytArr(!D.TABLE_SIZE < 256), $
;             B:BytArr(!D.TABLE_SIZE < 256), INDEX:0, NAME:""}
;
;          The ID field will be filled out with NOTIFYID[0, n] and the TOP
;          field will be filled out with NOTIFYID[1, n]. The R, G, and B
;          fields will have the current color table vectors, obtained by
;          exectuing the command TVLCT, r, g, b, /Get. The INDEX field will
;          have the index number of the just-loaded color table. The name
;          field will have the name of the currently loaded color table.
;
;          Note that XCOLORS can't initially tell *which* color table is
;          loaded, since it just uses whatever colors are available when it
;          is called. Thus, it stores a -1 in the INDEX field to indicate
;          this "default" value. Programs that rely on the INDEX field of
;          the event structure should normally do nothing if the value is
;          set to -1. This value is also set to -1 if the user hits the
;          CANCEL button. (Note the NAME field will initially be "Unknown").
;
;          Typically the XCOLORS button will be defined like this:
;
;             xcolorsID = Widget_Button(parentID, Value='Load New Color Table...', $
;                Event_Pro='Program_Change_Colors_Event')
;
;          The event handler will be written something like this:
;
;             PRO Program_Change_Colors_Event, event
;
;                ; Handles color table loading events. Allows colors be to changed.
;
;             Widget_Control, event.top, Get_UValue=info, /No_Copy
;             thisEvent = Tag_Names(event, /Structure_Name)
;             CASE thisEvent OF
;
;                'WIDGET_BUTTON': BEGIN
;
;                     ; Color table tool.
;
;                   XColors, NColors=info.ncolors, Bottom=info.bottom, $
;                      Group_Leader=event.top, NotifyID=[event.id, event.top]
;                   ENDCASE
;
;                'XCOLORS_LOAD': BEGIN
;
;                     ; Update the display for 24-bit displays.
;
;                   Device, Get_Visual_Depth=thisDepth
;                   IF thisDepth GT 8 THEN BEGIN
;                   WSet, info.wid
;
;                    ...Whatever display commands are required go here. For example...
;
;                    TV, info.image
;
;                 ENDIF
;                 ENDCASE
;
;              ENDCASE
;
;              Widget_Control, event.top, Set_UValue=info, /No_Copy
;              END
;
;       NOTIFYOBJ: A vector of structures (or a single structure), with
;          each element of the vector defined as follows:
;
;             struct = {XCOLORS_NOTIFYOBJ, object:Obj_New(), method:''}
;
;          where the Object field is an object reference, and the Method field
;          is the name of the object method that should be called when XCOLORS
;          loads its color tables.
;
;             ainfo = {XCOLORS_NOTIFYOBJ, a, 'Draw'}
;             binfo = {XCOLORS_NOTIFYOBJ, b, 'Display'}
;             XColors, NotifyObj=[ainfo, binfo]
;
;          Note that the XColors program must be compiled before these structures
;          are used. Alternatively, you can put this program, named
;          "xcolors_notifyobj__define.pro" (*three* underscore characters in this
;          name!) in your PATH:
;
;             PRO XCOLORS_NOTIFYOBJ__DEFINE
;              struct = {XCOLORS_NOTIFYOBJ, OBJECT:Obj_New(), METHOD:''}
;             END
;
;          Or, you can simply define this structure as it is shown here in your code.
;
;          "Extra" keywords added to the XCOLORS call are passed along to
;          the object method, which makes this an alternative way to get information
;          to your methods. If you expect such keywords, your methods should be defined
;          with an _Extra keyword.
;          
;          If you set the /OBJECT_DATA keyword, write your XColors notification
;          routine with an XCOLORS_INFO keyword. This will return the same
;          structure defined for the COLORINFO keyword above to your object method.
;          
;       NOTIFYPRO: The name of a procedure to notify or call when the color
;          tables are loaded. If the DATA keyword is also defined, it will
;          be passed to this program via an DATA keyword. But note that *any*
;          keyword appropriate for the procedure can be used in the call to
;          XCOLORS. For example, here is a procedure that re-displays and image
;          in the current graphics window:
;
;             PRO REFRESH_IMAGE, Image=image, _Extra=extra, WID=wid
;             IF N_Elements(wid) NE 0 THEN WSet, wid
;             TVIMAGE, image, _Extra=extra
;             END
;
;          This program can be invoked with this series of commands:
;
;             IDL> Window, /Free
;             IDL> TVImage, image, Position=[0.2, 0.2, 0.8, 0.8]
;             IDL> XColors, NotifyPro='Refresh_Image', Image=image, WID=!D.Window
;
;          Note that "extra" keywords added to the XCOLORS call are passed along to
;          your procedure, which makes this an alternative way to get information
;          to your procedure. If you expect such keywords, your procedure should
;          be defined with an _Extra keyword as illustrated above.
;          
;       OBJECT_DATA: Set this keyword if you wish color information to be
;          supplied to your object notification method via an XCOLORS_DATA
;          keyword. This keyword is ignored unless the NOTIFYOBJ keyword is
;          also used. The color information is supplied as a structure and is
;          defined in the COLORINFO keyword definition above.
;
;       TITLE: This is the window title. It is "Load Color Tables" by
;          default. The program is registered with the name 'XCOLORS:' plus
;          the TITLE string. The "register name" is checked before the widgets
;          are defined. If a program with that name has already been registered
;          you cannot register another with that name. This means that you can
;          have several versions of XCOLORS open simultaneously as long as each
;          has a unique title or name. For example, like this:
;
;            IDL> XColors, NColors=100, Bottom=0, Title='First 100 Colors'
;            IDL> XColors, NColors=100, Bottom=100, Title='Second 100 Colors'
;
;       XOFFSET: This is the X offset of the program on the display. The
;          program will be placed approximately in the middle of the display
;          by default.
;
;       YOFFSET: This is the Y offset of the program on the display. The
;          program will be placed approximately in the middle of the display
;          by default.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Colors are changed. Events are sent to widgets if the NOTIFYID
;       keyword is used. Object methods are called if the NOTIFYOBJ keyword
;       is used. This program is a non-blocking widget.
;
; RESTRICTIONS:
;
;       None.
;
; EXAMPLE:
;
;       To load a color table into 100 colors, starting at color index
;       50 and send an event to the widget identified at info.drawID
;       in the widget heirarchy of the top-level base event.top, type:
;
;       XCOLORS, NCOLORS=100, BOTTOM=50, NOTIFYID=[info.drawID, event.top]
;
; MODIFICATION HISTORY:
;       Written by:     David W. Fanning, 15 April 97. Extensive modification
;         of an older XCOLORS program with excellent suggestions for
;         improvement by Liam Gumley. Now works on 8-bit and 24-bit
;         systems. Subroutines renamed to avoid ambiguity. Cancel
;         button restores original color table.
;       23 April 1997, added color protection for the program. DWF
;       24 April 1997, fixed a window initialization bug. DWF
;       18 June 1997, fixed a bug with the color protection handler. DWF
;       18 June 1997, Turned tracking on for draw widget to fix a bug
;         in TLB Tracking Events for WindowsNT machines in IDL 5.0. DWF
;       20 Oct 1997, Changed GROUP keyword to GROUP_LEADER. DWF
;       19 Dec 1997, Fixed bug with TOP/BOTTOM reversals and CANCEL. DWF.
;        9 Jun 1998, Fixed bug when using BOTTOM keyword on 24-bit devices. DWF
;        9 Jun 1998, Added Device, Decomposed=0 for TrueColor visual classes. DWF
;        9 Jun 1998, Removed all IDL 4 compatibility.
;       21 Oct 1998, Fixed problem with gamma not being reset on CANCEL. DWF
;        5 Nov 1998. Added the NotifyObj keyword, so that XCOLORS would work
;         interactively with objects. DWF.
;        9 Nov 1998. Made slider reporting only at the end of the drag. If you
;         want continuous updating, set the DRAG keyword. DWF.
;        9 Nov 1998. Fixed problem with TOP and BOTTOM sliders not being reset
;         on CANCEL. DWF.
;       10 Nov 1998. Fixed fixes. Sigh... DWF.
;        5 Dec 1998. Added INDEX field to the XCOLORS_LOAD event structure. This
;         field holds the current color table index number. DWF.
;        5 Dec 1998. Modified the way the colorbar image was created. Results in
;         greatly improved display for low number of colors. DWF.
;        6 Dec 1998. Added the ability to notify an unlimited number of objects. DWF.
;       12 Dec 1998. Removed obsolete Just_Reg keyword and improved documetation. DWF.
;       30 Dec 1998. Fixed the way the color table index was working. DWF.
;        4 Jan 1999. Added slightly modified CONGRID program to fix floating divide
;          by zero problem. DWF
;        2 May 1999. Added code to work around a Macintosh bug in IDL through version
;          5.2 that tries to redraw the graphics window after a TVLCT command. DWF.
;        5 May 1999. Restore the current window index number after drawing graphics.
;          Not supported on Macs. DWF.
;        9 Jul 1999. Fixed a couple of bugs I introduced with the 5 May changes. Sigh... DWF.
;       13 Jul 1999. Scheesh! That May 5th change was a BAD idea! Fixed more bugs. DWF.
;       31 Jul 1999. Substituted !D.Table_Size for !D.N_Colors. DWF.
;        1 Sep 1999. Got rid of the May 5th fixes and replaced with something MUCH simpler. DWF.
;       14 Feb 2000. Removed the window index field from the object notify structure. DWF.
;       14 Feb 2000. Added NOTIFYPRO, DATA, and _EXTRA keywords. DWF.
;       20 Mar 2000. Added MODAL, BLOCK, and COLORINFO keywords. DWF
;       20 Mar 2000. Fixed a slight problem with color protection events triggering
;          notification events. DWF.
;       31 Mar 2000. Fixed a problem with pointer leakage on Cancel events, and improved
;          program documentation. DWF.
;       17 Aug 2000. Fixed a problem with CANCEL that occurred only if you first
;          changed the gamma settings before loading a color table. DWF.
;       10 Sep 2000. Removed the requirement that procedures and object methods must
;          be written with an _Extra keyword. DWF.
;        5 Oct 2000. Added the File keyword to LOADCT command, as I was suppose to. DWF.
;        5 Oct 2000. Now properly freeing program pointers upon early exit from program. DWF.
;        7 Mar 2001. Fixed a problem with the BLOCK keyword. DWF.
;       12 Nov 2001. Renamed Congrid to XColors_Congrid. DWF.
;       14 Aug 2002. Moved the calculation of NCOLORS to after the draw widget creation
;          to fix a problem with !D.TABLE_SIZE having a correct value when no windows had
;          been opened in the current IDL session. DWF.
;       14 Aug 2002. Fixed a documentation problem in the NOTIFYID keyword documentation
;          that still referred to !D.N_COLORS instead of the current !D.TABLE_SIZE. DWF.
;       27 Oct 2003. Added INDEX keyword. DWF.
;       29 July 2004. Fixed a problem with freeing colorInfoPtr if it didn't exist. DWF.
;        5 December 2005. Added NOSLIDERS keyword and performed some small cosmetic changes. DWF.
;       27 Sep 2006. Fixed a problem in which XCOLORS set device to indexed color mode. DWF.
;       14 May 2008. Added ability to read the Brewer Color Table file, if available. DWF
;       8 July 2008. Small change in how the program looks for the Brewer file. DWF.
;       1 October 2008. Added a button to switch from either BREWER color tables to IDL 
;           color tables, depending upon which is showing currently. The button ONLY appears
;           if the file fsc_brewer.tbl can be found somewhere in the IDL distribution. DWF.
;       7 October 2008. Whoops! The 1 Oct change wasn't as simple as that. Fixed problems with
;           destroy and restarting the program with respect to keywords that are set, etc. DWF.
;       12 October 2008. Changed license to BSD Open Software License. DWF.
;       21 October 2008. Found a problem with MODAL operations that I fixed. However, that 
;          led to problems with how I was switching from IDL to Brewer colors and visa versa,
;          so I changed that mechanism, too.
;       21 October 2008. Added OBJECT_DATA keyword so that I can get additional information
;          about the state of the color table tool into object methods. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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

Function XColors_Congrid, arr, x, y, z, INTERP=int, MINUS_ONE=m1, CUBIC = cubic

    ON_ERROR, 2      ;Return to caller if error
    s = Size(arr)

    if ((s[0] eq 0) or (s[0] gt 3)) then $
      Message, 'Array must have 1, 2, or 3 dimensions.'

    ;;  Supply defaults = no interpolate, and no minus_one.
    if (N_ELEMENTS(int) le 0) then int = 0 else int = KEYWORD_SET(int)
    if (N_ELEMENTS(m1) le 0) then m1 = 0 else m1 = KEYWORD_SET(m1)
    if (N_ELEMENTS(cubic) eq 0) then cubic = 0
    if (cubic ne 0) then int = 1 ;Cubic implies interpolate


    case s[0] of
        1: begin                ; *** ONE DIMENSIONAL ARRAY
            ; DWF modified: Check divide by zero.
            srx = float(s[1] - m1)/((x-m1) > 1e-6) * findgen(x) ;subscripts
            if (int) then $
              return, INTERPOLATE(arr, srx, CUBIC = cubic) else $
              return, arr[ROUND(srx)]
        endcase
        2: begin                ; *** TWO DIMENSIONAL ARRAY
            if (int) then begin
                srx = float(s[1] - m1) / ((x-m1) > 1e-6) * findgen(x)
                sry = float(s[2] - m1) / ((y-m1) > 1e-6) * findgen(y)
                return, INTERPOLATE(arr, srx, sry, /GRID, CUBIC=cubic)
            endif else $
              return, POLY_2D(arr, $
                              [[0,0],[(s[1]-m1)/(float(x-m1) > 1e-6),0]], $ ;Use poly_2d
                              [[0,(s[2]-m1)/(float(y-m1) > 1e-6)],[0,0]],int,x,y)

        endcase
        3: begin                ; *** THREE DIMENSIONAL ARRAY
            srx = float(s[1] - m1) / ((x-m1) > 1e-6) * findgen(x)
            sry = float(s[2] - m1) / ((y-m1) > 1e-6) * findgen(y)
            srz = float(s[3] - m1) / ((z-m1) > 1e-6) * findgen(z)
            return, interpolate(arr, srx, sry, srz, /GRID)
        endcase
    endcase

    return, arr_r
END ; ***************************************************************



PRO XColors_NotifyObj__Define

   ; Structure definition module for object notification.

struct = {  XColors_NotifyObj, $  ; The structure name.
            object:Obj_New(),  $  ; The object to notify.
            method:'' }           ; The name of the object method to call.

END ; ***************************************************************



PRO XColors_Set, info

TVLCT, r, g, b, /Get

   ; Make sure the current bottom index is less than the current top index.

IF info.currentbottom GE info.currenttop THEN BEGIN
   temp = info.currentbottom
   info.currentbottom = info.currenttop
   info.currenttop = temp
ENDIF

r(info.bottom:info.currentbottom) = info.bottomcolor(0)
g(info.bottom:info.currentbottom) = info.bottomcolor(1)
b(info.bottom:info.currentbottom) = info.bottomcolor(2)
r(info.currenttop:info.top) = info.topcolor(0)
g(info.currenttop:info.top) = info.topcolor(1)
b(info.currenttop:info.top) = info.topcolor(2)

red = info.r
green = info.g
blue = info.b
number = ABS((info.currenttop-info.currentbottom) + 1)

gamma = info.gamma
index = Findgen(info.ncolors)
distribution = index^gamma > 10e-6
index = Round(distribution * (info.ncolors-1) / (Max(distribution) > 10e-6))

IF info.currentbottom GE info.currenttop THEN BEGIN
   temp = info.currentbottom
   info.currentbottom = info.currenttop
   info.currenttop = temp
ENDIF

IF info.reverse EQ 0 THEN BEGIN
   r(info.currentbottom:info.currenttop) = XColors_Congrid(red(index), number, /Minus_One)
   g(info.currentbottom:info.currenttop) = XColors_Congrid(green(index), number, /Minus_One)
   b(info.currentbottom:info.currenttop) = XColors_Congrid(blue(index), number, /Minus_One)
ENDIF ELSE BEGIN
   r(info.currentbottom:info.currenttop) = $
      Reverse(XColors_Congrid(red(index), number, /Minus_One))
   g(info.currentbottom:info.currenttop) = $
      Reverse(XColors_Congrid(green(index), number, /Minus_One))
   b(info.currentbottom:info.currenttop) = $
      Reverse(XColors_Congrid(blue(index), number, /Minus_One))
ENDELSE

TVLct, r, g, b

WSet, info.windowindex
Device, Get_Decomposed=theState, Decomposed=0
TV, info.colorimage
Device, Decomposed=theState
WSet, info.thisWindow

(*info.colorInfoPtr).R = r
(*info.colorInfoPtr).G = g
(*info.colorInfoPtr).B = b
(*info.colorInfoPtr).name = info.ctname
(*info.colorInfoPtr).index = info.index
;; (*info.colorInfoPtr).type = info.colortabletype

   ; Don't bother with notification if this is just a color
   ; protection event.

IF info.from EQ 'PROTECT' THEN RETURN

   ; Are there widgets to notify?

s = SIZE(info.notifyID)
IF s(0) EQ 1 THEN count = 0 ELSE count = s(2)-1
FOR j=0,count DO BEGIN
   colorEvent = { XCOLORS_LOAD, $            ;
                  ID:info.notifyID(0,j), $   ;
                  TOP:info.notifyID(1,j), $
                  HANDLER:0L, $
                  R:r, $
                  G:g, $
                  B:b, $
                  index:info.index, $
                  name:info.ctname}
                  
;;                  type:info.colortabletype }
   IF Widget_Info(info.notifyID(0,j), /Valid_ID) THEN $
      Widget_Control, info.notifyID(0,j), Send_Event=colorEvent
ENDFOR

   ; Is there an object to notify?

nelements = SIZE(info.notifyobj, /N_Elements)
FOR j=0,nelements-1 DO BEGIN
   IF Obj_Valid((info.notifyobj)[j].object) THEN BEGIN
      IF N_Elements(*info.xcolorsData) EQ 0 THEN BEGIN
         s = Size(*info.extra)
         IF s[s[0]+1] EQ 0 THEN BEGIN
            IF info.object_data $
               THEN Call_Method, (info.notifyobj)[j].method, $
                     (info.notifyobj)[j].object, $
                     XCOLORS_DATA=*info.colorInfoPtr $
               ELSE Call_Method, (info.notifyobj)[j].method, ($
                     info.notifyobj)[j].object
         ENDIF ELSE BEGIN
            IF info.object_data $
               THEN Call_Method, (info.notifyobj)[j].method, $
                      (info.notifyobj)[j].object,_Extra=*info.extra, $
                      XCOLORS_DATA=*info.colorinfoptr $ 
               ELSE Call_Method, (info.notifyobj)[j].method, $
                      (info.notifyobj)[j].object,_Extra=*info.extra
         ENDELSE
      ENDIF ELSE BEGIN
        s = Size(*info.extra)
        IF s[s[0]+1] EQ 0 THEN BEGIN
            IF info.object_data $
               THEN Call_Method, (info.notifyobj)[j].method, $
                  (info.notifyobj)[j].object, DATA=*info.xcolorsData, $
                  XCOLORS_DATA=*info.colorinfoptr $ 
               ELSE Call_Method, (info.notifyobj)[j].method, $
                  (info.notifyobj)[j].object, DATA=*info.xcolorsData
        ENDIF ELSE BEGIN
            IF info.object_data $
               THEN Call_Method, (info.notifyobj)[j].method, $
                  (info.notifyobj)[j].object, DATA=*info.xcolorsData, $
                  _Extra=*info.extra, XCOLORS_DATA=*info.colorinfoptr  $
               ELSE Call_Method, (info.notifyobj)[j].method, $
                  (info.notifyobj)[j].object, DATA=*info.xcolorsData, $
                  _Extra=*info.extra
        ENDELSE
      ENDELSE
   ENDIF
ENDFOR

   ; Is there a procedure to notify?

IF info.notifyPro NE "" THEN BEGIN
   IF N_Elements(*info.xcolorsData) EQ 0 THEN BEGIN
      s = Size(*info.extra)
      IF s[s[0]+1] EQ 0 THEN BEGIN
         Call_Procedure, info.notifyPro
      ENDIF ELSE BEGIN
         Call_Procedure, info.notifyPro, _Extra=*info.extra
      ENDELSE
   ENDIF ELSE BEGIN
      s = Size(*info.extra)
      IF s[s[0]+1] EQ 0 THEN BEGIN
         Call_Procedure, info.notifyPro, DATA=*info.xcolorsData
      ENDIF ELSE BEGIN
         Call_Procedure, info.notifyPro, DATA=*info.xcolorsData, _Extra=*info.extra
      ENDELSE
   ENDELSE
ENDIF

END ; ***************************************************************



PRO XCOLORS_TOP_SLIDER, event

   ; Get the info structure from storage location.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Update the current top value of the slider.

currentTop = event.value
Widget_Control, info.botSlider, Get_Value=currentBottom
currentBottom = currentBottom + info.bottom
currentTop = currentTop + info.bottom

   ; Error handling. Is currentBottom = currentTop?

IF currentBottom EQ currentTop THEN BEGIN
   currentBottom = (currentTop - 1) > 0
   thisValue = (currentBottom-info.bottom)
   IF thisValue LT 0 THEN BEGIN
      thisValue = 0
      currentBottom = info.bottom
   ENDIF
   Widget_Control, info.botSlider, Set_Value=thisValue
ENDIF

   ; Error handling. Is currentBottom > currentTop?

IF currentBottom GT currentTop THEN BEGIN

   bottom = currentTop
   top = currentBottom
   bottomcolor = info.topColor
   topcolor = info.bottomColor
   reverse = 1

ENDIF ELSE BEGIN

   bottom = currentBottom
   top = currentTop
   bottomcolor = info.bottomColor
   topcolor = info.topColor
   reverse = 0

ENDELSE

   ; Create a pseudo structure.

pseudo = {currenttop:top, currentbottom:bottom, reverse:reverse, $
   bottomcolor:bottomcolor, topcolor:topcolor, gamma:info.gamma, index:info.index, $
   top:info.top, bottom:info.bottom, ncolors:info.ncolors, r:info.r, $
   g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage, $
   windowindex:info.windowindex, from:'TOP', notifyObj:info.notifyObj, extra:info.extra, $
   thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, $
   colorInfoPtr:info.colorInfoPtr, colornamesPtr:info.colornamesPtr, ctname:info.ctname, $
;;   needColorInfo:info.needColorInfo,
;;   colortabletype:info.colortabletype, $
   needColorInfo:info.needColorInfo, $
   object_data:info.object_data}

   ; Update the colors.

XColors_Set, pseudo

info.currentTop = currentTop

   ; Put the info structure back in storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ************************************************************************



PRO XCOLORS_BOTTOM_SLIDER, event

   ; Get the info structure from storage location.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Update the current bottom value of the slider.

currentBottom = event.value + info.bottom
Widget_Control, info.topSlider, Get_Value=currentTop
;currentBottom = currentBottom + info.bottom
currentTop = currentTop + info.bottom

   ; Error handling. Is currentBottom = currentTop?

IF currentBottom EQ currentTop THEN BEGIN
   currentBottom = currentTop
   Widget_Control, info.botSlider, Set_Value=(currentBottom-info.bottom)
ENDIF

   ; Error handling. Is currentBottom > currentTop?

IF currentBottom GT currentTop THEN BEGIN

   bottom = currentTop
   top = currentBottom
   bottomcolor = info.topColor
   topcolor = info.bottomColor
   reverse = 1

ENDIF ELSE BEGIN

   bottom = currentBottom
   top = currentTop
   bottomcolor = info.bottomColor
   topcolor = info.topColor
   reverse = 0

ENDELSE

   ; Create a pseudo structure.

pseudo = {currenttop:top, currentbottom:bottom, reverse:reverse, $
   bottomcolor:bottomcolor, topcolor:topcolor, gamma:info.gamma, index:info.index, $
   top:info.top, bottom:info.bottom, ncolors:info.ncolors, r:info.r, $
   g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage, $
   windowindex:info.windowindex, from:'BOTTOM', notifyObj:info.notifyObj, extra:info.extra, $
   thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, $
   colorInfoPtr:info.colorInfoPtr, colornamesPTR:info.colornamesPTR, ctname:info.ctname, $
;;   needColorInfo:info.needColorInfo,
;;   colortabletype:info.colortabletype, $
   needColorInfo:info.needColorInfo, $
   object_data:info.object_data}

   ; Update the colors.

XColors_Set, pseudo

info.currentBottom = currentBottom

   ; Put the info structure back in storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ************************************************************************




PRO XCOLORS_GAMMA_SLIDER, event

   ; Get the info structure from storage location.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the gamma value from the slider.

Widget_Control, event.id, Get_Value=gamma
gamma = 10^((gamma/50.0) - 1)

   ; Update the gamma label.

Widget_Control, info.gammaID, Set_Value=String(gamma, Format='(F6.3)')

   ; Make a pseudo structure.

IF info.currentBottom GT info.currentTop THEN $
   pseudo = {currenttop:info.currentbottom, currentbottom:info.currenttop, $
      reverse:1, bottomcolor:info.topcolor, topcolor:info.bottomcolor, $
      gamma:gamma, top:info.top, bottom:info.bottom, index:info.index, $
      ncolors:info.ncolors, r:info.r, g:info.g, b:info.b, $
      notifyID:info.notifyID, colorimage:info.colorimage, extra:info.extra, $
      windowindex:info.windowindex, from:'SLIDER', notifyObj:info.notifyObj, $
      thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, $
      colorInfoPtr:info.colorInfoPtr, colornamesPtr:info.colornamesPtr, ctname:info.ctname, $
;;      needColorInfo:info.needColorInfo,
;;      colortabletype:info.colortabletype, $
      needColorInfo:info.needColorInfo, $
      object_data:info.object_data} $
ELSE $
   pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom, $
      reverse:0, bottomcolor:info.bottomcolor, topcolor:info.topcolor, $
      gamma:gamma, top:info.top, bottom:info.bottom, index:info.index, $
      ncolors:info.ncolors, r:info.r, g:info.g, b:info.b, $
      notifyID:info.notifyID, colorimage:info.colorimage, extra:info.extra, $
      windowindex:info.windowindex, from:'SLIDER', notifyObj:info.notifyObj, $
      thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, $
      colorInfoPtr:info.colorInfoPtr, colornamesPtr:info.colornamesPtr, ctname:info.ctname, $
       ;;      needColorInfo:info.needColorInfo, colortabletype:info.colortabletype, $
      needColorInfo:info.needColorInfo, $
      object_data:info.object_data}

   ; Load the colors.

XColors_Set, pseudo

info.gamma = gamma

   ; Put the info structure back in storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ************************************************************************



PRO XCOLORS_COLORTABLE, event

   ; Get the info structure from storage location.

Widget_Control, event.top, Get_UValue=info, /No_Copy

LoadCT, event.index, File=info.file, /Silent, $
   NColors=info.ncolors, Bottom=info.bottom

TVLct, r, g, b, /Get
info.r = r(info.bottom:info.top)
info.g = g(info.bottom:info.top)
info.b = b(info.bottom:info.top)
info.topcolor = [r(info.top), g(info.top), b(info.top)]
info.bottomcolor = [r(info.bottom), g(info.bottom), b(info.bottom)]

   ; Update the slider positions and values.

IF 1 - info.nosliders THEN BEGIN
   Widget_Control, info.botSlider, Set_Value=0
   Widget_Control, info.topSlider, Set_Value=info.ncolors-1
   Widget_Control, info.gammaSlider, Set_Value=50
   Widget_Control, info.gammaID, Set_Value=String(1.0, Format='(F6.3)')
ENDIF
info.currentBottom = info.bottom
info.currentTop = info.top
info.gamma = 1.0
info.index = event.index
info.ctname = (*(info.colornamesPtr))[event.index]

   ; Create a pseudo structure.

pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom, $
   reverse:info.reverse, windowindex:info.windowindex, index:event.index, $
   bottomcolor:info.bottomcolor, topcolor:info.topcolor, gamma:info.gamma, $
   top:info.top, bottom:info.bottom, ncolors:info.ncolors, r:info.r, $
   g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage, $
   from:'LIST', notifyObj:info.notifyObj, thisWindow:info.thisWindow, $
   notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, extra:info.extra, $
   colorInfoPtr:info.colorInfoPtr, colornamesPtr:info.colornamesPtr, ctname:info.ctname, $
 ;;         needColorInfo:info.needColorInfo, colortabletype:info.colortabletype, $
   needColorInfo:info.needColorInfo, $
   object_data:info.object_data}

   ; Update the colors.

XColors_Set, pseudo

   ; Put the info structure back in storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ************************************************************************



PRO XCOLORS_PROTECT_COLORS, event

   ; Get the info structure from storage location.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Create a pseudo structure.

pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom, $
   reverse:info.reverse, $
   bottomcolor:info.bottomcolor, topcolor:info.topcolor, gamma:info.gamma, $
   top:info.top, bottom:info.bottom, ncolors:info.ncolors, r:info.r, index:info.index, $
   g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage, $
   windowindex:info.windowindex, from:'PROTECT', notifyObj:info.notifyObj, extra:info.extra, $
   thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, $
   colorInfoPtr:info.colorInfoPtr, colornamesPtr:info.colornamesPtr, ctname:info.ctname, $
          ;; needColorInfo:info.needColorInfo,
          ;; colortabletype:info.colortabletype, $
   needColorInfo:info.needColorInfo, $
   object_data:info.object_data}

   ; Update the colors.

XColors_Set, pseudo

   ; Put the info structure back in storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ************************************************************************


PRO XCOLORS_SAVEFILE, event
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XCOLORS_SAVEFILE:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
 Widget_Control, event.top, Get_UValue=info, /No_Copy
 wdir=ProgramRootDir()
 result=rmd_pickfile(  $
        filter_in = "*.tbl",              $
        get_path = wdir,          $
        title = "Select a file",          $
        /save,$
        cancelled = cancelled)
 IF NOT(cancelled) THEN BEGIN
    result=result[0]
    ;; print, "new color table file" + result
    IF ((result NE "") AND FileIsWriteable(result)) THEN BEGIN
                                ;SplitPath, result, direct, filename
          print, "% Xcolors_savefile: Saving " + result
          oldfile=info.file
          newfile = result
          names=''
          LoadCT, Get_Names=names, File=oldfile
          colorNamesIndex = StrArr(N_Elements(names))
          FOR j=0,N_Elements(names)-1 DO BEGIN
             ;; Load colortable and save it to new file
             LoadCT, j, File=oldfile
             TVLCT, r, g, b, /GET
             IF (j EQ 0) THEN mymodifyct, j,  names[j], r, g, b, FILE=newfile, /NEWFILE ELSE mymodifyct, j,  names[j], r, g, b, FILE=newfile
          END
          ;; load previous table
          info.file=newfile
          LoadCT, info.index, FILE=info.file
          set_default_colortable, FILE=info.file, INDEX=info.index
          ;; XColors_ColorTable
    END
 END     
 ;; nothing changed, just a copy, so we do not need to update the info field          
 Widget_Control, event.top, Set_UValue=info, /No_Copy
END                              ; ************************************************************************

PRO XCOLORS_IMPORTCSV, event
IF NOT(GetDebugFlag()) THEN BEGIN
   CATCH, Error_status
   IF (Error_status NE 0) THEN BEGIN
      PrintToCon, "% XCOLORS_IMPORTCSV:  Fatal error "
      PrintToCon, "%   Error status  - " + STRING(error_status)
      PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
      ;; XConsole_PopState
      CATCH, /Cancel
      return
   END
END
Widget_Control, event.top, Get_UValue=info, /No_Copy
wdir=ProgramRootDir()
newfile=0
maxcol=255
result=rmd_pickfile(  $
       filter_in = "*.csv",              $
       get_path = wdir,          $
       title = "Select a file or files",          $
       /open,$
       /multiple, $
       cancelled = cancelled)
IF NOT(cancelled) THEN BEGIN
   IF (N_ELEMENTS(result) EQ 1) THEN BEGIN
      result=result[0]
      ;; print, "new color table file" + result
      IF (result NE "") THEN BEGIN
                                ;SplitPath, result, direct, filename
         details="cancelled"         
         print, "% Xcolors: Importing csv color table"
         rgb_data = READ_CSV(result)
         ;; check whether is normalized data
         r=rgb_data.FIELD1 & g=rgb_data.FIELD2 & b=rgb_data.FIELD3
         STOP
         IF ((Max(r) LE 1) AND (Max(g) LE 1) AND (Max(b) LE 1)) THEN BEGIN
            r=BYTE(r*255) & g=BYTE(g*255) & b=BYTE(b*255)
         END ELSE BEGIN
            r=BYTE(r) & g=BYTE(g) & b=BYTE(b)
         END
         ;; replace current or append, current is info.index
         s=list()
         SplitPath, result, thepath, thefilename
         dotpos=strpos(thefilename, '.',/reverse_search)
         IF (dotpos GE 0) THEN BEGIN
            extension=strmid(thefilename,dotpos)
            name=File_basename(result,extension)
         END ELSE name=thefilename
         s.Add, {value:name,label:"Table name",newrow:0B}
         c=list()
         excllist=list()
         excllist.Add, {value:0B,label:"Replace current table in this list",newrow:0B}
         excllist.Add, {value:1B,label:"Append table to this list",newrow:1B}
         excllist.Add, {value:1B,label:"Insert table into this list at current position",newrow:1B}
         excllist.Add, {value:0B,label:"Create a new table list with this table",newrow:1B}
         IF (XMDataChoiceField(s, c, E=excllist, TITLE="How to import the table into the list?",XWIDTH=30) EQ 1) THEN BEGIN
            name=s[0].value
            newfile=excllist[3].value
            IF ((newfile EQ 0) AND (info.file EQ Filepath(SubDir=['resource','colors'], 'colors1.tbl'))) THEN BEGIN
               ;; do not touch the IDL color file,
               ;; create a new file
               ErrMsg, "Cannot overwrite IDL colors, please choose a new .tbl file"
               newfile=1
            END
            IF (newfile EQ 1) THEN BEGIN
               result=rmd_pickfile(  $
                      filter_in = "*.tbl",              $
                      get_path = wdir,          $
                      title = "Select a file",          $
                      /save,$
                      cancelled = cancelled)
               IF NOT(cancelled) THEN BEGIN
                  ;; creating a new file
                  info.file=result[0]
                  info.index=0
                  mymodifyct, 0,  name, r, g, b, FILE=result[0], /NEWFILE
                  set_default_colortable, FILE=result[0] ;; index s zero
                  
               END 
            END ELSE BEGIN
               IF (excllist[0].value EQ 1)  THEN BEGIN
                  ;; replace
                  currindex=info.index
                  currfile=info.file
                  mymodifyct, currindex,  name, r, g, b, FILE=currfile
                  info.index=currindex
               END
               IF (excllist[1].value EQ 1)  THEN BEGIN
                  ;; append
                  currfile=info.file
                  currindex=info.index
                  myinsertct, currindex, currfile, name, r, g, b, /APPEND
                  info.index=currindex
               END
               IF (excllist[2].value EQ 1)  THEN BEGIN
                  ;; insert
                  currfile=info.file
                  currindex=info.index
                  myinsertct, currindex, currfile, name, r, g, b
                  info.index=currindex
               END
            END 
            printtocon,"% Xcolors_savefile: Saved color table to: " + info.file
         END
      END 
   END ELSE BEGIN
      ;; multiple files
      s=list()
      c=list()
      excllist=list()
      excllist.Add, {value:1B,label:"Append tables to this list",newrow:0B}
      excllist.Add, {value:0B,label:"Insert tables into this list at the current position",newrow:0B}
      excllist.Add, {value:0B,label:"Create a new table list with these tables",newrow:0B}
      IF (XMDataChoiceField(s, c, E=excllist, TITLE="How to import the tables into the list?",XWIDTH=30) EQ 1) THEN BEGIN
         newfile=excllist[2].value
         NumTables=FIX(N_Elements(result))
         if ((info.index + NumTables) GT 255) THEN BEGIN
            newfile=1
            printtocon,"% XcolorsImportCSV: Too many tables for this list, creating a new file."
         END
         FOR iter=0,NumTables-1 Do BEGIN
            print, "% Xcolors: Importing csv color table"
            rgb_data = READ_CSV(result[iter])
            ;; check whether is normalized data
            r=rgb_data.FIELD1 & g=rgb_data.FIELD2 & b=rgb_data.FIELD3
            IF ((Max(r) LE 1) AND (Max(g) LE 1) AND (Max(b) LE 1)) THEN BEGIN
               r=BYTE(r*255) & g=BYTE(g*255) & b=BYTE(b*255)
            END ELSE BEGIN
               r=BYTE(r) & g=BYTE(g) & b=BYTE(b)
            END
            ;; replace current or append, current is info.index
            s=list()
            SplitPath, result[iter], thepath, thefilename
            dotpos=strpos(thefilename, '.',/reverse_search)
            IF (dotpos GE 0) THEN BEGIN
               extension=strmid(thefilename,dotpos)
               name=File_basename(result[iter],extension)
            END ELSE name=thefilename
            if (((iter+1) MOD maxcol) EQ 0) THEN BEGIN
               newfile=1
               printtocon,"% XcolorsImportCSV: A file can only contain "+MyString(maxcol)+" tables, the current file is complete."
               printtocon,"      Choose another file for the next tables."
            END
            IF (newfile EQ 1) THEN BEGIN
               ;; creating a new file
               outfile=rmd_pickfile(  $
                       filter_in = "*.tbl",              $
                       get_path = wdir,          $
                       title = "Select a file",          $
                       /save,$
                       cancelled = cancelled)
               IF NOT(cancelled) THEN BEGIN
                  ;; create a new file
                  info.file=outfile[0]
                  currindex=0
                  mymodifyct, currindex,  name, r, g, b, FILE=outfile[0], /NEWFILE
                  info.index=currindex
                  set_default_colortable, FILE=outfile[0], INDEX=0
                  newfile=0
               END ELSE BEGIN
                  iter=NumTables
                  printtocon,"% XcolorsImportCSV: Cancelled."
               END
            END   ELSE BEGIN
               IF (excllist[1].value EQ 1) THEN BEGIN
                  ;; insert
                  currfile=info.file
                  currindex=info.index
                  myinsertct, currindex,  currfile, name, r, g, b
                  info.index=currindex+1
               END ELSE BEGIN
                  ;; append
                  currfile=info.file
                  currindex=info.index
                  myinsertct, currindex,  currfile, name, r, g, b, /APPEND
                  info.index=currindex
               END 
            END 
         END  
      END
   END ;; multiple files block
   ;; update info field
   cnames=''
   LoadCT, Get_Names=cnames, File=info.file
   colorNamesIndex = StrArr(N_Elements(cnames))
   FOR j=0,N_Elements(cnames)-1 DO colorNamesIndex[j] = StrTrim(j,2) + ' - ' + cnames[j]
   WIDGET_CONTROL, info.tablelist, SET_VALUE=colorNamesIndex 
                           ;;;  XColors_set, info
   currfile=info.file
   currindex=info.index
   set_default_colortable, FILE=currfile, INDEX=currindex ;; in case we created a new file we have to update the parameter object
   ;; STOP
   ;;Update the info field
   TVLCT, rr, gg, bb, /Get
   info.r=rr & info.g=gg & info.b=bb
   IF PTR_VALID(info.colorNamesPtr) THEN PTR_FREE, (info.colorNamesPtr)
   info.colorNamesPtr=PTR_NEW(cnames)
   info.ctname=cnames[currindex]
   ;;
   Widget_Control, event.top, Set_UValue=info, /No_Copy
END      


END                              ; ************************************************************************


PRO XCOLORS_Delete_current, event
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XCOLORS_Delete_current:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
 Widget_Control, event.top, Get_UValue=info, /No_Copy
 name=(*info.colornamesPtr)[info.index]
 IF (Dialog_Message(["","Delete colortable "+name+"?",""],/QUESTION) EQ "Yes") THEN BEGIN
       file=info.file
       index=info.index
       MyDeleteCT, index, info.file
       info.index=index
       LoadCT, info.index, FILE=info.file
       set_default_colortable, FILE=info.file, INDEX=info.index
       ;;Update the info field
       cnames=''
       LoadCT, Get_Names=cnames, File=info.file
       colorNamesIndex = StrArr(N_Elements(cnames))
       FOR j=0,N_Elements(cnames)-1 DO colorNamesIndex[j] = StrTrim(j,2) + ' - ' + cnames[j]
       WIDGET_CONTROL, info.tablelist, SET_VALUE=colorNamesIndex 
                           ;;;  XColors_set, info
       currfile=info.file
       currindex=info.index
       TVLCT, rr, gg, bb, /Get
       info.r=rr & info.g=gg & info.b=bb
       IF PTR_VALID(info.colorNamesPtr) THEN PTR_FREE, (info.colorNamesPtr)
       info.colorNamesPTR=PTR_NEW(cnames)
       info.ctname=cnames[currindex]
;; update info fields
 END     
 ;; nothing changed, just a copy, so we do not need to update the info field          
 Widget_Control, event.top, Set_UValue=info, /No_Copy
 ;;
 ;;   XColors_ColorTable
END                              ; ************************************************************************


PRO XCOLORS_Rename_current, event
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XCOLORS_Rename_current:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
 Widget_Control, event.top, Get_UValue=info, /No_Copy
 name=(*info.colornamesPtr)[info.index]
 newname=name
 IF (XStringField(newname,"New table name", XSIZE=32, TITLE="Rename Color Table") EQ 1) THEN BEGIN
       file=info.file
       index=info.index
       MyRenameCT, index, info.file, newname
       ;;Update the info field
       cnames=''
       LoadCT, Get_Names=cnames, File=info.file
       colorNamesIndex = StrArr(N_Elements(cnames))
       FOR j=0,N_Elements(cnames)-1 DO colorNamesIndex[j] = StrTrim(j,2) + ' - ' + cnames[j]
       WIDGET_CONTROL, info.tablelist, SET_VALUE=colorNamesIndex 
                           ;;;  XColors_set, info
     
       IF PTR_VALID(info.colorNamesPtr) THEN PTR_FREE, (info.colorNamesPtr)
       info.colorNamesPTR=PTR_NEW(cnames)
       info.ctname=cnames[info.index]
 END     
 ;; nothing changed, just a copy, so we do not need to update the info field          
 Widget_Control, event.top, Set_UValue=info, /No_Copy
 ;;
 ;;   XColors_ColorTable
END                              ; ************************************************************************


PRO XCOLORS_Modify, event
CATCH, Error_status
IF (Error_status NE 0) THEN BEGIN
    PrintToCon, "% XCOLORS_SAVEFILE:  Fatal error "
    PrintToCon, "%   Error status  - " + STRING(error_status)
    PrintToCon, "%   Error message - " + !ERROR_STATE.MSG
    ;; XConsole_PopState
    CATCH, /Cancel
    return
END
Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Update the colors.
XPalette, /BLOCK
; get current colors and write them to info
TVLCT, myred, mygreen, myblue, /GET
;XColors_Set, info
(*info.colorInfoPtr).R=myred
(*info.colorInfoPtr).G=mygreen
(*info.colorInfoPtr).B=myblue
;; how to save?
info.r=myred
info.g=mygreen
info.b=myblue
 pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom, $
      reverse:0, bottomcolor:info.bottomcolor, topcolor:info.topcolor, $
      gamma:info.gamma, top:info.top, bottom:info.bottom, index:info.index, $
      ncolors:info.ncolors, r:info.r, g:info.g, b:info.b, $
      notifyID:info.notifyID, colorimage:info.colorimage, extra:info.extra, $
      windowindex:info.windowindex, from:'SLIDER', notifyObj:info.notifyObj, $
      thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, $
      colorInfoPtr:info.colorInfoPtr, colornamesPtr:info.colornamesPtr, ctname:info.ctname, $
          ;; needColorInfo:info.needColorInfo, colortabletype:info.colortabletype, $
      needColorInfo:info.needColorInfo, $
      object_data:info.object_data}
;; XColors_Set, pseudo
 Widget_Control, event.top, Set_UValue=info, /No_Copy
 ;;
 ;;  XColors_ColorTable
END ; ************************************************************************




PRO XCOLORS_CANCEL, event
Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Update the colors.

XColors_Set, info.cancelStruct
Widget_Control, event.top, Set_UValue=info, /No_Copy
Widget_Control, event.top, /Destroy
END ; ************************************************************************



PRO XCOLORS_DISMISS, event
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  pobj=GetDLGParObject()
  IF NOT(OBJ_VALID(pobj)) THEN BEGIN
     print, "Fatal error: parameter object is not defined." 
  END ELSE BEGIN
     pobj->set_token_value, 'COLORMAP','DEFAULT', info.file
     pobj->set_token_value, 'COLORMAP','INDEX', info.index
     pobj->set_token_value, 'COLORMAP', 'DECOMPOSED', 0
  END
Widget_Control, event.top, /Destroy
END ; ************************************************************************




PRO XCOLORS_LOADFILE, event
   Widget_Control, event.top, Get_UValue=info, /No_Copy
   parent = Widget_Info(event.id, /PARENT)
   oldList = info.tableList
   wdir=ProgramRootDir()
   SplitPath, info.file, wdir, filename
   result=rmd_pickfile(  $
          filter_in = "*.tbl", $
          get_path = wdir,          $
          title = "Select a color table file",          $
          cancelled = cancelled)
   IF NOT(cancelled) THEN BEGIN
      result=result[0]
      ;;print, "new color table file" + result
      IF (result NE "") THEN BEGIN
                                
         dirread=FILE_TEST(result, /READ)
         ;; print, "file is readable" + dirread
         IF (dirread EQ 0) THEN BEGIN
            printtocon, "%  Color table file "+direct+" does not exist or is readable!"
         END ELSE BEGIN
            print, "% Loading custom colors"
            info.file = result
            ;; overwrite with file from parameter object if available
            pobj=GetDLGParObject()
            IF NOT(OBJ_VALID(pobj)) THEN BEGIN
               print, "Fatal error: parameter object is not defined." 
            END ELSE BEGIN
               pobj->set_token_value, 'COLORMAP','DEFAULT', info.file
               pobj->set_token_value, 'COLORMAP','INDEX', 0
               pobj->set_token_value, 'COLORMAP', 'DECOMPOSED', 0
            END
            ;; oldlist=info.tableList
            ;; Widget_Control, oldList, /Destroy
            ;; do not destroy the widget, change its Value field
            cnames=''
            LoadCT, Get_Names=cnames, File=info.file
            colorNamesIndex = StrArr(N_Elements(cnames))
            info.index=0
            set_default_colortable, FILE=info.file, INDEX=info.index 
            FOR j=0,N_Elements(cnames)-1 DO colorNamesIndex[j] = StrTrim(j,2) + ' - ' + cnames[j]
            ;;
            WIDGET_CONTROL, info.tablelist, SET_VALUE=colorNamesIndex
            ;; info.tableList = Widget_List(info.tableListBase,
            ;; Value=colorNamesIndex, YSize=12 +
            ;; (12*Keyword_Set(nosliders)), Scr_XSize=256, Event_Pro='XColors_ColorTable')
               ;;; we have to update the info field
            set_default_colortable, FILE=info.file, INDEX=info.index 
            LoadCT, info.index, File=info.file
            TVLCT, rr, gg, bb, /Get
            info.r=rr & info.g=gg & info.b=bb
            IF PTR_VALID(info.colornamesPtr) THEN PTR_FREE, (info.colornamesPtr)
            info.colornamesPTR=PTR_NEW(cnames)
            info.ctname=cnames[info.index]
            ;;
            Widget_Control, event.top, Set_UValue=info, /No_Copy
            
         END 
      END
   END 
END                              ; *****************************************************************

PRO XCOLORS_CLEANUP, tlb
    Widget_Control, tlb, Get_UValue=info, /No_Copy
    IF N_Elements(info) NE 0 THEN BEGIN
       Ptr_Free, info.xcolorsData
       Ptr_Free, info.extra
       IF info.needColorInfo EQ 0 THEN Ptr_Free, info.colorInfoPtr
    ENDIF
END ; ************************************************************************



PRO XCOLORS, $
    Block=block, $
    Bottom=bottom, $
    ColorInfo=colorinfoPtr, $
    Data=xColorsData, $
    Drag=drag, $
    Group_Leader=group_leader, $
    Index=index, $
    Modal=modal, $
    NColors=ncolors, $
    NoSliders=nosliders, $
    NotifyID=notifyID, $
    NotifyObj=notifyObj, $
    NotifyPro=notifyPro, $
    Object_Data=object_data, $
    Title=title, $
    XOffset=xoffset, $
    YOffset=yoffset, $
    _EXTRA=extra

   ; This is a procedure to load color tables into a
   ; restricted color range of the physical color table.
   ; It is a highly simplified, but much more powerful,
   ; version of XLoadCT.

On_Error, 1

   ; Current graphics window.

thisWindow = !D.Window

   ; Check keyword parameters. Define defaults.

IF N_Elements(title) EQ 0 THEN title = 'Color Tables'
IF N_Elements(drag) EQ 0 THEN drag = 0

   ; Try to locate the brewer file. Check resource/colors directory, then look for it
   ; in the IDL path if it is not found there.

;; brewerfile = ProgramRootDir()+Path_Sep()+'custom-colortables.tbl'
;; IF File_Test(brewerfile, /READ) EQ 0 THEN brewerfile = ProgramRootDir()+Path_Sep()+'custom-colortables.tbl'
;; IF brewerfile EQ "" THEN BEGIN
;;     locatedBrewerFile = 0 
;; ENDIF ELSE BEGIN
;;     locatedBrewerFile = 1
;;     IF Keyword_Set(brewer) THEN file = brewerfile
;; ENDELSE
;; IF locatedBrewerFile AND Keyword_Set(brewer) $
;;    THEN colortabletype = 'BREWER' $
;;    ELSE colortabletype = 'IDL'


file=Filepath(SubDir=['resource','colors'], 'colors1.tbl')
index=0
load_default_colortable, FILE=file, INDEX=index

object_data = Keyword_Set(object_data)
IF N_Elements(notifyID) EQ 0 THEN notifyID = [-1L, -1L]
IF N_Elements(notifyObj) EQ 0 THEN BEGIN
   notifyObj = {object:Obj_New(), method:'', wid:-1}
ENDIF
IF Size(notifyObj, /Type) NE 8 THEN BEGIN
   ok = Dialog_Message(['Arguments to the NotifyObj keyword must', $
      'be structures. Returning...'])
   RETURN
END
nelements = Size(notifyObj, /N_Elements)
FOR j=0,nelements-1 DO BEGIN
   tags = Tag_Names(notifyObj[j])
   check = Where(tags EQ 'OBJECT', count1)
   check = Where(tags EQ 'METHOD', count2)
   IF (count1 + count2) NE 2 THEN BEGIN
      ok = Dialog_Message('NotifyObj keyword has incorrect fields. Returning...')
   RETURN
   ENDIF
ENDFOR
IF N_Elements(notifyPro) EQ 0 THEN notifyPro = ""
IF N_Elements(xcolorsData) EQ 0 THEN xdata = Ptr_New(/Allocate_Heap) ELSE $
   xdata = Ptr_New(xcolorsData)
IF N_Elements(extra) EQ 0 THEN extra = Ptr_New(/Allocate_Heap) ELSE $
   extra = Ptr_New(extra)
IF Arg_Present(colorinfoPtr) THEN needcolorInfo = 1 ELSE needcolorInfo = 0
noblock = 1 - Keyword_Set(block)
block = Keyword_Set(block)

   ; Find the center of the display.

DEVICE, GET_SCREEN_SIZE=theScreenSize
IF theScreenSize[0] GT 2000 THEN theScreenSize[0] = theScreenSize[0]/2
xCenter = FIX(theScreenSize[0] / 2.0)
yCenter = FIX(theScreenSize[1] / 2.0)

IF N_ELEMENTS(xoffset) EQ 0 THEN xoffset = xCenter - 150
IF N_ELEMENTS(yoffset) EQ 0 THEN yoffset = yCenter - 200

registerName = 'XCOLORS:' + title

   ; Only one XCOLORS with this title.

IF XRegistered(registerName) GT 0 THEN BEGIN
   Ptr_Free, xdata
   Ptr_Free, extra
   IF N_Elements(colorInfoPtr) NE 0 THEN Ptr_Free, colorInfoPtr
   RETURN
ENDIF

   ; Create the top-level base. No resizing.

IF Keyword_Set(modal) AND N_Elements(group_leader) NE 0 THEN BEGIN
   tlb = Widget_Base(Column=1, Title=title, TLB_Frame_Attr=1, $
      XOffSet=xoffset, YOffSet=yoffset, Base_Align_Center=1, $
      Modal=1, Group_Leader=group_leader,MBAR=bar)
   modal = 1

ENDIF ELSE BEGIN
   tlb = Widget_Base(Column=1, Title=title, TLB_Frame_Attr=1, $
      XOffSet=xoffset, YOffSet=yoffset, Base_Align_Center=1, Space=5,MBAR=bar)
   modal = 0
   IF N_Elements(group_leader) EQ 0 THEN group_leader = -1L
ENDELSE

   ; Create a draw widget to display the current colors.

   draw = Widget_Draw(tlb, XSize=256, YSize=40, Retain=2)

IF N_Elements(bottom) EQ 0 THEN bottom = 0
IF N_Elements(ncolors) EQ 0 THEN ncolors = (256 < !D.Table_Size) - bottom
IF (ncolors + bottom) GT 256 THEN ncolors = 256 - bottom

   ; Load colors in INDEX if specified.

IF N_Elements(index) NE 0 THEN BEGIN
   LoadCT, index, File=file, /Silent, NColors=ncolors, Bottom=bottom
ENDIF ELSE index = -1

   ; Create a pointer to the color information.

TVLCT, rr, gg, bb, /Get
;;colorInfoPtr = Ptr_New({R:rr, G:gg, B:bb, Name:'Unknown', $
 ;;                        Index:index, Type:colortabletype})
colorInfoPtr = Ptr_New({R:rr, G:gg, B:bb, Name:'Unknown', $
   Index:index})

   ; Calculate top parameter.

top = ncolors + bottom - 1

   ; Create sliders to control stretchs and gamma correction.

IF 1 - Keyword_Set(nosliders) THEN BEGIN
   sliderbase = Widget_Base(tlb, Column=1, XPad=0, YPad=0)
   botSlider = Widget_Slider(sliderbase, Value=0, Min=0, $
      Max=ncolors-1, XSize=256,Event_Pro='XColors_Bottom_Slider', $
      Title='Stretch Bottom', Drag=drag)
   topSlider = Widget_Slider(sliderbase, Value=ncolors-1, Min=0, $
      Max=ncolors-1, XSize=256, Event_Pro='XColors_Top_Slider', $
      Title='Stretch Top', Drag=drag)
   gammaID = Widget_Label(sliderbase, Value=String(1.0, Format='(F6.3)'))
   gammaSlider = Widget_Slider(sliderbase, Value=50.0, Min=0, Max=100, $
      Drag=drag, XSize=256, /Suppress_Value, Event_Pro='XColors_Gamma_Slider', $
      Title='Gamma Correction')
ENDIF ELSE BEGIN
   botSlider = 0L
   topSlider = 0L
   gammaID = 0L
   gammaSlider = 0L
ENDELSE

   ; A row for additional control.
   ; IF locatedBrewerFile THEN BEGIN
   ;      colorTypeBase = Widget_Base(tlb, ROW=1, BASE_ALIGN_CENTER=1)
   ;      colorType = Widget_Droplist(colorTypeBase, Value=[' IDL Colors ', ' Custom Colors '], $
   ;          /DYNAMIC_RESIZE, EVENT_PRO='XCOLORS_SWITCH_COLORS', UVALUE=['IDL','BREWER']) 
   ;          IF Keyword_Set(Brewer) THEN Widget_Control, colorType, Set_Droplist_Select=1
   ;  ENDIF ELSE colorTypeBase = -1

   

   ; Get the colortable names for the list widget.


names=''
LoadCT, Get_Names=names, File=file
colorNamesPtr=PTR_NEW(names)
IF index NE -1 THEN ctname = (*colorNamesPtr)[index] ELSE ctname = 'Unknown'
colorNamesIndex = StrArr(N_Elements(*colorNamesPtr))
FOR j=0,N_Elements(colorNamesIndex)-1 DO colorNamesIndex[j] = StrTrim(j,2) + ' - ' + (*colorNamesPtr)[j]
tableListBase = Widget_Base(tlb, XPad=0, YPad=0)
tablelist = Widget_List(tableListBase, Value=colorNamesIndex, YSize=12 + (12*Keyword_Set(nosliders)), Scr_XSize=256, $
   Event_Pro='XColors_ColorTable')

   ; Dialog Buttons

;; dialogbase0 = WIDGET_BASE(tlb, Row=1, BASE_ALIGN_CENTER=1)
dialogbase0 = WIDGET_BUTTON(bar, VALUE='File', /MENU)
dialogbase1 = WIDGET_BUTTON(bar, VALUE='Edit', /MENU)

xpal = Widget_Button(dialogbase0, Value='Open Colortable File', $
                     Event_Pro='XCOLORS_LOADFILE', UVALUE='LOADFILE')
xpal = Widget_Button(dialogbase0, Value='Import CSV RGB Table(s)', $
                     Event_Pro='XCOLORS_IMPORTCSV', UVALUE='IMPORTCSV')
xpal = Widget_Button(dialogbase0, Value='Save Colortable to File', $
                     Event_Pro='XColors_SAVEFILE', UVALUE='SAVEFILE')

dialogbase = WIDGET_BASE(tlb, Row=1, BASE_ALIGN_CENTER=1)
dismiss = Widget_Button(dialogbase, Value='   Apply   ', $
                        Event_Pro='XColors_Dismiss', UVALUE='ACCEPT')
xpal = Widget_Button(dialogbase1, Value='Modify Current Palette', $
                     Event_Pro='XColors_Modify', UVALUE='MODIFY')
xpal = Widget_Button(dialogbase1, Value='Delete Current Palette', $
                     Event_Pro='XCOLORS_Delete_current', UVALUE='DELETE')
xpal = Widget_Button(dialogbase1, Value='Rename Current Palette', $
                     Event_Pro='XCOLORS_Rename_current', UVALUE='RENAME')
cancel = Widget_Button(dialogbase, Value='   Close   ', $
   Event_Pro='XColors_Cancel', UVALUE='CANCEL')




Widget_Control, tlb, /Realize

   ; If you used INDEX, then position this color table near the top of the list.
IF index NE -1 THEN BEGIN
   Widget_Control, tablelist, Set_List_Top=(0 > (index-3))
   Widget_Control, tablelist, Set_List_Select=index
ENDIF

   ; Get window index number of the draw widget.

Widget_Control, draw, Get_Value=windowIndex

   ; Put a picture of the color table in the window.

bar = BINDGEN(ncolors) # REPLICATE(1B, 10)
bar = BYTSCL(bar, TOP=ncolors-1) + bottom
bar = XColors_Congrid(bar, 256, 40, /INTERP)
WSet, windowIndex
Device, Get_Decomposed=theState, Decomposed=0
TV, bar
Device, Decomposed=theState

   ; Get the colors that make up the current color table
   ; in the range that this program deals with.

TVLCT, rr, gg, bb, /Get
r = rr(bottom:top)
g = gg(bottom:top)
b = bb(bottom:top)

topColor = [rr(top), gg(top), bb(top)]
bottomColor = [rr(bottom), gg(bottom), bb(bottom)]

   ; Create a cancel structure.

cancelstruct = {currenttop:top, currentbottom:bottom, $
   reverse:0, windowindex:windowindex, $
   bottomcolor:bottomcolor, topcolor:topcolor, gamma:1.0, $
   top:top, bottom:bottom, ncolors:ncolors, r:r, $
   g:g, b:b, notifyID:notifyID, index:index, $
   colorimage:bar, from:'CANCEL', notifyObj:notifyObj, extra:extra, $
   thisWindow:thisWindow, notifyPro:notifyPro, xcolorsData:xData, $
   colorInfoPtr:colorInfoPtr, colornamesPtr:colornamesPtr, ctname:ctname, $
      ;;          needColorInfo:needColorInfo, colortabletype:colortabletype, $
   needColorInfo:needColorInfo, $
   object_data:object_data}


   ; Create an info structure to hold information to run the program.

info = {  windowIndex:windowIndex, $         ; The WID of the draw widget.
          botSlider:botSlider, $             ; The widget ID of the bottom slider.
          currentBottom:bottom, $            ; The current bottom slider value.
          currentTop:top, $                  ; The current top slider value.
          topSlider:topSlider, $             ; The widget ID of the top slider.
          gammaSlider:gammaSlider, $         ; The widget ID of the gamma slider.
          gammaID:gammaID, $                 ; The widget ID of the gamma label
          ncolors:ncolors, $                 ; The number of colors we are using.
          gamma:1.0, $                       ; The current gamma value.
          file:file, $                       ; The name of the color table file.
          bottom:bottom, $                   ; The bottom color index.
          top:top, $                         ; The top color index.
          topcolor:topColor, $               ; The top color in this color table.
          bottomcolor:bottomColor, $         ; The bottom color in this color table.
          reverse:0, $                       ; A reverse color table flag.
          nosliders:Keyword_set(nosliders), $; A no slider flag.
          notifyID:notifyID, $               ; Notification widget IDs.
          notifyObj:notifyObj, $             ; An vector of structures containng info about objects to notify.
          notifyPro:notifyPro, $             ; The name of a procedure to notify.
          r:r, $                             ; The red color vector.
          g:g, $                             ; The green color vector.
          b:b, $                             ; The blue color vector.
          extra:extra, $                     ; A pointer to extra keywords.
          oindex:index, $                    ; The original color table number.
          index:index, $                     ; The current color table number.
          thisWindow:thisWindow, $           ; The current graphics window when this program is called.
          xcolorsData:xdata, $               ; A pointer to the xcolorData variable passed into the program.
          rstart:r, $                        ; The original red color vector.
          gstart:g, $                        ; The original green color vector.
          bstart:b, $                        ; The original blue color vector.
          colorInfoPtr:colorInfoPtr, $       ; A pointer to the color information.
          colornamesPtr:colornamesPtr, $           ; The names of the color tables.
          ctname:ctname, $                   ; The current color table name.
          needColorInfo:needColorInfo, $     ; A flag that indicates color information is requested.
          cancelStruct:cancelStruct, $       ; The cancel structure.
          drag:drag, $                       ; Need additional information for switching to BREWER colors and visa versa.
          modal:modal, $
          group_leader:group_leader, $
          block:block, $
          title:title, $
          tableListBase:tableListBase, $     ; The base holding the color table list.
          tableList:tableList, $
          object_data:object_data, $         ; Flag to indicate data should be sent with object notification.
 ;;         colortabletype:colortabletype, $   ; The type of color table, e.g, BREWER or IDL.
          colorimage:bar }                   ; The color table image.

   ; Turn color protection on.

IF !D.NAME NE 'MAC' THEN Widget_Control, draw, Draw_Expose_Events=1

   ; Store the info structure in the user value of the top-level base.
Widget_Control, tlb, Set_UValue=info, /No_Copy
Widget_Control, tlb, /Managed
WSet, thisWindow

XManager, registerName, tlb, Group=(group_leader GE 0) ? group_leader : xx, No_Block=noblock, Cleanup="XColors_Cleanup"
   ; Return the colorInfo information as a structure if this program
   ; was called as a modal widget.

IF (Keyword_Set(modal) AND N_Elements(group_leader) NE 0 AND needColorInfo) OR (noblock EQ 0 AND needColorInfo) THEN BEGIN
   colorStruct = *colorInfoPtr
   Ptr_Free, colorInfoPtr
   colorInfoPtr = colorStruct
ENDIF

END ; ************************************************************************
