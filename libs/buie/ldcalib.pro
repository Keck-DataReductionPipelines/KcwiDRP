;+
; NAME: 
;  ldcalib
; PURPOSE: 
;  Load calibration frames and information as instructed by calib file.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  ldcalib,calibfile,calib,valid
; INPUTS:
;  calibfile - Name of calibration file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CALIBPATH - Optional path to the calibration information directory.
;                Default is the current directory.
;  EMPTY     - Don't look for or load files.cal.  Instead, create a calib
;                structure with "empty" values.  The empty values depend on
;                the field but should work with programs requiring this
;                structure.  The empty values will ensure that nothing would
;                be done in "calibrating" an image.
; OUTPUTS:
;  calib - anonymous structure with calibration information
;           tag contents and usage:
;              cxsize - Xsize of calibration frames (unknown if -1)
;              cysize - Ysize of calibration frames (unknown if -1)
;              xl     - overscan left boundary (-1 means no overscan).
;              xr     - overscan right boundary (-1 means no overscan).
;              x1     - LLHC (x) of cropping region.
;              x2     - URHC (x) of cropping region.
;              y1     - LLHC (y) of cropping region.
;              y2     - URHC (y) of cropping region.
;              bias   - Bias image.
;              dark   - dark image (scalar 0 if no dark correction needed).
;              flat   - Array of flat images.
;              frngarr- Array of fringe correction images.
;              frngptr- Array of pointers into frngarr for each filter.
;              filter - String array of filter names for flat array.
;  valid - Flag, if true indicates a valid calibration set was loaded.  The
;            contents of calib are unreliable if this flag is false.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  This program reads the given calibration file, decodes the information into
;    a structure with all the was in the file.  It handles reading all the
;    supported version of calibration files with the hope that all older
;    versions can always be read and made (in some way) to be upward
;    compatible.  The organization of the returned structure is, however,
;    subject to future change and expansion as the need arises.
;
;  The following describe the file format for the supported versions.
;
;  Version 1 files:
;     Line 1 - ID line, "calib_file_v01"
;     Line 2 - Instrument  (rest of file depends on this).
;
;     LCCD
;        Line 3 - Name, including path, of bias frame.
;        Line 4 to end - Filter code, and file name of flat field
;                       image for this filter.
;
;     LORAL
;        Line 3 - overscan limits (x1, x2)
;        Line 4 - cropping region (x1:x2,y1:y2)
;        Line 5 - Name, including path, of bias frame.
;        Line 6 to end - Filter code, and file name of flat field
;                     image for this filter.
;
;  Version 2 files:
;     Line 1 - ID line, calib_file_v02
;     Line 2 - overscan limits (x2,x2), if none, use -1 -1
;     Line 3 - cropping region (x1:x2,y1:y2)
;     Line 4 - Name, including path, of bias frame.
;     Line 5 to end - Filter code, and file name of flat field
;                       image for this filter.
;
;  Version 3 files:
;     Line 1 - ID line, calib_file_v03
;     Line 2 - overscan limits (x2,x2), if none, use -1 -1
;     Line 3 - cropping region (x1:x2,y1:y2)
;     Line 4 - Name, including path, of bias frame.
;     Line 5 - Name, including path, of dark frame.  [none] means not needed.
;     Line 6 to end - Filter code, and file name of flat field
;                       image for this filter plus optional fringe
;                       correction image.
;
; MODIFICATION HISTORY:
;  95/08/08, Written by Marc W. Buie, Lowell Observatory
;  99/05/16, MWB, Added support for optional fringe correction image(s).
;  99/05/20, MWB, near total rewrite to legitimize this routine for
;                   inclusion in ccdphot.pro.  Numerous changes were made
;                   that change the contents of the calibration structure
;                   but make it more general as is needed by ccdphot.
;  99/10/28, MWB, fixed cxsize,cysize bug
;  99/11/15, MWB, added EMPTY keyword
;  2000/10/12, MWB, fixed bug that caused crash when loading a file that
;                   did not have any flat field frame information.
;  2001/05/01, MWB, changed behavior so that if x1,x2 or y1,y2 make it to the
;                   end, then these values are loaded from the cxsize or cysize.
;  2010/04/22, MWB, changed to ignore blank lines in the filter section.
;  2010/11/17, MWB, added silent keyword
;-
pro ldcalib,calibfile,calib,valid, $
       CALIBPATH=calibpath,EMPTY=empty,SILENT=silent

   valid=0

   if badpar(calibfile,7,0,caller='LDCALIB: (calibfile) ') then return
   if badpar(calibpath,[0,7],0,caller='LDCALIB: (CALIBPATH) ', $
                               default='') then return
   if badpar(empty,[0,1,2,3],0,caller='LDCALIB: (EMPTY) ', $
                               default=0) then return
   if badpar(silent,[0,1,2,3],0,caller='LDCALIB: (SILENT) ', $
                                default=0) then return

   if not exists(addslash(calibpath)+calibfile) and not empty then begin
      print,'Calibration file ',addslash(calibpath)+calibfile, $
            ' does not exist.  Cannot continue.'
      return
   endif

   ; Pre-declare strings for later use.
   version=''
   instrument=''
   flatinfo=''
   bias=0
   biasname=''
   in_biasname='[none]'
   dark=0
   darkname=''
   in_darkname='[none]'
   filter=''
   nfilters=0
   filter0=''
   flat0=''
   flatarr=1
   in_flatname=''
   flatptr=0
   nflats=0
   frngarr=0
   in_frngname=''
   frngptr=0
   xl = -1
   xr = -1
   x1 = -1
   x2 = -1
   y1 = -1
   y2 = -1
   cxsize = -1
   cysize = -1

   if not empty then begin
      if not silent then $
         print,'Load calibration info from ',addslash(calibpath)+calibfile
      openr, lun, addslash(calibpath)+calibfile, /get_lun

      ;First, read in the calibration information and make sure it is a
      ;   recognized file version.
      readf, lun, version
      if version ne 'calib_file_v01' and $
         version ne 'calib_file_v02' and $
         version ne 'calib_file_v03' then begin
         print,'File version tag ['+version+'] is incorrect, cannot proceed.'
         free_lun,lun
         return
      endif

      if version eq 'calib_file_v01' then begin
         readf, lun, instrument
         if instrument eq 'LCCD' then begin
            readf, lun, in_biasname
            biasname=in_biasname
            relpath,biasname,calibpath
            if not eof(lun) then begin
               readf, lun, flatinfo
               filter   = gettok(flatinfo,' ')
               flatname = gettok(flatinfo,' ')
               in_flatname = flatname
               relpath,flatname,calibpath
               nflats=1
            endif else begin
               nflats=0
               in_flatname=''
            endelse
            while not eof(lun) do begin
               readf, lun, flatinfo
               filter   = [filter,gettok(flatinfo,' ')]
               newflat = gettok(flatinfo,' ')
               in_flatname = [in_flatname,newflat]
               relpath,newflat,calibpath
               flatname = [flatname,newflat]
            endwhile
            xl = -1
            xr = -1
            x1 = -1
            x2 = -1
            y1 = -1
            y2 = -1
         endif else if instrument eq 'LORAL' then begin
            readf, lun, xl, xr
            readf, lun, x1, x2, y1, y2
            readf, lun, in_biasname
            biasname=in_biasname
            relpath,biasname,calibpath
            if not eof(lun) then begin
               readf, lun, flatinfo
               filter   = gettok(flatinfo,' ')
               flatname = gettok(flatinfo,' ')
               in_flatname=flatname
               relpath,flatname,calibpath
            endif else begin
               nflats=0
               in_flatname=''
            endelse
            while not eof(lun) do begin
               readf, lun, flatinfo
               flatinfo = strtrim(strcompress(flatinfo),2)
               if flatinfo eq '' then continue
               filter   = [filter,gettok(flatinfo,' ')]
               newflat = gettok(flatinfo,' ')
               in_flatname = [in_flatname,newflat]
               relpath,newflat,calibpath
               flatname = [flatname,newflat]
            endwhile
         endif else begin
            print,'Unrecognized instrument name [',instrument, $
                  '], cannot proceed.'
            free_lun,lun
            return
         endelse
         darkname = '[none]'
         in_darkname = '[none]'
         if nflats eq 0 then $
            frngname = '' $
         else $
            frngname = strarr(nflats)
         in_frngname = frngname
         nfringe  = 0
      endif

      if version eq 'calib_file_v02' then begin
         readf, lun, xl, xr, format='(2i)'
         readf, lun, x1, x2, y1, y2
         readf, lun, in_biasname
         biasname=in_biasname
         relpath,biasname,calibpath
         if not eof(lun) then begin
            readf, lun, flatinfo
            filter   = gettok(flatinfo,' ')
            flatname = gettok(flatinfo,' ')
            in_flatname = flatname
            relpath,flatname,calibpath
            nflats=1
         endif else begin
            in_flatname = ''
            nflats=0
         endelse
         while not eof(lun) do begin
            readf, lun, flatinfo
            if flatinfo eq '' then continue
            filter   = [filter,gettok(flatinfo,' ')]
            newflat = gettok(flatinfo,' ')
            in_flatname = [in_flatname,newflat]
            relpath,newflat,calibpath
            flatname = [flatname,newflat]
         endwhile
         darkname = '[none]'
         in_darkname = '[none]'
         if nflats eq 0 then $
            frngname = '' $
         else $
            frngname = strarr(nflats)
         in_frngname = frngname
         nfringe  = 0
      endif

      if version eq 'calib_file_v03' then begin

         ; Load the easy stuff from the file.
         readf, lun, xl, xr
         readf, lun, x1, x2, y1, y2

         ; Bias is next
         readf, lun, in_biasname
         biasname=in_biasname
         relpath,biasname,calibpath

         ; Then dark
         readf, lun, in_darkname
         darkname=in_darkname
         relpath,darkname,calibpath

         ; At this point, the file might end or might contain one or more
         ;  filters and their information
         nfilters = 0
         nflats   = 0
         nfringe  = 0
         while not eof(lun) do begin

            ; Get the next line from the file.  This has between 1 and 3 fields.
            ;  The first field is the filter name, the second is the flat field
            ;  name, the third is the fringe correction frame.  It is not
            ;  possible to have a fringe frame without a flat frame.
            readf, lun, flatinfo
            flatinfo = strtrim(strcompress(flatinfo),2)
            if flatinfo eq '' then continue
            newfilter= gettok(flatinfo,' ')
            newflat  = gettok(flatinfo,' ')
            newfrng  = gettok(flatinfo,' ')
            in_newflat = newflat
            in_newfrng = newfrng

            ; Resolve file names by adding path
            relpath,newflat,calibpath
            relpath,newfrng,calibpath

            if newfilter ne '' then begin
               if nfilters eq 0 then begin
                  filter=newfilter
               endif else begin
                  filter=[filter,newfilter]
               endelse
               nfilters=nfilters+1
            endif

            ; Add to flat total and the file name list.
            if nfilters eq 1 then begin
               in_flatname = in_newflat
               flatname    = newflat
            endif else begin
               in_flatname = [in_flatname,in_newflat]
               flatname    = [flatname,newflat]
            endelse
            if in_newflat ne '' then nflats  = nflats + 1

            ; Add to the fringe total and the file name list.
            if nfilters eq 1 then begin
               in_frngname = in_newfrng
               frngname    = newfrng
            endif else begin
               in_frngname = [in_frngname,in_newfrng]
               frngname    = [frngname,newfrng]
            endelse
            if in_newfrng ne '' then nfringe = nfringe + 1

         endwhile

         if nfilters eq 0 then begin
            filter=''
            in_flatname=''
            in_frngname=''
         endif

      endif

      free_lun,lun

      ; These arrays will point into the master storage array for the flat and
      ;   fringe frames.
      flatptr = replicate(-1,nfilters > 1)
      frngptr = replicate(-1,nfilters > 1)

      if calibpath ne '' then $
         if not silent then print,'Load calibration frames from ',calibpath $
      else $
         if not silent then print,'Load calibration frames'

      ;Load the bias frame.
      if in_biasname ne '[none]' then begin
         if not exists(biasname) then begin
            print,'Bias frame ',biasname,' could not be found, disabling.'
            in_biasname = '[none]'
            bias=0
         endif else begin
            bias=readfits(biasname,/silent)
            if not silent then print,'Loading bias  -- ',in_biasname
            sz=size(bias,/dimen)
            cxsize = sz[0]
            cysize = sz[1]
         endelse
      endif else begin
         bias = 0
         if not silent then print,'Bias calibration step disabled.'
      endelse

      ;Load the dark frame.
      if in_darkname ne '[none]' then begin
         if not exists(darkname) then begin
            print,'Dark frame ',darkname,' could not be found, disabling.'
            in_darkname = ''
            dark=0
         endif else begin
            dark=readfits(darkname,/silent)
            if not silent then print,'Loading dark  -- ',in_darkname
            sz=size(dark,/dimen)
            if cxsize eq -1 then begin
               cxsize = sz[0]
               cysize = sz[1]
            endif else begin
               if sz[0] ne cxsize then begin
                  print,'Warning! Width of dark frame does not equal width of bias.'
                  print,'  Dark xw=',sz[0],'   bias xw=',cxsize
               endif
               if sz[1] ne cysize then begin
                  print,'Warning! Height of dark frame does not equal height of bias.'
                  print,'  Dark yw=',sz[1],'   bias yw=',cysize
               endif
            endelse
         endelse
      endif else begin
         dark = 0
         if not silent then print,'Dark calibration step disabled.'
      endelse

      ; Go through the list of flats and make sure they all exist.  Disable those
      ;   that don't.
      for i=0,nfilters-1 do begin
         if in_flatname[i] ne '' then begin
            if not exists(flatname[i]) then begin
               print,'Flat frame ',flatname[i],' could not be found.'
               print,'flat field for filter ',filter[i],' has been disabled.'
               in_flatname[i]=''
               flatname[i]=''
               nflats=nflats-1
            endif else if cxsize eq -1 then begin
               hdr=headfits(flatname[i])
               cxsize = sxpar(hdr,'NAXIS1')
               cysize = sxpar(hdr,'NAXIS2')
            endif
         endif
      endfor

      ; Setup flat storage array.
      if nflats eq 0 then begin
         flatarr=0
      endif else begin
         flatarr=fltarr(cxsize,cysize,nflats,/nozero)
      endelse

      ; Load the flats
      ptr=0
      for i=0,nfilters-1 do begin
         if in_flatname[i] ne '' then begin
            if not silent then $
               print,'Loading flat (',filter[i],') -- ',in_flatname[i]
            flatarr[*,*,ptr] = readfits(flatname[i],/silent)
            flatptr[i]=ptr
            ptr=ptr+1
         endif
      endfor

      ; Go through the list of fringe correction frames and make sure they all
      ;   exist.  Disable those that don't.
      for i=0,nfilters-1 do begin
         if in_frngname[i] ne '' then begin
            if not exists(frngname[i]) then begin
               print,'Fringe frame ',frngname[i],' could not be found.'
               print,'fringe correction for filter ',filter[i],' has been disabled.'
               in_frngname[i]=''
               frngname[i]=''
               nfringe=nfringe-1
            endif
         endif
      endfor

      ; setup storage for all fringe correction frames.
      if nfringe eq 0 then begin
         frngarr=0
      endif else begin
         frngarr=fltarr(sz[0],sz[1],nfringe,/nozero)
      endelse

      ; Load the fringe correction frames
      ptr=0
      for i=0,nfilters-1 do begin
         if in_frngname[i] ne '' then begin
            if not silent then $
               print,'Loading fringe correction (', $
                         filter[i],') -- ',in_frngname[i]
            frngarr[*,*,ptr] = readfits(frngname[i],/silent)
            frngptr[i]=ptr
            ptr=ptr+1
         endif
      endfor
   endif else begin
      if not silent then print,'Creating empty calibration info'
   endelse

   if x1 eq -1 and x2 eq -1 then begin
      x1 = 0
      x2 = cxsize-1
   endif

   if y1 eq -1 and y2 eq -1 then begin
      y1 = 0
      y2 = cysize-1
   endif

   calib = { $
               xl: xl, $
               xr: xr, $
               x1: x1, $
               x2: x2, $
               y1: y1, $
               y2: y2, $
               cxsize: cxsize, $
               cysize: cysize, $
               bias: temporary(bias), $
               bname: in_biasname, $
               dark: temporary(dark), $
               dname: in_darkname, $
               flat: temporary(flatarr), $
               flname: in_flatname,$
               flatptr: flatptr, $
               nflats: nflats,$
               frngarr: temporary(frngarr), $
               frngname: in_frngname,$
               frngptr: frngptr, $
               filter: filter, $
               nfilters: nfilters $
           }

   valid=1

end
