;+
; NAME:
;  calibchg
; PURPOSE:
;  Calibration structure maintenance utility.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  calibchg,calib,item,subitem,CALIBPATH=calibpath
; INPUTS:
;  calib - Input calibration structure (note this precise
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  99/06/05, Written by Marc W. Buie, Lowell Observatory
;  99/11/15 - MWB, fixed bug for case of adding a filter to an empty flat list.
;-
pro calibchg,calib,in_field,tag,in_name,CALIBPATH=in_calibpath

   if badpar(in_calibpath,[0,7],0,caller='CALIBED: (CALIBPATH) ', $
                                  default='') then return

   if in_calibpath eq '' then calibpath = '' $
   else calibpath = addslash(in_calibpath)


   ; Regularize the new name
   if in_name eq '' then in_name='[none]'

   ; Make a copy of the name for adding the path
   name=in_name
   relpath,name,calibpath

   if in_name ne '[none]' and not exists(name) then begin
      print,'CALIBCHG: Error!  Calibration file ',name,' was not found.'
      return
   endif

   cmdstr = 'calib={'

   field = strlowcase(in_field)
   tags = strlowcase(tag_names(calib))
   ntags = n_tags(calib)
   foundit = 0

   case field OF

      'bias': begin

         ; Set the new values for the structure
         if in_name eq '[none]' then begin
            print,'CALIBCHG: disable bias correction'
            bias=0
         endif else begin
            print,'CALIBCHG: load bias (',in_name,')=',name
            bias=readfits(name,/silent)
         endelse

         ; Build the command that will rebuild the structure using new stuff
         for i=0,ntags-1 do begin
            if tags[i] eq 'bias' then begin
               newtag = tags[i]+':temporary(bias)'
               foundit = 1
            endif else if tags[i] eq 'bname' then begin
               newtag = tags[i]+':in_name'
            endif else begin
               newtag = tags[i]+':temporary(calib.'+tags[i]+')'
            endelse
            cmdstr = cmdstr + newtag
            if i ne ntags-1 then cmdstr=cmdstr+','
         endfor

      end ; bias

      'dark': begin

         ; Set the new values for the structure
         if in_name eq '[none]' then begin
            print,'CALIBCHG: disable dark correction'
            dark=0
         endif else begin
            print,'CALIBCHG: load dark (',in_name,')=',name
            dark=readfits(name,/silent)
         endelse

         ; Build the command that will rebuild the structure using new stuff
         for i=0,ntags-1 do begin
            if tags[i] eq 'dark' then begin
               newtag = tags[i]+':temporary(dark)'
               foundit = 1
            endif else if tags[i] eq 'dname' then begin
               newtag = tags[i]+':in_name'
            endif else begin
               newtag = tags[i]+':temporary(calib.'+tags[i]+')'
            endelse
            cmdstr = cmdstr + newtag
            if i ne ntags-1 then cmdstr=cmdstr+','
         endfor

      end ; dark

      'flat': begin

         ; Blank tag not allowed
         if tag eq '' then begin
            print,'CALIBCHG: Error!  filter name (tag) must not be empty.'
            return
         endif

         if calib.nfilters eq 0 then begin
            if in_name eq '[none]' then in_name=''
            flname   = ['']
            flatptr  = [-1]
            filter   = [tag]
            nfilters =  1
            z = nfilters-1
            frngname = ['']
            frngptr  = [-1]
            nflats=0
         endif else begin
            ; See if filter (tag) is in the current list
            z=where(tag eq calib.filter)
            z=z[0]

            if in_name eq '[none]' then in_name=''

            ; not in list, must add to it
            if z eq -1 then begin
               flname   = [calib.flname,'']
               flatptr  = [calib.flatptr,-1]
               filter   = [calib.filter,tag]
               nfilters =  calib.nfilters+1
               z = nfilters-1
               frngname = [calib.frngname,'']
               frngptr  = [calib.frngptr,-1]
            endif else begin
               flname   = calib.flname
               flatptr  = calib.flatptr
               filter   = calib.filter
               nfilters = calib.nfilters
               frngname = calib.frngname
               frngptr  = calib.frngptr
            endelse
            nflats = calib.nflats
         endelse

         ; Set the new values for the structure
         if in_name eq '' then begin
            if flatptr[z] eq -1 then begin
               flat = temporary(calib.flat)
            endif else begin
               zk = where(filter ne tag and flatptr ne -1,count)
               if count eq 0 then begin
                  flat = 1
                  nflats = 0
                  flatptr[z] = -1
               endif else begin
                  flat = temporary(calib.flat[*,*,zk])
                  nflats = nflats - 1
                  flatptr[z] = -1
                  flatptr = flatptr - (((flatptr > z) - z ) < 1)
               endelse
            endelse

         endif else begin
            print,'CALIBCHG: load flat [',tag,'] (',in_name,')=',name
            flat0=readfits(name,/silent)

            if flatptr[z] eq -1 then begin
               flat = [[[temporary(calib.flat)]],[[flat0]]]
               flatptr[z] = nflats
               nflats = nflats + 1
            endif else begin
               flat = temporary(calib.flat)
               flat[*,*,flatptr[z]] = flat0
            endelse
         endelse
         flname[z] = in_name

         ; Build the command that will rebuild the structure using new stuff
         for i=0,ntags-1 do begin
            if tags[i] eq 'flat' then begin
               newtag = tags[i]+':temporary(flat)'
               foundit = 1
            endif else if tags[i] eq 'flname' then begin
               newtag = tags[i]+':flname'
            endif else if tags[i] eq 'flatptr' then begin
               newtag = tags[i]+':flatptr'
            endif else if tags[i] eq 'filter' then begin
               newtag = tags[i]+':filter'
            endif else if tags[i] eq 'nfilters' then begin
               newtag = tags[i]+':nfilters'
            endif else if tags[i] eq 'nflats' then begin
               newtag = tags[i]+':nflats'
            endif else if tags[i] eq 'frngname' then begin
               newtag = tags[i]+':frngname'
            endif else if tags[i] eq 'frngptr' then begin
               newtag = tags[i]+':frngptr'
            endif else begin
               newtag = tags[i]+':temporary(calib.'+tags[i]+')'
            endelse
            cmdstr = cmdstr + newtag
            if i ne ntags-1 then cmdstr=cmdstr+','
         endfor

      end

      'fringe': begin

         ; Blank tag not allowed
         if tag eq '' then begin
            print,'CALIBCHG: Error!  filter name (tag) must not be empty.'
            return
         endif

         ; See if filter (tag) is in the current list
         z=where(tag eq calib.filter)
         z=z[0]

         if in_name eq '[none]' then in_name=''

         ; not in list, must add to it
         if z eq -1 then begin
            flname   = [calib.flname,'']
            flatptr  = [calib.flatptr,-1]
            filter   = [calib.filter,tag]
            nfilters =  calib.nfilters+1
            z = nfilters-1
            frngname = [calib.frngname,'']
            frngptr  = [calib.frngptr,-1]
         endif else begin
            flname   = calib.flname
            flatptr  = calib.flatptr
            filter   = calib.filter
            nfilters = calib.nfilters
            frngname = calib.frngname
            frngptr  = calib.frngptr
         endelse

         ; Set the new values for the structure
         if in_name eq '' then begin
            if frngptr[z] eq -1 then begin
               frngarr = temporary(calib.frngarr)
            endif else begin
               zk = where(filter ne tag and frngptr ne -1,count)
               if count eq 0 then begin
                  frngarr = 1
                  frngptr[z] = -1
               endif else begin
                  frngarr = temporary(calib.frngarr[*,*,zk])
                  frngptr[z] = -1
                  frngptr = frngptr - (((frngptr > z) - z ) < 1)
               endelse
            endelse

         endif else begin
            print,'CALIBCHG: load fringe [',tag,'] (',in_name,')=',name
            fringe0=readfits(name,/silent)

            if frngptr[z] eq -1 then begin
               if max(frngptr) eq -1 then begin
                  frngarr = temporary(fringe0)
                  frngptr[z] = 0
               endif else begin
                  frngarr = [[[temporary(calib.frngarr)]],[[temporary(fringe0)]]]
                  frngptr[z] = max(frngptr)+1
               endelse
            endif else begin
               frngarr = temporary(calib.frngarr)
               frngarr[*,*,frngptr[z]] = fringe0
            endelse
         endelse
         frngname[z] = in_name

         ; Build the command that will rebuild the structure using new stuff
         for i=0,ntags-1 do begin
            if tags[i] eq 'frngarr' then begin
               newtag = tags[i]+':temporary(frngarr)'
               foundit = 1
            endif else if tags[i] eq 'flname' then begin
               newtag = tags[i]+':flname'
            endif else if tags[i] eq 'flatptr' then begin
               newtag = tags[i]+':flatptr'
            endif else if tags[i] eq 'filter' then begin
               newtag = tags[i]+':filter'
            endif else if tags[i] eq 'nfilters' then begin
               newtag = tags[i]+':nfilters'
            endif else if tags[i] eq 'frngname' then begin
               newtag = tags[i]+':frngname'
            endif else if tags[i] eq 'frngptr' then begin
               newtag = tags[i]+':frngptr'
            endif else begin
               newtag = tags[i]+':temporary(calib.'+tags[i]+')'
            endelse
            cmdstr = cmdstr + newtag
            if i ne ntags-1 then cmdstr=cmdstr+','
         endfor

      end

      else: begin
         print,'CALIBCHG: Error!  Field [',field, $
               '] cannot be changed with this routine.'
         return
      end
   endcase

   cmdstr = cmdstr + '}'

   if not foundit then begin
      print,'CALIBCHG: Error!  Field [',field,'] was not found in the structure.'
      return
   endif

   r=execute(cmdstr)

end
