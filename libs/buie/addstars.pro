;+
; NAME:
;  addstars
; PURPOSE: (one line)
;  Manipulate the master star catalog (add/replace)
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  addstars,name,ra,dec,rap,decp,alias
; INPUTS:
;     name   - Name of the star.
;     ra     - Right Ascension in radians. J2000
;     dec    - Declination in radians.     J2000
;     rap    - RA proper motion, "/year
;     decp   - Dec proper motion, "/year
; OPTIONAL INPUT PARAMETERS:
;     alias  - String with aliases for star name, each alias must be separated
;                by the | character, imbedded blanks are allowed but only one
;                is preserved and all leading and trailing blanks are removed.
;                the default is have no aliases.
; KEYWORD INPUT PARAMETERS:
;     FILE    - Name of star catalog file.  Default is:
;                 '/net/frakir/raid/buie/starcat/starcat.dat'
;     FLUSH_INFO - Flush all cached information, force a clean start
;     NOLOAD  - Flag, if set, suppresses the call to LOADSTAR.  This is
;                  intended to be used ONLY when this routine is called by
;                  LOADSTAR to prevent infinite recursion.  When set, it
;                  is assumed that the common block is empty and all its
;                  contents will be overwritten.
;
; OUTPUTS:
;   common block and file are updated
; COMMON BLOCKS:
;     MWB_STARCAT
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/21, Written by Marc W. Buie, Lowell Observatory
;  2004/02/09, MWB, changed path to starcat.dat file.
;-
pro addstars,name,ra,dec,rap,decp,alias, $
       FILE=file,FLUSH_INFO=flush_info,NOLOAD=noload

   common mwb_starcat, info

   if n_params() ne 6 then begin
      ;Show the calling sequence.
      print,'addstars,name,ra,dec,rap,decp,alias,FILE=file'
      return
   endif

   if badpar(name,7,[0,1],CALLER='ADDSTARS: (name) ',npts=n1) then return
   if badpar(ra,[4,5],[0,1],CALLER='ADDSTARS: (ra) ',npts=n2) then return
   if badpar(dec,[4,5],[0,1],CALLER='ADDSTARS: (dec) ',npts=n3) then return
   if badpar(rap,[4,5],[0,1],CALLER='ADDSTARS: (rap) ',npts=n4) then return
   if badpar(decp,[4,5],[0,1],CALLER='ADDSTARS: (decp) ',npts=n5) then return
   if badpar(alias,7,[0,1],CALLER='ADDSTARS: (alias) ', $
             default=strarr(n1),npts=n6) then return

   if badpar(file,[0,7],0,CALLER='RDLAND2: (file) ', $
         default='/net/frakir/raid/buie/starcat/starcat.dat') then return

   n=[n1,n2,n3,n4,n5,n6]
   if min(n) ne max(n) then begin
      message,'Input quantities must all have same length.',/info
      return
   endif
   if not keyword_set(noload) then begin
      ; Ensure common has the current stars.
      loadstar,FLUSH_INFO=flush_info,file=file
   endif

   if info.nobj eq 0 then begin
      new_nobj  = 0
   endif else begin
      ; Make a copy of the common block information for manipulation.
      new_name  = info.name
      new_ra    = info.ra
      new_dec   = info.dec
      new_rap   = info.rap
      new_decp  = info.decp
      new_alias = info.alias
      new_nobj  = info.nobj
   endelse

; Nothing in catalog, just add all the new stuff.
   if new_nobj eq 0 then begin
      new_name  = name
      new_ra    = ra
      new_dec   = dec
      new_rap   = rap
      new_decp  = decp
      new_alias = alias
      new_nobj  = n1

; Must augment existing catalog.
   endif else begin

      ; Step through the new objects, if found in existing list, then replace
      ;  the entry.  If not found, add to end of list.
      for i=0,n_elements(name)-1 do begin
         z=where(name[i] eq new_name,count)
         z=z[0]

         ; Replace
         if count eq 1 then begin
            new_ra[z]    = ra[i]
            new_dec[z]   = dec[i]
            new_rap[z]   = rap[i]
            new_decp[z]  = decp[i]
            new_alias[z] = alias[i]

         ; Add new
         endif else if count eq 0 then begin
            new_name  = [new_name, name[i]]
            new_ra    = [new_ra,   ra[i]  ]
            new_dec   = [new_dec,  dec[i] ]
            new_rap   = [new_rap,  rap[i] ]
            new_decp  = [new_decp, decp[i]]
            new_alias = [new_alias,alias[i]]
            new_nobj  = new_nobj+1

         ; Duplicate entries (shouldn't happen)
         endif else begin
            print,'%%ADDSTARS: WARNING! duplicate catalog entry found for ',name[i]
            new_ra[z]    = ra[i]
            new_dec[z]   = dec[i]
            new_rap[z]   = rap[i]
            new_decp[z]  = decp[i]
            new_alias[z] = alias[i]
         endelse

      endfor

   endelse

   ; Sort the catalog by ra and regenerate the common block
   idx=sort(new_ra)
   info = { $
      name:    new_name[idx], $
      ra:      new_ra[idx], $
      dec:     new_dec[idx], $
      rap:     new_rap[idx], $
      decp:    new_decp[idx], $
      alias:   new_alias[idx], $
      nobj:    new_nobj, $
      catdate: '' $
      }

   ; force the contents of the common block back to the catalog file.
   if not keyword_set(noload) then savestar,FILE=file

end

