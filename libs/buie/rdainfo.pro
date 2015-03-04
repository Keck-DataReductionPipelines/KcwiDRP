;+
; NAME:
;  rdainfo
; PURPOSE:
;  Read final astrometry information file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdainfo,file,object,firstfile,nobs,rmag,rmagerr,rate,angle,arc,
;                  dt,vsig,sel,dist,nobj
; INPUTS:
;  file - Information file to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  object    - Name of object measured (string)
;  firstfile - Name of file (may include chip id) of first measure (string)
;  nobs      - Number of observations of this object in this dataset (int)
;  rmag      - Average R magnitude of object (float)
;                 99.9 means no good magnitude could be measured
;  rmagerr   - Error on R magnitude of object (float)
;  rate      - Rate of motion, arcsec/hour (float)
;  angle     - Direction of motion, E from N, degrees (float)
;  arc       - Arclength of this measurement, arcsec (float)
;  dt        - Time span of this measurement, hours (float)
;  vsig      - velocity dispersion of measurements, sigma of "/hr (float)
;                  valid only when nobs>2
;  sel       - Solar elongation, degrees (int)
;  dist      - Approximate distance to object, AU (float)
;                  this assumes object is precisely at opposition.
;  nobj      - Total number of obects (0 if file is empty or missing).
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
;  2003/04/30, Written by Marc W. Buie, Lowell Observatory
;-
pro rdainfo,file,object,firstfile,nobs,rmag,rmagerr,rate,angle,arc, $
                 dt,vsig,sel,dist,nobj

   if not exists(file) then begin
      print,'File: ',file,' does not exists.  Unable to continue.'
      nobj=0
      return
   endif

   openr,lun,file,/get_lun
   nobj=0
   line=''
   ; skip header line
   readf,lun,line,format='(a)'
   while not eof(lun) do begin
      readf,lun,line,format='(a)'
      nobj=nobj+1
   endwhile
   point_lun,lun,0

   object    = strarr(nobj)
   firstfile = strarr(nobj)
   nobs      = intarr(nobj)
   rmag      = fltarr(nobj)
   rmagerr   = fltarr(nobj)
   rate      = fltarr(nobj)
   angle     = fltarr(nobj)
   arc       = fltarr(nobj)
   dt        = fltarr(nobj)
   vsig      = fltarr(nobj)
   sel       = intarr(nobj)
   dist      = fltarr(nobj)

   ; skip header line
   readf,lun,line,format='(a)'
   for i=0,nobj-1 do begin
      readf,lun,line,format='(a)'
      words=strsplit(line,' ',/extract)

      object[i]    = words[0]
      firstfile[i] = words[1]
      nobs[i]      = fix(words[2])
      if nobs[i] gt 1 then begin
         if words[10] eq '****' then words[10]='9.99'
         rmag[i]      = float(words[3])
         rmagerr[i]   = float(words[5])
         rate[i]      = float(words[6])
         angle[i]     = float(words[7])
         arc[i]       = float(words[8])
         dt[i]        = float(words[9])
         vsig[i]      = float(words[10])
         sel[i]       = fix(words[11])
         dist[i]      = float(words[12])
      endif else begin
         print,'Warning!  ',object[i],' has only 1 measurement.'
      endelse
   endfor

   free_lun,lun
   
end
