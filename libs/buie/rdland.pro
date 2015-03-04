;+
; NAME:
;  rdland
; PURPOSE: (one line)
;  Read the Landolt Standards data file.
; DESCRIPTION:
;  This program automates the process of reading the Landolt 1983 Standard
;       file as provided on the CD-ROM from NSSDC.  Selected Astronomical
;       Catalogs, Volume 1.  Also supported is the table for the 1992 catalog
;       as supplied by Arlo Landolt at Louisiana State University.
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdland,catalog,id,ra,dec,mags,colors,errs,cerrs
; INPUTS:
;     catalog - Integer key for the catalog to read in.  The only valid
;                 inputs are:
;                    83 - for the 1983 catalog.
;                    92 - for the 1992 catalog.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     EPOCH   - JD of epoch for of-date precession.
;                 Default = catalog epoch (1985.0 for 83 catalog or
;                 2000.0 for 92 catalog).
;     FILE    - Name of star catalog file to read.  Defaults are:
;                 For 83 = '/net/frakir/raid/buie/photometry/landolt/landolt.dat'
;                 For 92 = '/net/frakir/raid/buie/photometry/landolt/landolt92.dat'
;
;     Only one of the following is allowed to be set.  If none are set,
;        the coordinates will be returned in the equinox of the catalog.
;     B1950   - Coordinates should be referred to equinox of B1950.
;     J2000   - Coordinates should be referred to equinox of J2000.
;     OFDATE  - Coordinates should be referred to equinox of date.
;                 If selected, EPOCH must be provided.
;
; OUTPUTS:
;     id     - Name of the stars.
;     ra     - Right Ascension in radians. Catalog epoch, unless one of the
;                other choices, via keyword, is specified.
;     dec    - Declination in radians. Catalog epoch, unless one of the
;                other choices, via keyword, is specified.
;     mags   - Stellar magnitudes, UBVRI
;     colors - Stellar colors, U-B, B-V, V-R, R-I, V-I
;     merrs  - Uncertainties on the magnitudes.
;     cerrs  - Uncertainties on the colors.
;     nobs   - Number of observations
;     nnig   - Number of nights of observations
; COMMON BLOCKS:
;     None.
; SIDE EFFECTS:
; RESTRICTIONS:
;     This procedure is hard-coded for these particular files.  It is not
;       intended to be a general file reading program.
;
;     If file is not specified, the filename defaults are:
;          /net/frakir/raid/buie/photometry/landolt/landolt.dat   (for 83 catalog)
;          /net/frakir/raid/buie/photometry/landolt/landolt92.dat (for 92 catalog)
; PROCEDURE:
; MODIFICATION HISTORY:
;  93/01/19, Written by Marc W. Buie, Lowell Observatory
;  93/10/01, Modifications by Doug Loucks, Lowell Observatory.  The
;            1992 Landolt Catalog (Epoch 2000) is now supported.  A new
;            parameter allows selection of the the 1983 or the 1992 catalog.
;  96/11/20, MWB, now returns number of nights and number of observations.
;  2004/02/09, MWB, changed default path to catalog
;-
pro rdland,in_cat,id,ra,dec,mags,colors,errs,cerrs,nobs,nnig, $
              EPOCH=epoch,FILE=file,B1950=b1950,J2000=j2000,OFDATE=ofdate

if n_params() lt 4 then begin
   ;Show the calling sequence.
   print, 'rdland, catalog,id,ra,dec,mags,colors,errs,cerrs,'
   print, '        EPOCH=epoch,FILE=file,B1950=b1950,J2000=j2000,OFDATE=ofdate'
   return
endif

on_ioerror,bad

if badpar(in_cat,2,0,CALLER='RDLAND: (catalog) ' ) then return
if badpar(epoch,[0,5],0,CALLER='RDLAND: (epoch) ') then return
if badpar(file,[0,7],0,CALLER='RDLAND: (file) ', $
          default='default') then return
if badpar(B1950,[0,1,2,3],0,CALLER='RDLAND: (B1950) ') then return
if badpar(J2000,[0,1,2,3],0,CALLER='RDLAND: (J2000) ') then return
if badpar(OFDATE,[0,1,2,3],0,CALLER='RDLAND: (OFDATE) ') then return

set=0
if keyword_set(B1950)  then set = set+1
if keyword_set(J2000)  then set = set+1
if keyword_set(OFDATE) then set = set+1
if set gt 1 then begin
   print,'RDLAND: Error. Only one of B1950, J2000, or OFDATE can be selected.'
   return
endif

if keyword_set(OFDATE) and not keyword_set(epoch) then begin
   print,'RDLAND: Error. Epoch must be provided with OFDATE request.'
   return
endif

;Default file definitions.
def83 = '/net/frakir/raid/buie/photometry/landolt/landolt.dat'
def92 = '/net/frakir/raid/buie/photometry/landolt/landolt92.dat'

;Default format definitions.
fmt83 = '(a11,3i3,1x,a1,i2,2i3,6f7.3,2i3,1x,6f7.4)'
fmt92 = '(a11,1x,i2,1x,i2,1x,i2,2x,a1,i2,2i3,1x,6f7.3,2i4,1x,6f7.4)'

case in_cat of
   83 : begin
      fmt = fmt83
      nrec = 223
      if file eq 'default' then usefile = def83 else usefile=file
   end
   92 : begin
      fmt = fmt92
      nrec = 526
      if file eq 'default' then usefile = def92 else usefile=file
   end
   else : begin
      print, 'RDLAND: Error. Catalog parameter must be 83 or 92.'
      return
   end
endcase

if not exists( usefile ) then begin
   message,'File '+file+' does not exist, cannot continue.'
   return
endif

in_id=''
sign=''

npts=0

id     = strarr(nrec)
ra     = fltarr(nrec)
dec    = fltarr(nrec) 
mags   = fltarr(5,nrec)
colors = fltarr(5,nrec)
errs   = fltarr(5,nrec)
cerrs  = fltarr(5,nrec)
nobs   = intarr(nrec)
nnig   = intarr(nrec)

openr, lun, usefile, /get_lun

while (not eof(lun)) do begin

   if npts eq nrec+1 then begin
      free_lun, lun
      message, 'Input file is too long, expected '+nrec+' lines, probably the wrong input file.'
      return
   endif
   readf, lun, format=fmt, $
      in_id, rah, ram, ras, sign, decd, decm, decs, in_v, in_bmv, in_umb, in_vmr, $
      in_rmi, in_vmi, in_nobs, in_nnig, in_ve, in_bmve, in_umbe, in_vmre, in_rmie, in_vmie

   b = in_bmv + in_v
   u = in_umb + b
   r = in_v - in_vmr
   i = in_v - in_vmi

   be = sqrt( in_bmve^2 + in_ve^2 )
   ue = sqrt( in_umbe^2 + be^2 )
   re = sqrt( in_vmre^2 + in_ve^2 )
   ie = sqrt( in_vmie^2 + in_ve^2 )

   id[npts]       = in_id
   ra[npts]       = (rah + (ram + ras/60.0)/60.0)*15.0 / !radeg
   dec[npts]      = (decd + (decm + decs/60.0)/60.0) / !radeg
   if sign eq '-' then dec[npts] = -1.0*dec[npts]
   mags[*,npts]   = [ u, b, in_v, r, i ]
   colors[*,npts] = [ in_umb, in_bmv, in_vmr, in_rmi, in_vmi ]
   errs[*,npts]   = [ ue, be, in_ve, re, ie ]
   cerrs[*,npts]  = [ in_umbe, in_bmve, in_vmre, in_rmie, in_vmie ]
   nobs[npts]     = in_nobs
   nnig[npts]     = in_nnig

   npts=npts+1

endwhile

;id = strcompress(id,/remove_all)

free_lun, lun

if npts ne nrec then begin
   print,'RDLAND: Unexpected file length of '+string(npts)+ $
   ', should have been '+nrec+'.'
   return
endif

; Make a copy in degrees for the precession routines.
rad  = ra  * 180.0d0 / !dpi
decd = dec * 180.0d0 / !dpi

; Second, convert to requested equinox.
if keyword_set(B1950) then begin
   if in_cat eq 83 then begin
      for i=0,n_elements(rad)-1 do begin
         new_ra  = rad[i]
         new_dec = decd[i]
         precess,new_ra,new_dec,1985.0,1950.0
         rad[i]  = new_ra
         decd[i] = new_dec
      endfor
   endif else begin
      for i=0,n_elements(rad)-1 do begin
         bprecess, rad[i], decd[i], new_ra, new_dec
         rad[i]  = new_ra
         decd[i] = new_dec
      endfor
   endelse
endif

if keyword_set(J2000) and in_cat eq 83 then begin
   for i=0,n_elements(rad)-1 do begin
      new_ra  = rad[i]
      new_dec = decd[i]
      precess,new_ra,new_dec,1985.0,1950.0
      jprecess,new_ra,new_dec,new_ra1,new_dec1
      rad[i]  = new_ra1
      decd[i] = new_dec1
   endfor
endif

if keyword_set(OFDATE) then begin
   jd2year,epoch,year
   case in_cat of
      83 : begin
         for i=0,n_elements(rad)-1 do begin
            new_ra  = rad[i]
            new_dec = decd[i]
            precess,new_ra,new_dec,1985.0,year
            rad[i]  = new_ra
            decd[i] = new_dec
         endfor
      end
      92 : begin
         for i=0,n_elements(rad)-1 do begin
            bprecess, rad[i], decd[i], new_ra, new_dec
            precess,new_ra,new_dec,1950.0,year
            rad[i]  = new_ra
            decd[i] = new_dec
         endfor
      end
      else : print, 'This should never happen!'
   endcase
endif

; Convert back to radians for output.
ra  = rad  / 180.0d0 * !dpi
dec = decd / 180.0d0 * !dpi

return

bad:

free_lun, lun
message,'Error reading file.'

end
