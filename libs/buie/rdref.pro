;+
; NAME:
;  rdref
; PURPOSE:   (one line only)
;  Read a astrometry support (.ref) file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdref,file,ref,error
; INPUTS:
;  file - Name of file to be read
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NOFIX - Flag, if set will suppress the attempt to rewrite a file that
;            is in the wrong byte order.
; OUTPUTS:
;  ref  - Anonymous structure that contains the information read from file.
;            Tags in structure always provided:
;              objrad - object aperture radius (pixels)
;              nstars - Number of stars in the list
;            Tags provided if nstars > 0
;              xpos    - float, X position of star
;              ypos    - float, Y position of star
;              fwhm    - float, FWHM of object (pixels)
;              mag     - float, instrumental magnitude of star
;              err     - float, uncertainty on magnitude
;              sig_max - float, DN value of peak pixel in image
;              ra      - double, RA of star (radians)
;              dec     - double, Dec of star (radians)
;              catmag  - float, Catalog magnitude for star
;  error  - Flag, if set indicates there was a problem reading the file.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;   Files written by astrom.pro (versions prior to 2008/10/31) could have
;     been written with the wrong byte ordering.  When a file is read it
;     is checked to see if objrad and nstars make sense.  If not, the byte
;     order is swapped.  If that extra swap was needed then an attempt is
;     made to rewrite the data back to the file thus repairing the file.
;     If the file cannot be written to this program will print a warning
;     message but will still return good data to the caller.  If the file
;     can be written there will be no outward indication that this happened
;     other than the file will get changed.
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written 2008/10/31 by Marc W. Buie, Southwest Research Institute.
;-
pro rdref,file,ref,error,NOFIX=nofix

   self='rdref: '
   if badpar(file,7,0,caller=self+'(file) ') then return
   if badpar(nofix,[0,1,2,3],0,caller=self+'(NOFIX) ',default=0) then return

   on_ioerror, bad

   extraswap = 0
   
   ; Read the object radius and number of stars from the file
   openr,lun,file,/get_lun
   objrad=0.0
   nstars=0L
   readu,lun,objrad,nstars
   swap_endian_inplace,objrad,/swap_if_little_endian
   swap_endian_inplace,nstars,/swap_if_little_endian

   ; Check to see if the data need to be re-twisted
   if objrad lt 1.0 or objrad gt 20.0 then begin
      extraswap = 1
      swap_endian_inplace,nstars
      swap_endian_inplace,objrad
   endif

   ; Read the rest of the data
   if nstars gt 0 then begin
      xpos    = fltarr(nstars)
      ypos    = fltarr(nstars)
      fwhm    = fltarr(nstars)
      mag     = fltarr(nstars)
      err     = fltarr(nstars)
      sig_max = fltarr(nstars)
      ra      = dblarr(nstars)
      dec     = dblarr(nstars)
      catmag  = fltarr(nstars)
      readu,lun,xpos,ypos,fwhm,mag,err,sig_max,ra,dec,catmag
      swap_endian_inplace,xpos,/swap_if_little_endian
      swap_endian_inplace,ypos,/swap_if_little_endian
      swap_endian_inplace,fwhm,/swap_if_little_endian
      swap_endian_inplace,mag, /swap_if_little_endian
      swap_endian_inplace,err, /swap_if_little_endian
      swap_endian_inplace,sig_max,/swap_if_little_endian
      swap_endian_inplace,ra,  /swap_if_little_endian
      swap_endian_inplace,dec, /swap_if_little_endian
      swap_endian_inplace,catmag,/swap_if_little_endian
      if extraswap then begin
         swap_endian_inplace,xpos
         swap_endian_inplace,ypos
         swap_endian_inplace,fwhm
         swap_endian_inplace,mag
         swap_endian_inplace,err
         swap_endian_inplace,sig_max
         swap_endian_inplace,ra
         swap_endian_inplace,dec
         swap_endian_inplace,catmag
      endif
   endif

   free_lun,lun
   error=0

   ; Pack the data up for return
   if nstars eq 0 then begin
      ref = { objrad: objrad, nstars: nstars }
   endif else begin
      ref = { $
            objrad: objrad, $
            nstars: nstars, $
            xpos:   xpos, $
            ypos:   ypos, $
            fwhm:   fwhm, $
            mag:    mag, $
            err:    err, $
            sig_max: sig_max, $
            ra:     ra, $
            dec:    dec, $
            catmag: catmag $
            }
   endelse

   ; Try to save the file in the right format if extraswap was needed
   if not nofix and extraswap then begin
      wrref,file,ref,error
   endif
   return

bad:
   print,self,'File name - ',file
   print,!error_state.msg
   print,!error_state.sys_msg
   error=1

end
