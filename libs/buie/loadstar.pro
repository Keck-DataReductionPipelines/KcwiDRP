;+
; NAME:
;  loadstar
; PURPOSE: (one line)
;  Load the master star catalog file
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  loadstar
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     FILE    - Name of star catalog file.  Default is:
;                 '/net/frakir/raid/buie/starcat/starcat.dat'
;     FLUSH_INFO - Flush all cached information, force a clean start
;
; OUTPUTS:
;   common block is updated
; COMMON BLOCKS:
;     MWB_STARCAT
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/21, Written by Marc W. Buie, Lowell Observatory
;  2004/02/09, MWB, changed path to starcat.dat file.
;-
pro loadstar,FILE=file,FLUSH_INFO=flush_info

   common mwb_starcat, info

   if badpar(file,[0,7],0,CALLER='LOADSTAR: (file) ', $
         default='/net/frakir/raid/buie/starcat/starcat.dat') then return

   if keyword_set(flush_info) then info=0

   fmt='(a16,2(1x,i2),1x,f4.1,1x,a1,i2,2(1x,i2),2(1x,f6.3),a)'

   sz_info=size(info)

   ; If no structure yet, the data must be loaded fresh
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      reload=1

   ; If there is a structure, check file date to see if reload is needed.
   endif else begin
      if !version.os_family eq 'Windows' then begin
         result='loaded'
      endif else begin
         spawn,'ls -l '+file,result
      endelse
      if info.nobj eq 0 then $
         reload=1 $
      else $
         if result[0] ne info.catdate then reload=1 else reload=0
   endelse

   ; This reads from the file and calls ADDSTARS to add data to common
   if reload then begin

      info = { nobj: 0 }

      if exists( file ) then begin
         in_name=''
         sign=''
         in_alias=''

         openr,lun,file,/get_lun
         nobj=0
         while not eof(lun) do begin
            readf,lun,in_name,format='(a)'
            nobj=nobj+1
         endwhile
         point_lun,lun,0

         name   = strarr(nobj)
         ra     = fltarr(nobj)
         rap    = fltarr(nobj)
         dec    = fltarr(nobj) 
         decp   = fltarr(nobj)
         alias  = strarr(nobj)

         for i=0,nobj-1 do begin
            readf, lun, format=fmt, $
               in_name, rah, ram, ras, sign, decd, decm, decs, $
               in_rap, in_decp, in_alias

            name[i] = strtrim(in_name,2)
            ra[i]   = (rah + (ram + ras/60.0)/60.0)*15.0 / !radeg
            dec[i]  = (decd + (decm + decs/60.0)/60.0) / !radeg
            if sign eq '-' then dec[i] = -1.0*dec[i]
            rap[i]  = in_rap / 3600.0 / !radeg
            decp[i] = in_decp / 3600.0 / !radeg
            alias[i]= strtrim(strcompress(in_alias),2)
         endfor

         free_lun, lun

         addstars,name,ra,dec,rap,decp,alias,/NOLOAD
         if !version.os_family eq 'Windows' then begin
            result='loaded'
         endif else begin
            spawn,'ls -l '+file,result
         endelse
         info.catdate = result[0]

      endif ; file read

   endif ; RELOAD

end
