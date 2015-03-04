;+
; NAME: 
;  rdobscod
; PURPOSE:
;  Read standard observatory code data file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdobscod,code,lon,rhosinp,rhocosp,obsname,valid
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
;  FILE - Name of observatory code file to read.  Default=obscode.dat
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  code    - array of observatory codes
;  lon     - longitude of observatory (degrees)
;  rhosinp - rho * sin(co-lat)
;  rhocopp - rho * cos(co-lat)
;  obsname - Name of observatory
;  valid - Flag, true if the file was found and read properly.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2000/11/19, MWB, added different default file for Windows.
;  2001/03/12, MWB, changed method of locating the obscode.dat file.  Now
;                 looks in the user's IDL_PATH for this file.
;  2002/09/09, MWB, added support for string obscode values
;  2005/11/28, MWB, format of obscode.dat changed, fixed format for reading.
;-
pro rdobscod,code,lon,rhosinp,rhocosp,obsname,valid,FILE=in_file

   valid=0

   if badpar(in_file,[0,7],0,caller='rdobscod (FILE) ',default='obscode.dat') then return
   if in_file eq '' then in_file='obscode.dat'

   if not exists(in_file) then begin
      file=find_with_def(in_file,!path)
      if file eq '' then begin
         print,'RDOBSCOD: File '+in_file+' does not exist, cannot continue.'
         return
      endif
   endif else begin
      file = in_file
   endelse

   openr, lun, file, /get_lun

   fmt = '(a3,1x,f8.4,1x,f7.5,1x,f8.5,1x,a60)'
   in_obsname=''
   in_code=''

   maxcodes = 1999

   code    = strarr(maxcodes)
   lon     = fltarr(maxcodes)
   rhosinp = fltarr(maxcodes)
   rhocosp = fltarr(maxcodes)
   obsname = strarr(maxcodes)

   npts=0

   while (not eof(lun)) do begin

      if npts eq maxcodes then begin
         free_lun, lun
         message, 'Input file is too long, maximum is '+string(maxcodes)+' lines'
         return
      endif

      readf, lun, format=fmt, $
         in_code, in_lon, in_rsp, in_rcp, in_obsname

         code[npts]    = in_code
         lon[npts]     = in_lon
         rhosinp[npts] = in_rsp
         rhocosp[npts] = in_rcp
         obsname[npts] = in_obsname

      npts=npts+1

   endwhile

   free_lun, lun

   code    = code[0:npts-1]
   lon     = lon[0:npts-1]
   rhosinp = rhosinp[0:npts-1]
   rhocosp = rhocosp[0:npts-1]
   obsname = obsname[0:npts-1]
   valid   = 1

   return

bad:

   print,in_code
   free_lun, lun
   message,'Error reading file.'
   return

end
