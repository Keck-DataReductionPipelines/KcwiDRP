;+
; NAME:
;  ppmsprmt
; PURPOSE:
;  Promote version of photometry parmameter file to highest version.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  ppmsprmt,parmfile
; INPUTS:
;
;  parmfile - Phot Params file.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  GUI - Flag, if set will query for needed input with a GUI tool.  The default
;          is to ask on the command line.
; OUTPUTS:
;
;  The  parm file is updated to the most recent version.  Not changed if already
;    current.
;
;   The following formats are supported:
;   v01  Version line at the start:   phot_parms_v01
;         Total of 11 lines including the version.
;         version, photometry log file name, aperture radius, inner sky radius,
;         outer sky radius, local max box radius, ccd gain, plate scale,
;         exact position flag, nominal extinction, zero pt.
;   v02  Version line at the start:   phot_parms_v02
;         Total of 12 lines including the version.
;         version, photometry log file name, aperture radius, inner sky radius,
;         outer sky radius, local max box radius, ccd gain, plate scale,
;         read noise, exact position flag, nominal extinction, zero pt.
;
;         v02 adds the read noise line after line 8 (plate scale) to v01.
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
;  2006/08/07, Written by Peter L. Collins, Lowell Observatory
;
;-
pro ppmsprmt,parmsfile,GUI=gui

   self='ppmsprmt: '
   if badpar(parmsfile,7,0,caller=self+'(parmsfile) ') then return
   if badpar(gui,[0,1,2,3],0,caller=self+'(GUI) ',default=0) then return

   ; If not present, don't do anything.
   if not exists(parmsfile) then return

   ; Check the file version by reading the first line of the file.
   version=''
   openr,lun,parmsfile,/get_lun
   readf,lun,version,format='(a)'
   free_lun, lun

   v01='phot_parms_v01'
   v02='phot_parms_v02'

   latest=v02

   ; If it's current, do nothing.
   if version eq latest then return

   if version ne v02 and version ne v01 then begin
      print, self, ' parms file has unrecognized version ', version
      return
   endif

   print,self, ' Upgrading file from ',version,' to ',latest


   ; Read the file
   openr,lun,parmsfile,/get_lun

   ;Read through and count the number of lines.
   line=''
   nobs=0
   while(not eof(lun)) do begin
      readf,lun,line,format='(a1)'
      nobs=nobs+1
   endwhile

   ;don't count the version line
   nobs=nobs-1

   if version eq v01 then begin
      if gui then begin
         rdnoise=qinput(prompt='Read Noise (e-)? ',/floating)
      endif else begin
         read,prompt='Read Noise (e-)? ',rdnoise
      endelse
      noiseline = strtrim(string(rdnoise, '  CCD read noise (e-/pixel)', $
                          format='(f6.2,a)'),1)
      print, 'adding line: '
      print, noiseline
   endif

   ;Rewind file.
   point_lun,lun,0

   ;Create the output data vector
   infoline = strarr(nobs)

   ; skip the version line.
   readf,lun,line,format='(a)'

   for i=0,nobs-1 do begin

      ; Get the next input line.
      readf,lun,line,format='(a)'

      infoline[i] = line
   endfor


   free_lun,lun

   ; Now write out the new file
   openw,lun,parmsfile,/get_lun
   printf,lun,latest  ; new version line
   for i=0,nobs-1 do begin
      printf, lun, format='(a)',infoline[i]
      ;
      ;add the read noise after line 8, which is numbered 6 here
      ; since we count from 0 and don't include the version.
      if i eq 6 then $
         printf, lun, format='(a)', noiseline
   endfor

   free_lun,lun

end
