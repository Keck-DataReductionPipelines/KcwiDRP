;+
; NAME:
;  reducprmt
; PURPOSE:
;  Promote version of a reductor info file to highest version.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  reducprmt,infofile
; INPUTS:
;
;  infofile - Reductor info file.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  RDNOISE    -       This sets the value of the read noise parameter
;                        on line 5 when promoting the file. Otherwise,
;                        the user is prompted for the value.
;  GUI-               Flag, if set use modal widget to prompt for rdnoise.
; OUTPUTS:
;
;  The  info file is updated to the most recent version.  Not changed if already
;    current.
;
;   The following formats are supported:
;   v1.0  Version line at the start:   REDUCTOR v1.0
;         line 5 contains the photometry extraction parameters,
;         Object radius, inner sky, outer sky, gain.  
;   v1.1  Version line at the start:   REDUCTOR v1.1
;         line 5 contains the photometry extraction parameters,
;         Object radius, inner sky, outer sky, gain, readout noise.  
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
;  2006/06/22, Written by Peter L. Collins, Lowell Observatory
;  2006/08/26, PLC, modified to prompt for rdnoise if it is
;                   undefined, using qinput for this if /GUI.
;  2006/08/28, PLC, modified for instrument defaults.
;  2006/10/19, PLC, changed pccd rdnoise default to match reductor.
;
;-
pro reducprmt,infofile,RDNOISE=rdnoise,GUI=gui


;  instruments 
   instrs = ['PCCD','Nasacam','Loral/2.5']

; defaults in that order- 
;  PCCD
;  Nasacam
;  Loral/2.5
;  the last set of entries is for any other instrument.
   ;                    rad   sky1  sky2   gain  rdnoise
 instr_defaults = [ [ 15.0, 25.0, 130.0, 2.60, 16.0],  $
                    [ 15.0, 25.0, 100.0, 1.80, 15.0],  $
                    [ 15.0, 25.0, 100.0, 2.56, 20.0], $
                    [  5.0, 25.0, 40.0, 1.00, -1.00] ] 

   self='reducprmt: '
   if badpar(infofile,7,0,caller=self +  '(infofile) ') then return
   if badpar(rdnoise,[0,2,3,4,5],0,CALLER=self + ' (rdnoise) ', $
                                    default='none') then return
   if badpar(gui,[0,1,2,3],0,CALLER=self + ' (GUI) ', $
             default=0) then return

   ; If not present, don't do anything.
   IF not exists(infofile) THEN return

   ; Check the file version by reading the first line of the file.
   version=''
   openr,lun,infofile,/get_lun
   readf,lun,version,format='(a)'
   free_lun, lun

   v1pt1='REDUCTOR v1.1'
   v1pt0='REDUCTOR v1.0'

   latest=v1pt1

   ; If it's current, do nothing.
   if version eq latest then return

   if version ne v1pt1 and version ne v1pt0 then begin
      print, 'REDUCPRMT: info file has unrecognized version ', version
      return
   endif

   print,'REDUCPRMT: Upgrading file from ',version,' to ',latest


      ; Read the file
   openr,lun,infofile,/get_lun

   ;Read through and count the number of lines.
   line=''
   nobs=0
   while(not eof(lun)) do begin
      readf,lun,line,format='(a1)'
      nobs=nobs+1
   endwhile

   ;don't count the version line
   nobs=nobs-1

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

   if rdnoise eq 'none' then begin
      inst = strtrim(infoline[0],2)
      ii = where( inst eq instrs)
      ii = ii[0]
      if ii lt 0 then begin
          sz = size(instr_defaults,/DIMENSIONS)
          ii = sz[1] - 1
      endif
      rdnoise = instr_defaults[4,ii]
   endif
   if rdnoise lt 0.0 then begin
      rdnoise=10.0
      if gui then rdnoise = qinput(/FLOATING, $
                                   PROMPT='Please enter read noise', $
                                   DEFAULT=rdnoise) $
      else read,prompt='Read Noise (e-)? ',rdnoise
   endif

   free_lun,lun

   ; Now write out the new file
   openw,lun,infofile,/get_lun
   printf,lun,latest
   for i=0,nobs-1 do begin
   ; add the read noise to line 5
   ;
      if i eq 3 then begin
         printf, lun, format='(a,1x,f6.2)', infoline[i],rdnoise
      endif else begin
         printf, lun, format='(a)',infoline[i]
      endelse
   endfor

   free_lun,lun
end

