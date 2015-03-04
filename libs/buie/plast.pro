;+
; NAME:
;  plast
; PURPOSE:
;  Support routine for calling ``PLAST'' to get asteroids on an image.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  plast,jd,ra,dec,width,height,bmaglim,rmaglim,outfile
;
; INPUTS:
;  jd      - Julian date of field.
;  ra      - Right ascension of center of field (J2000)
;              input can be in radians (double,float) or
;              a string HH:MM:SS.S  (see RAPARSE for valid syntax).
;  dec     - Declination of center of field for (J2000).
;              input can be in radians (double,float) or
;              a string +DD:MM:SS.S  (see DECPARSE for valid syntax).
;  width   - Width of field to extract (arcsec).
;  height  - Height of field to extract (arcsec).
;  outfile - File name for the results of the extraction.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  maglim  - Limiting V magnitude of asteroids to extract (default=30).
;  title   - Internal title for plast extraction (default='auto plast').
;  obscode - Observatory code (default = '688', Lowell Observatory).
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
;   If the accelerator program has previously been run, (plastset), there
;   will be a file, allsky.plast, in the current directory.  If this file
;   exists, the new mode extraction is enabled and doplast is run instead
;   of plast.  To force the old style plast even with this accelerator file,
;   use the /oldplast flag.
;
; MODIFICATION HISTORY:
;  97/11/24, Written by Marc W. Buie, Lowell Observatory
;  2000/09/22, MWB, modified for new version of plast
;  2002/08/16, MWB, added transparent option to use the new plast procedure
;                      for KBO searching.
;  2002/09/09, MWB, added support for string obscode values
;
;-
pro plast,jd,ra,dec,width,height,outfile, $
          TITLE=title,MAGLIM=maglim,OBSCODE=obscode,OLDPLAST=oldplast, $
          DEBUG=debug

   if badpar(jd,5,0,CALLER='PLAST: (jd) ') then return
   if badpar(ra,[4,5,7],0,CALLER='PLAST: (ra) ',TYPE=ratype) then return
   if badpar(dec,[4,5,7],0,CALLER='PLAST: (dec) ',TYPE=dectype) then return

   if badpar(title,[0,7],0,CALLER='PLAST: (TITLE) ',DEFAULT='auto plast') then return
   if badpar(maglim,[0,2,3,4,5],0,CALLER='PLAST: (MAGLIM) ',DEFAULT=30) then return
   if badpar(obscode,[0,1,2,3,7],0,CALLER='PLAST: (OBS) ', $
                DEFAULT=688,type=codetype) then return
   if badpar(oldplast,[0,1,2,3],0,CALLER='PLAST: (OLDPLAST) ',DEFAULT=0) then return
   if badpar(debug,[0,1,2,3],0,CALLER='PLAST: (DEBUG) ',DEFAULT=0) then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   if ratype  eq 7 then ra =raparse(ra)
   if dectype eq 7 then dec=decparse(dec)

   rastr,ra,3,ras
   decstr,dec,2,decs
   ras=repchar(ras,':',' ')
   decs=repchar(decs,':',' ')

   jdstr,jd,0,jds
   year=fix(strmid(jds,0,4))
   month=fix(strmid(jds,5,2))
   dayd = fix(strmid(jds,8,2)) + jd - (long(jd+0.5d0)-0.5d0)

   if exists('allsky.plast') and not oldplast then begin

      openw,luntmp,'plast.in',/get_lun
      printf,luntmp,year,month,dayd,format='(i4,",",i2.2,",",f5.2)'
      printf,luntmp,ras         ; plate center
      printf,luntmp,decs        ; plate center
      printf,luntmp,'1.0'       ; plate scale in arcsec/mm
      printf,luntmp,strn(width/25.4,format='(f10.2)')  ; width of plate in inches
      printf,luntmp,strn(height/25.4,format='(f10.2)') ; height of plate in inches
      printf,luntmp,outfile     ; output file name
      free_lun,luntmp
      if debug then begin
         spawn,'rm -f '+outfile+' ; ' + $
               'doplast < plast.in > /dev/null'
      endif else begin
         spawn,'rm -f '+outfile+' ; ' + $
               'doplast < plast.in > /dev/null ; ' + $
               'rm plast.in'
      endelse

   endif else begin

      openw,luntmp,'plast.in',/get_lun
      printf,luntmp,'O'         ; output only
      printf,luntmp,'0.1'       ; probability of being on image
      printf,luntmp,'Y'         ; Show if on plate regardless of probability?
      printf,luntmp,title       ; Title of plate
      printf,luntmp,year,month,dayd,format='(i4,",",i2.2,",",f5.2)'
      printf,luntmp,''          ; end of plate center input
      printf,luntmp,ras         ; plate center
      printf,luntmp,decs        ; plate center
      printf,luntmp,'1.0'       ; plate scale in arcsec/mm
      printf,luntmp,'r'         ; r - rectangular plate shape
      printf,luntmp,strn(width/25.4,format='(f10.2)')  ; width of plate in inches
      printf,luntmp,strn(height/25.4,format='(f10.2)') ; height of plate in inches
      printf,luntmp,'n'         ; Compute plate quadrant centers?
      printf,luntmp,'R'         ; magnitude to use (B/V/R)
      printf,luntmp,strn(float(maglim),format='(f10.1)')      ; faint limit to grab
      printf,luntmp,'y'         ; emp8 and comet data base
      printf,luntmp,outfile     ; output file name
      printf,luntmp,obscode     ; observatory code
      printf,luntmp,'Y'         ; Is this correct?
      printf,luntmp,''          ; 
      printf,luntmp,'N'         ; 
      free_lun,luntmp
      spawn,'rm -f '+outfile+' ; ' + $
            'plast < plast.in > /dev/null ; ' + $
            'rm plast.in plast.out'

   endelse

end
