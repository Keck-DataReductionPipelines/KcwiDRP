;+
; NAME:
;  lplastchk
; PURPOSE:
;  Scan for linkages among a collection of asteroid astrometric measurements
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  lplastchk,matchfile
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
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
;  98/04/27, Written by Marc W. Buie, Lowell Observatory
;  2002/09/03, MWB, changed Str_sep call to strsplit
;
;-
PRO lplastchk,match

   if badpar(match,7,0,caller='LPLASTCHK: (match) ') then return
   obs=688

   blanks='                                             '

   spawn,'ls *.ast',localfn
   IF localfn[0] eq '' THEN BEGIN
      print,'No astrometry files found in current directory.  Aborting.'
      return
   ENDIF

   IF not exists(match) THEN BEGIN
      print,'Match list ',match,' file not found.  Aborting.'
      return
   ENDIF

   nlocal=n_elements(localfn)
   cr = string("15b)  ;"
   rt = string("12b)  ;"
   form='($,a,a15,1x,i5)'

   openr,lun,match,/get_lun
   line=''
   nmatches=0
   while not eof(lun) do begin
      readf,lun,line,format='(a1)'
      nmatches=nmatches+1
   endwhile
   point_lun,lun,0L
   oblist=strarr(nmatches)
   firstfn=strarr(nmatches)
   for i=0,nmatches-1 do begin
      readf,lun,line,format='(a80)'
      words=strsplit(strcompress(line),' ',/extract)
      oblist[i]  = words[0]
      firstfn[i] = strmid(words[1],4,2)+strmid(words[1],7,3)
   endfor
   free_lun,lun
   goodcnt=intarr(nmatches)

   FOR i=0,nmatches-1 DO BEGIN

      z=where(firstfn[i] eq strmid(localfn,0,5),count)

      goodcnt[i]=count

      for j=0,count-1 do begin
         rdrawast,localfn[z[j]],fn,jd,raobs,decobs,mag,nlines

         ; Generate ephemeris for intended object
         ephem,jd,obs,2+50,'a'+oblist[i],eph

         ; Compute mean ephemeris offset
         err=angsep(raobs,decobs,eph[0,*],eph[1,*])*!radeg*3600.0
         IF n_elements(err) gt 3 THEN BEGIN
            robomean,err,3.0,0.5,merr
         ENDIF ELSE BEGIN
            merr=mean(err)
         ENDELSE

         ; Compute total motion from first to last point.
         hmotobj=angsep(raobs[0],decobs[0],raobs[nlines-1],decobs[nlines-1]) $
              / ((jd[nlines-1]-jd[0]) * 24.0 ) * !radeg * 3600.0
         hmoteph=angsep(eph[0,0],eph[1,0],eph[0,nlines-1],eph[1,nlines-1]) $
              / ((jd[nlines-1]-jd[0]) * 24.0 ) * !radeg * 3600.0

         ; Compute direction of motion using first and last points.
         objdir = atan(decobs[nlines-1]-decobs[0],raobs[0]-raobs[nlines-1])*!radeg
         ephdir = atan(eph[1,nlines-1] -eph[1,0], eph[0,0]-eph[0,nlines-1])*!radeg

         closerate = abs(hmotobj-hmoteph) lt 2.0

         closedir  = abs(objdir-ephdir) lt 5.0

         IF not closedir and objdir lt 0.0 THEN $
            closedir = abs((objdir+360.0)-ephdir) lt 5.0

         if closedir and closerate then $
            print,oblist[i]+blanks,strmid(localfn[z[j]],0,8), $
               merr,hmotobj,objdir,hmoteph,ephdir, $
               format='(6x,a10,1x,a,1x,f6.1,1x,2(1x,f6.1,1x,f6.1))' $
         else $
            goodcnt[i]=goodcnt[i]-1

      endfor

   ENDFOR

   z=where(goodcnt eq 0,count)
   if count ne 0 then begin
      print,''
      if count ne 0 then print,'Objects that were not found.'
      for i=0,count-1 do begin
         print,firstfn[z[i]]+' '+oblist[z[i]]
      endfor
   endif

   z=where(goodcnt gt 1,count)
   if count ne 0 then begin
      print,''
      print,'Objects that matched more than once.'
      for i=0,count-1 do begin
         print,firstfn[z[i]]+' '+oblist[z[i]]
      endfor
   endif

print,''
END
