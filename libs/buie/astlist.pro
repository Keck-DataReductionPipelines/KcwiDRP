;+
; NAME:
;  astlist
; PURPOSE:
;  Create a summary listing from a final astrometry file.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astlist,file,outfile
; INPUTS:
;  file - Final astrometry file to read and summarize.
;
; OPTIONAL INPUT PARAMETERS:
;  outfile - (Optional), name of file to write results to.
;  slowfile - (Optional), Name of file to write slow moving (or DES) objects to
;
; KEYWORD INPUT PARAMETERS:
;  DESOBJ = optional string array of names of DES interesting objects.
;            if provided, this controls the objects that are written to slowfile
;
; OUTPUTS:
;  All information is printed to screen.
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
;  98/03/26, Written by Marc W. Buie, Lowell Observatory
;  98/11/04, MWB, now filters out magnitudes fainter than 80.0
;  2001/04/17, MWB, added slowfile output option
;  2004/07/24, MWB, added DESOBJ keyword
;
;-
PRO astlist,file,outfile,slowfile,DESOBJ=desobj

   if n_params() eq 0 then begin
      print,'astlist,file[,outfile]'
      return
   endif

   blanks = '                       '

   if badpar(file,7,0,CALLER='ASTLIST: (file) ') then return
   if badpar(outfile,[0,7],0,CALLER='ASTLIST: (outfile) ',default='[none]') then return
   if badpar(slowfile,[0,7],0,CALLER='ASTLIST: (slowfile) ',default='[none]') then return
   if badpar(desobj,[0,7],[0,1],CALLER='ASTLIST: (DESOBJ) ', $
             default='') then return

   if not exists(file) then begin
      print,'ASTLIST: Error!  File ',file,' does not exist.'
      return
   endif

   rdast,file,fn,jd,raobs,decobs,mag,obs,id,nobs
   IF nobs eq 0 THEN BEGIN
      print,'ASTLIST: Error!  File ',file,' is empty.'
      return
   ENDIF

   checkdes = n_elements(desobj) gt 1 or desobj[0] ne ''

   ; Sort astrometry by jd.
   idx=sort(jd)
   fn=fn[idx]
   jd=jd[idx]
   raobs=raobs[idx]
   decobs=decobs[idx]
   mag=mag[idx]
   obs=obs[idx]
   id=id[idx]

   ; Get a unique list of objects
   object = id[uniq(id,sort(id))]
   nobj = n_elements(object)

   avgmag=fltarr(nobj)
   magerr=fltarr(nobj)
   objdir=fltarr(nobj)
   dis   =fltarr(nobj)
   dt    =fltarr(nobj)
   rate  =fltarr(nobj)
   count =intarr(nobj)
   rsig  =fltarr(nobj)
   ffn   =strarr(nobj)
   elong =intarr(nobj)
   desflag=bytarr(nobj)

   for i=0,nobj-1 do begin

      z=where(id eq object[i],count0)
      count[i]  = count0
      ffn[i]    = fn[z[0]]

      zg=where(id eq object[i] and mag lt 80.0 ,countgood)
      IF countgood gt 1 THEN begin
         avgmag[i] = mean(mag[zg])
         magerr[i] = stdev(mag[zg])
      endif else if countgood eq 1 then begin
         avgmag[i] = mag[zg[0]]
         magerr[i] = 0.
      endif else begin
         avgmag[i] = 99.9
         magerr[i] = 0.
      endelse

      i1 = z[0]
      i2 = z[count0-1]

      ; Compute rate and direction of motion using first and last points.
      objdir[i] = atan(decobs[i2]-decobs[i1],raobs[i1]-raobs[i2])*!radeg
      dis[i]    = angsep(raobs[i1],decobs[i1],raobs[i2],decobs[i2])*!radeg*3600.0
      dt[i]     = (jd[i2]-jd[i1])*24.0
      rate[i]   = dis[i]/dt[i]

      ; compute rate from all adjacent pairs
      if count0 ge 3 then begin
         i1 = z[0:count0-2]
         i2 = z[1:count0-1]
         dr = angsep(raobs[i1],decobs[i1],raobs[i2],decobs[i2])*!radeg*3600.0 $
                 / ((jd[i2]-jd[i1])*24.0)
         rsig[i] = stdev([dr,rate[i]])
      endif else begin
         rsig[i] = 0.
      endelse

      ; compute solar elongation at time of first point
      sunpos,jd[z[0]],sra,sdec,/radian
      elong[i] = fix(angsep(raobs[z[0]],decobs[z[0]],sra,sdec)*!radeg+0.5)

      ; optional check against DES object list
      if checkdes then begin
         a = where(object[i] eq desobj,counta)
         if counta ne 0 then desflag[i] = 1B
      endif

   endfor

   ; Compute an estimate of the target range in AU.  This assumes object
   ;   is precisely at opposition.  Rather than inverting the VT equation
   ;   interpolate to get an idea of the distance.
   rt=reverse(findgen(21*5)*3+0.5)
   vt=148.0*((1.0-1.0/sqrt(rt))/(rt-1))
   interp,vt,rt,rate,range

;   range = 29.785 / (rate/3600.0/3600.0/!radeg) / 1.495978e8 / 2.0

   fmt = '(a10,1x,a,1x,i2,2x,f4.1," +- ",f3.1,1x,f6.2,1x,f6.1,2x,' + $
          'f6.1,1x,f4.1,2x,f4.2,2x,i3,1x,f5.1)'

   idx=sort(rate)
;   idx=sort(avgmag)
;   idx=sort(objdir)
;   idx=sort(elong)

   title = 'Object       1st file   N  R mag (err)'+ $
           '   "/hr    dir  dr(") dt(h) vsig  sel  AU'

   print,title
   for i=0,nobj-1 do begin
      print,object[idx[i]]+blanks,ffn[idx[i]], $
         count[idx[i]],avgmag[idx[i]],magerr[idx[i]], $
         rate[idx[i]],objdir[idx[i]],dis[idx[i]],dt[idx[i]],rsig[idx[i]], $
         elong[idx[i]],range[idx[i]], $
         format=fmt
   endfor

   if outfile ne '[none]' then begin
      openw,lun,outfile,/get_lun
      printf,lun,title
      for i=0,nobj-1 do begin
         printf,lun,object[idx[i]]+blanks,ffn[idx[i]], $
            count[idx[i]],avgmag[idx[i]],magerr[idx[i]], $
            rate[idx[i]],objdir[idx[i]],dis[idx[i]],dt[idx[i]],rsig[idx[i]], $
            elong[idx[i]],range[idx[i]], $
            format=fmt
      endfor
      free_lun,lun
   endif

   if slowfile ne '[none]' then begin
      openw,lun,slowfile,/get_lun
      printf,lun,title
      for i=0,nobj-1 do begin
         if ((rate[idx[i]] le 15.0 and $
              (elong[idx[i]] ge 150.0 or elong[idx[i]] lt 80.0)) or $
              desflag[idx[i]] eq 1B) then begin
            printf,lun,object[idx[i]]+blanks,ffn[idx[i]], $
               count[idx[i]],avgmag[idx[i]],magerr[idx[i]], $
               rate[idx[i]],objdir[idx[i]],dis[idx[i]],dt[idx[i]],rsig[idx[i]], $
               elong[idx[i]],range[idx[i]], $
               format=fmt
            if checkdes and not desflag[idx[i]] then $
               desobj = [desobj,object[idx[i]]]
         endif
      endfor
      free_lun,lun
   endif

end
