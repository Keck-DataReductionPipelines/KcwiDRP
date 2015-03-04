;+
; NAME:
;  ast2ted
; PURPOSE:
;  Convert astrometry file to a Ted Bowell format astrometry file.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  ast2ted,file,outfile,slowfile
; INPUTS:
;  file     - Final astrometry file to read and summarize.
;  outfile  - Name of file to write results to.
;  slowfile - Name of file to write slow moving (or DES) objects to
;                 (default=don't save slow objects to a separate file)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DESOBJ   - optional string array of names of DES interesting objects.
;              if provided, this controls the objects that are written to
;              slowfile.  The slow objects are not written to outfile.
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
;  2000/02/15, Written by Marc W. Buie, Lowell Observatory
;  2001/04/18, MWB, added slowfile output option
;  2002/09/09, MWB, added support for string obscode values
;  2004/07/24, MWB, added DESOBJ keyword
;  2005/05/25, MWB, changed output to ted files when handling DES objects
;                      Slow moving/KBO objects are no longer saved to the
;                      main output file AND to the slowfile.  Now objects
;                      are saved in only one file.  If slowfile name is not
;                      provided then DESOBJ is ignored and all objects are
;                      saved to outfile.
;  2005/10/12, MWB, fixed bug that prevented saving slow moving objects to a
;                     separate file if the mysql-based list was empty
;
;-
pro ast2ted,file,outfile,slowfile,DESOBJ=desobj

   if n_params() eq 0 then begin
      print,'ast2ted,file,outfile,slowfile'
      return
   endif

   if badpar(file,7,0,CALLER='AST2TED: (file) ') then return
   if badpar(outfile,7,0,CALLER='AST2TED: (outfile) ') then return
   if badpar(slowfile,[0,7],0,CALLER='AST2TED: (slowfile) ', $
             default='[none]') then return
   if badpar(desobj,[0,7],[0,1],CALLER='AST2TED: (DESOBJ) ', $
             default='xxNODESOBJxx') then return

   if not exists(file) then begin
      print,'AST2TED: Error!  File ',file,' does not exist.'
      return
   endif

   blanks='                   '

   rdast,file,fn,jd,ra,dec,mag,obs,id,nobs
   if nobs eq 0 then begin
      print,'AST2TED: Error!  File ',file,' is empty.'
      return
   endif

   checkdes = desobj[0] ne 'xxNODESOBJxx' and slowfile ne '[none]'

   ; Sort astrometry by jd.
   idx=sort(jd)
   fn=fn[idx]
   jd=jd[idx]
   ra=ra[idx]
   dec=dec[idx]
   mag=mag[idx]
   obs=obs[idx]
   id=id[idx]

   ; Get a unique list of objects
   object = id[uniq(id,sort(id))]
   nobj = n_elements(object)

   avgmag=fltarr(nobj)
   magerr=fltarr(nobj)
   rate  =fltarr(nobj)
   elong =intarr(nobj)
   desflag=bytarr(nobj)

   ; For each object compute the average magnitude and the
   ;  object's rate of motion.
   for i=0,nobj-1 do begin

      z=where(id eq object[i],count)

      zg=where(id eq object[i] and mag lt 80.0 ,countgood)
      if countgood gt 1 then begin
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
      i2 = z[count-1]

      if count ge 2 then begin
         ; Compute rate and direction of motion using first and last points.
         dis       = angsep(ra[i1],dec[i1],ra[i2],dec[i2])*!radeg*3600.0
         dt        = (jd[i2]-jd[i1])*24.0
         rate[i]   = dis/dt
      endif else begin
         rate[i]   = 99999.0
      endelse

      ; compute solar elongation at time of first point
      sunpos,jd[z[0]],sra,sdec,/radian
      elong[i] = fix(angsep(ra[z[0]],dec[z[0]],sra,sdec)*!radeg+0.5)

      ; optional check against DES object list
      if checkdes then begin
         a = where(object[i] eq desobj,count)
         if count ne 0 then desflag[i] = 1B
      endif

   endfor

   idx=sort(rate)
   openw,lted,outfile,/get_lun,width=200
   if checkdes then $
      openw,lkted,slowfile,/get_lun,width=200
   for i=0,nobj-1 do begin
      z=where(id eq object[idx[i]],count)
      for j=0,count-1 do begin
         caldatm,jd[z[j]],year,month,day,hour,minute,second
         day=float(day)+(float(hour)+(float(minute)+float(second)/60.0)/60.0)/24.0
         rastr,ra[z[j]],3,ras
         decstr,dec[z[j]],2,decs
         ras=repchar(ras,':')
         decs=repchar(decs,':')
         if j eq 0 and (avgmag[idx[i]] lt 90.0 and magerr[idx[i]] lt 2.0) then begin
            magstr=string(avgmag[idx[i]],format='(f5.1)')+'R'
         endif else begin
            magstr='      '
         endelse
         if ((rate[idx[i]] le 15.0 and $
              (elong[idx[i]] ge 150.0 or elong[idx[i]] lt 80.0)) or $
              desflag[idx[i]] eq 1B) and checkdes then begin
            if j eq 0 then desobj = [desobj,object[idx[i]]]
            printf,lkted,year,month,day,ras,decs,magstr,id[z[j]]+blanks,obs[z[j]], $
               format='(i4,1x,i2.2,2x,f08.5,2x,a,2x,a,1x,a6,1x,a9,1x,a3)'
         endif else begin
            printf,lted,year,month,day,ras,decs,magstr,id[z[j]]+blanks,obs[z[j]], $
               format='(i4,1x,i2.2,2x,f08.5,2x,a,2x,a,1x,a6,1x,a9,1x,a3)'
         endelse
      endfor
   endfor
   free_lun,lted
   if checkdes then free_lun,lkted

end
