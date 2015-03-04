;+
; NAME:
;  focscan
; PURPOSE:
;  Summarize and plot focus log files from PCCD.
; DESCRIPTION:
;
; CATEGORY:
;  2-D plotting
; CALLING SEQUENCE:
;  focscan,file
; INPUTS:
;  file - String with name of focus log file to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  PRINT - Flag, if set will generate plots (two per page) to PS device
;            and printed on the default printer.
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
;  Written by Marc W. Buie, 2001/03/22, Lowell Observatory.
;-
pro focscan,file,PRINT=print

   if badpar(print,[0,1,2,3],0,caller='focscan: (PRINT) ',default=0) then return

   openr,lun,file,/get_lun

   block=strarr(100)
   xt='Secondary Position'

   if print then begin
      d_name=!d.name
      portrait
      !p.multi=[0,2,4]
   endif else begin
      !p.multi=[0,2,2]
   endelse

   line=''
   while not eof(lun) and line ne 'q' do begin
      readf,lun,line,format='(a)'
      if line ne '+++++' then begin
         print,'file format of ',file,' is incorrect'
         free_lun,lun
         return
      endif

      i=0
      repeat begin
         readf,lun,line,format='(a)'
         block[i] = line
         i=i+1
      endrep until line eq '-----'

      words=strsplit(block[0],' ',/extract)
      time=words[0]+' '+words[1]
      foc0=long(words[6])
      focstart='Initial focus = '+words[6]
      words=strsplit(block[1],' ',/extract)
      filter = words[0]
      posn   = words[1]+' '+words[2]
      ha     = words[3]
      airmass= words[4]
      temp   = words[5]

      focus=fltarr(i-5)
      fwhm =fltarr(i-5)
      peak =fltarr(i-5)
      flux =fltarr(i-5)
      fom  =fltarr(i-5)
      bflux=fltarr(i-5)
      for i=3,i-3 do begin
         reads,block[i],focus0,fwhm0,peak0,flux0,fom0,bflux0
         focus[i-3] = focus0
         fwhm[i-3]  = fwhm0
         peak[i-3]  = peak0
         flux[i-3]  = flux0
         fom[i-3]   = fom0
         bflux[i-3] = bflux0
      endfor

      fom2 = peak/bflux

      if max(fom) gt 2*min(fom) then begin
         bestfoc1 = focus[min(where(fom eq max(fom)))]
         besttxt1 = ' best fom'
      endif else begin
         bestfoc1 = foc0
         besttxt1 = ' fom failed'
      endelse

      bestfoc2 = focus[min(where(fwhm eq min(fwhm)))]
      besttxt2 = ' best fwhm'

      if min(fwhm) le 0.0 then begin
         bestfoc1 = foc0
         bestfoc2 = foc0
         besttxt1 = ' fom failed'
         besttxt2 = ' fwhm failed'
      endif

      if bestfoc1 eq min(focus) or bestfoc1 eq max(focus) then $
         besttxt1 = ' fom edge'
      if bestfoc2 eq min(focus) or bestfoc2 eq max(focus) then $
         besttxt2 = ' fwhm edge'

      plot,focus,fwhm,yr=maxmin(fwhm),xtitle=xt,ytitle='FWHM (pixels)', $
         title=time
      xyouts,mean(minmax(focus)),mean(minmax(fwhm)),strn(long(bestfoc2))+besttxt2

      plot,focus,peak,xtitle=xt,ytitle='Peak Signal (DN)', $
         title='Filter='+filter+'   '+focstart

      plot,focus,flux,xtitle=xt,ytitle='Total Flux (DN)', $
         title=posn+'  HA='+ha,yrange=minmax([flux,bflux])
      oplot,focus,bflux

      plot,focus,fom,xtitle=xt,ytitle='FOM', $
         title='Airmass='+airmass+'   T='+temp+'F'
      oplot,focus,fom2
      oplot,minmax(focus),replicate(2*min(fom),2)
      xyouts,mean(minmax(focus)),mean(minmax(fom)),strn(long(bestfoc1))+besttxt1

      if not eof(lun) and not print then read,prompt='next? ',line,format='(a1)'
   endwhile

   free_lun,lun

   if print then begin
      hardcopy
      set_plot,d_name
   endif

   !p.multi=0

end
