;+
; NAME:
;	plotphot
; PURPOSE: (one line)
;	Plot extinction fits from Tholen's ltcrv program.
; DESCRIPTION:
;	This program will automatically generate a summary plot from an
;	extinction fit produced by David Tholen's ltcrv program.  If the
;	file does not exist or if there aren't enough points to generate
;	a meaningful plot, then no plot is generated and the top summary
;	portion of the output file is printed (if found).
; CATEGORY:
;	Photometry
; CALLING SEQUENCE:
;	plotphot,filename
; INPUTS:
;	filename - string containing filename to read and plot.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Plots to the current graphics device.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	Generates a plot in the current active graphics window.
; RESTRICTIONS:
;	None.
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1992 June 18
;-
pro photplot,filename

   on_error,2
   on_ioerror,bad

   openr,lun,filename,/get_lun

   line=strarr(9)
   new=''

; Read in the summary information at the top of the file.

   readf,lun,new,format='(a60)' ; Dump blank line
   for i=0,6 do begin
      readf,lun,new
      line[i] = new
   endfor
   readf,lun,new,format='(a60)' ; Dump blank line
   readf,lun,new,format='(a60)' ; Dump header line

; Decode information from the summary
   eql=strpos(line[6],'=')+1
   nobs = fix(strmid(line[6],eql,strlen(line[6])-eql))

; Check for bad files.
   bad=0
   if (strpos(line[0],'*') ne -1) then bad=1
   if (strpos(line[1],'*') ne -1) then bad=1
   if (nobs le 1) then bad=2

   if bad ne 0 then begin
      if bad eq 1 then $
         print,'Bad fit, unable to plot.   Here is the header:'
      if bad eq 2 then $
         print,'Not enough observations to plot.  Here is the the header:'
      for i=0,6 do print,line[i]
      free_lun,lun
      return
   endif

; Finish decoding.
   eql=strpos(line[0],'=')+1
   extin = float(strmid(line[0],eql,strlen(line[0])-eql))
   eql=strpos(line[1],'=')+1
   mag0 = float(strmid(line[1],eql,strlen(line[1])-eql))

   air=fltarr(nobs)
   mag=fltarr(nobs)
   omc=fltarr(nobs)

; Read the individual measurements
   j=0
   for i=0,nobs-1 do begin
      readf,lun,j,a,m,o,format='(i3,1x,f5.3,1x,f7.4,1x,f7.4)'
      air[i]=a
      mag[i]=m
      omc[i]=o
   endfor
   readf,lun,new,format='(a60)' ; Dump blank line
   readf,lun,new,format='(a60)' ; Dump header line

;Inspect last line read for an extinction override
   force=0
   pos=strpos(new,'overridden')
   if pos ne -1 then begin
      force=1
      readf,lun,new,format='(a60)' ; Dump blank line
      readf,lun,new
      line[7] = new
      readf,lun,new
      line[8] = new
      readf,lun,new,format='(a60)' ; Dump blank line
      readf,lun,new,format='(a60)' ; Dump header line
      eql=strpos(line[7],'=')+1
      extin = float(strmid(line[7],eql,strlen(line[7])-eql))
      eql=strpos(line[8],'=')+1
      mag0  = float(strmid(line[8],eql,strlen(line[8])-eql))
      omc = mag - (air*extin+mag0)
   endif

; Read the set measurements
   j=0
   while not eof(lun) do begin
      readf,lun,i,a,m,ms,e,es, $
                   format='(i3,1x,f5.3,1x,f7.4,1x,f6.4,1x,f7.4,1x,f6.4)'
      if j eq 0 then begin
         set_air=a
         set_mag=m
         set_sig=ms
         set_ext=e
         set_esg=es
      endif else begin
         set_air=[set_air,a]
         set_mag=[set_mag,m]
         set_sig=[set_sig,ms]
         set_ext=[set_ext,e]
         set_esg=[set_esg,es]
      endelse
      j = j + 1
   end
   nset = j

   free_lun,lun

; Now, do the plotting.
   pmulti=!p.multi

; First, the raw photometry panel.
   x=[min(air),max(air)]
   y=mag0+x*extin
   !p.multi=[0,1,3]
   plot,air,mag,yr=[max([mag,y]),min([mag,y])],psym=4,charsize=1.4, $
                xtit='Airmass',ytit='Raw Magnitude',title=filename
   oplerr,set_air,set_mag,set_sig,psym=8
   oplot,x,y

; Second, the residuals from the extinction fit.
   if (min(omc) ne max(omc)) then $
   plot,air,omc,yr=[max(omc),min(omc)],psym=4,charsize=1.4, $
                xtit='Airmass',ytit='O-C'

; Third, the extinction vs. "time" plot
   !p.multi=[2,2,3]
   i=indgen(nset)+1
   if nset gt 1 then $
   plot,i,set_ext,psym=8,yr=[min(set_ext-set_esg),max(set_ext+set_esg)], $
                  charsize=1.4,xtit='Set number',ytit='Extinction mag/X'
   oplerr,i,set_ext,set_esg,psym=3

; Last, the annotation from the summary
   plot,[0,1],[0,1],/nodata,xstyle=4,ystyle=4
   if force eq 1 then $
      xyouts,0,1,'Extinction Overridden',charsize=0.8

   for i=0,1 do begin
      y=(6-i)/8.+0.08
      xyouts,0,y,line[i+7*force],charsize=0.8
   endfor
   for i=2,6 do begin
      y=(6-i)/8.+0.08
      xyouts,0,y,line[i],charsize=0.8
   endfor

; Cleanup at the end.
   !p.multi=pmulti
   return

bad:
   print,!error_state.msg
   free_lun,lun

end
