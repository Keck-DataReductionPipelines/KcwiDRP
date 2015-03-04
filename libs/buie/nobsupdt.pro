;+
; NAME:
;  nobsupdt
; PURPOSE:
;  Update the number of observations in the undesignated objects list.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  nobsupdt
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/10/17
;  2003/10/01, MWB, converted my Filemove call to system file_move routine
;-
pro nobsupdt

   if not exists('oblist.dat') then begin
      print,'oblist.dat file not found in current directory.'
      return
   endif

   readcol,'oblist.dat',objnam,disdate,observer,format='a,a,a'
   file_move,'oblist.dat','oblist.sav',/noexpand_path,/overwrite

   ; scan the observer string and strip off the leading (nnn) prefix
   pos = strpos(observer,')')

   z=where(pos ge 0,count)
   if count ne 0 then begin
      for i=0,count-1 do $
         observer[z[i]] = strmid(observer[z[i]],pos[z[i]]+1,99)
   endif

   code = 'A'+objnam
   nobj = n_elements(code)

   ephem,replicate(systime(/julian),nobj),500,23,code,eph
   ssgeom,eph[0:7,*],sun,earth,phang,selong

   jdlast = 14+8
   arc    = 15+8
   nobs   = 16+8

   since = systime(/julian)-eph[jdlast,*]

   observer0=observer

   openw,lun,'oblist.dat',/get_lun
   for i=0,n_elements(objnam)-1 do begin
      if since[i] gt 180.0 then observer[i] = 'lost'
      if selong[i] lt 60.0 then observer[i] = 'lost'
      if eph[nobs,i] gt 2 then begin
         observer[i] = '('+strn(fix(eph[nobs,i]))+')'+observer[i]
      endif
      printf,lun,objnam[i],' ',disdate[i],' ',observer[i]

print,objnam[i],since[i],selong[i],observer0[i],observer[i], $
   format='(a10,1x,f6.1,1x,f5.1,a10,1x,a10)'

   endfor
   free_lun,lun

end
