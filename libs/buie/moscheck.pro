;+
; NAME:
;  moscheck
; PURPOSE:
;  Scan Mosaic astrometry data and look for inconsistencies and problems
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  moscheck
; INPUTS:
;  None
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  All output is to the screen
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/10/09
;  2002/09/03, MWB, changed Str_sep call to strsplit
;-
pro moscheck

   if not exists('centers.dat') then begin
      print,'MOSCHECK: cannot find file, centers.dat, unable to proceed.'
      return
   endif

   readcol,'centers.dat',fn0,ra,dec,hra,hdec,dra,ddec,format='a,a,a,a,a,d,d'
   ra=raparse(ra)
   dec=decparse(dec)

   nobs = n_elements(fn0)

   if nobs eq 0 then begin
      print,'MOSCHECK: centers.dat is either empty or is of the old format.'
      print,'          Check the file and try again.'
      return
   endif

   n_ext=8

   print,'First pass check:'

   fn = strarr(nobs)
   tag = intarr(nobs)
   for i=0,nobs-1 do begin
      words = strsplit(fn0[i],'x',/extract)
      fn[i] = words[0]
      tag[i] = fix(words[1])
   endfor

   fnlist=fn[uniq(fn,sort(fn))]
   nfn = n_elements(fnlist)

   all=indgen(n_ext)+1
   missing=0
   for i=0,nfn-1 do begin
      z=where(fnlist[i] eq fn,count)
      if count ne n_ext then begin
         missing=missing+count
         done=tag[z]
         intrsect,all,done,missed,/nnot
         print,fnlist[i],' is missing ',strn(n_ext-count),' fields: ',missed, $
            format='(a,a,a,a,'+strn(n_ext)+'(1x,i2))'
      endif
   endfor

   if missing gt 0 then return

   ; Next leve check
   print,'Second pass check:'

   raoff=fltarr(n_ext,nfn)
   decoff=fltarr(n_ext,nfn)
   for i=0,nfn-1 do begin
      z=where(fnlist[i] eq fn,count)
      thistag = tag[z]
      thisra  = ra[z]
      thisdec = dec[z]
      z=sort(thistag)
      raoff[*,i]  = thisra[z]
      decoff[*,i] = thisdec[z]
   endfor
   ra = raoff
   dec = decoff

   for i=0,n_ext-1 do begin
      raoff[i,*]  = raoff[i,*]  - ra[0,*]
      decoff[i,*] = decoff[i,*] - dec[0,*]
   endfor

   bad=bytarr(n_ext,nfn)
   dra=fltarr(n_ext)
   ddec=fltarr(n_ext)
   for i=1,n_ext-1 do begin
      tmpbad = bad[i,*]
      robomean,raoff[i,*],6.0,0.5,dra0,bad=tmpbad
      dra[i]=dra0
      robomean,decoff[i,*],6.0,0.5,ddec0,bad=tmpbad
      ddec[i]=ddec0
      bad[i,*]=tmpbad
   endfor

;   print,dra
;   print,ddec

   goofy = 0
   for i=0,nfn-1 do begin
      z=where(bad[*,i] eq 1,count)
      if count gt 0 then begin
         goofy=goofy+count
         print,fnlist[i],' bad astrom on ',strn(count),' fields: ',z+1, $
            format='(a,a,a,a,'+strn(n_ext)+'(1x,i2))'
      endif
   endfor

   setwin,0
   plot,raoff[1,*]*!radeg*3600.0,ytitle='RA offset (arcsec)'
   setwin,1
   plot,decoff[1,*]*!radeg*3600.0,ytitle='Dec offset (arcsec)'

   if goofy eq 0 then begin
      print,'Everything seems to be ok.'
   endif
end
