;+
; NAME:
;  optaxis
; PURPOSE:   (one line only)
;  Search for the optical axis in an astronomical image
; DESCRIPTION:
;  This routine only makes sense if you need a non-linear solution
;    for your astrometry from an image.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  optaxis,fn,terms,xopt,ypot,raopt,decopt
; INPUTS:
;  fn - file name of image to analyze
;          This is a valid name that will lead to finding other related
;             files based on this name.  The name is typically of the form
;             root.NNN where NNN is a three digit number.  In this case,
;             the ref file is NNN.ref and is found in the Refstars
;             directory relative to the current directory.
;  terms - string array with the names of terms to fit (see astterms.pro)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  xopt  - X coordinate of the optical axis in native pixel coordinates.
;  yopt  - Y coordinate of the optical axis in native pixel coordinates.
;  raopt - (radians) Right Ascension of the position (xopt,yopt)
;  decopt - (radians) Declination of the position (xopy,yopt)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/12/08
;-
pro optaxis,fn,terms,xopt,ypot,raopt,decopt,path=path

   self='optaxis: '
   if badpar(fn,7,0,caller=self+'(filename)') then return
   if badpar(terms,7,1,caller=self+'(terms)') then return
   if badpar(path,[0,7],0,caller=self+'(PATH)',default='') then return
   if path ne '' then path=addslash(path)

   if n_elements(terms) le 3 then begin
      print,self,' This routine does not work with three or fewer terms.'
      return
   endif

   ; build the name of the associated ref file
   words=strsplit(fn,'.',/extract)
   fnref=words[n_elements(words)-1]+'.ref'

   if not exists('Refstars/'+fnref) then begin
      print,self,' ref file, ',fnref,', could not be found.'
      return
   endif
   rdref,'Refstars/'+fnref,ref,error
   if error then begin
      print,self,' Error reading ref file.'
      return
   endif

   if not exists(path+fn) then begin
      print,self,' Unable to locate image file ',path+fn
      return
   endif
   hdr=headfits(path+fn)
   naxis1=double(sxpar(hdr,'NAXIS1'))
   naxis2=double(sxpar(hdr,'NAXIS2'))
   renormfac = sqrt(naxis1^2+naxis2^2)

   ; set initial optical axis position to center of array
   xopt = naxis1/2.0
   yopt = naxis2/2.0

   ; set the initial tangent point to the middle of the extrema in the
   ;   ref file positions.
   raopt=mean(minmax(ref.ra))
   decopt=mean(minmax(ref.dec))

   xp = (ref.xpos-xopt)/renormfac
   yp = (ref.ypos-yopt)/renormfac
   astrd2sn,ref.ra,ref.dec,raopt,decopt,xi,eta,/ARCSEC
   bad=bytarr(ref.nstars)
   astsolve,xp,yp,xi,eta,terms,renormfac,bad,cxi,ceta, $
      xichi=xichi,etachi=etachi,/silent
   chi=etachi+xichi
;   print,xopt,yopt,xichi,etachi,chi
;print,cxi

   info={renormfac:renormfac,cxi:cxi,ceta:ceta,terms:terms,prot:0.0, $
         xcref:xopt,ycref:yopt,raref:raopt,decref:decopt}
   astxy2rd,xopt,yopt,info,ra,dec,/full
   astrd2sn,ref.ra,ref.dec,ra,dec,xi,eta,/ARCSEC
   astsolve,xp,yp,xi,eta,terms,renormfac,bad,cxi,ceta, $
      xichi=xichi,etachi=etachi,/silent
   chi=etachi+xichi
   chiref=chi
   etachiref=etachi
   xichiref=xichi
   print,xopt,yopt,xichi,etachi,chi
;print,cxi

   xopt0 = xopt
   yopt0 = yopt

   ;for del = -2000.,2000,200 do begin
   for del = -200.,200,20 do begin
      ;xopt = xopt0 + 0.0 + del
      xopt = 2266.0 + del 
      ;yopt = yopt0 + del
      yopt = 1844.0

      xp = (ref.xpos-xopt)/renormfac
      yp = (ref.ypos-yopt)/renormfac
      astrd2sn,ref.ra,ref.dec,raopt,decopt,xi,eta,/ARCSEC
      bad=bytarr(ref.nstars)
      astsolve,xp,yp,xi,eta,terms,renormfac,bad,cxi,ceta, $
         xichi=xichi,etachi=etachi,/silent
      chi=etachi+xichi
;   print,xopt,yopt,xichi,etachi,chi
;print,cxi

      info={renormfac:renormfac,cxi:cxi,ceta:ceta,terms:terms,prot:0.0, $
            xcref:xopt,ycref:yopt,raref:raopt,decref:decopt}
      astxy2rd,xopt,yopt,info,ra,dec,/full
      astrd2sn,ref.ra,ref.dec,ra,dec,xi,eta,/ARCSEC
      astsolve,xp,yp,xi,eta,terms,renormfac,bad,cxi,ceta, $
         xichi=xichi,etachi=etachi,/silent
      chi=etachi+xichi
      print,xopt,yopt,xichi/xichiref,etachi/etachiref,chi/chiref
;print,cxi
   endfor

end
