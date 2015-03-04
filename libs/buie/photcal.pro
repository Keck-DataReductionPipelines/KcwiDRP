;+
; NAME:
;  photcal
; PURPOSE:   (one line only)
;  Photometric calibration using two lists of partially overlapping sources.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  photcal,ra,dec,imag,imagerr,catra,catdec,catmag,ngood,photzp,
;          photzperr,magrange
; INPUTS:
;  ra   - J2000 right ascension of source (usually from a CCD image)
;  dec  - J2000 declination of source (usually from a CCD image)
;  imag - instrumental magnitude
;  imagerr - uncertainty on the instrument magnitude
;  catra   - J2000 right ascension of the catalog (known) source.
;  catdec  - J2000 declination of the catalog (known) source.
;  catmag  - Standard catalog magnitude
;  catmagerr  - Uncertainty of the standard catalog magnitude
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  MAXSEP - Maximum allowable separation in arcseconds between sources in the
;              two lists that will be considered a match (default=1.5 arcsec).
;  NODISPLAY - Flag, if set will suppress all plots
;  UNIQUE - Flag, if set will force the linking of the two lists to be
;              a one-to-one mapping.
; OUTPUTS:
;  ngood   - Number of good matches
;  photzp  - Photometric zero-point that when added to the instrumental
;              magnitude will return the standard magnitude in the system
;              of the catalog.
;  photzperr - Uncertainty on the zero-point
;  magrange  - magnitude range of the catalog stars used, 2-element vector
;                 with [faintmag,brightmag]
; KEYWORD OUTPUT PARAMETERS:
;  INDEX1 - index into catalog list for the matched list
;  INDEX2 - index into star list for the matched list
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2007/02/01
;  2008/08/07, MWB, added MAXSEP, UNIQUE, and NODISPLAY keywords
;-
pro photcal,ra,dec,imag,imagerr,catra,catdec,catmag,catmagerr, $
            ngood,photzp,photzperr,magrange, $
            MAXSEP=maxsep,UNIQUE=unique,NODISPLAY=nodisplay, $
            INDEX1=ind1,INDEX2=ind2

   self='photcal: '
   if badpar(ra,[4,5],[0,1],caller=self+'(ra) ') then return
   if badpar(dec,[4,5],[0,1],caller=self+'(dec) ') then return
   if badpar(imag,[4,5],[0,1],caller=self+'(imag) ') then return
   if badpar(imagerr,[4,5],[0,1],caller=self+'(imagerr) ') then return
   if badpar(catra,[4,5],[0,1],caller=self+'(catra) ') then return
   if badpar(catdec,[4,5],[0,1],caller=self+'(catdec) ') then return
   if badpar(catmag,[4,5],[0,1],caller=self+'(catmag) ') then return
   if badpar(catmagerr,[4,5],[0,1],caller=self+'(catmag) ') then return
   if badpar(maxsep,[0,2,3,4,5],0,caller=self+'(MAXSEP) ',default=1.5) then return
   if badpar(unique,[0,1,2,3],0,caller=self+'(UNIQUE) ',default=0) then return
   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ',default=0) then return

   dcr = maxsep / 3600.0 * !pi / 180.0
;   srcor,catra,catdec,ra,dec,dcr,ind1,ind2,option=1 ; for unique match
   srcor,catra,catdec,ra,dec,dcr,ind1,ind2,option=unique

   if n_elements(ind1) ne n_elements(ind2) then begin
print,'This cannot happen.'
stop
   endif

   if ind1[0] lt 0 then begin
      ngood=0
      photzp=0.
      photzperr=0.
      magrange=[0.,0.]
      return
   endif

   zpall = catmag[ind1]-imag[ind2]
   zpallerr = sqrt(catmagerr[ind1]^2+imagerr[ind2]^2)
   bad=bytarr(n_elements(zpall))
   robomean,zpall,2.0,0.5,photzp,stdmean=photzperr,bad=bad
   z=where(bad eq 0,ngood)
   magrange=maxmin(catmag[ind1[z]])

;   ; extra filtering to remove a little bit of the top of the range
;   zz = where(catmag[ind1] lt magrange[1]+0.5 and bad eq 0,count)
;   if count ne 0 then begin
;      bad[zz]=1B
;      robomean,zpall,2.0,0.5,photzp,stdmean=photzperr,bad=bad
;      z=where(bad eq 0,ngood)
;      magrange=maxmin(catmag[ind1[z]])
;   end

   if n_elements(ind1) gt 1 and not nodisplay then begin
      setwin,0
      ploterror,catmag[ind1],imag[ind2],catmagerr[ind1],imagerr[ind2], $
         psym=8,xr=maxmin(catmag[ind1]),yr=maxmin(imag[ind2]),/nohat, $
         xtitle='Catalog Magnitude',ytitle='Instrumental Magnitude'

      setwin,1
      ploterror,catmag[ind1],zpall,catmagerr[ind1],zpallerr,psym=8, $
         xr=maxmin(catmag[ind1]),yr=maxmin(zpall),/nohat, $
         xtitle='Catalog Magnitude',ytitle='Zero-point offset (cat-inst)'

      oplot,catmag[ind1[z]],zpall[z],psym=8,color='ff00ff'xl
   endif

   ind1=ind1[z]
   ind2=ind2[z]

end
