;+
; NAME:
;  moschipcal
; PURPOSE:   (one line only)
;  Determine the photometric calibration of a single Mosaic CCD
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  moschipcal,root,num,ccd
; INPUTS:
;  root - root of data area and file names
;  num  - image number to process
;  ccd  - CCD (extension) number to process
; OPTIONAL INPUT PARAMETERS:
;  DBLUN - If provided is used for database queries and is left open.
;            Otherwise, a database connection is opened and closed with each
;            call.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2008/08/07
;  2009/12/30, MWB, modified for new fitting coefficients methodology
;-
pro moschipcal,root,num,ccd,DBLUN=dblun

   self='MOSCHIPCAL '
   if badpar(root,7,0,caller=self+'(root) ') then return
   if badpar(num,[2,3],0,caller=self+'(num) ') then return
   if badpar(ccd,[2,3],0,caller=self+'(ccd) ') then return
   if badpar(dblun,[0,2,3],0,caller=self+'(DBLUN) ',default=-1) then return

   dodblun = dblun lt 0

   ; where to find images
   ddir='/net/amber/raid1/buie/rawfits/des/'+root+'/'

   ; where to find reduction location
   rdir='/net/frakir/raid/reduced/'

   ; Get the astrometric solution for this image
   rdastfc,rdir+root+'/fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,terms, $
      coeffarr,ncoeffs,nlines

   ; Constants needed for converting to xi,eta
   nx = 1024.0
   ny = 2048.0
   renormfac=sqrt(float(nx)^2+float(ny)^2)

   ; Basic part of the image file name
   imnameroot = root+'.'+strn(num,format='(i3.3)')

   exttag = 'x'+strn(ccd)
   imname = imnameroot+exttag

   ; get pointer into the astrometric solutions for this image:ccd
   z = where(imname eq ffn,count)

   if count eq 0 then begin
      valid=0
      return
   endif

   valid = 1
   ; Get the index to the coefficients
   if ftype[z[0]] eq 'eta' then begin
      floce = z[0]
      flocx = z[1]
   endif else begin
      floce = z[1]
      flocx = z[0]
   endelse

   ; Extract the coefficients
   cxi  = trimrank(coeffarr[flocx,*])
   ceta = trimrank(coeffarr[floce,*])

   ; Need to know the boundary of the image on the sky, first start with
   ; the pixel boundaries
   npts=10
   lside = findgen(npts+1)/npts * 2048.0
   sside = findgen(npts+1)/npts * 1024.0
   strut = fltarr(npts+1)

   ; This a vector pair of points that trace out the perimeter of the CCD.
   xpos=[strut,sside,     strut+1024,reverse(sside)]
   ypos=[lside,strut+2048,reverse(lside),     strut]

   ; compute perimeter position relative to optical center
   dx=(xpos-xc[flocx])/renormfac
   dy=(ypos-yc[flocx])/renormfac

   ; compute xi,eta for perimeter based on solution
   xi = asteval(dx,dy,cxi,terms)/3600.0d0*!dpi/180.0d0
   eta= asteval(dx,dy,ceta,terms)/3600.0d0*!dpi/180.0d0

   ; convert to ra,dec of perimeter
   astsn2rd,xi,eta,cra[flocx],cdec[floce],ra,dec

   setwin,0
   plot,eta,xi,/iso

end
