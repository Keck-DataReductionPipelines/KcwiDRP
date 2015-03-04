;+
; NAME:
;  astrepro
; PURPOSE:
;  Re-reduce existing astrometry originally measured with ASTROM
; DESCRIPTION:
;  This file attempts to rereduce astrometric measurements in the current
;    directory.  These measures are x,y positions that are found in the
;    file, position.dat.  The files fitcoeff.dat and centers.dat are
;    scanned for the corresponding transformation relations between x,y
;    and RA,DEC.  Any object.ast files in this directory will be overwritten.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astrepro
; INPUTS:
;  All input information comes from files.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  All output information is sent to files.
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
; MODifICATION HISTORY:
;  97/06/20, Written by Marc W. Buie, Lowell Observatory
;  99/03/18, MWB, extracted fitcoeff reader (see rdastfc.pro)
;  2000/01/19, MWB, modified for new rdastfc version.
;  2009/08/05, MWB, modified for new rdastfc and astxy2rd versions.
;  2009/12/30, MWB, modified for new fitting coefficients methodology
;-
pro astrepro

   ; first check to see if the three required files exist.  If not, quit.
   err=''
   if not exists('centers.dat') then err=[err,'centers.dat']
   if not exists('position.dat') then err=[err,'position.dat']
   if not exists('fitcoeff.dat') then err=[err,'fitcoeff.dat']

   if n_elements(err) ne 1 then begin
      if n_elements(err) eq 2 then begin
         print,'Input file ',err[1],' is not present.'
      endif else if n_elements(err) eq 3 then begin
         print,'Input files ',err[1],' and ',err[2],' are not present.'
      endif else if n_elements(err) eq 4 then begin
         print,'Input files ',err[1],', ',err[2],', and ',err[3],' are not present.'
      endif
      print,'Unable to continue.'
      return
   endif

   ; Pre-declare strings for later use.
   version=''
   line=''

   ; First load the position.dat file.
   readcol,'position.dat',pfn,pname,pobjrad,px,py,format='a,a,f,d,d',/silent
   npos=n_elements(pfn)
   print,npos,' total object positions found.'

   ; Next load the fit coefficients file.
   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renorm, $
      cra,cdec,photzp,terms,coeffarr,ncoeffs,nlines

   if min(renorm) le 0. then begin
      print,'old fitcoeff.dat file, need to promote with renormfac specified'
      return
   endif

   if nwords ne nterms+14 then begin
      print,'fitcoeff.dat file is empty, aborting.'
      return
   endif
   print,nlines,' total coefficient sets found.'

   ; Create a list of unique objects in position data.
   oblist=pname[uniq(pname,sort(pname))]
   nobj=n_elements(oblist)

   ; Now, loop over the objects
   for j=0,nobj-1 do begin
      obfile = oblist[j]+'.ast'
      obnew  = oblist[j]+'.astnew'
      obold  = oblist[j]+'.old'

      if exists(obfile) then begin
         readcol,obfile,ofn,jd,mag,format='a,a,x,x,a',/silent
         print,obfile,': ',n_elements(ofn),' points'

         openw,lun,obnew,/get_lun
      
         for i=0,n_elements(ofn)-1 do begin

            ; Locate index for coefficients information
            z=where(ofn[i] eq ffn,count)
            if count ne 2 then begin
               print,'Error: No coefficients found for file: ',ofn[i]
               ploc = -1
            endif else begin
               if ftype[z[0]] eq 'eta' then begin
                  floce = z[0]
                  flocx = z[1]
               endif else begin
                  floce = z[1]
                  flocx = z[0]
               endelse
               ploc = 0
            endelse

            ; Locate index for centroid information
            if ploc ne -1 then begin
               z=where(ofn[i] eq pfn and oblist[j] eq pname,count)
               if count ne 1 then begin
                  print,'Error: No ',oblist[j],' centroid found for file: ',ofn[i]
                  ploc = -1
               endif else begin
                  ploc = z[0]
               endelse
            endif

            if ploc ne -1 then begin
               cxi  = trimrank(coeffarr[flocx,*])
               ceta = trimrank(coeffarr[floce,*])

               info = { $
                  cxi: cxi, $
                  ceta: ceta, $
                  terms: terms, $
                  prot: prot[flocx], $
                  renormfac: renorm[flocx],$
                  xcref: xc[flocx], $  ; xi and eta values are always the same
                  ycref: yc[flocx], $
                  raref: cra[flocx], $
                  decref: cdec[flocx] $
                  }

               astxy2rd,px[ploc],py[ploc],info,ra,dec,/full
               rastr,ra,4,ras
               decstr,dec,3,decs
               printf,lun,ofn[i],jd[i],ras,decs,mag[i], $
                  format='(a,1x,a,1x,a,1x,a,1x,a4)'
;               print,ofn[i],' ',jd[i],' ',ras,' ',decs,' ',mag[i]
            endif

         endfor ; loop over object points
         free_lun,lun
         spawn,'mv '+obfile+' '+obold+' ; mv '+obnew+' '+obfile

      endif else begin
         print,'File ',obfile,' not found.'
      endelse

   endfor ; object for loop

end
