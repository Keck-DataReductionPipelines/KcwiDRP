;+
; NAME:
;   objast
; PURPOSE:   (one line only)
;   Generate astrometry from Looker (obj) files
; DESCRIPTION:
; CATEGORY:
;   Astrometry
; CALLING SEQUENCE:
;  objast,objlist
; INPUTS:
;  objlist - String (scalar or vector) of name of obj file to process.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DDIR    - Directory where image data reside (default = current directory)
;  GAIN    - Gain of CCD (defalt = 1.0)
;  OBJRAD  - Object aperture radius (default=10)
;  KEYLIST - header correspondence file
; OUTPUTS:
;  Generates a .ast file for every 'y' object in each obj file.  Also, any
;    objects marked with 's' are saved to the 'psf' sub-directory
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/04/07
;  2002/09/10, MWB, fixed bug for non-Mosaic data
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;  2004/07/20, MWB, added support for implanted fake objects in obj files.
;  2009/08/05, MWB, modified for new rdastfc and astxy2rd versions.
;  2009/12/30, MWB, modified for new fitting coefficients methodology
;-
pro objast,objlist,DDIR=ddir,KEYLIST=keylist,GAIN=gain,OBJRAD=objrad

   self='OBJAST: '
   if badpar(objlist,7,[0,1],caller=self+'(objlist) ', $
                             npts=nobjfiles) then return
   if badpar(ddir,[0,7],0,caller=self+'(DDIR) ',default='') then return

   if badpar(keylist,[0,7],0,CALLER=self+'(KEYLIST) ', $
                              default='[[DEFAULT]]') then return

   if badpar(gain,[0,2,3,4,5],0,CALLER=self+'(GAIN) ', $
                                default=1.0) then return

   if badpar(objrad,[0,1,2,3,4,5],0,CALLER=self+'(OBJRAD) ', $
                              default=10) then return

   ddir=addslash(ddir)

   if exists('fitcoeff.dat') ne 1 then begin
      print,'fitcoeff.dat support file is missing, unable to continue.'
      return
   endif

   ; Open error log file.
   openw,lulog,'objast.err',/get_lun

   ; Get header correspondence list.
   loadkeys,keylist,hdrlist

   blanks='                      '

   ; Get all the astrometric solutions for this directory.
   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renorm, $
           cra,cdec,photzp,terms,coeffarr,ncoeffs,nlines
   goodrenorm=0B

   ; Loop over all the obj files provided.
   for i=0,nobjfiles-1 do begin

      if exists(objlist[i]) then begin

         ; Read in the obj file
         rdoblist,objlist[i],nobj,fnlist,dt,offvals,xyvals,flags,idstr,nfiles
         print,objlist[i]+blanks,strn(nobj),' objects: ',fnlist[*], $
            format='(a18,1x,a3,a,6(1x,a))'

         if nobj eq 0 then continue

         ; Need to get magnitudes for the object somehow, this is just a stub
         ;   for now.  Probably need to go back to the image and regenerate
         ;   the instrumental magnitude.   What a pain!
         astmag=fltarr(nobj)

         if nfiles eq 0 then begin
            print,'  *** Warning!  ',objlist[i],' contains zero files.'
            printf,lulog,'  *** Warning!  ',objlist[i],' contains zero files.'
            continue
         endif

         ; Courtesy warning.
         z = where(flags eq '?',count)
         if count ne 0 then begin
            print,'  *** Warning!  There are ',strn(count),' ? objects in ', $
               objlist[i]
            printf,lulog,'  *** Warning!  There are ',strn(count),' ? objects in ', $
               objlist[i]
         endif

         ; Bust up the object name and get the extension.
         words=strmid(objlist[i],0,strlen(objlist[i])-4)
         xpos = strpos(words,'x',/reverse_search)
         if xpos ge 1 then begin
            field  = strmid(words,0,xpos)
            extstr = strmid(words,xpos+1,999)
            extno  = fix(extstr)
            exttag = 'x'+extstr
         endif else begin
            field  = words
            exttag = ''
            extno  = 0
            extstr = ''
         endelse

         ; Get base object id tag
         words=strsplit(fnlist[0],'.',/extract)
         root   = words[0]
         suffix = words[1]
         objtag=strmid(root,strlen(root)-2,2) + $
                strmid(suffix,strlen(suffix)-3,3) + extstr

;         print,'  field [',field,']  tag [',exttag,'] ',objtag

         for j=0,nfiles-1 do begin

            imname = fnlist[j]+exttag
            loaded = 0B

            z = where(imname eq ffn,count)
            if count gt 0 then begin

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

               ; Grab the x,y values for this file.
               x = trimrank(xyvals[j*2,*])
               y = trimrank(xyvals[j*2+1,*])

               if renorm[flocx] lt 0.0 then begin
                  ; Get the renorfac setup if not already valid
                  if goodrenorm eq 0B then begin
                     ft=''
                     if not exists(ddir+fnlist[j]+ft) then begin
                        if exists(ddir+fnlist[j]+'.fits') then ft='.fits'
                     endif
                     fits_read,ddir+fnlist[j]+ft,raw,hdr, $
                        exten_no=extno,noscale=noscale
                     sz = size(raw,/dimen)
                     nx = sz[0]
                     ny = sz[1]
                     renormfac=sqrt(float(nx)^2+float(ny)^2)
                     goodrenorm=1B
                  endif
                  renorm[flocx] = renormfac
               endif

               info = { $
                  renormfac: renorm[flocx], $
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

               astxy2rd,x,y,info,ra,dec,/full,xi=xi,eta=eta,dx=dx,dy=dy

               rastr,ra,4,ras
               decstr,dec,3,decs
               for k=0,nobj-1 do begin
                  tobjname=objtag+strb36(k,pad=2)
                  resfile = tobjname + '.ast'
                  if (flags[k] eq 'y' or flags[k] eq 's') and $
                      x[k] ge 0.0 and y[k] ge 0.0 then begin

                     if flags[k] eq 's' then begin
                        if exists(resfile) then begin
                           print,'  Deleting ',resfile
                           file_delete,resfile,/quiet,/noexpand_path
                        endif
                        if not exists('psf') then file_mkdir,'psf'
                        resdir=addslash('psf')
                        saltstr = ' FAKE'
                     endif else begin
                        resdir=''
                        saltstr = ''
                     endelse

                     ; Load this image if it hasn't already been loaded.
                     if not loaded then begin
                        ft=''
                        if not exists(ddir+fnlist[j]+ft) then begin
                           if exists(ddir+fnlist[j]+'.fits') then ft='.fits'
                        endif
                        if not exists(ddir+fnlist[j]+ft) then begin
                           print,'Image ',ddir+fnlist[j]+ft,' not found.'
                           printf,lulog,'Image ',ddir+fnlist[j]+ft,' not found.'
                           break
                        endif
                        print,'    Loading ',ddir+fnlist[j]+ft,' ',exttag
                        fits_read,ddir+fnlist[j]+ft,raw,hdr, $
                           exten_no=extno,noscale=noscale
                        parsekey,hdr,hdrlist,hdrinfo
                        if hdrinfo.exptime eq 0. then hdrinfo.exptime=1.0
                        loaded=1B
                     endif

                     ; Generate the instrumental magnitude
                     basphote,gain,raw,hdrinfo.exptime,x[k],y[k], $
                        objrad,objrad+10,objrad+30, $
                        /exact,/nolog,/silent,mag=mag,fwhm=fwhm
                     astmag=(mag+photzp[flocx]) < 99.9

                     infostr=string(imname,hdrinfo.jd,ras[k],decs[k],astmag, $
                                    format='(a,1x,f13.5,1x,a,1x,a,1x,f4.1)')
                     print,'  ',tobjname,' ',infostr,saltstr
                     repwrite,resdir+resfile,imname,infostr
                  endif else if flags[k] eq 'n' then begin
                     if exists(resfile) then begin
                        print,'  Deleting ',resfile
                        file_delete,resfile,/quiet,/noexpand_path
                     endif
                  endif
               endfor

            endif else begin
               print,'No astrometric solution found for ',imname
               printf,lulog,'No astrometric solution found for ',imname
            endelse
         endfor
      endif else begin
         print,'Error!  File ',objlist[i],' not found.'
         printf,lulog,'Error!  File ',objlist[i],' not found.'
      endelse

   endfor

   free_lun,lulog

end
