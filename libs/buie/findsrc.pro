;+
; NAME:
;  findsrc
; PURPOSE:
;  Automatic source detection and photometry from a digital image.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  findsrc,file
; INPUTS:
;  file - Name of image file to search for sources.  This can also be a
;           2-d array rather than forcing this program to read from a file.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  BINFAC   - Amount to bin the image by prior to source detection step.
;               Default=1 (ie., no binning)
;
;  EXTLIST  - If image is a multi-extension FITS image, this list will
;                force the reduction of only the extension numbers listed.
;                The default is to do all the extensions, one at a time.
;
;  EXPTIME   - Exposure time of image in seconds, used only when input
;                is an array.  In this case the header processing inputs
;                are not needed.
;
;  GAIN      - Gain of image, in photons/DN, default=1.0
;
;  GAP       - This is a number used to avoid looking at pixels near the
;                 object.  It should be set to a value that is roughly equal
;                 to the FWHM of a typical stellar image.  Default=2 pixels.
;
;  KEYLIST   - Name of a file containing a correspondence list. This list
;                 associates a set of standard names with the actual keyword
;                 names found in a FITS file header. If this keyword is
;                 omitted, a default list is used, as if a file with the
;                 following contents had been supplied:
;                  AIRMASS   K  AIRMASS
;                  DATE      K  DATE-OBS
;                  DATETMPL  T  DD-MM-YYYY
;                  EXPDELTA  V  0.0
;                  EXPTIME   K  EXPTIME
;                  FILTER    K  FILTERS
;                  FILENAME  K  CCDFNAME
;                  OBJECT    K  OBJECT
;                  UT        K  UT 
;                 The middle column is a flag. It may be K, for Keyword,
;                 T, for Template, or V, for Value. If it is V, the contents
;                 of the third field on that line should make sense for the
;                 name in the first field.
;
;  MAXPHOTSIG- Maximum DN value for a useful signal.  Any source with a peak
;                above this level is passed over.  Default=60000.0 DN
;
;  NODISPLAY - Flag, when set will suppress all image display allowing program
;                to be run in background or batch mode.  This will be somewhat
;                faster as well.  The display steps take a small but non-trivial
;                amount of time.
;
;  OBJRAD    - Radius of object aperture, in pixels, for photometry extraction.
;                Default=GAP
;
;  OUTPATH   - Optional path for output directory for source files.
;                If not specified, the current directory is used.
;
;  PATH      - Optional path for original image directory.
;                If not specified, the current directory is used.
;
;  PHOTOGRAPHIC - Flag, if set will use photphot.pro for centroid and photometry
;                   calculations rather than basphote.  This is for use on
;                   scanned photographic data.
;
;  SIGTHRESH - Sigma threshold for source detection.  Anything brighter
;                than this many sigma above sky will be considered a source.
;                Default = 2.5
;
;  SILENT - Flag, if set suppresses all printed (non-error) output.
;
;  WINDOW    - Size of region to average over in each direction, default=6
;
; OUTPUTS:
;  If the input is a file name, then a fits file is written with the results
;    of the extraction.  The file name is the same as the input file name
;    with '.src' appended.  If the input file is a multi-group fits file
;    then there will be as many output files as there are extensions.  In
;    that case, the tag added looks like .srcxN where N is the extension
;    number.  The data is written as a 2-D array but it is really a table of
;    numbers (all floating point).  If you read the fits file you can extract
;    the following information from the resulting array:
;
;      xpos = data[*,0]       X position of source
;      ypos = data[*,1]       Y position of source
;      fwhm = data[*,2]       FWHM of source in pixels
;      mag  = data[*,3]       Instrumental magnitude of source
;      err  = data[*,4]       Uncertainty on the instrumental magnitude
;      snr  = data[*,5]       SNR of source detection
;
;  In addition to the array data, other useful information is stored in the
;    header of the fits file.
;
;  If the input is an array, then the only output is the anonymous structure
;    returned via the RESULTS output keyword.  No files are written for this
;    case.
;
; KEYWORD OUTPUT PARAMETERS:
;
; RESULTS    - The results of the source extraction are returned to this
;                variable as an anonymous structure.  The following tags
;                are returned in the structure:
;
;               xc       - X position of sources [pixels]
;               yc       - Y position of sources [pixels]
;               fwhm     - full-width at half-max of sources [pixels]
;               flux     - Raw flux of sources [photons/sec]
;               mag      - Instrumental magnitude of sources
;               err      - Uncertainty on the magnitude
;               sky      - Mean sky signal for each source [DN]
;               skysig   - Sky noise for each source [DN]
;               snr      - signal-to-noise ratio for source
;               nobj     - Number of sources
;               avgfwhm  - Robust average of FWHM of all sources [pixels]
;               avgsky   - Robust average of sky values [DN]
;               skysg    - Robust average of sky noise [DN]
;               obscura1 - Fraction of image obscured at 5*skysig
;               obscura2 - Fraction of image obserred at 50*skysig
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
;   98/03/11, Written by Marc W. Buie, Lowell Observatory
;   98/03/22, MWB, added OBJRAD keyword
;   98/03/23, MWB, added EXTLIST keyword
;  2003/02/21, MWB, added a blank padding character to make sure object name
;                      never abuts the file name in the log file.
;  2004/03/04, MWB, added option for array input
;  2004/07/15, MWB, extracted the collapse function to an external routine
;  2007/10/29, MWB, added PHOTOGRAPIC keyword for scanned plate data.
;  2008/07/25, MWB, add flux to output results structure.  Impose minimum
;                     sigma and flux values to 1 in CRS calculation
;  2009/11/17, MWB, added OUTPATH keyword
;  2010/05/06, MWB, added EXPTIME keyword, added more documentation
;  2010/08/03, MWB, minor fix for pathological input
;-
pro findsrc_detect,a,sz,sigthresh,x0,dx,y0,dy,detect,SILENT=silent

   fmt='($,a)'

   for i=0,sz-1 do begin
      if i eq 0 then begin
         avg=shift(a,x0,y0)
      endif else begin
         avg=temporary(avg)+shift(a,x0+dx*i,y0+dy*i)
      endelse
   endfor
   avg=temporary(avg)/sz
   if not silent then print,'.',format=fmt
   for i=0,sz-1 do begin
      if i eq 0 then begin
         std=(shift(a,x0,y0)-avg)^2
      endif else begin
         std=temporary(std)+(shift(a,x0+dx*i,y0+dy*i)-avg)^2
      endelse
   endfor
   std=sqrt(temporary(std)/(sz-1))
   std = (a-avg)/temporary(std)
   z=where(std gt sigthresh,count)
   if count ne 0 then detect[z]=ishft(detect[z],1)

end

pro findsrc_phot,fn,exttag,disp,detect,nx,ny,gain,a,osz,binfac,info, $
                 sz,gap,objrad,maxphotsig,sigthresh,nobj,NOSAVE=nosave, $
                 RESULTS=results,PHOTOGRAPHIC=photo, $
                 SILENT=silent,OUTPATH=outpath

   self = 'FINDSRC_PHOT: '
   nobj=0
   if disp then setwin,1
   z=where(detect,count)
   if count ne 0 then begin
      x = z mod nx
      y = z / nx
      zg=where(a[x,y] lt maxphotsig,countg)
      if countg gt 0 then begin
         if countg gt 0  and countg ne n_elements(xc) then begin
            x = x[zg]
            y = y[zg]
         endif
         basphote,gain,a,info.exptime,x,y,objrad,objrad+1.0,objrad+5.0, $
            xcen=xc,ycen=yc,mag=mag,err=err,fwhm=fwhm,max=max,flux=flux, $
            skymean=sky,skyerr=skyerr,skysig=skysig,boxmrad=-1*objrad, $
            /nolog,/silent
         snr = (max-sky)/(skysig > 1)
         if photo then begin
            dw = fix(2.0*fwhm+0.5) > 5
            photphot,a,xc,yc,dw,xcen,ycen,xfwhm,yfwhm,xflux,yflux,xback,yback, $
               xcenerr,ycenerr,xfluxerr,yfluxerr
            xc = xcen
            yc = ycen
            flx2mag,xflux/info.exptime,xfluxerr/info.exptime, $
                    xmag,xmagerr,zeropt=24.0
            flx2mag,yflux/info.exptime,yfluxerr/info.exptime, $
                    ymag,ymagerr,zeropt=24.0
            fwhm = (xfwhm+yfwhm)/2.0
            fwhmdev = abs(xfwhm-fwhm)
            backdev = abs(xback-(xback+yback)/2.0)
            mag  = (xmag+ymag)/2.0
            err  = sqrt(xmagerr^2+ymagerr^2)/sqrt(2.0) > abs(xmag-ymag)
;            zg = where(xflux gt 0. and yflux gt 0. and fwhmdev lt 2.3 and $
;                       fwhm gt 1.0 and snr ge sigthresh, countg)
;            zg = where(xflux gt 0. and yflux gt 0. and fwhmdev lt 2.5,countg)
            zg = where(xflux gt 0. and yflux gt 0. and fwhmdev lt 5.0,countg)
         endif else begin
            crsdet = (max-sky)/((flux > 1)/gain*info.exptime)
            zg = where(fwhm gt 1.0 and mag lt 30.0 and $
                       (crsdet ge 0.0 and crsdet le 0.25 and $
                       finite(crsdet)) and snr ge sigthresh, countg)
         endelse
         if countg gt 0 and countg ne n_elements(xc) then begin
            xc   = xc[zg]
            yc   = yc[zg]
            fwhm = fwhm[zg]
            flux = flux[zg]
            mag  = mag[zg]
            err  = err[zg]
            snr  = snr[zg]
            sky  = sky[zg]
            skysig= skysig[zg]
         endif
      endif
      nobj=countg
      if countg eq 0 then begin
         print,self+'Warning, no valid objects found!'
         xc   = 0.
         yc   = 0.
         fwhm = 0.
         flux = 0.
         mag  = 99.999
         err  = 99.999
         snr  = 0.000
         sky  = 0.0
         skysig=99.999
      endif

      if disp and nobj ne 0 then $
         plots,xc,yc,psym=8,/device,symsize=2.0,color=120

      robomean,fwhm*binfac,3.0,0.5,avgfwhm
      robomean,sky,3.0,0.5,avgsky
      robomean,skysig,3.0,0.5,skysg

      zo=where(a gt avgsky+skysg*50.0,countz)
      obscura1= float(countz)/float(n_elements(a))
      zo=where(a gt avgsky+skysg*5.0,countz)
      obscura2= float(countz)/float(n_elements(a))

      data=[[xc*binfac],[yc*binfac],[fwhm*binfac],[mag],[err],[snr]]
      if not keyword_set(nosave) then begin
         mkhdr,hdr,data
         sxaddpar,hdr,'OBJECT',info.object,' Object name'
         sxaddpar,hdr,'AIRMASS',info.airmass,' Airmass of observation'
         sxaddpar,hdr,'XSIZE',osz[0],' Original x-size of image'
         sxaddpar,hdr,'YSIZE',osz[1],' Original y-size of image'
         sxaddpar,hdr,'SIGTHRSH',sigthresh,' Sigma threshold for source detection'
         sxaddpar,hdr,'GAP',gap*binfac,' Object gap size, is approximately the FWHM'
         sxaddpar,hdr,'OBJRAD',objrad*binfac,' Object aperture radius for photometry'
         sxaddpar,hdr,'SIGWSIZE',sz,' Sigma window size for source detection'
         sxaddpar,hdr,'GAIN',gain/(binfac^2),' Gain of CCD in e-/DN'
         sxaddpar,hdr,'BINFAC',binfac,' Software binning factor used'
         sxaddpar,hdr,'EXPTIME',info.exptime,' Exposure time in seconds'
         sxaddpar,hdr,'MAXSIG',maxphotsig,' Saturated above this DN level'
         sxaddpar,hdr,'MEANFWHM',avgfwhm,' Mean FWHM in pixels'
         sxaddpar,hdr,'SKYLEVEL',avgsky,' Average sky signal level counts/pixel'
         sxaddpar,hdr,'SKYSIGMA',skysg,' Standard deviation of the sky signal'
         sxaddpar,hdr,'OBSCURA1',obscura1,' Fraction of image obscured by 50*skysig bright pixels'
         sxaddpar,hdr,'OBSCURA2',obscura2,' Fraction of image obscured by 5*skysig bright pixels'
         writefits,outpath+fn+'.src'+exttag,data,hdr
      endif

      results = { $
         xc: xc*binfac, $
         yc: yc*binfac, $
         fwhm: fwhm*binfac, $
         flux: flux, $
         mag: mag, $
         err: err, $
         sky: sky, $
         skysig: skysig, $
         snr: snr, $
         nobj: nobj, $
         avgfwhm: avgfwhm, $
         avgsky: avgsky, $
         skysg: skysg, $
         obscura1: obscura1, $
         obscura2: obscura2 $
         }

      if not silent then $
         print,'('+strn(avgfwhm,format='(f10.1)')+') s'+ $
            strn(avgsky,format='(i5)',length=5)+' +- '+ $
            strn(skysg,format='(i4)',length=4)+' '+ $
            strn(obscura1,format='(f5.3)')+' '+ $
            strn(obscura2,format='(f5.3)'), $
            format='($,a)'

      ; send information to log file
      if not keyword_set(nosave) then begin
         fnlog = 'info'+exttag+'.log'
         tag   = fn+exttag
         info  = ' '+strn(info.object,padtype=1,length=10) + ' ' + $
                 strn(info.exptime,length=6,format='(f6.1)') + ' ' + $
                 strn(info.airmass,length=4,format='(f4.2)') + ' ' + $
                 strn(avgfwhm,length=5,format='(f5.2)') + ' ' + $
                 strn(avgsky,length=5,format='(i5)') + ' '+ $
                 strn(skysg,length=4,format='(i4)') + ' '+ $
                 strn(nobj,length=5,format='(i5)') + ' ' + $
                 strn(obscura1,format='(f5.3)') + ' ' + $
                 strn(obscura2,format='(f5.3)')
         repwrite,fnlog,tag,tag+info
      endif

   endif

end

pro findsrc,fn, $
       BINFAC=binfac, $
       EXPTIME=in_exptime, $
       EXTLIST=extlist, $
       GAIN=in_gain, $
       GAP=gap, $
       KEYLIST=keylist, $
       MAXPHOTSIG=maxphotsig, $
       NODISPLAY=nodisplay, $
       OBJRAD=objrad, $
       OUTPATH=outpath, $
       PATH=path, $
       RESULTS=results, $
       SIGTHRESH=sigthresh, $
       SILENT=silent, $
       PHOTOGRAPHIC=photo, $
       WINDOW=sz

   self = 'FINDSRC: '
   if badpar(fn,[2,3,4,5,7],[0,2],caller=self+'(file) ',type=fntype) then return
   if badpar(keylist,[7,0],0,caller=self+'(KEYLIST) ', $
                default='[[DEFAULT]]') then return
   if badpar(outpath,[0,7],0,caller=self+'(OUTPATH) ',default='') then RETURN
   if outpath ne '' then outpath=addslash(outpath)
   if badpar(path,[0,7],0,caller=self+'(PATH) ',default='') then RETURN
   if path ne '' then path=addslash(path)
   if badpar(gap,[0,2,3],0,caller=self+'(GAP) ',default=2) then return
   if badpar(sz,[0,2,3],0,caller=self+'(WINDOW) ',default=6) then return
   if badpar(sigthresh,[0,2,3,4,5],0,caller=self+'(SIGTHRESH) ', $
                default=2.5) then return
   if badpar(in_exptime,[0,2,3,4,5],0,caller=self+'(EXPTIME) ', $
                default=1.0) then return
   if badpar(in_gain,[0,2,3,4,5],0,caller=self+'(GAIN) ', $
                default=1.0) then return
   if badpar(maxphotsig,[0,2,3,4,5],0,caller=self+'(MAXPHOTSIG) ', $
                default=60000.0) then return
   if badpar(objrad,[0,2,3,4,5],0,caller=self+'(OBJRAD) ', $
                default=gap) then return
   if badpar(extlist,[0,1,2,3],[0,1],caller=self+'(EXTLIST) ', $
                default=-1) then return
   if badpar(binfac,[0,2,3],0,caller=self+'(BINFAC) ',default=1) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                default=0) then return
   if badpar(photo,[0,1,2,3],0,caller=self+'(PHOTOGRAPHIC) ', $
                default=0) then return

   disp = (!d.name eq 'X' or !d.name eq 'PS') and not keyword_set(nodisplay)
   fmt='($,a)'

   if fntype eq 7 then begin

      gain=in_gain*(binfac^2)

      loadkeys,keylist,hdrlist

      ; Check header of image to see if it is a multi-extension image.
      if exists(path+fn+'.fits') then ft='.fits' else ft=''
      hdr=headfits(path+fn+ft)
      numext=sxpar(hdr,'NEXTEND')
      extend=sxpar(hdr,'EXTEND')
      if numext eq 0 or extend ne 1 then begin
         extlist=0
      endif else begin
         if extlist[0] eq -1 then begin
            extlist=indgen(numext)+1
         endif else begin
            if max(extlist) gt numext then begin
               print,self,'Input extension list is incompatible with the ', $
                          'number of extensions'
               print,'in the file.  This file had ',numext,' extensions and ', $
                          'the largest item in'
               print,'your list is ',max(extlist)
               print,'Aborting.'
               return
            endif else if min(extlist) le 0 then begin
               print,self,'Input extension list is invalid.  You have one or ', $
                          'more values less'
               print,'than or equal to zero.'
               print,'Aborting.'
               return
            endif
         endelse
      endelse
      numext=n_elements(extlist)

      for ext=0,numext-1 do begin

         if extlist[ext] eq 0 then begin
            extstr = ''
            exttag = ''
         endif else begin
            extstr = strb36(extlist[ext])
            exttag = 'x'+extstr
         endelse

         if not silent then print,fn+exttag+' '+systime(),format=fmt

         a=0.
         fits_read,path+fn+ft,a,hdr,exten_no=extlist[ext]
         if binfac ne 1 then begin
            arrsz=size(a)
            nx=(arrsz[1]/binfac)*binfac
            ny=(arrsz[2]/binfac)*binfac
            if not silent then print,' RB',format=fmt
            a=rebin(a[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
         endif
         parsekey,hdr,hdrlist,info
         arrsz=size(a)
         nx=arrsz[1]
         ny=arrsz[2]
         if arrsz[3] ne 4 then a = float(a)
         if disp then begin
            setwin,0,xsize=nx,ysize=ny
            tvscl,a
         endif

         if ext eq 0 then detect=replicate(1B,nx,ny) else detect[*] = 1B

         if not silent then print,' UP',format=fmt
         findsrc_detect,a,sz,sigthresh,0,0,-gap,-1,detect,silent=silent

         if not silent then print,'DOWN',format=fmt
         findsrc_detect,a,sz,sigthresh,0,0,gap,1,detect,silent=silent

         if not silent then print,'RIGHT',format=fmt
         findsrc_detect,a,sz,sigthresh,-gap,-1,0,0,detect,silent=silent

         if not silent then print,'LEFT',format=fmt
         findsrc_detect,a,sz,sigthresh,gap,1,0,0,detect,silent=silent

         if not silent then print,' THRSH',format=fmt

         ; requires detection in 3 or 4 directions.
         detect=(temporary(detect) and 24B) < 1B

         if disp then begin
            if not silent then print,' DISP',format=fmt
            setwin,1,xsize=nx,ysize=ny
            skysclim,a,lowval,hival,meanval,sigma
            lowval=meanval-8*sigma
            hival=meanval+16*sigma
            tv,bytscl(a,min=lowval,max=hival,top=!d.n_colors-1)
            setwin,2,xsize=nx,ysize=ny
            tvscl,detect
         endif

         detect=median(detect,2)
         detect[0:4,*]=0
         detect[*,0:4]=0
         detect[nx-5:nx-1,*]=0
         detect[*,ny-5:ny-1]=0

         if disp then begin
            setwin,3,xsize=nx,ysize=ny
            tvscl,detect
         endif

         if not silent then print,' LOC',format=fmt
         collapse,a,detect,gap,xmax,ymax
         if disp then begin
            setusym,-1
            plots,xmax,ymax,psym=8,/device,symsize=2.0,color=64
         endif

         if not silent then print,' PHOT',format=fmt
         findsrc_phot,fn,exttag,disp,detect,nx,ny,gain, $
                   a,[arrsz[1],arrsz[2]], $
                   binfac,info,sz,gap,objrad,maxphotsig,sigthresh,nobjs, $
                   results=results,PHOTOGRAPHIC=photo, $
                   outpath=outpath,silent=silent

         if disp then setusym,1
         if not silent then print,' '+systime(),nobjs

      endfor ; extension loop

   endif else begin

      arrsz = size(fn)
      nx = arrsz[1]
      ny = arrsz[2]
      gain = in_gain

      info = { $
         exptime: in_exptime $
         }

      if disp then begin
         setwin,0,xsize=nx,ysize=ny
         tvscl,fn
      endif

      detect=replicate(1B,nx,ny)

      if not silent then print,' UP',format=fmt
      findsrc_detect,fn,sz,sigthresh,0,0,-gap,-1,detect,silent=silent

      if not silent then print,'DOWN',format=fmt
      findsrc_detect,fn,sz,sigthresh,0,0,gap,1,detect,silent=silent

      if not silent then print,'RIGHT',format=fmt
      findsrc_detect,fn,sz,sigthresh,-gap,-1,0,0,detect,silent=silent

      if not silent then print,'LEFT',format=fmt
      findsrc_detect,fn,sz,sigthresh,gap,1,0,0,detect,silent=silent

      if not silent then print,' THRSH',format=fmt
      detect=(temporary(detect) and 24B) < 1B

      if disp then begin
         if not silent then print,' DISP',format=fmt
         setwin,1,xsize=nx,ysize=ny
         skysclim,fn,lowval,hival,meanval,sigma,NPTS=30000
         lowval=meanval-3*sigma
         hival=meanval+5*sigma
         tv,bytscl(fn,min=lowval,max=hival,top=!d.n_colors-1)
         setwin,2,xsize=nx,ysize=ny
         tvscl,detect
      endif

      detect=median(detect,2)
      detect[0:4,*]=0
      detect[*,0:4]=0
      detect[nx-5:nx-1,*]=0
      detect[*,ny-5:ny-1]=0

      if disp then begin
         setwin,3,xsize=nx,ysize=ny
         tvscl,detect
      endif

      if not silent then print,' LOC',format=fmt
      collapse,fn,detect,gap,xmax,ymax
      if disp then begin
         setusym,-1
         plots,xmax,ymax,psym=8,/device,symsize=2.0,color=64
      endif

      if not silent then print,' PHOT',format=fmt
      findsrc_phot,fn,exttag,disp,detect,nx,ny,gain,fn,[arrsz[1],arrsz[2]], $
                   1,info,sz,gap,objrad,maxphotsig,sigthresh,nobjs, $
                   /nosave,results=results,PHOTOGRAPHIC=photo, $
                   outpath=outpath,silent=silent

      if not silent then print,' '+systime(),nobjs

   endelse

end
