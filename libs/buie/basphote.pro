;+
; NAME:
;    basphote
; PURPOSE: (one line)
;    Circular aperture photometry extraction from images.
; DESCRIPTION:
;
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;    Basphote,gain,image,exptime,xloc,yloc,radius,sky1,sky2,logfile[,objnum]
;
; INPUTS:
;    gain       : Gain of the CCD.  Photons per count (DN).
;    image      : CCD image array.
;    exptime    : Exposure time for the image in seconds.
;    xloc, yloc : Current location of the cursor in image coordinates.
;    radius     : Current aperture radius in pixels.
;    sky1       : Inner radius of the sky annulus (in pixels).
;    sky2       : Outer radius of the sky annulus (in pixels).
;                   If sky2<0 then sky1 is taken to be the actual sky signal
;                   in DN/pixel and |sky2| is the error on the sky.
;    logfile    : Name of the photometry log file.
;
; OPTIONAL INPUT PARAMETERS:
;    objnum     : Starting serial number for object in frame, default=0
;                    If supplied as a scalar it will be incremented by
;                    the number of positions supplied in xloc/yloc.
;                 If given as a vector, will specify the exact number to
;                    and there will be no incrementing when done.
;
; KEYWORD INPUT PARAMETERS: (default on flags is false):
;    AIRMASS - Optional airmass value.
;    ALTLOG  - Flag, if set, output is generated in an alternate format.
;    BAD     - Flag (or array) marking data as bad, default=good
;    BOXMRAD - Size of the box to look for local max in.  Default=radius.
;                 If boxmrad is negative, then the call to BOXM that finds the
;                 local max is suppressed.  In effect, basphote assumes that
;                 the input location is already the maximum.  You still need
;                 to provide a non-zero number for boxmrad so that a local
;                 area is defined for the fwhm calculation.
;    DT      - Delta-time, in seconds, between any two frames.
;    EXACT   - Flag, if true: take position as exact; otherwise find it.
;    FNAME   - File name of image.
;               if ALTLOG set use the actual filename for the image on disk.
;               if not set this should be an 8 character code that relates
;                  to the image (Default is blank).
;    FILTER  - Filter code, required if ALTLOG is set.
;    JD      - Julian date of observation (mid-time), required if ALTLOG set.
;    NAME    - Object name(s), required if ALTLOG set.
;    NOLOG   - If set, no output logfile information is generated.
;                 The default is to generate a log file.
;    NOMEXT  - Optional nominal extinction.
;    PRINTALL- Print all objects even if off chip (if printing enabled)
;    PSCALE  - Plate scale in arc-sec per pixel, required if ALTLOG is set.
;    RDNOISE - Optional CCD readout noise in (e-/pixel).   Default=10 e-,
;                 default is also used if RDNOISE is given as a negative number.
;    SILENT  - Flag, if true --> Do not generate any screen output.
;    ZPOINT  - Optional zero point.
;
; KEYWORD OUTPUT PARAMETERS:
;    ERR     - Optional return of the magnitude error.
;    FLERR   - Uncertainty (1 sigma) of object flux.
;    FLUX    - Object flux (photons per second)
;    FWHM    - FWHM of object image(s), in arcsec if PSCALE provided, otherwise
;                 returned in pixels
;    ONCHIP  - Byte array of flags that indicate if object was on-chip.
;    OUTJD   - Optional output of Julian dates (for cubes). This will be a
;              vector of length rank(image). Contents are computed from the
;              input Julian date (JD=) and the input delta-time (DT=).
;    MAG     - Optional return of the instrumental magnitude.
;    MAX     - Optional return of peak signal in object.
;    SKYMEAN - Optional return of sky signal, counts/pixel.
;    SKYERR  - Optional return of sky signal uncertainty, counts/pixel.  This
;                  number is the standard deviation of the mean.
;    SKYSIG  - Optional return of the standard deviation of the sky signal.
;    XCEN    - Optional output of centroid x-position(s).
;    YCEN    - Optional output of centroid y-position(s).
;
; OUTPUTS:
;    All output is sent to the screen and the logfile.  Selected variables are
; returned via optional keywords.
;
; COMMON BLOCKS:
;    None.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Ported by Doug Loucks, Lowell Observatory, 1992 Oct from the
;    C-language version written by Marc Buie.
;
;    93/6/1, Marc W. Buie, Lowell Observatory, added the alternate logfile
;      format to the program.  Streamlined the required input parameters.
;      Calling sequence is now different.
;
;    1/25/93, MWB, changed default on boxmrad to be the LAST value in the
;      radius vector.
;
;    2/3/93, DWL. Moved the file format declarations outside of the main loop.
;    2/4/93, DWL. Modified to accept all valid combinations of scalar and
;      vector values for xloc, yloc, radius, sky1, and sky2 inputs.
;    2/4/93, DWL. Added checks for 'seeing' sub-array extraction.
;    2/4/93, DWL, Added keywords XCEN and YCEN to allow return of the refined
;      center(s) to the calling program.  Also, xcen, ycen, xmax, and ymax
;      will be vectors if xloc and yloc are vectors.
;    2/10/93,DWL, Removed XMAX and YMAX keywords.
;    2/10/93,DWL, Added checks for off-chip and too-near-edge conditions.
;    4/21/93,DWL, Added keywords MAG and ERR.  The instrumental magnitude and
;      uncertainty are returned to the caller if these keywords are present.
;      If the inputs xcen and ycen are vectors, these variables will also be
;      returned as vectors.
;    5/21/93, MWB, Fixed variables that are undefined if off-chip condition
;      is found.  Dummy values are set to prevent printouts from dying.
;    5/26/93, MWB, Added FLUX, FLUXERR, and NOLOG keywords.  Fixed bug on
;      computation of sky background error, errant gain factor removed.
;    9/30/93, MWB, Modified usage of NAME to allow vector input.
;    2/2/1994, DWL, Mods to allow 3-D image cube photometry. Also added
;      OUTJD keyword.
;    4/28/94, MWB, Fixed bug that recomputed a new centroid position if
;      the EXACT flag was set.  This had NO effect on photometry.
;    8/30/94, MWB, Added ONCHIP and PRINTALL keywords.
;    5/18/95, MWB, Added FWHM optional output keyword.
;    6/16/95, MWB, If PSCALE negative, fwhm not computed.
;    1/8/96, MWB, Added override on serial number, objnum can now be a vector.
;    7/13/96, MWB, changed FWHM calculation to remove function fitting.
;   10/31/96, MWB, added BAD= keyword for flag pass through
;    3/17/97, MWB, added MAX keyword
;   97/09/10, MWB, removed extraneous sub-expression evaluation (5x faster).
;   98/03/04, MWB, Added SKYMEAN, SKYERR return keywords.
;   98/09/21, MWB, added suppression of BOXM call through a negative BOXMRAD.
; 2000/01/18, MWB, added SKYSIG return keywords.
; 2000/06/02, MWB, upgrade to photometry log file version that includes sky
;                  measurement.
; 2001/01/25, MWB, fixed bug in certain cases when overriding the object
;                  serial number.  Also took out all "message" calls to make
;                  sure info and warning messages are seen.
; 2004/07/15, MWB, changed loop variables to LONG.
; 2006/04/19, MWB, fixed bug in ALTLOG usage for upgrading photometry files
; 2006/04/26, Peter L. Collins, Lowell Observatory, upgrade to include and
;                   log CCD readout noise (keyword RDNOISE). The log level
;                   is promoted to v1.1 from v1.0.
; 2006/05/22, PLC, added another way to default the rdnoise, using keyword
;                  RDNOISE=v where v is some negative value- as a convenience
;                  to other code.
; 2008/03/25, MWB, fixed minor bug related to an aperture that falls off chip
;-
pro basphote, gain, image, exptime, xloc, yloc, radius, sky1, sky2, $
              logfile, in_objnum, $
              AIRMASS=in_airmass, ALTLOG=altlog, BAD=bad, $
              BOXMRAD=boxmrad, DT=in_dt, $
              ERR=out_magerr, EXACT=exact, FILTER=filter, FLERR=out_fluxerr, $
              FLUX=out_flux, FNAME=fname, FWHM=out_fwhm, $
              JD=in_jd, OUTJD=out_jd, MAX=out_max, $
              MAG=out_imag, NAME=name, NOLOG=nolog, $
              NOMEXT=in_nomext, ONCHIP=out_onchip, $
              RDNOISE=rdnoise, SKYMEAN=out_skymean, $
              SKYERR=out_skyerr, SKYSIG=out_skysig, SILENT=silent, $
              PSCALE=in_pscale, XCEN=out_xcen, YCEN=out_ycen, $
              ZPOINT=in_zpoint ;, TIMEIT=timeit

   ;common com_basphote,info

   ;timeit = keyword_set(timeit)

   ;if timeit then cputime,time0

   ;if timeit and n_elements(info) eq 0 then begin
   ;   info = { t: fltarr(20), ncalls: 0L }
   ;   ; t[0] = startup overhead (to start of main FOR loop)
   ;   ; t[1] = boxm
   ;   ; t[2] = 1st center
   ;   ; t[3] = sky   42%
   ;   ; t[4] = final centrod call
   ;   ; t[5] = FWHM
   ;   ; t[6] = I/O
   ;endif

   ;if timeit then info.ncalls = info.ncalls+1

   ;Command line verification, 9 parameters are required, the tenth is optional.
   if n_params() lt 8 or n_params() GT 10 then begin
      print,'basphote,gain,image,exptime,xloc,yloc,' + $
            'radius,sky1,sky2,logfile[,objnum]'
      return
   endif

   self = 'BASPHOTE'
   default_rdnoise=10.0;

   if badpar(gain,[2,3,4,5],0,CALLER=self+' (gain): ') then return
   if badpar(image,[1,2,3,4,5,12,13,14,15],[2,3],CALLER=self+' (image): ', $
                                     RANK=im_rank, DIMEN=im_dimen) then return
   if badpar(exptime,[2,3,4,5],0,CALLER=self+' (exptime): ') then return
   if badpar(xloc,[2,3,4,5],[0,1],CALLER=self+' (xloc): ') then return
   if badpar(yloc,[2,3,4,5],[0,1],CALLER=self+' (yloc): ') then return
   if badpar(radius,[2,3,4,5],[0,1],CALLER=self+' (radius): ') then return
   if badpar(sky1,[2,3,4,5],[0,1],CALLER=self+' (sky1): ') then return
   if badpar(sky2,[2,3,4,5],[0,1],CALLER=self+' (sky2): ') then return
   if badpar(nolog,[0,1,2,3],0,CALLER=self+' (NOLOG): ',DEF=0) then return
   if not nolog then $
      if badpar(logfile,7,0,CALLER=self+' (logfile): ') then return
   if badpar(in_objnum,[0,2,3],[0,1],CALLER=self+' (objnum): ',DEF=0, $
      RANK=num_rank, NPTS=num_npts) then return

   if badpar(boxmrad,[0,2,3,4,5],0,CALLER=self+' (boxmrad): ', $
                DEF=radius[n_elements(radius)-1]      ) then return

   if badpar(exact,[0,1,2,3],0,CALLER=self+' (exact): ',DEF=0) then return
   if badpar(silent,[0,1,2,3],0,CALLER=self+' (SILENT): ',DEF=0) then return
   if badpar(printall,[0,1,2,3],0,CALLER=self+' (printall): ',DEF=0) then return
   if badpar(rdnoise,[0,2,3,4,5],0,CALLER=self+' (rdnoise): ', $
                                   default=default_rdnoise) then return

   if keyword_set(in_pscale) then pscale=in_pscale ELSE pscale=1.0

   if badpar(bad,[0,2,3],[0,1],CALLER=self+' (BAD): ',default=0) then error=1

   if keyword_set(altlog) then begin
      error=0
      if badpar(fname,    7,    0,CALLER=self+' (fname): ')     then error=1
      if badpar(filter,   7,    0,CALLER=self+' (filter): ')    then error=1
      if badpar(in_jd,    5,    0,CALLER=self+' (jd): ')        then error=1
      if badpar(name,     7,[0,1],CALLER=self+' (name): ')      then error=1
      if badpar(in_pscale,[4,5],0,CALLER=self+' (in_pscale): ') then error=1
      if error then return
   endif

   if badpar( in_dt, [0,2,3,4,5], 0, CALLER=self+' (dt): ', $
                                     DEFAULT=0.0D0) then return

   if rdnoise lt 0.0 then rdnoise=default_rdnoise;

   if not keyword_set(fname) then fname=''

   if not keyword_set(name) then name=''

   check=[n_elements(xloc),n_elements(radius), $
          n_elements(sky1),n_elements(sky2),n_elements(name),n_elements(bad)]

   z=where(check ne 1, count)

   if n_elements(xloc) ne n_elements(yloc) then begin
      print,self+': xloc and yloc must be the same length.'
      return
   endif

   if count ne 0 then begin
      if min(check[z]) ne max(check[z]) then begin
         if n_elements(xloc) eq 1 then begin
            print, $
            'radius, sky1, sky2, and name must be the same length or scalar.'
         endif else begin
            print, $
            'radius, sky1, sky2, name, and bad must match the length of xloc or be scalar.'
         endelse
         return
      endif
   endif

   ; Check for a few more keywords.
   if keyword_set( in_airmass ) then airmass=in_airmass else airmass=0.0
   if keyword_set( in_nomext )  then nomext=in_nomext   else nomext=0.0
   if keyword_set( in_zpoint )  then zpoint=in_zpoint   else zpoint=0.0
   if keyword_set( in_jd )      then jd=in_jd           else jd=0.0D0

   ; Input image array size needs to be known.
   xsize = im_dimen[ 0 ]
   ysize = im_dimen[ 1 ]
   if im_rank eq 3 then zsize=im_dimen[2] else zsize=1

   ; Set-up the local copy of the Julian date.
   if zsize gt 1 then begin
      l_jd = jd + dindgen( zsize ) * ( double( in_dt ) / 86400.0D0 )
   endif else begin
      l_jd = jd
   endelse

   ; Make local copies of the inputs xloc, yloc, radius, sky1, and sky2.
   ; If any are vectors, the others of type scalar will be expanded into
   ; vectors having the same length.  At this point, length requirements have
   ; been verified.
   l_xloc   = xloc
   l_yloc   = yloc
   l_radius = radius
   l_sky1   = sky1
   l_sky2   = sky2
   l_name   = name
   l_bad    = bad

   ; Information about these inputs is needed to determine which are scalars
   ; and which are vectors.
   xloc_stat   = size( xloc )
   radius_stat = size( radius )
   sky1_stat   = size( sky1 )
   sky2_stat   = size( sky2 )
   name_stat   = size( name )
   bad_stat    = size( bad  )
   ;
   largest = max( check )
   if largest gt 1 then begin
      ; At least one is a vector with two or more elements.  Make those of type
      ; scalar into vectors of the same length.
      if xloc_stat[0] eq 0 then begin
         l_xloc = replicate( xloc, largest )
         l_yloc = replicate( yloc, largest )
      endif
      if radius_stat[0] eq 0 then l_radius = replicate( radius, largest )
      if sky1_stat[0]   eq 0 then l_sky1   = replicate( sky1, largest )
      if sky2_stat[0]   eq 0 then l_sky2   = replicate( sky2, largest )
      if name_stat[0]   eq 0 then l_name   = replicate( name, largest )
      if bad_stat[0]    eq 0 then l_bad    = replicate( bad, largest )
   endif

   if num_rank eq 1 and num_npts ne largest then begin
      print,self+': serial number vector must match length of other vectors'
      return
   endif

   l_xcen   = fltarr( largest, zsize )
   l_ycen   = fltarr( largest, zsize )
   localmax = fltarr( largest, zsize )
   imag     = fltarr( largest, zsize )
   magerr   = fltarr( largest, zsize )
   flux     = fltarr( largest, zsize )
   fluxerr  = fltarr( largest, zsize )
   fwhm     = fltarr( largest, zsize )
   skymean  = fltarr( largest, zsize )
   skyerr   = fltarr( largest, zsize )
   skysig   = fltarr( largest, zsize )
   l_onchip = bytarr( largest, zsize )

   ; Open the log file for appending the new results.
   if not nolog then begin
      if not exists(logfile) then begin
         openw, logfile_lu, logfile, /GET_LUN
         printf,logfile_lu,'PHOTFILE v1.1'
      endif else begin
         if keyword_set(altlog) then photprmt,logfile
         openw, logfile_lu, logfile, /APPEND, /GET_LUN
      endelse
   endif

   ; Define the log file formats.
   if keyword_set( altlog ) then begin
      fmt1 = '(a,1x,"''",a,"''",1x,a,1x,f13.5,1x,f8.3,1x,f6.2,1x,' + $
               'f6.2, 1x,' + $
               'f7.3,1x,f7.3,' + $
             '1x,f7.3,1x,i4.4,1x,f8.3,1x,f8.3,1x,f5.2,1x,f7.1,1x,f8.2,1x,f6.2,' + $
             '1x,f8.4,1x,f7.4,1x,i1)'
   endif else begin
      fmt1 = '(A8,1X,I4.4,1X,F8.3,1X,F8.3,1X,F8.3,1X,F5.2,1X,F6.3,' + $
             '1X,F4.1,1X,F5.1,1X,F8.4,1X,F6.4)'
   endelse

   fmt2 = '(14X,F8.3,1X,F8.3,1X,F8.2,1X,F6.2,1X,F11.2,1X,F8.2,1X,F5.2)'

   boxsize = fix( abs(boxmrad) + 0.5 )

   ;if timeit then begin
   ;   cputime,time1
   ;   info.t[0]=info.t[0]+(time1-time0)
   ;endif

   ; Do the requested photometry for all inputs.
   for frame=0L, zsize-1 do begin

      ; Cull out the working image plane for processing.  This is needed to
      ;   avoid expensive array copies later.
      if zsize eq 1 then begin
         wimage = temporary(image)
      endif else begin
         wimage = image[*,*,frame]
      endelse

      ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	   for i=0L, largest-1 do begin
   ;      if timeit then begin
   ;         cputime,time1
   ;         time2=time1
   ;         time3=time1
   ;         time4=time1
   ;         time5=time1
   ;         time6=time1
   ;         time7=time1
   ;         time8=time1
   ;         time9=time1
   ;         time10=time1
   ;      endif
         if num_rank eq 0 then begin
            objnum = in_objnum + i
         endif else begin
            objnum = in_objnum[i]
         endelse
	      area = !PI * l_radius[i]*l_radius[i]
	      xcen  = l_xloc[i]
	      ycen  = l_yloc[i]
	      xcen1 = l_xloc[i]
	      ycen1 = l_yloc[i]
	      autosky = l_sky2[i] gt 0

         ; Set some initial variables to values to be printed if off-chip
         ; conditions
         ; are encountered.
         photons = 0.0
         tmp_skymean = 0.0
         tmp_skyerr = 0.0
         tmp_skysig = 0.0
         skyphot = 0.0
         counts  = 0.0
         photerr = 0.0
         sknfrac = 0.0

         imag[i, frame] = 99.9999
         magerr[i, frame] = 0.0
         flux[i, frame] = 0.0
         fluxerr[i, frame] = 0.0
         fwhm[i, frame] = -1.0
         skymean[i, frame] = 0.0
         skyerr[i,frame] = 0.0
         skysig[i,frame] = 0.0
         localmax[i, frame] = 0.0

         ; First, find the maximum within a box centered on the last known
         ;object position.
         if frame eq 0 then begin
            ;Use the provided input position.
            objxloc = fix( l_xloc[i] + 0.5 )
            objyloc = fix( l_yloc[i] + 0.5 )
         endif else begin
            ;Use the refined center from the last frame.
            objxloc = fix( l_xcen[i, frame-1] + 0.5 )
            objyloc = fix( l_ycen[i, frame-1] + 0.5 )
         endelse

   ;      if timeit then cputime,time2
         if boxmrad gt 0.0 then begin
            boxm, wimage, objxloc, objyloc, boxsize, boxsize, xmax, ymax
         endif else begin
            xmax = objxloc
            ymax = objyloc
         endelse
   ;      if timeit then cputime,time3

         offchip = min([xmax, ymax, xsize-1-xmax, ysize-1-ymax]) lt 0
         if offchip then begin
            onchip = 0B
            GOTO, skipcalc
         endif

         localmax[i,frame] = wimage[ xmax, ymax ]

         if exact then begin
         ;  Use the provided position as the location of the aperture.
            xcen = l_xloc[i]
            ycen = l_yloc[i]
         endif else begin

         ;  Refine the center by computing a center of mass of object aperture,
         ;  no sky correction. Use the location of the brightest pixel near the
         ;  provided location for the center.
            tmp_skymean = 0.0
            too_near_edge = ( xmax + l_radius[i] gt xsize - 0.5 ) or $
               ( ymax + l_radius[i] gt ysize - 0.5 ) or $
               ( xmax - l_radius[i] lt -0.5 ) or ( ymax - l_radius[i] lt -0.5 )
            if too_near_edge then begin
               xcen1 = xmax
               ycen1 = ymax
               onchip = 0B
               goto, skipcalc
            endif
            centrod, wimage, xmax, ymax, l_radius[i], 0.0, 0.0, $
               tmp_skymean, xcen, ycen, counts
         endelse

   ;      if timeit then cputime,time4
         ; Compute the sky signal.
         if autosky then begin
            ;Sky is to be determined.
            Getannul, wimage, xcen, ycen,  l_sky1[i], l_sky2[i], skybuf
            Robomean, skybuf, 3.0, 0.5, tmp_skymean, avgdev, tmp_skysig, $
               var, skew, kurt, nsky
            if nsky ne 0 then begin
               tmp_skyerr = tmp_skysig / sqrt( float( nsky ) )
            endif else begin ; This should never happen, warn if it does.
               tmp_skyerr = 0.0
               print,self+'-WARNING: Sky annulus is empty or entirely off chip'
            endelse
         endif else begin
         ;  Sky is being provided externally.
            tmp_skymean = l_sky1[i]
            tmp_skysig = abs( l_sky2[i] )
            tmp_skyerr = abs( l_sky2[i] )
         endelse
   ;      if timeit then cputime,time5

         ; Compute the final result for the object.

         too_near_edge = ( xcen + l_radius[i] gt xsize - 0.5 ) or $
            ( ycen + l_radius[i] gt ysize - 0.5 ) or $
            ( xcen - l_radius[i] lt -0.5 ) or ( ycen - l_radius[i] lt -0.5 )
         if too_near_edge then begin
            xcen1 = xcen
            ycen1 = ycen
            onchip = 0B
            goto, skipcalc
         endif
   ;      if timeit then cputime,time6
         centrod, wimage, xcen, ycen, l_radius[i], 0.0, 0.0, tmp_skymean, $
            xcen1, ycen1, counts
   ;      if timeit then cputime,time7
         if exact then begin
            xcen1 = xcen
            ycen1 = ycen
         endif
         photons = counts * gain
         skyphot = tmp_skymean * gain
         sigphotsky = area * tmp_skyerr * gain
         varphotobj = photons + sigphotsky * sigphotsky 
         photerr = sqrt( varphotobj + sigphotsky^2 + area*rdnoise^2 )
         flux[i, frame] = photons / exptime
         fluxerr[i, frame] = photerr / exptime
         skymean[i,frame] = tmp_skymean
         skysig[i,frame] = tmp_skysig
         skyerr[i,frame] = tmp_skyerr
         onchip = 1B

         if varphotobj ne 0.0 then begin
            sknfrac = sigphotsky^2/photerr^2
         endif else begin
            sknfrac = 0.0
         endelse

         if photons gt 0.0 then begin
            imag[i, frame] = -2.5 * alog10( flux[i, frame] ) + 24.0
            magerr[i, frame] = 1.085736205 * photerr / photons
         endif else begin
            imag[i, frame] = 99.999
            magerr[i, frame] = 0.0
         endelse
   ;      if timeit then cputime,time8

         ; Find out what the seeing was on the object.
         ; First, extract a sub-box around the max.
         lx = xmax - boxsize
         ly = ymax - boxsize
         hx = xmax + boxsize
         hy = ymax + boxsize
         if lx lt 0 then lx = 0
         if ly lt 0 then ly = 0
         if hx ge xsize then hx = xsize - 1
         if hy ge ysize then hy = ysize - 1
         sub = wimage[ lx : hx, ly : hy ]
         if pscale gt 0.0 then begin
            xsum=fltarr(hx-lx+1)
            ysum=fltarr(hy-ly+1)
            for ii=0L,hy-ly do xsum = xsum+sub[*,ii]-tmp_skymean
            for ii=0L,hx-lx do ysum = ysum+sub[ii,*]-tmp_skymean
            xwd = total(xsum)/max(xsum)
            ywd = total(ysum)/max(ysum)
            tmp_fwhm = mean([xwd,ywd])
   ;         radp, sub, xcen1-lx, ycen1-ly, r, im, tmp_fwhm
         endif else begin
            tmp_fwhm = 1.0
         endelse
         fwhm[i,frame] = tmp_fwhm*abs(pscale)
         if fwhm[i,frame] gt 99.994 or fwhm[i,frame] lt -9.98 then fwhm[i,frame] = -1.0
   ;      if timeit then cputime,time9

         skipcalc:

         if ( not nolog ) and (printall or onchip) then begin
            if keyword_set( altlog ) then begin
               ; New log file format
               printf, logfile_lu, format=fmt1, $
                  fname, l_name[i], filter, l_jd[frame], exptime, gain, $
                  rdnoise, l_radius[i], $
                  l_sky1[i], l_sky2[i], objnum, xcen1, ycen1, fwhm[i,frame], $
                  localmax[i,frame], skymean[i,frame], skyerr[i,frame], $
                  imag[i, frame], magerr[i, frame], l_bad[i]
            endif else begin
               ; Old log file format
               printf, logfile_lu, format=fmt1, $
                  fname, objnum, xcen1, ycen1, exptime, gain, l_radius[i], $
                  l_sky1[i], l_sky2[i], imag[i, frame], magerr[i, frame]
               printf, logfile_lu, format=fmt2, $
                  xcen, ycen, skyphot, tmp_skyerr, photons, photerr, sknfrac
            endelse
         endif

         ; Send the results to the screen if wanted.
         if ( not silent ) or printall then begin
            print, ' '
            print, '    Image ', fname, '  Object ', l_name[i]
            print, 'Serial # ', objnum, $
                   ', ', gain, ' e-/DN', $
                   ', RN=', rdnoise, ' e-', $
                   ', ', exptime, ' sec', $
                   ', obj=', l_radius[i], $
                   format='(A,I4.4, A,G0.2,A, A,G0.2,A,A,G0.4,A, A,G0.1,$)'
            if autosky then begin
               print, ', sky (',l_sky1[i],',',l_sky2[i],')', $
               format='(A,G0.3,A,G0.3,A)'
            endif else begin
               print, ', fixed sky.'
            endelse

            print, '   Position         x        y     Peak counts=', $
            localmax[i,frame], ' (', localmax[i,frame]-tmp_skymean, $
            ' above sky)', format='(A,G0.5,A,G0.5,A)'

            if counts gt 999999.0 then $
               fmt='(A,I10,I9,A,G9.4,A,G7.2,A)' $
            else $
               fmt='(A,I10,I9,A,F9.2,A,F7.2,A)'
            print, '      Input', l_xloc[i], l_yloc[i], $
               '     Object ', counts, ' +/- ', photerr/gain, ' counts', $
               format=fmt

            if not onchip then begin
               print, '          \\\\\\\\ off-chip position ////////'
               goto, continue
            endif

            if photons/exptime gt 999999.0 then $
               fmt='(A,I10,I9,12X,G9.4,A,G7.2,A)' $
            else $
               fmt='(A,I10,I9,12X,F9.2,A,F7.2,A)'
            print, '    Maximum', xmax, ymax, photons/exptime, ' +/- ', $
               photerr/exptime, ' phot/sec', format=fmt

            if tmp_skymean gt 999999.0 then $
               fmt='(A,F10.3,F9.3,A,G9.4,A,G7.2,A)' $
            else $
               fmt='(A,F10.3,F9.3,A,F9.2,A,F7.2,A)'
            print, '   Light #1', xcen, ycen,'     Sky    ', tmp_skymean, $
               ' +/- ', tmp_skyerr, ' counts/pix', format=fmt

            if tmp_skymean/exptime gt 999999.0 then $
               fmt='(A,F10.3,F9.3,12X,G9.4,A,G7.2,A)' $
            else $
               fmt='(A,F10.3,F9.3,12X,F9.2,A,F7.2,A)'
            print, '   Light #2', xcen1, ycen1, tmp_skymean/exptime, $
               ' +/- ', tmp_skyerr/exptime, ' counts/pix/sec', format=fmt

            if tmp_skymean*gain/exptime*area gt 999999.0 then $
               fmt='(A,F5.3,"/",F5.3,"/",F5.3,9X,G9.4,A,G7.2,A)' $
            else $
               fmt='(A,F5.3,"/",F5.3,"/",F5.3,9X,F9.2,A,F7.2,A)'
            print, '  NOISE (O/S/D):', $
               varphotobj/photerr^2, $
               sigphotsky^2/photerr^2, $
               area*rdnoise^2/photerr^2, $
               tmp_skymean*gain/exptime*area, ' +/- ', $
               tmp_skyerr*gain/exptime*area, ' phot/sec', format=fmt

            if keyword_set(in_pscale) then begin
               print,'      FWHM:  ',fwhm[i,frame]/pscale,' pixels, ', $
                                     fwhm[i,frame],' arc-sec', $
                  format='(a,1x,f5.2,1x,a,2x,f5.2,1x,a)'
            endif else begin
               print,'      FWHM:  ',fwhm[i,frame],' pixels', $
                  format='(a,1x,f6.2,1x,a)'
            endelse

            print, ' '

            print, '===> ', imag[i, frame], ' +/- ', magerr[i, frame], $
            ' <===  Instrumental magnitude', format='(A,F8.4,A,F8.4,A)'

            if (nomext ne 0 or zpoint ne 0) and airmass lt 9.0 then begin
               smag = imag[i, frame] - nomext * airmass + zpoint
               print, '===> ', smag, ' +/- ', magerr[i, frame], $
               ' <===  Provisional standard magnitude', format='(A,F8.4,A,F8.4,A)'
            endif

         endif

         l_xcen[i, frame]   = xcen1
         l_ycen[i, frame]   = ycen1
         l_onchip[i, frame] = onchip
   ;      if timeit then cputime,time10

   continue:
   ;      if timeit then begin
   ;         if time2-time1 gt 0. then info.t[0] = info.t[0] + (time2-time1)
   ;         if time3-time2 gt 0. then info.t[1] = info.t[1] + (time3-time2)
   ;         if time4-time3 gt 0. then info.t[2] = info.t[2] + (time4-time3)
   ;         if time5-time4 gt 0. then info.t[3] = info.t[3] + (time5-time4)
   ;         if time6-time5 gt 0. then info.t[0] = info.t[0] + (time6-time5)
   ;         if time7-time6 gt 0. then info.t[4] = info.t[4] + (time7-time6)
   ;         if time8-time7 gt 0. then info.t[0] = info.t[0] + (time8-time7)
   ;         if time9-time8 gt 0. then info.t[5] = info.t[5] + (time9-time8)
   ;         if time10-time9 gt 0. then info.t[6] = info.t[6] + (time10-time9)
   ;      endif
      endfor
      ;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

      ; For simple 2-d image, resurrect original copy.
      if zsize eq 1 then begin
         image = temporary(wimage)
      endif
   endfor

   ;if timeit then cputime,time1
   if not nolog then free_lun, logfile_lu

   ; Return some computations if the keywords are defined.
   if max( [ largest, zsize ] ) eq 1 then begin
      out_xcen    = l_xcen[0,0]
      out_ycen    = l_ycen[0,0]
      out_imag    = imag[0,0]
      out_magerr  = magerr[0,0]
      out_flux    = flux[0,0]
      out_fluxerr = fluxerr[0,0]
      out_fwhm    = fwhm[0,0]
      out_skymean = skymean[0,0]
      out_skysig  = skysig[0,0]
      out_skyerr  = skyerr[0,0]
      out_max     = localmax[0,0]
      out_onchip  = l_onchip[0,0]
      out_sky     = skymean[0,0]
   endif else begin
      if (largest gt 1) and (zsize gt 1) then begin
         out_xcen    = l_xcen
         out_ycen    = l_ycen
         out_imag    = imag
         out_magerr  = magerr
         out_flux    = flux
         out_fluxerr = fluxerr
         out_fwhm    = fwhm
         out_skymean = skymean
         out_skysig  = skysig
         out_skyerr  = skyerr
         out_max     = localmax
         out_onchip  = l_onchip
      endif else begin
         out_xcen    = l_xcen[*]
         out_ycen    = l_ycen[*]
         out_imag    = imag[*]
         out_magerr  = magerr[*]
         out_flux    = flux[*]
         out_fluxerr = fluxerr[*]
         out_fwhm    = fwhm[*]
         out_skymean = skymean[*]
         out_skysig  = skysig[*]
         out_skyerr  = skyerr[*]
         out_max     = localmax[*]
         out_onchip  = l_onchip[*]
      endelse
   endelse
   out_jd = l_jd

   if num_rank eq 0 then begin
      ; Update serial number to point at the next number.
      in_objnum = in_objnum + largest
   endif

   ;if timeit then begin
   ;   cputime,time2
   ;   info.t[0] = info.t[0] + (time2-time1)
   ;   if info.ncalls eq 100 then begin
   ;      alltime = total(info.t)
   ;      print,': ',alltime,info.t[0:6]/alltime*100, $
   ;         format='(a,f6.1,7(1x,f4.1))'
   ;      info.ncalls = 0
   ;      info.t[*] = 0.0
   ;   endif
   ;endif

end
