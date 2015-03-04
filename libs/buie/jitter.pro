;+
; NAME:
;    jitter
; PURPOSE: (one line)
;    Generate a jitter convolution kernel for one WFPC2 observation.
; DESCRIPTION:
;   Given an observation, read its associated jitter file and create
;     a convolution kernel that matches the tracking/jitter history
;     of the object.  This is setup for either a sidereal object or
;     planetary object.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;    jitter, visitid, imgid
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
; visitid      : If specified, the images corresponding to the visitid
;                will be refit. Otherwise, all images in kbobin.object
;                will be refit based on the setting of the FORCE flag.
; imgid        : If specified, a single image, within the visit specified
;                by visitd, will be refit. Otherwise all images are processed
;                within the visit (or all images in kbobin.object). It is 
;                an error to specify imgid without visitid.
;
; KEYWORD PARAMETERS:
;
; PATH  - Path to directory in which compressed fits files
;         of the form rootname_c0f.fits.gz exist. 
;         Default is /net/frakir/data1/grundy/hst_binaries/c0f.
; SAMPFACT- sub-sampling factor for kernel, default=3
;
; PROGRAM - String that identifies the program ID, default='u9zo' (Will's
;             cycle 16 large program)
;
; OUTPUTS:
; kernel - Output convolution kernel
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
; Uses HST PSF images which may cause the memory and disk PSF caches to be
; populated. Modifies entries in KBOBIN data base table postable. These entries
; reflect the most recent values obtained from the fit,
;
; RESTRICTIONS:
;   Ephemeris is geocentric, not HST-centric
;   Assumes data is from WFPC2
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2008/04/22, Written by Marc W. Buie, Lowell Observatory
;-


pro jitter,visitid,imgid,kernel,PATH=path,PROGRAM=program,SAMPFACT=sampfact, $
      MOVING=moving

   self = 'JITTER: '
   if badpar(imgid,   7, 0, caller=self+'(imgid)')   then return
   if badpar(visitid, 7, 0, caller=self+'(visitid)') then return

   if badpar(path,      [7,0],        0, caller=self + '(PATH)', $
             default='/net/frakir/data1/grundy/hst_binaries/')     then return
   if badpar(program,   [7,0],        0, caller=self + '(PROGRAM)', $
             default='u9zo')     then return
   if badpar(sampfact, [0,2,3],       0, caller=self + '(SAMPFACT)', $
             default=3)        then return
   if badpar(moving, [0,1,2,3],       0, caller=self + '(MOVING)', $
             default=0)        then return

   path = addslash(path)
   c0fpath = path+'c0f/'
   jitpath = path+'jit/'

   if visitid eq '' and imgid ne '' then begin
      print, self + ' visitid must be specified for imgid: ', imgid
      return
   endif

   rootname = program+visitid+imgid

   fndata = rootname+'m_c0f.fits'
   tag ='.gz'
   compress=1

   if not exists(c0fpath+fndata+tag) then begin
      tag = ''
      compress=0
      if not exists(c0fpath+fndata) then begin
         print,fndata,' not found in ',c0fpath
         return
      endif
   endif

;   hdrdata = headfits(c0fpath+fndata+tag,exten=0,compress=compress)

;   dateobs = sxpar(hdrdata,'DATE-OBS')
;   timeobs = sxpar(hdrdata,'TIME-OBS')
;   exptime = sxpar(hdrdata,'EXPTIME')

   hstjit,fndata,579,216, $
      seconds,deltax,deltay,undistortedx,undistortedy, $
      distortedx,distortedy,instr,v2,v3, $
      path=c0fpath,jitpath=jitpath,exptime=exptime ; ,/distort
;   deltax = distortedx-579
;   deltay = distortedy-216

setwin,5
   jitterk,seconds,deltax,deltay,exptime,kernel,subsample=sampfact,moving=moving
;   jitterk,seconds,deltax,deltay,exptime,kernel,subsample=100,moving=moving
   print,fndata,': Exposure time ',exptime,' seconds'

   setwin,0,xsize=500,ysize=500
   xr=[-0.5,0.5]
   yr=[-0.5,0.5]
   plot,deltax,deltay,xr=xr,yr=yr,/iso,xtitle='X (pixels)',ytitle='Y (pixels)'

   setwin,1
   !p.multi=[0,1,2]
   plot,seconds,deltax,xtitle='Time (seconds)',ytitle='X offset (pixels)'
   plot,seconds,deltay,xtitle='Time (seconds)',ytitle='Y offset (pixels)'
   !p.multi=0

   setwin,2,xsize=500,ysize=500
   shade_surf,kernel,charsize=2.0

   setwin,3,xsize=500,ysize=500
   contour,kernel

   setwin,4
   !p.multi=[0,1,2]
   plot,seconds,v2,xtitle='Time (seconds)',ytitle='RA offset (pixels)'
   plot,seconds,v3,xtitle='Time (seconds)',ytitle='Dec offset (pixels)'
   !p.multi=0

;   itool,kernel,/block

end
