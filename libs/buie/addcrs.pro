;+
; NAME:
;   addcrs
; PURPOSE:
;   Add synthetic cosmic ray strikes to a CCD image.
; DESCRIPTION:
;   Using an estimate of mean strikes on the image, an actual strike
;   count is created as a Poisson deviate.
;   Based on a normal distribution of maximum pixel intensity,
;   the strikes are then added to the ccd image array.
;   It is possible to save the struck image array as a fits file.
; CATEGORY:
;   CCD data processing
; CALLING SEQUENCE:
;   addcrs,meancrs,mincount,maxcount,img
; INPUTS:
;   meancrs  - Average number of strikes to add per image.
;   mincount - lowest DN for cosmic ray strikes
;   maxcount - highest DN for cosmic ray strikes
;   seed     - seed value for random number generation
;   img      - 2-D image array to be modified
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   OVERSCAN - number of columns in overscan to skip, ie, if img is
;                 m X n,  the region from  [0:m-overscan-1, 0:n-1] 
;                 is where the cosmic rays will be added.
;   FITSFN   - full path name to write FITS file of the modified img.
;   DEBUG    - Flag, if set, turn on debug output.
; OUTPUTS:
;   img         2-D image array to be modified
; KEYWORD OUTPUT PARAMETERS:
;   CRSOUT -  Output, number of cosmic rays added to the image.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006/07/02,  Written, Peter L. Collins, Lowell Observatory  
;                 (as addcrstrikes,pro,  for avgclip testing)
;   2006/07/10,   PLC, extend pixel size of cr strike for visibility in itool.
;                  (keyword PIXEXT)
;   2006/07/11,   changed name and dns values chosen by uniform draw.
;   2006/07/13,   PLC, add keyword CRSOUT.
;   2006/07/14, MWB, final cleanup and addition to library
;-
pro  addcrs,meancrs,mincount,maxcount,seed,img,FITSFN=fn, $
                  OVERSCAN=overscan,DEBUG=debug,CRSOUT=crsout

   self='ADDCRS: '
   if badpar(meancrs,[1,2,3,4,5],0,caller=self+'(meancrs) ') then return
   if badpar(mincount,[2,3,4,5],0,caller=self+'(mincount) ') then return
   if badpar(maxcount,[2,3,4,5],0,caller=self+'(maxcount) ') then return
   if badpar(img,[2,3,4,5],2,caller=self+'(img) ') then return
   if badpar(overscan,[0,1,2,3],0,caller=self+'(OVERSCAN) ', default=0) $
      then return
   if badpar(fn,[0,7],0,caller=self+'(FITSFN) ', default='') then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ', default=0) then return

   sz = size(img,/dimen)
   nx = sz[0]
   ny = sz[1]

   if overscan gt 0 then nx -= overscan

   nstrikes = randomn(seed,POISSON=meancrs)
   crsout = nstrikes
   if debug then print, self, ' mean strikes ', meancrs, ' , ', nstrikes
   if nstrikes eq 0 then return

   strikedns = long(randomu(seed,nstrikes)*(maxcount-mincount)+mincount+0.5)
   x = (fix(randomu(seed,nstrikes)*(nx-1)+0.5) < (nx-1)) > 0
   y = (fix(randomu(seed,nstrikes)*(ny-1)+0.5) < (ny-1)) > 0
   img[x,y] += strikedns

   if debug then begin
      print,nstrikes,' cosmic ray strikes on image ',fn
      for i=0,nstrikes-1 do $
         print,x[i],y[i],img[x[i],y[i]]
   endif

   if fn ne '' then begin
      mkhdr,hdr,img
      writefits,fn,img,hdr
   endif

end
