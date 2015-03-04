;+
; NAME:
;  showsrc
; PURPOSE:   (one line only)
;  Show image with source lists and astrometric references overlain.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  showsrc,image,srclist,reflist
; INPUTS:
;  imname - String, file name of image to be read (FITS format assumed)
;             You can also provide the actual image to display.
; OPTIONAL INPUT PARAMETERS:
;  srclist - String, name of FITS file containing the source list.  (Optional)
;  reflist - String, name of raw file containing the reference list. (Optional)
; KEYWORD INPUT PARAMETERS:
;  BINFAC - binning factor to use for display.  Default is the smallest binning
;              needed so that the binned image fits on the display.
;  LOWSIG - low end of the image stretch range in units of sigma of the sky
;              background noise.  The min will be this many sigma below sky.
;              The default is -3 sigma (you must provide a signed number).
;  HISIG  - high end of the image stretch range in units of sigma of the sky
;              background noise.  The max will be this many sigma above sky.
;              The default is 5 sigma.
;  NEGATIVE - Flag, if set will invert the display range (ie., a negative)
;  WINDOWNUM - direct graphics window number to use for display, default=7
;  OUTFILE - Name of file to save a screen shot of display image (PNG format).
;  FORCESTRETCH - Flag, if set will set the stretch to a specific DN range
;                   as given by LOWVAL and HIVAL
;  SRCMAGLIM - faint magnitude limit of the source file to plot.  Default=99
;                 Note that this is using the instrumental magnitudes found
;                 in the source file.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;    These keywords are pure output if FORCESTRETCH is not set.  If it
;      is set then these carry the stretch values and must be provided.
;  LOWVAL - scaling value for the lower boundery of the image stretch
;  HIVAL  - scaling value for the upper boundery of the image stretch
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2007/11/06
;  2008/10/31, MWB, changed to use rdref
;  2009/06/23, MWB, added LOWSIG and HISIG keywords
;  2009/12/18, MWB, fixed bug, new keywords weren't connected to anything.
;                     Also add option to input actual array to show.
;  2009/12/21, MWB, added FORCESTRETCH, LOWVAL, and HIVAL output keywords
;  2010/01/11, MWB, fixed a bug with the BINFAC default value.  Also added
;                     the SRCMAGLIM keyword.
;-
pro showsrc,imname,srclist,reflist,OUTFILE=outfile,BINFAC=binfac, $
       WINDOWNUM=windownum,LOWSIG=lowsig,HISIG=hisig,NEGATIVE=negative, $
       LOWVAL=lowval,HIVAL=hival,FORCESTRETCH=forcestretch,SRCMAGLIM=srcmaglim

   self='showsrc: '
   if badpar(imname,[2,3,4,5,7],[0,2],caller=self+'(imname) ',type=imtype) then return
   if badpar(srclist,[0,7],0,caller=self+'(srclist) ',default='') then return
   if badpar(reflist,[0,7],0,caller=self+'(reflist) ',default='') then return
   if badpar(outfile,[0,7],0,caller=self+'(OUTFILE) ',default='') then return
   if badpar(windownum,[0,2,3],0,caller=self+'(WINDOWNUM) ',default=7) then return
   if badpar(negative,[0,1,2,3],0,caller=self+'(NEGATIVE) ',default=0) then return
   if badpar(lowsig,[0,2,3,4,5],0,caller=self+'(LOWSIG) ',default=-3.0) then return
   if badpar(hisig,[0,2,3,4,5],0,caller=self+'(HISIG) ',default=5.0) then return
   if badpar(srcmaglim,[0,2,3,4,5],0,caller=self+'(SRCMAGLIM) ',default=99.0) then return

   if badpar(forcestretch,[0,1,2,3],0,caller=self+'(FORCESTRETCH) ',default=0) then return

   if forcestretch then begin
      if badpar(lowval,[2,3,4,5],0,caller=self+'(LOWVAL) ') then return
      if badpar(hival,[2,3,4,5],0,caller=self+'(HIVAL) ') then return
   endif

   if srclist ne '' and not exists(srclist) then begin
      print,self,'Source list ',srclist,' does not exist'
      return
   endif

   if reflist ne '' and not exists(reflist) then begin
      print,self,'Astrometry reference list ',reflist,' does not exist'
      return
   endif

   if imtype eq 7 then begin
      if not exists(imname) then begin
         print,self,'Image ',imname,' does not exist'
         return
      endif

      image = readfits(imname)
   endif else begin
      image = imname
   endelse

   sz=size(image,/dimen)

   ; figure out the scaling factor for the image.  This is to handle cases
   ;  where the image to be displayed is much larger than the display.
   device,get_screen_size=wsz
   if sz[0] le wsz[0] and sz[1] le wsz[1] then begin
      sf = 1
   endif else begin
      sf = max(ceil(float(sz)/wsz))
   endelse
   if badpar(binfac,[0,2,3],0,caller=self+'(BINFAC) ',default=sf) then return
   sf = binfac

   xsm = sz[0] / sf
   ysm = sz[1] / sf
   bim = rebin(float(image[0:xsm*sf-1,0:ysm*sf-1]),xsm,ysm)
   if not forcestretch then begin
      skysclim,bim,lowval,hival,meanval,sigma,npts=30000
      lowval = max([meanval+lowsig*sigma,min(bim)])
      hival  = min([meanval+hisig*sigma,max(bim)])
   endif
   setwin,windownum,xsize=xsm,ysize=ysm
   if negative then $
      tv,255B-bytscl(bim,min=lowval,max=hival,top=255) $
   else $
      tv,bytscl(bim,min=lowval,max=hival,top=255)
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,xsm*sf-1],yr=[0,ysm*sf-1],xstyle=5,ystyle=5,/noerase

   setusym,-1

   if srclist ne '' then begin
      src = readfits(srclist)
      xsrc = src[*,0]
      ysrc = src[*,1]
      magi = src[*,3]
      z=where(magi le srcmaglim,count)
      if count ne 0 then $
         plots,xsrc[z],ysrc[z],psym=4,color='00ff00'xl,symsize=2
   endif

   if reflist ne '' then begin
      rdref,reflist,ref,referr
      if referr eq 0 then begin
         if ref.nstars gt 0 then $
            plots,ref.xpos,ref.ypos,psym=8,color='00ffff'xl,symsize=3
      endif
   endif

   setusym,1

   if outfile ne '' then begin
      tvgrab,outfile,windownum,/png
   endif

end
