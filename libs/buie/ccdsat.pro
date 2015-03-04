;+
; NAME:
;  ccdsat
; PURPOSE:   (one line only)
;  Find saturation properties of a CCD from one or more images
; DESCRIPTION:
;  This program will scan one or more CCD images and will save the highest
;    count seen at each row.  This will not be particularly interesting for
;    just one image but when fed data from one or more nights a curve will
;    emerge that shows the saturation level of the device as a function of
;    row number.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  ccdsat,fn
; INPUTS:
;  fn - one or more file names to scan, default='' which means to use PATH and
;          PATTERN to get the files
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OUTFILE - Name of output file, default = 'ccdsat.dat'
;  PATH - Path of where to find the source images, default=current directory.
;  PATTERN - File searching pattern, default='*.fits'
;  PEAKS - Array containing previous peak values vs. row number for each
;            amplifier.  Default is to start with a fresh array filled with
;            zeros.
;  FITS - Flag, if set will indicate that the output file should be written
;            in FITS format.  The default is to write an ASCII file which
;            contains the numbers in tabular format, one amplifier to a column.
;  NOPLOT-Flag, if set, the plots will not be generated.
; OUTPUTS:
;  A file, name given in OUTFILE, is written with the peak value seen on each
;    row for each amplifier.
; KEYWORD OUTPUT PARAMETERS:
;  PEAKS - Array containing peak values vs. row number for each amplifier
; COMMON BLOCKS:
;  None.
; SIDE EFFECTS:
;  Plot window 0 is used to show the results of the peak finding process
;    as each image is scanned.  The green curve is the overall peak and the
;    red curve is the peak of each individual image.
; RESTRICTIONS:
;  The images given to this program should all be the same size and must match
;    the size of the PEAKS input array (if provided).  Also, this program won't
;    help if the data don't contain truly saturated pixels somewhere in the
;    images.
;    Any frame whose dimensions do not match the first frame processed will
;    be skipped with a diagnostic message.
;
;  This program is still new and is not fully general yet.  It only works
;    on multi-group FITS images at this point and the plot window is hardcoded
;    to a 4x4 grid of plots that make the most sense for the 16-amp readout
;    data from the CTIO Mosaic camera.
; PROCEDURE:
;  The simplest usage of this program is to give it one night's worth of data.
;    Ex:
;      ccdsat,path='/mydata/night1'
;
;  this will look in /mydata/night1 for any file that ends in .fits  You
;  will see a plot with each image and the saturation level will begin to
;  emerge as each successive image is readout.  If you need to customize the
;  file list, you can do so by providing your own array of file names and
;  bypassing the automatic scanning.
;
;  If you wish to see an aggregate saturation curve that covers multiple
;    nights, use the PEAKS keyword and collect the answer into a variable
;    that will be reused for later nights, ie.:
;
;    ccdsat,path='/mydata/night1',peaks=peaks
;    ccdsat,path='/mydata/night2',peaks=peaks
;    ccdsat,path='/mydata/night3',peaks=peaks        and so on.
;
;  Each run of the program will start where the previous run left off and the
;    final run will contain the cumulative answer for all nights.
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/08/30
;  2003/09/10, MWB, added PEAKS keyword, additional cleanup of code
;  2003/11/13, MWB, added code for simple images (no extensions)
;  2004/9/21, MWB, removed obsolete call to Findfile
;  2007/12/04, Peter L. Collins, Lowell Observatory, added /NOPLOT and
;              test to skip odd sized (sub) frames.
;-
pro ccdsat,fn,OUTFILE=outfile,PATH=path,PATTERN=pattern,PEAKS=peak,FITS=fits, $
              NOPLOT=noplot,RIGHTSIZE=rightsize

   self='CCDSAT: '
   if badpar(fn,[0,7],[0,1],CALLER=self+'(fn) ',default='') then return
   if badpar(outfile,[0,7],0,CALLER=self+'(OUTFILE) ', $
                default='ccdsat.dat') then return
   if badpar(path,[0,7],0,CALLER=self+'(PATH) ',default='') then return
   if badpar(pattern,[0,7],0,CALLER=self+'(PATTERN) ', $
                default='*.fits') then return
   if badpar(fits,[0,1,2,3],0,CALLER=self+'(FITS) ',default=0) then return
   if badpar(noplot,[0,1,2,3],0,CALLER=self+'(NOPLOT) ',default=0) then return
   if badpar(rightsize,[0,1,2,3],[0,1],CALLER=self+'(RIGHTSIZE) ', $
             default=0, npts=nsize) then return

   if fn[0] eq '' then begin
      path=addslash(path)
      fn=file_search(path+pattern,count=nfiles)
      if nfiles eq 0 then begin
         print,self+'No files found with pattern [',path+pattern,']'
         return
      endif
   endif

   !p.multi=[0,4,4]
   first=1
   for j=0,n_elements(fn)-1 do begin

      print,fn[j]
      if not exists(fn[j]) then begin
         print, self, 'File ', fn[j], ' can not be found, aborting.'
         return
      endif

      primehdr=headfits(fn[j],exten=0)

      nextend=sxpar(primehdr,'NEXTEND')

      if nextend eq 0 then begin
         image=readfits(fn[j])
         if first then begin
            sz = size(image,/dimen)
            if nsize eq 2 then begin
               if max(abs(sz-rightsize)) gt 0  then begin
                  print, fn[j], ' is being skipped due to odd size'
                  help, image
                  continue
               endif
            endif
            szps = size(peak,/dimen)
            if n_elements(szps) eq 1 then begin
               print,'init array'
               peak = uintarr(sz[1])
            endif
            first=0
         endif else begin
            sz1 = size(image,/dimen)
            if nsize eq 2 then sztest=rightsize else sztest=sz
            if max(abs(sz1-sztest)) gt 0  then begin
               print, fn[j], ' is being skipped due to odd size'
               help, image
               continue
            endif
         endelse
         maxloc,image,xpos,ypos,/y
         peak0=image[xpos,ypos]
         oldpeak = peak
         tmp = peak0 > oldpeak
         peak = tmp
         if noplot le 0 then begin
            plot,ypos,tmp,charsize=1.5,yr=[0,max(tmp)]
            oplot,ypos,oldpeak,color='00ff00'xl
            oplot,ypos,peak0,color='0000ff'xl
         endif
      endif else begin
         print,strn(nextend),' extensions'
         for i=1,nextend do begin
            image=readfits(fn[j],exten=i)
            if first then begin
               sz = size(image,/dimen)
               szps = size(peak,/dimen)
               if n_elements(szps) eq 1 then begin
                  print,'init array'
                  peak = uintarr(nextend,sz[1])
               endif
               first=0
            endif else begin
               sz1 = size(image,/dimen)
               if max(abs(sz1-sz)) gt 0  then begin
                  print, fn[j], '.', strn(i, FORMAT='(I1)'), $
                         ' is being skipped due to odd size.'
                  help, image
                  continue
               endif
            endelse
            maxloc,image,xpos,ypos,/y
            peak0=image[xpos,ypos]
            oldpeak = peak[i-1,*]
            oldpeak = oldpeak[*]
            tmp = peak0 > oldpeak
            peak[i-1,0:sz[1]-1] = tmp
            if noplot le 0 then begin 
               plot,ypos,tmp,charsize=1.5,yr=[0,max(tmp)]
               oplot,ypos,oldpeak,color='00ff00'xl
               oplot,ypos,peak0,color='0000ff'xl
            endif
         endfor
      endelse

   endfor
   !p.multi=0

   if fits then begin
      writefits,outfile,peak
   endif else begin
      openw,lun,outfile,/get_lun
      if nextend gt 0 then begin
         fmt = '(' + strn(nextend+1) + '(1x,i7))'
         for i=0,n_elements(ypos)-1 do begin
            printf,lun,ypos[i],peak[*,i],format=fmt
         endfor
      endif else begin
         fmt = '(i4,1x,i7)'
         for i=0,n_elements(ypos)-1 do begin
            printf,lun,ypos[i],long(peak[i]+0.5),format=fmt
         endfor
      endelse
      free_lun,lun
   endelse

end
