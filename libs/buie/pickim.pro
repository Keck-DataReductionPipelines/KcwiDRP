;+
; NAME: 
;  pickim
; PURPOSE: 
;  Interactive program to select best image from cube for SL9 data.
; DESCRIPTION:
;  This is a special purpose routine built for selecting one image from a
;    set of nearly identical images.  The SL9 data was taken as image cubes
;    ~5 images in rapid succesion in the same filter.  This program will
;    display all images raw and after unsharp masking.  Click on the best
;    (either version) to save it (after processing) to disk.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  pickim,root,imnum,
;     PATH=path, OUTPATH=outpath, BIAS=bias, DARK=dark, FLAT=flat, FORCE=force
; INPUTS:
;  root  - string containing the root of the data file names (no period)
;  imnum - vector contining a list of image numbers to process.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PATH    - Directory where data files reside (default = ./)
;  OUTPATH - Directory where output files will be written (default = ./)
;  BIAS    - Bias frame to subtract (default=none)
;  DARK    - Dark frame to subtract (default=none)
;  FLAT    - Flat field to divide into data (default=1)
;  FORCE   - set to sub-frame that should be selected (default=interactive)
;  DELTAT  - Readout time of each frame used to correct time for chosen image
;              from the time of the first frame.  (in seconds), the default
;              (for no good reason) is 1 second.
; OUTPUTS:
;  Output is a FITS file with the chosen image (after processing)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/06 - Marc W. Buie, Lowell Observatory
;  96/06/22, MWB, added OUTPATH and FLAT keywords
;  96/06/27, MWB, added DELTAT
;  96/07/03, MWB, added calib mis-match support
;-
pro pickim,root,imnum, $
       PATH=path, OUTPATH=outpath, BIAS=bias, DARK=dark, FLAT=flat, $
       FORCE=force, DELTAT=deltat

if badpar(root,7,0,CALLER='PICKIM: (root) ') then return
if badpar(imnum,[2,3],[0,1],CALLER='PICKIM: (imnum) ',npts=nframes) then return
if badpar(path,[0,7],0,CALLER='PICKIM: (path) ',default='./') then return
if badpar(outpath,[0,7],0,CALLER='PICKIM: (outpath) ',default='./') then return
if badpar(bias,[0,2,3,4,5],2,CALLER='PICKIM: (bias) ',default=0) then return
if badpar(dark,[0,2,3,4,5],2,CALLER='PICKIM: (dark) ',default=0) then return
if badpar(flat,[0,2,3,4,5],2,CALLER='PICKIM: (flat) ',default=1) then return
if badpar(force,[0,2,3],0,CALLER='PICKIM: (force) ',default=-1) then return
if badpar(deltat,[0,2,3,4,5],0,CALLER='PICKIM: (deltat) ',default=1.0) then return

bsz=size(bias)
dsz=size(dark)
fsz=size(flat)

smofac = 7

for f=0,nframes-1 do begin

   suffix = string(imnum[f],format='(i3.3)')
   inname = addslash(path)+root+'.'+suffix
   outname = addslash(outpath)+root+'m.'+suffix

   if exists(inname) then begin
      image = readfits(inname,hdr,/silent)
      instrument = sxpar(hdr,'INSTRUME')
      if instrument eq 'SNAPSHOT' then begin
         fixsnap,hdr,newhdr
         hdr=newhdr
         exptime=float(sxpar(hdr,'SHUTTER'))
         x1=sxpar(hdr,'CRVAL1')-1-399
         x2=x1+sxpar(hdr,'NAXIS1')-1
         y1=sxpar(hdr,'CRVAL2')-1-399
         y2=y1+sxpar(hdr,'NAXIS2')-1
         if sxpar(hdr,'SEQ-PRGM') ne 1 then $
            print,'WARNING: Lick data: SEQ-PRGM is NOT 1 [',sxpar(hdr,'SEQ-PRGM'),']'
         deltat=float(sxpar(hdr,'CDELT3'))-exptime
      endif else begin
         exptime=float(sxpar(hdr,'EXPTIME'))
         x1=sxpar(hdr,'LEFT1')-1
         x2=sxpar(hdr,'RIGHT1')-1
         y1=sxpar(hdr,'TOP1')-1
         y2=sxpar(hdr,'BOTTOM1')-1
      endelse
      sttime=sxpar(hdr,'STRTTIME')
      dateobs=sxpar(hdr,'DATE-OBS')
      hr=strmid(sttime,0,2)
      mn=strmid(sttime,3,2)
      sc=strmid(sttime,6,10)
      tim = float(hr) + float(mn)/60.0 + float(sc)/3600.0
      sz = size(image)
      if sz[0] ne 3 then begin
         print,'PICKIM:  Error, file ',inname,'is not a 3-d file.'
      endif else begin
         if exptime eq 0.0 then begin
            cfint3=float(sxpar(hdr,'CFINT3'))
            filpos=float(sxpar(hdr,'FILTER'))
            filter=sxpar(hdr,'FILTNAME')
            object=sxpar(hdr,'OBJECT')
            origfile=sxpar(hdr,'ORIGFILE')
            print,' '
            print,'Object =[',object,']    Filter position ', $
               string(filpos,format='(i1)'),'  [',filter,']'
            print,'  original filename ',origfile,'   CFINT3=',cfint3
            print,'  min and max of raw image cube ',minmax(image)
            print,' '
            read,newexp,prompt='Exposure time is ZERO!   Correct value? '
            exptime=newexp
            if exptime lt 0.0 then goto,skipit
         endif

         if force lt 0 then begin

            ; Process image cube
            for j=0,sz[3]-1 do begin
               ; Bias
               if bsz[0] eq 0 then $
                  image[*,*,j] = image[*,*,j] - bias $
               else if bsz[1] eq sz[1] and bsz[2] eq sz[2] then $
                  image[*,*,j] = image[*,*,j] - bias $
               else $
                  image[*,*,j] = image[*,*,j] - bias[x1:x2,y1:y2]

               ; Dark
               if dsz[0] eq 0 and dark[0] ne 0 then $
                  image[*,*,j] = image[*,*,j] - dark*exptime $
               else if dsz[1] eq sz[1] and dsz[2] eq sz[2] then $
                  image[*,*,j] = image[*,*,j] - dark*exptime $
               else if dsz[0] ne 0 then $
                  image[*,*,j] = image[*,*,j] - dark[x1:x2,y1:y2]*exptime

               ; Flat
               if fsz[0] eq 0 then $
                  image[*,*,j] = image[*,*,j]/flat $
               else if fsz[1] eq sz[1] and fsz[2] eq sz[2] then $
                  image[*,*,j] = image[*,*,j]/flat $
               else $
                  image[*,*,j] = image[*,*,j]/flat[x1:x2,y1:y2]
            endfor


            ; Present the data, up to five images at a time for selection.
            ;  at each step, one image is chosen.  If there are more left to
            ;  see the selected image is shown with up to 4 more images.

            ; When this array has a 1 in a slot, then that image is forever
            ;   ignored.
            nouse=intarr(sz[3])

            while fix(total(1-nouse)) gt 1 do begin
               print,fix(total(1-nouse)),' left to select from.'

               ; Select up to the first 5 that are still candidates.
               z=where(nouse eq 0,count)
               if count ge 5 then begin
                  try=z[0:4]
               endif else begin
                  try=z
               endelse

               nsel=n_elements(try)
               setwin,0,xsize=sz[1]*nsel,ysize=sz[2]*2,/show
               grade = fltarr(nsel)
               nx=image[0,0,try[0]]
               for i=0,nsel-1 do begin
                  nx = minmax([minmax(image[*,*,try[i]]),nx])
               endfor
               for i=0,nsel-1 do begin
                  tv,bytscl(image[*,*,try[i]],min=nx[0],max=nx[1],top=!d.n_colors-1),i
                  imqual,image[*,*,try[i]],0.2,smofac,g,usmim,/noprint
                  grade[i] = g
                  tvscl,usmim,i+nsel
               endfor
               best = reverse(sort(grade))
               for i=0,nsel-1 do begin
                  rank = where(i eq best)
                  xyouts,sz[1]*(i+0.5),sz[2], $
                     string(try[i],grade[i]/max(grade),format='(i2," : ",f5.3)'), $
                     /device,align=0.5
                  xyouts,sz[1]*(i+0.5),sz[2]-10, $
                     strcompress(string(max(image[*,*,try[i]])),/remove_all), $
                     /device,align=0.5
               endfor
               plot,[0],[1],/nodata,/noerase,position=[0,0,1,1], $
                  xr=[0,sz[1]*nsel-1],yr=[0,sz[2]*2-1],xstyle=5,ystyle=5
               bd=20
               for i=0,nsel-1 do begin
                  oplot,sz[1]*i+[bd,sz[1]-1-bd,sz[1]-1-bd,bd,bd], $
                                [bd,bd,sz[2]-1-bd,sz[2]-1-bd,bd]
                  oplot,sz[1]*i+[bd,sz[1]-1-bd,sz[1]-1-bd,bd,bd], $
                                [bd,bd,sz[2]-1-bd,sz[2]-1-bd,bd]+sz[2]
               endfor
               print,'Select image (click any mouse button)'
               cursor,x,y,/up,/device
               chosen = x/sz[1]
               print,'Selecting image ',try[chosen]
               nouse[try]=1
               nouse[try[chosen]]=0
            endwhile
            chosen = try[chosen]

         endif else begin
            chosen = force
            ; Bias
            if bsz[0] eq 0 then $
               image[*,*,chosen] = image[*,*,chosen] - bias $
            else if bsz[1] eq sz[1] and bsz[2] eq sz[2] then $
               image[*,*,chosen] = image[*,*,chosen] - bias $
            else $
               image[*,*,chosen] = image[*,*,chosen] - bias[x1:x2,y1:y2]

            ; Dark
            if dsz[0] eq 0 and dark[0] ne 0 then $
               image[*,*,chosen] = image[*,*,chosen] - dark*exptime $
            else if dsz[1] eq sz[1] and dsz[2] eq sz[2] then $
               image[*,*,chosen] = image[*,*,chosen] - dark*exptime $
            else $
               image[*,*,chosen] = image[*,*,chosen] - dark[x1:x2,y1:y2]*exptime

            ; Flat
            if fsz[0] eq 0 then $
               image[*,*,chosen] = image[*,*,chosen]/flat $
            else if fsz[1] eq sz[1] and fsz[2] eq sz[2] then $
               image[*,*,chosen] = image[*,*,chosen]/flat $
            else $
               image[*,*,chosen] = image[*,*,chosen]/flat[x1:x2,y1:y2]
         endelse
         sxaddpar,hdr,'FRAMENUM',chosen, $
            ' Manual selection, image index number from original', $
            BEFORE='OBJECT'
         tim = tim + chosen*(exptime+deltat)/3600.0
         hr = fix(tim)
         mn = fix((tim-hr)*60.0)
         sc = fix((tim-hr-mn/60.0)*3600.0+0.5)
         newtim = string(hr,format='(i2.2)')+':'+ $
                  string(mn,format='(i2.2)')+':'+ $
	               string(sc,format='(i2.2)')
         sxaddpar,hdr,'STRTTIME',newtim
         sxaddpar,hdr,'HISTORY','Image processed by PICKIM.PRO'
         print,'Frame ',imnum[f],' chosen ',chosen,' new time ',newtim, $
               ' old ',sttime
         print,'Writing file ',outname
         writefits,outname,image[*,*,chosen],hdr
      endelse
   endif else begin
      print,'PICKIM:  Error, file ',inname,' could not be found.'
   endelse

skipit:

endfor ; Main for loop for all frames requested.

end
