;+
; NAME: 
;  sl9proc
; PURPOSE: 
;  Interactive program to process and crop image for SL9 data.
; DESCRIPTION:
;  This is a special purpose routine built for processing SL9 data image and
;    selecting region of interest to crop.  There is also special code to
;    detect missing exposure times and allow user to enter a new value.
;    (either version) to save it (after processing) to disk.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  sl9proc,root,imnum,
;     PATH=path, OUTPATH=outpath, BIAS=bias, DARK=dark, FLAT=flat, SIZE=size
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
;  SIZE    - Size of array to save, default is 200.
; OUTPUTS:
;  Output is a FITS file with the chosen image (after processing)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  As this is a special purpose routine, there are many possible ways for
;    this program to fail if you try to process general data not from our
;    SL9 data.
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/07/02, Written by Marc W. Buie, Lowell Observatory, cloned from PICKIM
;  96/07/03, MWB, added calib mis-match support
;-
pro sl9proc,root,imnum, $
       PATH=path,OUTPATH=outpath,BIAS=bias,DARK=dark,FLAT=flat,SIZE=arrsize

if badpar(root,7,0,CALLER='PICKIM: (root) ') then return
if badpar(imnum,[2,3],[0,1],CALLER='PICKIM: (imnum) ',npts=nframes) then return
if badpar(path,[0,7],0,CALLER='PICKIM: (path) ',default='./') then return
if badpar(outpath,[0,7],0,CALLER='PICKIM: (outpath) ',default='./') then return
if badpar(bias,[0,2,3,4,5],2,CALLER='PICKIM: (bias) ',default=0) then return
if badpar(dark,[0,2,3,4,5],2,CALLER='PICKIM: (dark) ',default=0) then return
if badpar(flat,[0,2,3,4,5],2,CALLER='PICKIM: (flat) ',default=1) then return
if badpar(arrsize,[0,2,3],0,CALLER='PICKIM: (size) ',default=200) then return

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
      if sz[0] ne 2 then begin
         print,'PICKIM:  Error, file ',inname,' is a 3-d file.'
      endif else begin
         setwin,0,xsize=sz[1],ysize=sz[2],/show

         if exptime eq 0.0 then begin
            tvscl,image
            cfint3=float(sxpar(hdr,'CFINT3'))
            filpos=float(sxpar(hdr,'FILTER'))
            filter=sxpar(hdr,'FILTNAME')
            object=sxpar(hdr,'OBJECT')
            origfile=sxpar(hdr,'ORIGFILE')
            print,' '
            print,'Object =[',object,']    Filter position ', $
               string(filpos,format='(i1)'),'  [',filter,']'
            print,'  original filename ',origfile,'   CFINT3=',cfint3
            print,'  min and max of raw image ',minmax(image)
            print,' '
            read,newexp,prompt='Exposure time is ZERO!   Correct value? '
            exptime=newexp
            if exptime lt 0.0 then goto,skipit
         endif

         ; Process image

         if bsz[0] eq 0 then $
            image = image - bias $
         else if bsz[1] eq sz[1] and bsz[2] eq sz[2] then $
            image = image - bias $
         else $
            image = image - bias[x1:x2,y1:y2]

         if dsz[0] eq 0 then $
            image = image - dark*exptime $
         else if dsz[1] eq sz[1] and dsz[2] eq sz[2] then $
            image = image - dark*exptime $
         else $
            image = image - dark[x1:x2,y1:y2]*exptime

         if fsz[0] eq 0 then $
            image = image/flat $
         else if fsz[1] eq sz[1] and fsz[2] eq sz[2] then $
            image = image/flat $
         else $
            image = image/flat[x1:x2,y1:y2]

         usmim = 3.0*image - 2.5*smooth(image,smofac)

         setwin,1,xsize=sz[1],ysize=sz[2],/show
         tvscl,usmim

         setwin,0,xsize=sz[1],ysize=sz[2],/show

         nx = minmax(image)
         tv,bytscl(image,min=nx[0],max=nx[1],top=!d.n_colors-1)
         x1=-1
         y1=-1
         x2=-1
         y2=-1

         plot,[0],[1],/nodata,/noerase,position=[0,0,1,1], $
            xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5

         if arrsize ne 0 then begin
            print,'Left click on center, middle to abort, right click when ok.'
            !mouse.button=0
            while (!mouse.button ne 2 ) and ((!mouse.button ne 4) or (x1 eq -1)) do begin
               cursor,x,y,/change,/nowait,/device
               if (!mouse.button and 1) ne 0 then begin
                  while (!mouse.button ne 0) do begin
                     cursor,x1,y1,/up,/wait,/device
                  endwhile
                  if x1 ne -1 then $
                     tv,bytscl(image,min=nx[0],max=nx[1],top=!d.n_colors-1)
                  x1=max([x-arrsize/2,0])
                  y1=max([y-arrsize/2,0])
                  x2=min([x1+arrsize-1,sz[1]-1])
                  y2=min([y1+arrsize-1,sz[2]-1])
                  x1=x2-arrsize+1
                  y1=y2-arrsize+1
                  oplot,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1]
                  
               print,x1,':',x2,'  ',y1,':',y2
               endif
            endwhile
            if !mouse.button eq 2 then begin
               print,'ABORT!'
            endif else begin
               print,'Final ',x1,':',x2,'  ',y1,':',y2
               sxaddpar,hdr,'HISTORY','Image processed/cropped by PROCIM.PRO'
               sxaddpar,hdr,'LEFT1',x1+1
               sxaddpar,hdr,'RIGHT1',x2+1
               sxaddpar,hdr,'TOP1',y1+1
               sxaddpar,hdr,'BOTTOM1',y2+1
               sxaddpar,hdr,'CTYPE','float   '
               print,'Writing file ',outname
               writefits,outname,image[x1:x2,y1:y2],hdr
            endelse
         endif else begin
            filpos=float(sxpar(hdr,'FILTER'))
            filter=sxpar(hdr,'FILTNAME')
            object=sxpar(hdr,'OBJECT')
            origfile=sxpar(hdr,'ORIGFILE')
            print,' '
            print,'Object =[',object,']    Filter position ', $
               string(filpos,format='(i1)'),'  [',filter,']  ', $
               dateobs,' ',sttime
            print,'File ',root,'.',suffix,'  original filename ',origfile, $
               '    ',exptime
            print,'  min and max of processed image ',minmax(image)
            print,' '
            print,'Middle to abort, right click to save processed image.'
            !mouse.button=0
            while (!mouse.button ne 2 ) and (!mouse.button ne 4) do $
               cursor,x,y,/change,/nowait,/device
            if !mouse.button eq 4 then begin
               sxaddpar,hdr,'HISTORY','Image processed by PROCIM.PRO'
               print,'Writing file ',outname
               writefits,outname,image,hdr
            endif else begin
               print,'ABORT!  File not saved.'
            endelse
         endelse
      endelse
   endif else begin
      print,'SL9PROC:  Error, file ',inname,' could not be found.'
   endelse

skipit:

endfor ; Main for loop for all frames requested.

end
