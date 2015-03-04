;+
; NAME: 
;  clscan
; PURPOSE:
;  Scan a group of raw OSIRIS XD frames and find rough spectral location.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  clscan,calib,root,start,last,frno,mate,RANGE=range,PATH=path
; INPUTS:
;  calib- Anonymous structure containing all pertinent calibration
;           information.  This structure is usually loaded beforehand using
;           the routine, "ldcalir"
;  root - string containing the root of the file name (with leading path
;         if desired).  DO NOT include the . between the root and suffix.
;  start  - First spectrum number of sequence to scan.
;  last   - Last spectrum number of sequence to scan, if negative, this number
;             is taken to be the number of spectra to scan.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PATH - optional string that points to the directory containing the data.
;           This information is not used if the root already begins with '/'.
;           If root is not an absolute pathname, then PATH is prepended to
;           root for READ operations.  This path is not used for saving.
;           This allows reading from one directory (possible a read only area)
;           and then saving to the current directory.
;  XRANGE - range of column numbers to average in extracted spectral slice to
;           find rough location of spectrum.  Default is all columns.
;  YRANGE - range of rows that are valid locations.  Default is all rows but the
;           first and last.
;  EXCLUDE- List of frames in the indicated range that should be excluded from
;           consideration.
;
; OUTPUTS:
;  frno - A vector of frame numbers.
;  mate - A vector that gives a suggested frame number from this set for
;           a sky pair match that does not overlap the object.
;  
;  Also, information on location for each spectrum is printed to the console.
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Specifically written for OSIRIS cross-dispersed spectral data.
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/09/27, Written by Marc W. Buie, Lowell Observatory
;  96/05/29, MWB, changed 4th argument from NSPEC to LAST.
;  97/08/07, MWB, removed RANGE (buggy) and added XRANGE and YRANGE
;  97/12/10, MWB, fixed a really nasty bug that corrupt the location for
;                   very high signal-to-noise data.
;  98/07/01, MWB, added code to skip over missing files in sequence.
;-
pro clscan,calib,root,start,last,frno,mate, $
   PATH=path,XRANGE=xrange,YRANGE=yrange,EXCLUDE=exclude

if badpar(calib,8,1,CALLER='clscan (calib) ') then return
if badpar(root,7,0,CALLER='clscan (root) ') then return
if badpar(start,[2,3],0,CALLER='clscan (start) ') then return
if badpar(last,[2,3],0,CALLER='clscan (last) ') then return
if badpar(xrange,[0,2,3],1,CALLER='clscan (XRANGE) ',DEFAULT=[0,calib.npts-1]) then return
if badpar(yrange,[0,2,3],1,CALLER='clscan (YRANGE) ',DEFAULT=[1,calib.height-2]) then return
if badpar(exclude,[0,2,3],[0,1],CALLER='clscan (EXCLUDE) ',DEFAULT=-1) then return
if badpar(path,[0,7],0,caller='clscan: (PATH) ',default='./') then return

; If the first character of the file name root is / then it is assumed the
;   root contains a full path as well and the PATH value is NOT prepended.
;   Otherwise, the file name becomes PATH+ROOT+.+suffix.
if strmid(root,0,1) eq '/' then begin
   newroot = root
endif else begin
   newroot = addslash(path)+root
endelse

; find the number of spectra to examine, last<0 is the number, otherwise range.
if last lt 0 then $
   nspec=abs(last) $
else $
   nspec =  last - start + 1

; Build the index list of frames to do.  This starts with a list of all spectra
;   in range.  Later, these numbers may be set to -1 which indicates frame is
;   not to be looked at.
frno=indgen(nspec)+start

; Scan through the exclude keyword array.  Any numbers in exclude that match
;   a number in the FRNO array will be set to -1 in FRNO.
for i=0,nspec-1 do begin
   z=where(frno[i] eq exclude,count)
   if count ne 0 then frno[i]=-1
endfor

; Check to see that each file not excluded exist.  If it doesn't, exclude it.
for i=0,nspec-1 do begin
   if frno[i] ne -1 then begin
      fname = newroot+'.'+string(frno[i],format='(i3.3)')
      if not exists(fname) then frno[i] = -1
   endif
endfor

; Construct file name

; Sanity check to make sure there is still stuff to do.
z=where(frno ne -1,count)
if count eq 0 then $
   message,'Error ** you have excluded all frames, nothing to do.'

; Collapse the frame list to only those to be done.
frno = frno[z]

; This is the number of spectra to scan.
nsp = n_elements(frno)

; This array will contain the row location of the object for each frame.
loc = intarr(nsp)

; This array will indicate ...
perloc = fltarr(nsp)

; If there is no object
ranper = 1.0/float(calib.height)

; Loop over the frames to process.
for i=0,nsp-1 do begin

   ; Construct file name
   fname = root+'.'+string(frno[i],format='(i3.3)')

   ; Get the strip image of this frame.
   getstrip,calib,newroot,frno[i],strip

   ; Scan the strip sub-region for the local maximum in each column (x)
   ;   at this point xpos and ypos are relative to the sub-region.
   maxloc,strip[xrange[0]:xrange[1],yrange[0]:yrange[1]],xpos,ypos,/x

   ; Compute a histogram of the y positions returned.  The premise is that
   ;   the object is basically in the same spot and is located where the
   ;   histogram is seen to peak.  As a check, compute the fraction of the
   ;   columns that contain this peak.  If this fraction is near ranper then
   ;   it's likely that there is no object.
   hist=histogram(ypos,min=0,max=yrange[1]-yrange[0])
   hloc = where(hist eq max(hist))
   loc[i] = hloc[0]
   perloc[i] = float(hist[loc[i]])/float(n_elements(ypos))

;   if perloc(i) lt 2.0*ranper then begin
;      sz=size(strip)
;      for j=0,sz(1)-1 do begin
;         med = median(strip(j,*))
;         strip(j,*) = strip(j,*)-med
;      endfor
;      asdf
;   endif
endfor

; Convert from sub-region relative to absolute coordinates.
loc = loc + yrange[0]

minsep=9
mate = intarr(nsp)
matedis = intarr(nsp)

; Rule one, find nearest frame number spectrum that is further than MINSEP
;   away from current spectrum.
for i=0,nsp-1 do begin
   fname = root+'.'+string(frno[i],format='(i3.3)')
   ; distance in frame number from current frame
   fdist = abs(frno - frno[i])
   ; distance in pixels from current frame location.
   ydist = abs(loc - loc[i])

   ; find those that are further than MINSEP
   z = where(ydist ge minsep,count)

   ; there are frames futher than minsep, get the closest in frame distance.
   if count ne 0 then begin
      ydistz = ydist[z]
      fdistz = fdist[z]
      specz  = frno[z]
      z = where(fdistz eq min(fdistz))
      mate[i] = specz[z[0]]
      matedis[i] = ydistz[z[0]]

   ; nothing is further than minsep, take the furthest but still closest in frame #.
   endif else begin
      z = where(ydist eq max(ydist))
      ydistz = ydist[z]
      fdistz = fdist[z]
      specz  = frno[z]
      z = where(fdistz eq min(fdistz))
      mate[i] = specz[z[0]]
      matedis[i] = ydistz[z[0]]
   endelse
   if perloc[i] lt 2.5*ranper then $
      comment = 'sig  very faint check location' $
   else $
      comment ='sig'
   print,fname,mate[i],loc[i],matedis[i],perloc[i]/ranper,comment, $
      format='(a," - ",i3.3,4x,"@",i3,2x,i3,3x,f5.1,1x,a)'

endfor

end

