;+
; NAME:
;	rdphalt
; PURPOSE: (one line)
;	Reads photometry from an alternate format basphote log file.
; DESCRIPTION:
;	This routine reads a photometry log file such as is written by basphote.
;  The file is organized so that a single measurement is recorded on each
;  line.  The file format is a blend of variable and fixed length fields.
;  The argument list for this routine corresponds one-for-one to the columns
;  in the file.  The first three columns are read as variable length fields.
;  The filename of the image file is in the first column and is delimited
;  by a blank (' ').  The second column is the name of the object.  This
;  name must be enclosed within a pair of single quotes.  This is done to
;  permit object names with imbedded blanks.  The third column is the name
;  of the filter.  The rest of the line is read as a fixed format field
;  starting with the julian date.  The format of this portion is:
;     '(d13.5,f9.3,f7.2,3f8.3,i5,2f9.3,f6.2,f8.1,f9.4,f8.4,i2)'
;  Note that the last field (bad, 'i2') does not need to be present (for
;  reasons of backward compatibility).  If that field is absent it is treated
;  as 0.
;
;  Here's a few example lines from such a log file (just remember the leading
;  semicolon has been added for this documentation and does not appear in the
;  file):
;filename    object  filter   JD           exptime gain rdnoise    rad    sky1    sky2  serial  xpos     ypos    fwhm  maxcnt     sky  skyerr  mag      err     bad
;PHOTFILE v1.1

;010610.020 'SAO 160066' 2 2452070.68978    9.300   2.40  10.23   5.000  15.000  50.100 0000  206.714  410.874  4.93  5714.0     5.66   0.08  13.2072  0.0025 0
;010610.021 'SAO 160066' 1 2452070.69017   37.000   2.40  11.97   5.000  15.000  50.100 0000  205.951  411.056  5.31  4021.3    -1.84   0.09  14.8822  0.0027 0
;
;  Warning!  Do NOT attempt to line up the fields in a file like this by
;  adding extra blanks.  There is only supposed to be a single blank between
;  fields (note that some fields themselves contain blanks in the fixed field
;  areas).  This file is not meant to be usefully read by eye.
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;	rdphalt,logname,filename,obj,fil,jd,exptime,gain,rad,sky1,sky2,serial, $
;	           xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad,rdnoise
; INPUTS:
;	logname  - string containing the photometry log file to read from.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; SKIPLINES   - Initial number of lines to skip in the file. If not
;                  specified start at the beginning of the file.
; NUMLINES    - Number of lines to process, starting at the first
;                  line subsequent to those skipped (if any). If
;                  not specified, process to the end of the file.
; OUTPUTS:
;	filename - Original image file name for each point.
;	obj      - String array containing all object names.
;	fil      - String array with filter codes.
;	jd       - Julian date of mid-time of observation.
;	exptime  - Exposure time in seconds.
;	gain     - Gain of system in photons per count.
;	rad      - Radius of object aperture in pixels.
;	sky1     - Inner radius of sky annulus in pixels.
;	sky2     - Outer radius of sky annulus in pixels.
;	serial   - Serial number for observation.
;	xpos     - vector containing the x position for each point.
;	ypos     - vector containing the y position for each point.
;	fwhm     - Full-width at half maximum in arcseconds.
;	maxcnt   - Original DN of maximum in image.
;  sky      - Sky signal in counts/pixel.
;  skyerr   - Uncertainty of the sky signal.
;	mag      - Instrumental magnitude.
;	err      - Uncertainties of the magnitudes.
;  bad      - flag that marks data as bad.  1 is bad, 0 or missing is good.
;  rdnoise  - readout noise in electrons
; COMMON BLOCKS:
; SIDE EFFECTS:
;  The version change to this program affects the following programs:
;     crmatch, logmanip, onchip, reductor, rephot
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1/19/93 - Marc W. Buie, Lowell Observatory.
;	2/4/93, MWB, Rewrote file read to eliminate array concatenation.
;  96/01/11, MWB, added NUMLINE, SKIPLINE keywords
;  96/10/31, MWB, added bad flag argument and field.
;  2000/06/02, MWB, file format changed to PHOTFILE v1.0 (added sky and skyerr)
;  2006/05/03,  Peter L. Collins, Lowell Observatory
;                  file format changed to PHOTFILE v1.1 (added rdnoise)
;-
pro rdphalt,logname,filename,obj,fil,jd,exptime,gain,rad,sky1,sky2,serial, $
           xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad,rdnoise,$
           SKIPLINE=skipline,NUMLINE=numline

   on_ioerror,badio

   self='RDPHALT'
   if not exists(logname) then begin
      print,self + ': File ',logname,' not found.  Aborting.'
      return
   endif

   if badpar(skipline,[0,2,3],0,caller=self +': (skipline) ', $
                               default=0) then return
   if badpar(numline,[0,2,3],0,caller=self +': (numline) ', $
                               default=-1) then return

   photprmt,logname

   openr,lun,logname,/get_lun

   line=''

   ; Check the file version.
   version=''
   readf,lun,version,format='(a)'

   ; If not current, do nothing.
   latest='PHOTFILE v1.1'
   if version ne latest then begin
      print,'File ',logname,' is of the wrong format.  Version tag:',version
      free_lun,lun
      return
   endif

   ;Read through and count the number of lines.
   nobs=0
   while(not eof(lun)) do begin
      readf,lun,line,format='(a1)'
      nobs=nobs+1
   endwhile
   nlines=nobs

   if skipline gt nobs then begin
      print,self+': skipline is larger than file, aborting.'
      return
   endif

   if numline gt 0 then nobs=numline else nobs=nlines-skipline

   if skipline+nobs gt nlines then begin
      print,self+': file is too short for skipline and numlines'
      return
   endif

   ;Rewind file.
   point_lun,lun,0
   ; Skip the version
   readf,lun,line,format='(a)'

   ;Create the output data vectors
   filename = strarr(nobs)
   obj      = strarr(nobs)
   fil      = strarr(nobs)
   jd       = dblarr(nobs)
   exptime  = fltarr(nobs)
   gain     = fltarr(nobs)
   rdnoise  = fltarr(nobs)
   rad      = fltarr(nobs)
   sky1     = fltarr(nobs)
   sky2     = fltarr(nobs)
   serial   = intarr(nobs)
   xpos     = fltarr(nobs)
   ypos     = fltarr(nobs)
   fwhm     = fltarr(nobs)
   maxcnt   = fltarr(nobs)
   sky      = fltarr(nobs)
   skyerr   = fltarr(nobs)
   mag      = fltarr(nobs)
   err      = fltarr(nobs)
   bad      = intarr(nobs)

   jd0 = 0.0d0

   ; Skip leading lines (maybe)
   for i=0,skipline-1 do begin
      readf,lun,line,format='(a)'
   endfor

   for i=0,nobs-1 do begin

   ; Get the next input line.
      readf,lun,line,format='(a)'

   ; Read the filename, object name, and filter code as string bits.
      filename[i] = gettok(line,' ')
      obj[i]      = gettok(line,"'") ; This is a dummy read to drop the first quote
      obj[i]      = gettok(line,"'")
      fil[i]      = gettok(line,' ')

   ; Read the rest of the data which is all numeric.
      reads,line,format='(d13.5,f9.3,2f7.2,3f8.3,i5,2f9.3,f6.2,f8.1,f9.2,' + $
                        'f7.2,f9.4,f8.4,i2)', $
         jd0,exptime0,gain0,rdnoise0,rad0,sky1_0,sky2_0,serial0, $
         xpos0,ypos0,fwhm0,maxcnt0,sky0,skyerr0,mag0,err0,bad0

      jd[i]       = jd0
      exptime[i]  = exptime0
      gain[i]     = gain0
      rdnoise[i]  = rdnoise0
      rad[i]      = rad0
      sky1[i]     = sky1_0
      sky2[i]     = sky2_0
      serial[i]   = serial0
      xpos[i]     = xpos0
      ypos[i]     = ypos0
      fwhm[i]     = fwhm0
      maxcnt[i]   = maxcnt0
      sky[i]      = sky0
      skyerr[i]   = skyerr0
      mag[i]      = mag0
      err[i]      = err0
      bad[i]      = bad0

   endfor

badio:

   free_lun,lun

end
