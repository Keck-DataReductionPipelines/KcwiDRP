;+
; NAME: 
;  ldcalir
; PURPOSE: 
;  Load calibration information for OSIRIS XD data from a calib file.
; DESCRIPTION:
; CATEGORY:
;  Spectroscopy
; CALLING SEQUENCE:
;  ldcalir,calibfile,calib,valid
; INPUTS:
;  calibfile - Name of calibration file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  CALIBPATH - Path to location to look for calibration files (flat).
;                 The default is the current directory.
;  SILENT    - Flag, if true suppresses all informational output.
;  NOFLAT    - Flag, if true suppresses loading the flat field image.
; OUTPUTS:
;  calib - anonymous structure with calibration information
;           tag contents and usage:
;              height - height of slit in pixels.
;              nor    - number of orders in spectral image.
;              por    - maximum polynomial order for wavlength calibration.
;                         quadratic=2
;              slope  - slope of each order (dy/dx).
;              x1     - starting x value of each order.
;              x2     - ending x value of each order.
;              y0     - y value at bottom of slit for x=0 for each order.
;              flat   - Strip format flat image.
;              flatname- Name of flat field image loaded.
;              cof    - Wavelength calibration poly coeffs [nor,por].
;              npts   - Total number of points (all orders)
;              w      - Wavelength values for each order [npts].
;              o      - Range of points in each order [nor,2].
;  valid - Flag, if true indicates a valid calibration set was loaded.  The
;            contents of calib are unreliable if this flag is false.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/09/14, Written by Marc W. Buie, Lowell Observatory
;  96/05/27, MWB, changed structure organization and added a new, improved
;                 file format.
;  97/12/18, MWB, added NOFLAT keyword
;-
pro ldcalir,calibfile,calib,valid,CALIBPATH=in_calibpath,SILENT=silent, $
            NOFLAT=noflat

if n_params() eq 0 then begin
   print,'ldcalir,calibfile,calib,valid,CALIBPATH=calibpath'
   return
endif

valid=0
calib=0  ; don't return structure if there was an error.

if badpar(calibfile,7,0,caller='LDCALIB: (calibfile) ') then return
if badpar(in_calibpath,[0,7],0,caller='LDCALIB: (path) ',default='./') then return

silent = keyword_set(silent)
noflat = keyword_set(noflat)

calibpath=addslash(in_calibpath)

if not exists(calibpath+calibfile) then begin
   print,'Calibration file ',calibpath+calibfile,' does not exist.  Cannot continue.'
   return
endif

; Pre-declare strings for later use.
version=''

flatname0=''

if not silent then $
   print,'Load calibration info from ',calibpath+calibfile
openr, lun, calibpath+calibfile, /get_lun

;First, read in the calibration information and make sure it is a recognized
;   file version.
readf, lun, version
if version eq 'OSIRIS XD v1.0' then begin
   height = 0
   nor=4
   nt1  = 0
   nt2  = 0
   nt3  = 0
   nt4  = 0
   x1_1=0 & x1_2=0 & x1_3=0 & x1_4=0
   x2_1=0 & x2_2=0 & x2_3=0 & x2_4=0

   readf, lun, height
   readf, lun, x1_1, x2_1, y1_1, slope_1
   readf, lun, x1_2, x2_2, y1_2, slope_2
   readf, lun, x1_3, x2_3, y1_3, slope_3
   readf, lun, x1_4, x2_4, y1_4, slope_4

   x1=[x1_1,x1_2,x1_3,x1_4]
   x2=[x2_1,x2_2,x2_3,x2_4]
   y0=[y1_1,y1_2,y1_3,y1_4]
   slope=[slope_1,slope_2,slope_3,slope_4]

   readf, lun, nt1
   c1 = fltarr(nt1)
   readf, lun, c1
   readf, lun, nt2
   c2 = fltarr(nt2)
   readf, lun, c2
   readf, lun, nt3
   c3 = fltarr(nt3)
   readf, lun, c3
   readf, lun, nt4
   c4 = fltarr(nt4)
   readf, lun, c4

   por=max([nt1,nt2,nt3,nt4])
   por=por-1
   cof=fltarr(nor,por+1)
   cof[0,0:n_elements(c1)-1] = c1
   cof[1,0:n_elements(c2)-1] = c2
   cof[2,0:n_elements(c3)-1] = c3
   cof[3,0:n_elements(c4)-1] = c4

endif else if version eq 'OSIRIS XD v2.0' then begin

   height = 0
   nor    = 0
   por    = 0

   readf, lun, height
   readf, lun, nor
   readf, lun, por

   slope  = fltarr(nor)
   x1     = intarr(nor)
   x2     = intarr(nor)
   y0     = fltarr(nor)
   cof    = fltarr(nor,por+1)

   tmp = fltarr(4+por+1)
   for i=0,nor-1 do begin
      readf,lun,tmp
      x1[i] = fix(tmp[0]+0.5)
      x2[i] = fix(tmp[1]+0.5)
      y0[i] = tmp[2]
      slope[i] = tmp[3]
      cof[i,*] = tmp[4:*]
   endfor

endif else begin
   print,'File version tag ['+version+'] is incorrect, cannot proceed.'
   free_lun,lun
   return
endelse

readf, lun, flatname0
IF noflat THEN flatname0='[none]'
flatname=flatname0
relpath,flatname,calibpath

free_lun,lun

; Compute the start and stop index numbers for each order.  The indexing
; is cumulative from the start of the extracted spectral vector with each
; order abutting the previous.
n = x2 - x1 + 1
npts = fix(total(n))
o = intarr(nor,2)
for i=0,nor-1 do begin
   if i eq 0 then begin
      o[i,0] = 0
      o[i,1] = n[i]-1
   endif else begin
      o[i,0] = o[i-1,1]+1
      o[i,1] = o[i,0]+n[i]-1
   endelse
endfor

; Compute the wavelengths of each point.
w = fltarr(npts)
for i=0,nor-1 do w[o[i,0]:o[i,1]]=poly(findgen(n[i]),cof[i,*])

;Load the calibration frame.
if flatname ne '[none]' then begin
   if not exists(flatname) then begin
      print,'Flat frame ',flatname,' could not be found, aborting.'
      return
   endif
   flat=readfits(flatname,silent=silent)
   if not silent then print,'Loading flat  -- ',flatname
   sz = size(flat)
   if (sz[1] ne npts) then begin
      print,'Flat field width, ',sz[1],' does not match spectrum length, aborting.'
      return
   endif
   if (sz[2] ne height) then begin
      print,'Flat field height, ',sz[2],' does not match slit height, aborting.'
      return
   endif
endif else begin
   flat = 1
   if not silent then print,'No flat fielding reqested.'
endelse

calib = { $
            height: height, $
            nor:    nor,    $
            por:    por,    $
            slope:  slope,  $
            x1:     x1,     $
            x2:     x2,     $
            y0:     y0,     $
            cof:    cof,    $
            npts:   npts,   $
            o:      o,      $
            w:      w,      $
            flatname:flatname0, $
            flat:   flat $
        }

valid=1

end
