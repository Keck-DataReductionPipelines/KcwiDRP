;+
; NAME:
;    itool_ppsav
; PURPOSE: (one line)
;    Save itool photometry parameters to a file.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    itool_ppsav, ph_parms
; INPUTS:
;    ph_parms : The photometry parameters structure (See itool.pro).
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    ERROR_STR = A string variable in which an error message may be returned.
;                A null string is returned, if no errors were encountered.
; OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    2004, Apr, DWL. adapted from it_ppsav.pro. Added some GUI message
; dialogs.
;    2006/5/22 Peter L. Collins, Lowell Observatory
;                added the CCD readout noise parameter (rdnoise).
;-
; ------------------------------------------------------------------------------
; Procedure itool_ppsav
; ------------------------------------------------------------------------------
pro itool_ppsav, ph_parms, ERROR_STR=error_str
   fmt1 = '(G0.0,A)'
   error_str = ''

   get_lun, lu
   err = 0
   openw, lu, ph_parms.parmfile, ERROR=err

   if err ne 0 then begin
      free_lun, lu

      error_str = !error_state.msg
      return
   endif

   printf, lu, ph_parms.parmfilever
   printf, lu, ph_parms.logfile, '   Photometry log file name'
   printf, lu, ph_parms.radius,  '   Aperture radius (pixels)', FORMAT=fmt1

   printf, lu, ph_parms.sky1,    '   Inner radius of sky annulus (pixels)', $
      FORMAT=fmt1

   printf, lu, ph_parms.sky2,    '   Outer radius of sky annulus (pixels)', $
      FORMAT=fmt1

   printf, lu, ph_parms.boxmrad, '   Local maximum box radius', FORMAT=fmt1
   printf, lu, ph_parms.gain,    '   Gain of CCD (e-/DN)', FORMAT=fmt1

   printf, lu, ph_parms.platescale,$
      '   Plate scale (arcseconds/pixel)', FORMAT=fmt1

   printf, lu, ph_parms.rdnoise,$
      '   Readout Noise (electrons/pixel)', FORMAT=fmt1

   printf, lu, ph_parms.exact,   '   Exact position flag',      FORMAT=fmt1

   printf, lu, ph_parms.nomext,  '   Nominal extinction (mags/airmass)', $
      FORMAT=fmt1

   printf, lu, ph_parms.zpoint,  '   Zero point', FORMAT=fmt1

   free_lun, lu
   print, 'Photometry parameters saved to file ' + ph_parms.parmfile

   ph_parms.edtflg = 0
   return
end
