;+
; NAME:
;    itool_pplod
; PURPOSE: (one line)
;    Load itool photometry parameters from a file.
; DESCRIPTION:
;
; CATEGORY:
;    Widgets
; CALLING SEQUENCE:
;    itool_pplod, ph_parms
; INPUTS:
;    ph_parms : The photometry parameters structure (See itool.pro).
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    ERROR_STR    = A string variable in which an error string may be returned.
;                   A null string is returned, if no errors were encountered.
;
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
;    2004, Apr, DWL. Adapted from it_pplod.pro. Added some GUI message
; dialogs.
;    2006/5/22,  Peter L. Collins, Lowell Observatory.
;               Added CCD readout noise parameter (rdnoise).
;-
; ------------------------------------------------------------------------------
; Procedure itool_pplod
; ------------------------------------------------------------------------------
pro itool_pplod, ph_parms, ERROR_STR=error_str

   ppmsprmt,ph_parms.parmfile,/GUI

   get_lun, lu
   err = 0
   openr, lu, ph_parms.parmfile, ERROR=err
   error_str = ''

   if err NE 0 then begin
      free_lun, lu
      error_str = !error_state.msg
      return
   endif

   ph_parms.edtflg = 0

   vid = ' '
   readf, lu, vid

   if vid eq ph_parms.parmfilever then begin
      temp = ''
      readf, lu, temp
      ph_parms.logfile = temp
      p = strpos(temp, ' ')

      if p GT 0 then begin
         ph_parms.logfile = strmid(temp, 0, p)
      endif

      readf, lu, temp
      ph_parms.radius = temp
      readf, lu, temp
      ph_parms.sky1 = temp
      readf, lu, temp
      ph_parms.sky2 = temp
      readf, lu, temp
      ph_parms.boxmrad = temp
      readf, lu, temp
      ph_parms.gain = temp
      readf, lu, temp
      ph_parms.platescale = temp
      readf, lu, temp
      ph_parms.rdnoise = temp
      readf, lu, temp
      ph_parms.exact = temp
      readf, lu, temp
      ph_parms.nomext = temp
      readf, lu, temp
      ph_parms.zpoint = temp

      print, 'Photometry parameters loaded from file ' + $
         ph_parms.parmfile
   endif else begin
      error_str = 'Unknown photometry parameters file format ' + vid
   endelse

   free_lun, lu
   return
end
