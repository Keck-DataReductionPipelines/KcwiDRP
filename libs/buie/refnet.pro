;+
; NAME:
;  refnet
; PURPOSE:
;  Support routine for calling ``REFNET'' to get stars from master catalogs.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
;
; CALLING SEQUENCE:
;  refnet,ra,dec,width,height,bmaglim,rmaglim,starfile
;
; INPUTS:
;  ra      - Right ascension of center of field for extraction (J2000)
;              input can be in radians (double,float) or
;              a string HH:MM:SS.S  (see RAPARSE for valid syntax).
;  dec     - Declination of center of field for extraction (J2000, radians).
;              input can be in radians (double,float) or
;              a string +DD:MM:SS.S  (see DECPARSE for valid syntax).
;  width   - Width of field to extract (arcsec).
;  height  - Height of field to extract (arcsec).
;  bmaglim - Limiting Blue magnitude to extract.
;  rmaglim - Limiting Red magnitude to extract.
;  starfile- File name for the results of the catalog extraction.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  TWOMASS  - String.  Set to 'J', 'H', or 'K' to request 2MASS point-source
;                   catalog data in that filter.
;
; OUTPUTS:
;  The output is all to the file and contains a list of stars from the
;    USNO A2.0 catalog according to the input constraints.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  Currently this is rather restricted and is just an initial working version.
;  This is hardcoded to a specific location for the catalog and executable.
;  This program will work only on Unix platforms at present.
;  You must define an evironment variable, USNO_CAT, that points to the
;  directory where the data live.  Default is the current directory.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1997/05/08, Written by Marc W. Buie, Lowell Observatory
;  1997/11/24, MWB, Added CATPATH keyword
;  1999/06/22, MWB, changed CATPATH default
;  2002/02/06, MWB, rewrite for new refnet version
;  2005/06/26, MWB, added TWOMASS (2MASS catalog) support
;
;-
pro refnet,ra,dec,width,height,bmaglim,rmaglim,starfile,TWOMASS=twomass

   self='REFNET: '
   if badpar(ra,[4,5,7],0,CALLER=self+'(ra) ',TYPE=ratype) then return
   if badpar(dec,[4,5,7],0,CALLER=self+'(dec) ',TYPE=dectype) then return
   if badpar(twomass,[0,7],0,CALLER=self+'(TWOMASS) ',default='') then return

   if ratype  ne 7 then rastr,ra,1,ras else ras = ra
   if dectype ne 7 then decstr,dec,0,decs else decs = dec

   if twomass eq '' then begin

      spawn,'refnet -ra '+ras+' -dec '+decs+' -w '+string(width)+ $
                    ' -h '+string(height)+' -b '+string(bmaglim)+ $
                    ' -r '+string(rmaglim)+' > '+starfile

   endif else begin

      spawn,'refnet2 -ra '+ras+' -dec '+decs+' -w '+string(width)+ $
                    ' -h '+string(height)+' -m '+string(bmaglim)+ $
                    ' -f '+twomass+' -c > '+starfile

   endelse

end
