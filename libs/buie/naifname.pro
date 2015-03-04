;+
; NAME:
;  naifname
; PURPOSE:
;  Convert an ephem standard name to a common name (NAif name scheme)
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  propername = naifname(standardname)
; INPUTS:
;  standardname = string, standard name code (see EPHEM)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return is the proper string name, or, the input if standard name isn't
;    recognized.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODifICATION HISTORY:
;  1997/02/12 - Written by Marc W. Buie, Lowell Observatory
;  2009/08/19, MWB, added some new codes
;-
function naifname,stdname
   
   if badpar(stdname,7,0,caller='NAifNAME: (stdname) ') then return,''

   name=strlowcase(stdname)

   code = strmid(name,0,1)

   if code eq 'p' then begin
      case strmid(name,1,99) of
         '1': begin
            name = 'Mercury'
            end
         '2': begin
            name = 'Venus'
            end
         '3': begin
            name = 'Earth'
            end
         '301': begin
            name = 'Moon'
            end
         '4': begin
            name = 'Mars'
            end
         '5': begin
            name = 'Jupiter'
            end
         '501': begin
            name = 'Io'
            end
         '502': begin
            name = 'Europa'
            end
         '503': begin
            name = 'Ganymede'
            end
         '504': begin
            name = 'Callisto'
            end
         '6': begin
            name = 'Saturn'
            end
         '7': begin
            name = 'Uranus'
            end
         '8': begin
            name = 'Neptune'
            end
         '801': begin
            name = 'Triton'
            end
         '9': begin
            name = 'Pluto'
            end
         '901': begin
            name = 'Charon'
            end
         '902': begin
            name = 'Nix'
            end
         '903': begin
            name = 'Hydra'
            end
         else: begin
            name = stdname
            endelse
      endcase
   endif else begin
      name=stdname
   endelse

   return, name
end
