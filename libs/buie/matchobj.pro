;+
; NAME:
;     matchobj
; PURPOSE: (one line)
;     Find matches for non-standard names in a correspondence list.
; DESCRIPTION:
;
;     This program compares the items in the input list against the known
;     names that are in a correspondence list and returns a matched list
;     for the input.
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;     matchobj,proper,informal,rawname,goodname
; INPUTS:
;     proper   - String array of proper ephemeris names.
;     informal - String array of informal object names.
;     rawname  - String array of informal names to match up.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;     goodname - String array of proper ephemeris names that match rawname.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     3/26/93 - Written by Marc W. Buie, Lowell Observatory.
;     98/4/2, MWB, renamed to avoid conflict with Astron Lib routine.
;-

pro matchobj,proper,informal,rawname,goodname

if n_params() ne 4 then begin
   print,'Usage: matchobj,proper,informal,rawname,goodname'
   return
endif

if badpar(proper,7,[0,1],caller='MATCH: (proper) ',npts=n1) then return
if badpar(informal,7,[0,1],caller='MATCH: (informal) ',npts=n2) then return
if badpar(rawname,7,[0,1],caller='MATCH: (rawname) ',npts=npts) then return

if n1 ne n2 then begin
   print,'MATCH: proper and informal name lists must agree in length.'
   return
endif

goodname = strarr(npts)

for i=0,npts-1 do begin
   idx=where(rawname[i] eq informal,count)
   case count of
      0 : begin
            if strmid(rawname[i],0,3) eq 'NVt' then begin
               goodname[i] = rawname[i]
            endif else begin
               print,'Warning, informal name ['+rawname[i]+'] is undefined in standard list.'
               goodname[i] = 'unk'
            endelse
         end
      1 : goodname[i] = proper[idx[0]]
      else : begin
            print,'Warning, informal name ['+rawname[i]+'] is multiply defined.'
            goodname[i] = proper[idx[0]]
         end
   endcase
endfor

end
