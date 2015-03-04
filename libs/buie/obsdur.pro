;+
; NAME:
;  obsdur
; PURPOSE:
;  Calculate estimate of total duration of an exposure with PCCD.
; DESCRIPTION:
; CATEGORY:
;  Data Acquisition
; CALLING SEQUENCE:
;  ans=obsdur(filter,exptime,colormode,npat)
; INPUTS:
;  filter    - Filter for observation (or anchor filter for color mode).
;                 This is an integer from 0 to 9.
;  exptime   - Exposure time for indicated filter (in seconds).  Must
;                 be greater than zero.
;  colormode - Character, 'c' means use color sequence, anything else means
;                just this filter.
;  npat      - Number of exposures (or color patterns).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is the estimated time to complete the observation (in seconds).
;
;  NOTE: if any of the input parameters are invalid, this function will
;    return a negative number.  That is your indication of an error.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/27
;  2005/02/07, MWB, updated documentation, added parameter validation
;  2009/05/21, MWB, modified for NASAcam
;-
function obsdur,filter,exptime,colormode,npat

   self='OBSDUR: '
   if badpar(filter,[1,2,3],0,caller=self+'(filter) ') then return,-1
   if badpar(exptime,[1,2,3,4,5],0,caller=self+'(exptime) ') then return,-1
   if badpar(colormode,7,0,caller=self+'(colormode) ') then return,-1
   if badpar(npat,[1,2,3],0,caller=self+'(npat) ') then return,-1

   if filter lt 0 or filter gt 9 then return,-1
   if exptime le 0.0 then return,-1

   ; This table needs to be kept in sync with the PCCD daemon on hermitr
   ;       0    1    2    3    4    5    6    7    8     9
   ;      Open  B    V    R    w    x    p    I    M2    M    
   dwell=[0.6, 2.0, 1.2, 1.0, 1.3, 1.5, 2.5, 1.0, 3.1, 2.5]

   ; Overhead per exposure, this includes 8 sec for the readout and 2 sec
   ;  for the filter change
   overhead = 10.0  ; seconds

   if colormode eq 'c' then begin
      case filter of
         1: begin
            idx=[2,1,3]
            end
         2: begin
            idx=[2,1]
            end
         3: begin
            idx=[3,2]
            end
         4: begin
            idx=[2,1,4]
            end
         5: begin
            idx=[2,1,4,2,5]
            end
         6: begin
            idx=[2,1,4,2,5,6]
            end
         7: begin
            idx=[3,1,2,7]
            end
         8: begin
            idx=[8,9]
            end
         9: begin
            idx=[2,1,9]
            end
         else: begin
            idx=filter
            end
      endcase
      dur = npat * total(exptime*dwell[idx]/dwell[filter] + overhead) + $
               exptime*dwell[idx[0]]/dwell[filter] + overhead
   endif else begin
      dur = npat * (exptime + overhead)
   endelse

   return,dur

end
