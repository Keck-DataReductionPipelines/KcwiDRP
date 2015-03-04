;+
; NAME:
;	nobname
; PURPOSE: (one line)
;	Replace all blanks in a string with an underscore character.
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;	outstr = nobname(instr)
; INPUTS:
;	instr - Input string to massage.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	Return is the input string with blank converted to underscore.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1991 Feb 26.
;-

function nobname,instr

   temp = byte(instr)

   for i=0,n_elements(temp)-1 do if temp[i] eq byte(32) then temp[i] = byte(95)

   outstr = string(temp)

   return,outstr
end
