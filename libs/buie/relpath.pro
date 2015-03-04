;+
; NAME:
;  relpath
; PURPOSE: 
;  Prepend a path to the file name if file starts with {\tt +}
; DESCRIPTION:
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  repath,filename,path
; INPUTS:
;  filename - Name of file
;  path     - Name of directory
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  filename - path is prepended if filename starts with +, otherwise filename
;             is left unchanged.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  95/08/07 - Written by Marc W. Buie, Lowell Observatory
;-
pro relpath,filename,path
   path=addslash(path)
   if strmid(filename,0,1) eq '+' then $
      filename = path + strmid(filename,1,strlen(filename)-1)
end
