;+
; NAME:
;  nextfile
; PURPOSE:
;  Return the next (sorted) filename, from a search path, based upon a pattern.
; DESCRIPTION:
; CATEGORY:
;  General
; CALLING SEQUENCE:
;  result = nextfile(fname [,PATH=path,PATTERN=pattern,/PREVIOUS])
; INPUTS:
;  fname : Current name to use as the reference. Scalar string.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PATH     : Search path. Default is the null string, which implies the
;             current working directory.
;  PATTERN  : The search pattern. Default is '*.*'
;  PREVIOUS : If set, search in reverse instead of forward.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Doug Loucks, Lowell Observatory, 2005/10/25
;-

function nextfile,fname,PATTERN=pattern,PATH=path,PREVIOUS=previous

   self = 'NEXTFILE: '

   if badpar(fname,7,0,caller=self+'(fname) ') then return,''
   if badpar(path,7,0,caller=self+'(PATH) ',default='') then return,''
   if badpar(pattern,[7,0],0,caller=self+'(PATTERN) ',default='*.*') then return,''
   if badpar(previous,[0,1,2,3],0,caller=self+'(PREVIOUS) ',default=0) then return,''

   ; Null path means the current working directory.
   t_path = path
   if t_path eq '' then t_path='.'

   ; Verify existence of the path.
   path_check = file_search(t_path,/mark_directory)

   if path_check[0] eq '' then begin
      ; No such path.
      return, ''
   endif

   ; Get a list for the path and search pattern.
   list = file_search(path_check+pattern)

   if list[0] eq '' then begin
      ; No matches found.
      return, ''
   endif

   basename = file_basename(list)

   zindex = where(basename eq fname, count)

   if count ne 1L then begin
      ; Requested name not found.
      return, ''
   endif

   if previous then begin
      ; Return the previous name in the list, or the null string if the
      ; requested name is at the bottom of the list.
      nextindex = zindex - 1L
      return, (nextindex lt 0L) ? '' : basename[nextindex]
   endif else begin
      ; Return the next name in the list, or the null string if the
      ; requested name is at the top of the list.
      nextindex = zindex + 1L
      return, (nextindex ge n_elements(basename)) ? '' : basename[nextindex]
   endelse
end
