;+
; NAME:
;  rdlplast
; PURPOSE:
;  Read a Bowell format asteroid cross reference file (lplast.xrft)
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  rdplast, lookerid, realid, ncount
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  FILE   - Name of file to read (default=lplast.xrft).
;  PATH   - directory where the lplast.xrft file can be found, this defaults
;             to the current directory.  If FILE is specified, it supercedes
;             this keyword since FILE can also contain a path.
;  TRIM   - If set, will trim all leading and trailing blanks from names.
; OUTPUTS:
;  lookerid - This is a list of "poorly" formed or non-standard names.
;  realid   - This is a list of "proper" names.  These are supposed to be
;               names that are more official than the lookerid and provide
;               a cross-reference.
;  ncount   - number of names returned
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  01/04/02 - Written by David B. Tucker, Lowell Observatory
;  20001/08/15, MWB, restructured the calling sequence and defaults.
;-
pro rdlplast, lookerid, realid, ncount, TRIM=trim, PATH=path, SILENT=silent

   if badpar(silent,[0,1,2,3],0,caller='rdlplast: (SILENT) ', $
                    default=0) then return
   if badpar(trim,[0,1,2,3],0,caller='rdlplast: (TRIM) ', $
                    default=0) then return
   if badpar(file,[0,7],0,caller='rdlplast: (FILE) ', $
                    default='lplast.xrft') then return
   if badpar(path,[0,7],0,caller='rdlplast: (PATH) ', $
                    default='') then return

   on_ioerror,badio

   lfile = file
   if lfile eq 'lplast.xrft' and path ne '' then lfile = addslash(path)+lfile

   if not exists(lfile) then begin
	 if not silent then print, 'rdlplast: file ',lfile,' not found'
    ncount=0L
	 return
   endif

   ; count the number of lines
   line=''
   ncount=0L
   openr,lun,lfile,/get_lun
   while not eof(lun) do begin
      readf,lun,line,format='(a1)'
      ncount=ncount+1L
   endwhile

   if ncount gt 0 then begin

      ; rewind the file
      point_lun,lun,0L

      ; Create arrays
      lookerid = strarr(ncount)
      realid   = strarr(ncount)

      a=''
      b=''

      ; read the file and put data into the arrays.
      for i=0L,ncount-1L do begin
         readf,lun,a,b,format='(a8,1x,a30)'
         lookerid[i] = a
         realid[i]   = b
      endfor

      ; if requested, trim the strings
      if trim then begin
         lookerid = strtrim(lookerid,2)
         realid   = strtrim(realid,2)
      endif

   endif

badio:

   free_lun,lun

end
