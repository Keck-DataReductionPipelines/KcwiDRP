;+
; NAME:
;  mysqlsub
; PURPOSE:   (one line only)
;  Recursive string substitution from mySQL doc table for building documentation
; DESCRIPTION:
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  mysqlsub,lun,instr,outstr
; INPUTS:
;   lun   - The logical unit of the pipe (opened by openmysql).
;   instr - String (scalar or vector) to be scanned.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   outstr - Copy of instr where all occurrences of <<FIELD>> have been
;              replaced by the text contained in the database for FIELD
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   2005/01/20, Written by Marc W. Buie, Lowell Observatory
;-
pro mysqlsub,lun,instr,outstr

   self='mysqlsub: '
   if badpar(lun,[2,3],0,caller=self+'(lun) ') then return
   if badpar(instr,7,[0,1],caller=self+'(instr) ',npts=nstr) then return

   if nstr gt 1 then begin
      for i=0,nstr-1 do begin
         str0 = instr[i]
         mysqlsub,lun,str0,str1
         if i eq 0 then outstr=str1 else outstr=[outstr,str1]
      endfor
   endif else begin
      i0 = strpos(instr,'<<')
      if i0 lt 0 then begin
         outstr = instr[0]
      endif else begin
         i1 = strpos(instr,'>>')
         if i1 lt 0 then begin
            outstr = instr[0]
         endif else begin
            part1 = strmid(instr[0],0,i0)
            part2 = strmid(instr[0],i1+2)
            key = strmid(instr[0],i0+2,i1-i0-2)
            cmd='select info from doc where ' + $
                'tablename is NULL and field='+quote(key)+';'
            mysqlcmd,lun,cmd,result,nlines
            if nlines gt 1 then begin
               key=strsplit(result[1],'\\n',/extract,/regex)
            endif else begin
               key = '[meta string ('+key+') not found]'
            endelse
            if n_elements(key) eq 1 then begin
               str0 = part1+key+part2
            endif else begin
               str0 = [part1,key,part2]
            endelse
            mysqlsub,lun,str0,str1
            outstr = str1
         endelse
      endelse
   endelse

end
