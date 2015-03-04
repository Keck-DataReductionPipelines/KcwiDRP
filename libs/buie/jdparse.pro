;+
; NAME:
;  jdparse
; PURPOSE:
;  Read and parse a Julian Date from a calendar string
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  jd = jdparse(str)
; INPUTS:
;  str - String (or array) to parse as a date
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  DATETMPL - Date template for reading a date, default is YYYY/MM/DD
;                The number of characters in the string is not important and
;                you can use any delimiter you like.
;  STRICT   - Flag, if set forces jd returned to be zero if any syntax
;                errors are encountered.  If not set (default), the return
;                will contain as much information as was successfully
;                parsed.
;  QUIET    - Flag, if set will suppress printing messages when syntax problems
;                are detected.
;
; OUTPUTS:
;  return value is scalar or vector value of Julian Dates.
;
; KEYWORD OUTPUT PARAMETERS:
;  ERRFLG   - Return to indicate if a syntax error was detected.  The rank
;                and size of this return matches the input string variable.
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
;  2000/11/05, Written by Marc W. Buie, Lowell Observatory
;  2002/08/11, MWB, added code to handle a date with no time.
;  2005/05/06, MWB, improved error messages
;  2005/06/27, MWB, changed to permit T between date and time field,
;                     useful for parsing FITS date strings.
;-
function jdparse,in_str,DATETMPL=in_datetmpl,STRICT=strict,ERRFLG=errflg, $
             QUIET=quiet

   self='JDPARSE: '
   if badpar(strict,[0,1,2,3],0,caller=self+'(STRICT)', $
             default=0) then return,0.0d0
   if badpar(quiet,[0,1,2,3],0,caller=self+'(QUIET)', $
             default=0) then return,0.0d0

   nval = n_elements(in_str)
   jd = dblarr(nval)
   tm = strarr(nval)
   year = intarr(nval)
   month = intarr(nval)
   day = intarr(nval)

   if badpar(in_datetmpl,[0,7],0,caller=self+'(DATETMPL) ', $
                default='YYYY/MM/DD') then return,jd
   datetmpl=strupcase(in_datetmpl)

   ;Determine the date order and delimiter locations in the date template.
   ;Only the letters M, D, and Y are considered token characters. All others
   ;are treated as delimiters.
   x = byte( datetmpl )
   w = where( (x ne 68) and (x ne 77) and (x ne 89), count )
   if count eq 2 then begin
      ;may be good.
      toktmpl = [ strmid( datetmpl, 0, 1 ), $
                  strmid( datetmpl, w[0]+1, 1 ), $
                  strmid( datetmpl, w[1]+1, 1 ) ]
      result=stregex(toktmpl,'[YMD]',/boolean)
      if min(result) eq 0 then begin
         print,'JDPARSE: Warning: invalid DATETMPL string [',datetmpl, $
               '], using default (YMD).'
         toktmpl = ['Y','M','D']
      endif
   endif else begin
      print,'JDPARSE: Warning: unable to decode DATETMPL string [',datetmpl, $
            '], using default (YMD).'
      toktmpl = ['Y','M','D']
   endelse

   ;Parse the date, according to the template.  Leave the time for next pass.
   errflg = replicate(0B,nval)
   for i=0L,n_elements(jd)-1 do begin

      ; Make copy of string
      str = repchr(in_str[i],'T',' ') + ' 0'

      for j=0,2 do begin

         result=stregex(str,'[^0-9]')
         if result gt 0 then begin
            val = fix(strmid(str,0,result))
            str = strmid(str,result+1,999)
            case strmid( toktmpl[j], 0, 1 ) of
               'D' : begin
                  if val ge 1 and val le 31 then begin
                     day[i] = val
                  endif else begin
                     day[i]=1
                     if not quiet then begin
                        print,'JDPARSE: Day out of range! element ',strn(i),' is ',strn(val)
                        print,'JDPARSE: [',in_str[i],']  tok=[',strjoin(toktmpl),']'
                     endif
                     errflg[i]=1B
                  endelse
               end
               'M' : begin
                  if val ge 1 and val le 12 then begin
                     month[i]=val
                  endif else begin
                     month[i]=1
                     if not quiet then begin
                        print,'JDPARSE: Month out of range! element ',strn(i),' is ',strn(val)
                        print,'JDPARSE: [',in_str[i],']  tok=[',strjoin(toktmpl),']'
                     endif
                     errflg[i]=1B
                  endelse
               end
               'Y' : begin
                  year[i]=val
               end
               else : begin
                  print,'JDPARSE: Impossible token! ',strmid( toktmpl[j], 0, 1 )
                  print,'JDPARSE: [',in_str[i],']  tok=[',strjoin(toktmpl),']'
                  errflg[i]=1B
               end
            endcase
         end

      endfor ; run over date fields

      tm[i] = str

   endfor ; loop over all strings

   jdcnv,year,month,day,0.0,jd
   jd = jd + raparse(tm)/!dpi*0.5d0

   if strict then begin
      z=where(errflg,count)
      if count ne 0 then jd[z]=0.0d0
   endif

   z=where(in_str eq '',count)
   if count ne 0 then jd[z]=0.0d0
   if n_elements(jd) eq 1 then jd=jd[0]

   return,jd

end
