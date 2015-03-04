;+
; NAME:
;  mpcdcvt
; PURPOSE:
;  Convert to and from Minor Planet Center packed designation format.
; DESCRIPTION:
;  A Minor Planet Center designation starts with J, or K.  A "normal"
;    desination starts with a number.  Either type can have a leading '('
;    that indicates it is a secondary designation.  This leading character is
;    not stripped off.  If it does not start with J, K, or a numeral, the
;    name is left alone.
;  This routine converts the input into the other form.
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  outstr = mpcdcvt(instr)
; INPUTS:
;  instr - String (or array) to be converted.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  outstr - String (or array) of converted strings.
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
; MODIFICATION HISTORY:
;  2000/05/03, Written by Marc W. Buie, Lowell Observatory
;  2002/03/22, MWB, eliminated a minor nuisance message during conversion.
;  2002/05/28, MWB, fixed a formatting bug for numbered asteroids.
;  2003/08/12, MWB, fixed a bug when converting 2000-2029 designations to
;                     packed format.
;
;-
function mpcdcvt,instr

   if badpar(instr,7,[0,1],CALLER='MPCDCVT: (instr) ') then return,''

   if n_elements(instr) eq 1 then $
      outstr = '' $
   else $
      outstr = strarr(n_elements(instr))

   for i=0,n_elements(instr)-1 do begin
      str = instr[i]
      leading=strmid(str,0,1)
      if leading eq '(' then $
         str = strmid(str,1,999) $
      else $
         leading = ''

      if strn(long('0'+str),length=strlen(str),padchar='0') eq str then begin
         outstr[i] = str
      endif else begin

         c1 = strmid(str,0,1)
         c2 = strmid(str,0,3)
         c3 = strmid(str,strlen(str)-2,2)

         ; Convert from special MPC format to readable format
         if c2 eq 'PLS' or c2 eq 'T1S' or c2 eq 'T2S' or c2 eq 'T3S' then begin
            outstr[i] = strmid(str,3,99) + strmid(str,0,2)

         ; Convert from MPC to readable format
         endif else if c1 eq 'I' or c1 eq 'J' or c1 eq 'K' then begin
            case c1 OF
               'I': begin
                  century = '18'
               end
               'J': begin
                  century = '19'
               end
               'K': begin
                  century = '20'
               end
               else: begin
                  century = 0
               end
            endcase

            outstr[i] = century+strmid(str,1,2)
            outstr[i] = outstr[i]+strmid(str,3,1)
            str=strmid(str,4,999)
            outstr[i] = outstr[i]+strmid(str,strlen(str)-1>0,1)
            str=strmid(str,0,strlen(str)-1)
            digit=strn(strb62(strmid(str,0,1)))
            if digit ne '0' then outstr[i] = outstr[i]+digit
            outstr[i] = outstr[i]+strmid(str,strlen(str)-1>0,1)

         endif $
         else if c3 eq 'T1' or c3 eq 'T2' or c3 eq 'T3' or c3 eq 'PL' then begin
            outstr[i] = c3 + 'S' + strmid(str,0,strlen(str)-2)

         ; Convert from readable to MPC format
         endif else if c1 ge '0' and c1 le '9' then begin
            year = fix(str)
            if year lt 30 then begin
               kill = 2
               century = 'K'
            endif else if year lt 100 then begin
               kill = 2
               century = 'J'
            endif else begin
               kill = 4
               case year/100 OF
                  18: begin
                     century = 'I'
                  end
                  19: begin
                     century = 'J'
                  end
                  20: begin
                     century = 'K'
                  end
                  else: begin
                     century = '?'
                  end
               endcase
               year = year mod 100
            endelse
            str=strmid(str,kill,999)
            outstr[i] = century+strn(year,len=2,padchar='0')
            outstr[i] = outstr[i]+strmid(str,0,1)
            str=strmid(str,1,999)
            tag2=strmid(str,0,1)
            str=strmid(str,1,999)
            tag1=strmid(str,strlen(str)-1>0,1)
            str=strmid(str,0,strlen(str)-1>0)
            if str ne '' then str = strb62(long(str)) 
            outstr[i] = outstr[i]+str+tag1+tag2

         ; Non-standard designation, don't change it.
         endif else begin
            outstr[i] = str
         endelse

      endelse

      outstr[i] = leading+outstr[i]

   endfor

   return,outstr
end
