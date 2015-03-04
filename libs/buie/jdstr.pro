;+
; NAME:
;    jdstr
;
; PURPOSE: (one line)
;    Convert Julian date into an ASCII string.
;
; DESCRIPTION:
;
; CATEGORY:
;    Utility
;
; CALLING SEQUENCE:
;    jdstr, jd, places, str
;
; INPUTS:
;    jd     - Julian Date to be converted to a string.  May be a vector, in
;             which case the output is a vector.
;    places - Integer scalar.  Format and resolution of output string.  The
;             units, tens, and hundreds positions of this parameter control
;             the output format.  If this parameter is negative, the sign
;             is applied only to the units place.  Thus the following
;             possibilities:
;      units  = -8  decimal hours.                   HH.hhhhh
;             = -7  nearest hour.                    HH
;             = -6  nearest 30 minutes.              HH:(00,30)
;             = -5  nearest 15 minutes.              HH:(00,15,30,45)
;             = -4  nearest 10 minutes.              HH:M0
;             = -3  nearest 5 minutes.               HH:M(0,5)
;             = -2  nearest minute.                  HH:MM
;             = -1  nearest ten seconds.             HH:MM:S0
;             =  0  nearest second.                  HH:MM:SS
;             =  1  nearest tenth of a second.       HH:MM:SS.s
;             =  2  nearest hundredth of a second.   HH:MM:SS.ss
;             =  3  nearest thousandth of a second.  HH:MM:SS.sss
;
;       tens  =  0  month is numeric (ex 1993/08/06)
;             =  1  month is three-character abbreviated name (ex 1993 Aug 06)
;             =  2  month is full name (ex 1993 August 06)
;
;       hundreds =  0  append time using the units position code.
;                =  1  time is not appended.
;                =  2  time is appended as decimal days, in this case
;                        the units digit specifies how many decimal places to
;                        show on the day.
;                =  3  all separators are omitted (ex  19930806121243)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
; TIMESEP - When the default format puts '/' between time fields, this
;             keyword, if provided, will override the '/'
;
; OUTPUTS:
;    str         - Output string for the converted date.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;   Convert a time as a Julian date into a string.  The time is first
; rounded to the requested digit.  Then the date (after
; rounding) is converted to a string.  Thus, a time of 23:59:59.9
; rounded to the nearest second would actually cause the day to be
; incremented by one and generate a time of 00:00:00.  PLACES indicates
; the reported precision of the string, see INPUTS for posibilities.
;   Calls external routines: caldatm, jdcnv, and rastr.
; MODIFICATION HISTORY:
;  Copyright (C) 1991, by Marc W. Buie. Version dated 91/9/9
;  Ported by Doug Loucks, Lowell Observatory, August, 1993, from the
; C-Language version written by Marc Buie.
;  99/1/13, MWB, added decimal day output option.
;  2003/05/02, MWB, added hundreds=3 option
;  2005/06/27, MWB, fixed bug for hundreds=2 case
;  2009/06/23, MWB, fixed problem with places=202 and vector input
;-
pro jdstr, jd, places, str, TIMESEP=timesep

   if n_params() ne 3 then begin
      print, 'jdstr, jd, places, str'
      return
   endif

   self='JDSTR: '
   if badpar(jd,[4,5],[0,1],CALLER=self+'(jd) ',RANK=jdrank,NPTS=jdsize) then return
   if badpar(places,2,0,CALLER=self+'(places) ') then return

   if badpar(timesep,[0,7],0,caller=self+'(TIMESEP) ',default='/') then return

   ; Initialize the output string to the null string.
   str = ''

   ; Define a few constants.
   rad_per_hour = 0.26179938779914943654D0
   months = [ 'January', 'February', 'March', 'April', 'May', 'June', $
              'July', 'August', 'September', 'October', 'November', 'December' ]

   ; First, decompose the Julian date into its bits and pieces.
   caldatm, jd, year, month, day, hour, minute, second

   ; Get the Julian Date for 0h.
   jdcnv, year, month, day, day*0, jd0

   ; Convert time to radians; Preparation for rastr.
   time = ( jd - jd0 ) * 24.0 * rad_per_hour

   ;   Decode the places argument.  The tens digit contains information for
   ; this routine and must be stripped before sending to rastr.  If the
   ; tens digit is 0, the date is presented in YYYY/MM/DD format, all
   ; numeric.  If the tens digit is 1, the month is written as a 3
   ; character abbreviation, ie. YYYY MMM DD.  If the tens digit is 2,
   ; the full name of the month is written out.  In the case of 1 and
   ; 2, the first character of the month is capitalized.  The hundreds
   ; digit controls whether or not the time is appended to the string.
   ; If 0, the time is appended using the requested ones digit code.
   ; If 1, the time is ignored and only the date is converted by
   ; truncating the time to 0h after rounding to the nearest second.
   ; If 2, the time is appended as a the decimal day and the digit field
   ; specifies how many digits to show.

   ; Strip the sign off
   if places lt 0 then sign = -1 else sign = 1
   pplaces = abs( places )

   ; Strip off the hundreds digit time option code
   time_control = pplaces / 100
   pplaces = pplaces - time_control*100

   ; Strip off the tens digit month format code
   month_type = pplaces / 10

   ; Setup the time format code
   time_type  = pplaces - month_type * 10
   if time_control eq 0 then time_type = sign * time_type

   if time_control eq 1 then begin
      time_type = 0
   endif

   if max( [ month_type lt 0, month_type gt 2 ] ) eq 1 then begin
      print, 'Tens position of PLACES must be 0, 1, or 2.'
      return
   endif

   if max([time_type lt -8, time_type gt 3]) eq 1 and time_control ne 2 then begin
      print, 'Units position of PLACES must be in the range -8 to +3.'
      return
   endif

   ; Convert the time to a string.  Be mindful of the carry "bit".
   case time_control OF
      0: begin
            rastr, time, time_type, time_str, carry
            jd0 = jd0 + carry
            time_str = ' '+time_str
         end
      1: begin
            rastr, time, time_type, time_str, carry
            jd0 = jd0 + carry
            time_str=''
         end
      2: begin
            if time_type eq 0 then time_type=1
            if time_type gt 9 then time_type=9
            time = (jd - jd0)
            tfmt = string(time_type+2,time_type, $
                          format='("(f",i1,".",i1,")")')
            time_str = string( time, format=tfmt )
            z = where(strmid(time_str,0,1) eq '1',count)
            if count ne 0 then jd0[z] = jd0[z] + 1.0d0
            time_str = strmid(time_str,1,99)
         end
      3: begin
            rastr, time, time_type, time_str, carry
            jd0 = jd0 + carry
            for i=0,jdsize-1 do $
               time_str[i]=strjoin(strsplit(time_str[i],':',/extract))
         end
      else: begin
         time_str = 'illegal'
      end
   endcase

   ; Re-convert jd0 back to year, month, day.
   caldatm, jd0, year, month, day, hour, minute, second

   ; Construct the correct month string.
   CASE month_type OF
      0 : begin
         ; Month format is numeric.
         monstr = STRING( month, FORMAT='(I2.2)' )
      END

      1 : begin
         ; Month format is three-character abbreviation.
         monstr = strmid( months[ month-1 ], 0, 3 )
      END

      2 : begin
         ; Month format is full name.
         monstr = months[ month-1 ]
      END

      else : begin
         message, 'Month type-code must be 0, 1, or 2.', /INFO
         return
      END
   ENDCASE

   ; Put all of the pieces together into the output string.
   if time_control eq 3 then begin
      sepchar=''
   endif else begin
      if month_type eq 0 then sepchar=timesep else sepchar=' '
   endelse

   str = string( year, FORMAT='(I4)' ) + sepchar + $
         monstr + sepchar + $
         string( day, FORMAT='(I2.2)' )

   ; Append the time string
   str = str + time_str

end
