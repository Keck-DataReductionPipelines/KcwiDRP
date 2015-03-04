;+
; NAME:
;  cvtsixty
; PURPOSE:   (one line only)
;  General purpose routine for converting between sexigesimal and decimal.
; DESCRIPTION:
;  This routine encapsulates all of the parsing and conversion routines for
;    handling all conversions between sexigeismal strings and double
;    precision forms of angular coordinates.  Floating point is allowed on
;    input but its precision is not really good enough for the taks so any
;    decimal output will always be double precision.  Scalar and vector
;    inputs are permitted.  A string on input is always an indication that
;    the input is sexigesimal and the information is to be converted to
;    decimal format.  If the input is floating point or double then the
;    conversion is to a sexigeismal string.  There are a number of specialized
;    conversion routines in the library but they all call this basic routine
;    setting all the required options.
;
;  A word about the parser: When reading strings, the information is parsed
;    character by character.  This routine ignores leading blanks.  But, a
;    blank is considered to be a terminating character once the parser has
;    seen a non-blank.  Legal field separation characters are ':' and ','.
;    Any and all fields can be floating point though decimal fields only
;    really make sense if that is the last field provided.
;
;  On output you can modify the separator character to anything you like but
;    this routine will not be able to read arbitrary seperators.
;
;  Enough options are provided to permit meaningful range checking on the
;    input values for different types of angular quantities.  The quantities
;    envisioned for this routine include Right Ascension [0-24h), Declination
;    [-90,90], Hour angle [12W,12E), Longitude [0,360) to name a few.
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  cvtsixty,input,minvalue,maxvalue,wrap,signchar,output
; INPUTS:
;  input - scalar or vector.  String          ==> sexigesimal to decimal
;                             Double or float ==> decimal to sexigesimal
;  minvalue - scalar, minimum allowed value for coordinate in decimal form.
;  maxvalue - scalar, maximum allowed value for coordinates in decimal form.
;  signchar - two-element string giving the characters to be used to indicate
;               the sign of the number on reading or writing.
;                 signchar[0] = Character for positive numbers
;                 signchar[1] = Character for negative numbers
;  wrap     - Flag, if set indicates the coordinates wrap across from max
;                to min.  This further indicates the range is inclusive
;                of minvalue and exclusive of maxvalue.  If not set, the
;                range is inclusive of minvalue and maxvalue and an
;                out-of-range is an error.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;
;  HOURS    - Flag, if set, indicates the sexigesimal string is in units
;                of hours.  The default is degrees.
;  DEGREES  - Flag, if set, indicates the decimal value is in degrees.
;                The default is radians.
;
;  Keywords are grouped by the input data type since that dictates the
;    operation that ensues:
;
;  Double or float input:
;
;    PLACES - Resolution of output string (default=0)
;             = -8     Pure decimal output               XX.xxxxx
;             = -7     nearest top level unit            XX
;             = -6     nearest 30 minutes.               XX:(00,30)
;             = -5     nearest 15 minutes.               XX:(00,15,30,45)
;             = -4     nearest 10 minutes.               XX:M0
;             = -3     nearest 5 minutes.                XX:M(0,5)
;             = -2     nearest minute.                   XX:MM
;             = -1     nearest ten seconds.              XX:MM:S0
;             =  0     nearest second.                   XX:MM:SS
;             =  1     nearest tenth of a second.        XX:MM:SS.s
;             =  2     nearest hundredth of a second.    XX:MM:SS.ss
;             =  3     nearest thousandth of a second.   XX:MM:SS.sss
;             =  4     nearest thousandth of a second.   XX:MM:SS.ssss
;
;    SEPCHAR - Scalar string of the separator character to use between
;                fields on output.  Default=':'.  Note that if you set
;                this to something other than ':', ',' or ' ' the output cannot
;                be parsed by this routine as input.
;
; OUTPUTS:
;  output - length matches input.  Double if input is string, string if input
;              is double or float.
; KEYWORD OUTPUT PARAMETERS:
;  CARRY  - Used on decimal to sexigesimal string conversion only.  This is
;              a flag that will incidate if the number rolled over the cut
;              in the coordinate (if a cut exists).
;  ERROR  - Flag (length matches input).  Set if there was a conversion error.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;
;  Examples:
;  Right ascension, radians in decimal form, hours in sexigesimal
;    cvtsixty,'15:43:21.1',0.0d0,2.0d0*!dpi,1,['',''],output,/HOURS
;  Right ascension, degrees in decimal form, hours in sexigesimal
;    cvtsixty,'15:43:21.1',0.0d0,360.0d0,1,['',''],output,/HOURS,/DEGREES
;  Declination, radians in decimal form, degrees in sexigesimal
;    cvtsixty,'-45:28:12',-0.5d0*!dpi,0.5d0*!dpi,0,['+','-'],output
;  Hour angle, radians in decimal form, hours in sexigesimal
;    cvtsixty,'W02:28',-1.0d0*!dpi,1.0d0*!dpi,0,['W','E'],output,/HOURS
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/04/24
;    This is created from a large suite of independent but very similar
;    routines with the intent of removing all duplicate code and having
;    the independent routines call this master routine.
;  2010/04/18, MWB, modified internal loop variable to handle long input
;    vectors.
;-
pro cvtsixty_adjust0, xx, mm, carry, maxval, wrap
   z = where(mm eq 60,count)
   if count gt 0 then begin
      xx[z] = xx[z] + 1
      mm[z] = 0
      if wrap then begin
         zz = where(xx[z] eq maxval,count)
         if count gt 0 then begin
            xx[z[zz]] = 0
            carry[z[zz]] = 1
         endif
      endif
   endif
end

pro cvtsixty_adjust1, xx, mm, ss, carry, maxval, wrap
   z = where(ss ge 60.0,count)
   if count gt 0 then begin
      mm[z] = mm[z] + 1
      ss[z] =  0
      zz = where( mm[z] eq 60, count )
      if count gt 0 then begin
         mm[z[zz]] = 0
         xx[z[zz]] = xx[z[zz]] + 1
         if wrap then begin
            zzz = where( xx[z[zz]] eq maxval, count )
            if count gt 0 then begin
               xx[z[zz[zzz]]] = 0
               carry[z[zz[zzz]]] = 1
            endif
         endif
      endif
   endif
end

pro cvtsixty,input,minvalue,maxvalue,wrap,signchar,output, $
       CARRY=carry, ERROR=error, PLACES=places, SEPCHAR=sepchar, HOURS=hours, $
       DEGREES=degrees

   self='CVTSIXTY: '

   if badpar(input,[4,5,7],[0,1],caller=self+'(input) ', $
                type=intype,npts=nvals) then return
   if badpar(minvalue,5,0,caller=self+'(minvalue) ') then return
   if badpar(maxvalue,5,0,caller=self+'(maxvalue) ') then return
   if badpar(wrap,[1,2,3],0,caller=self+'(wrap) ') then return
   if badpar(signchar,7,1,caller=self+'(signchar) ',npts=nsign) then return

   if badpar(sepchar,[0,7],0,caller=self+'(SEPCHAR) ',default=':') then return
   if badpar(places,[0,2,3],0,caller=self+'(PLACES) ',default=0) then return
   if badpar(hours,[0,1,2,3],0,caller=self+'(HOURS) ',default=0) then return
   if badpar(degrees,[0,1,2,3],0,caller=self+'(DEGREES) ',default=0) then return

   if nsign ne 2 then begin
      print,self,'signchar input variable must be a two-element string. Aborting.'
      return
   endif

   ;  Setup the conversion factor from the most significant field of the
   ;    sexigesimal string to the decimal form of the value.  Equals the
   ;    units of the sexigesimal form divided by the decimal form.
   if hours then numerator = 24.0d0 else numerator = 360.0d0
   if degrees then denominator = 360.0d0 else denominator = 2.0d0*!dpi
   factor = numerator / denominator

   error  = bytarr(nvals)

   ; Input is sexigesimal, must convert to decimal
   if intype eq 7 then begin

      output = dblarr(nvals)
      maxval = maxvalue*factor
      minval = minvalue*factor

      for i=0L,nvals-1 do begin

         ; Make a working copy of the input, trim leading and trailing blanks
         ;   at the same time.
         wstr = strtrim(strcompress(input[i]),2)

         ; first convert separator tokens to blank
         wstr = repchar(wstr,':',' ')
         wstr = repchar(wstr,',',' ')

         ; Extract sign information from string
         valsign = strmid(wstr,0,1)
         sign=1
         if valsign eq signchar[0] then begin
            wstr = strmid(wstr,1,99)
         endif
         if valsign eq signchar[1] then begin
            wstr = strmid(wstr,1,99)
            sign = -1
         endif

         ; split into fields by blanks
         fields=double(strsplit(wstr,' ',/extract))
         nfields=n_elements(fields)
         if nfields eq 1 then begin
            fields = [fields,0.0,0.0]
         endif else if nfields eq 2 then begin
            fields = [fields[0],fields[1],0.0]
         endif

         if fields[1] lt 0.0d0 or fields[1] gt 60.0d0 then begin
            error[i] = 1
            continue
         endif
         if fields[2] lt 0.0d0 or fields[2] gt 60.0d0 then begin
            error[i] = 1
            continue
         endif

         newval = sign * (fields[0] + fields[1]/60.0d0 + fields[2]/3600.0d0)

         if wrap then begin
            if newval ge maxval then $
               newval = newval - (maxval-minval)
            if newval lt minval then $
               newval = newval + (maxval-minval)
            if newval lt minval or newval ge maxval then error[i]=1
         endif else begin
            if newval lt minval or newval gt maxval then error[i]=1
         endelse

         if not error[i] then output[i] = newval

      endfor

      output = output/factor

   ; Input is decimal, must convert to sexigesimal
   endif else begin

      carry  = intarr(nvals)
      if nvals gt 1 then $
         sign   = replicate(signchar[0],nvals) $
      else $
         sign   = signchar[0]
      maxval = round(maxvalue*factor)
      minval = round(minvalue*factor)
      if maxval ge 100 then digits=3 else digits=2
      fmt0 = '(i'+strn(digits)+'.'+strn(digits)+')'
      fmt1 = '(i2.2)'

      ; convert to output units
      oval = input * factor

      ; strip off the sign
      z=where(oval lt 0.0d0,count)
      if count gt 0 then begin
         sign[z]=signchar[1]
         oval[z] =abs(oval[z])
      endif

      val = oval
      field0 = long(val)
      val = (val-field0)*60.0d0

      field1 = long(val)
      field2 = (val-field1)*60.0d0

      case places of

         -8 : begin  ; XX.xxxxx, Decimal hours/degrees
            field0 = double(long(oval * 100000.0d0 + 0.5d0))/100000.0d0
            field1 = field0 - long(field0)
            field0 = long(field0)
            if wrap then begin
               z=where(field0 eq maxval,count)
               if count gt 0 then begin
                  field0[z] = field0[z]-(maxval-minval)
                  carry[z]  = 1
               endif
            endif
            output = sign + string(field0,format=fmt0) + $
                     strmid(string(field1,format='(f7.5)'),1,6)
         end

         -7 : begin  ; XX, Nearest hour/degree.
            z=where(double(field1)+field2/60.0d0 ge 30.0d0,count)
            if count gt 0 then field0[z]=field0[z]+1
            if wrap then begin
               z=where(field0 eq maxval,count)
               if count gt 0 then begin
                  field0[z] = field0[z]-(maxval-minval)
                  carry[z]  = 1
               endif
            endif
            output = sign + string(field0,format=fmt0)
         end

         -6 : begin  ; XX:(00,30), Nearest 30 minutes.
            field1 = nint((field1+field2/60.0d0)/30.0d0, /long) * 30.0d0
            cvtsixty_adjust0,field0,field1,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)
         end

         -5 : begin  ; XX:(00,15,30,45), Nearest 15 minutes.
            field1 = nint((field1+field2/60.0d0)/15.0d0, /long) * 15.0d0
            cvtsixty_adjust0,field0,field1,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)
         end

         -4 : begin  ; XX:M0, Nearest 10 minutes.
            field1 = nint((field1+field2/60.0d0)/10.0d0, /long) * 10.0d0
            cvtsixty_adjust0,field0,field1,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)
         end

         -3 : begin  ; XX:M(0,5), Nearest 5 minutes.
            field1 = nint((field1+field2/60.0d0)/5.0d0, /long) * 5.0d0
            cvtsixty_adjust0,field0,field1,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)
         end

         -2 : begin  ; XX:MM, Nearest minute.
            field1 = nint(field1+field2/60.0d0, /long)
            cvtsixty_adjust0,field0,field1,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)
         end

         -1 : begin  ; XX:MM:S0, Nearest 10 seconds.
            field2 = nint(field2/10.0d0, /long) * 10.0d0
            cvtsixty_adjust1,field0,field1,field2,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)+ $
                     sepchar+string(field2,format=fmt1)
         end

         0 : begin   ; XX:MM:SS, Nearest second.
            field2 = nint(field2,/long)
            cvtsixty_adjust1,field0,field1,field2,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)+ $
                     sepchar+string(field2,format=fmt1)
         end

         1 : begin   ; XX:MM:SS.s, Nearest tenth of a second.

            field2 = nint(field2/0.1d0,/long)*0.1d0
            ; Save the fractional part of the seconds.
            fss = field2-long(field2)
            cvtsixty_adjust1,field0,field1,field2,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)+ $
                     sepchar+string(field2,format=fmt1)+ $
                     strmid(string(fss,format='(F3.1)'),1,2)
         end

         2 : begin   ; XX:MM:SS.ss, Nearest hundredth of a second.
            field2 = nint(field2/0.01d0,/long)*0.01d0
            ; Save the fractional part of the seconds.
            fss = field2-long(field2)
            cvtsixty_adjust1,field0,field1,field2,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)+ $
                     sepchar+string(field2,format=fmt1)+ $
                     strmid(string(fss,format='(F4.2)'),1,3)
         end

         3 : begin   ; XX:MM:SS.ssx, Nearest thousandth of a second.
            field2 = nint(field2/0.001d0,/long)*0.001d0
            ; Save the fractional part of the seconds.
            fss = field2-long(field2)
            cvtsixty_adjust1,field0,field1,field2,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)+ $
                     sepchar+string(field2,format=fmt1)+ $
                     strmid(string(fss,format='(F5.3)'),1,4)
         end

         4 : begin   ; XX:MM:SS.sssx, Nearest ten thousandth of a second.
            field2 = nint(field2/0.0001d0,/long)*0.0001d0
            ; Save the fractional part of the seconds.
            fss = field2-long(field2)
            cvtsixty_adjust1,field0,field1,field2,carry,maxval,wrap
            output = sign+string(field0,format=fmt0)+ $
                     sepchar+string(field1,format=fmt1)+ $
                     sepchar+string(field2,format=fmt1)+ $
                     strmid(string(fss,format='(F6.4)'),1,5)
         end

         else : begin
            print,self,'PLACES parameter must be in the range -8 to +4.'
            return
         end
      end

   endelse

   if nvals eq 1 then begin
      output=output[0]
      error =error[0]
   endif

end
