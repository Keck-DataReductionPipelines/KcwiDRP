;+
; NAME:
;	moment4
; PURPOSE: (one line)
;	Compute various statistical moments of the data.
; DESCRIPTION:
;	This routine computes the average, average deviation, standard
;	deviation, variance, skew and kurtosis of the input data.  The various
;	output quantities are always returned as floating point scalars.
;	The statistics are compute with no regard for the dimensionality of
;	the input data.
; CATEGORY:
;	Statistics
; CALLING SEQUENCE:
;	moment4,data,avg,avgdev,stddev,var,skew,kurt
; INPUTS:
;	data - Input data to be analyzed.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	avg    - Sample mean.
;	avgdev - Average deviation of the data from the mean.
;	stddev - Standard deviation of the data from the mean.
;	var    - Variance of the data from the mean.
;	skew   - Skewness, third statistical moment.
;	kurt   - Kurtosis, fourth statistical moment.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1992 Jan 20
;  1997/09/06, MWB, changed name from moment to avoid name conflict.
;  2000/11/2, MWB, modified to use the IDL system function moment.  This
;     routine really does nothing more than rewrap the IDL function.
;-
pro moment4,data,avg,avgdev,stddev,var,skew,kurt

   n = n_elements(data)

   if n le 1 then begin
      print,'MOMENT4: Error, number of elements in input data must be at least 2.'
      return
   endif

   result=moment(data,mdev=avgdev,sdev=stddev,/nan)

   avg = result[0]
   var = result[1]
   skew= result[2]
   kurt= result[3]

   if not finite(skew) then skew=0.0
   if not finite(kurt) then kurt=0.0

end
