;+
; NAME:
;  pdm2
; PURPOSE: (one line)
;  Period search by phase dispersion minimization ($\chi^2$ based)
; DESCRIPTION:
;  This routine computes a Chi^2 statistic for period searching
;  in time-series data loosely based on the technique described by
;  Stellingwerf, ApJ, 224, pp. 953-960 (1978).
;
;  This algorithm is discused more in Buie and Bus, Icarus, 100, 288-294
;  (1992) "Physical Observations of (5145) Pholus".
;
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  pdm2,t,x0,sigx0,freq1,freq2,dfreq,freq,period,chi2
; INPUTS:
;  t     - independent variable (usually time)
;  x0    - dependent variable (usually magnitude or intensity)
;  sigx0 - Uncertainty on x.
;  freq1 - Lower limit to frequency to compute statistic  (units=[1/t])
;  freq2 - Upper limit to frequency to compute statistic  (units=[1/t])
;  dfreq - Frequency interval.  (units=[1/t])
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;  NBINS - Number of bins (default = 80)
;  SILENT - Flag, if set, inhibits all printed output.
; OUTPUTS:
;  freq   - Frequency vector.
;  period - inverse frequency.
;  chi2   - Chi^2 statistic.
;  avgyes  - Average number of points contained within non-empty bins.
;  nempty  - Total number of empty bins.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written 1992 Feb 11, by Marc W. Buie, Lowell Observatory
;  94/08/26 - MWB - added NBINS keyword
;  94/10/04 - MWB - moved cr to front of print statement (v3.6 bug workaround)
;  96/07/01 - MWB - added SILENT keyword
;-
pro pdm2,t,x0,sigx0,in_freq1,freq2,dfreq,freq,period,chi2,avgyes,nempty, $
      NBINS=nbins, SILENT=silent

if badpar(nbins,[0,1,2,3,4,5],0,CALLER='PDM2 (nbins) ',DEFAULT=80) then return

freq1=in_freq1

if ( (freq1 le 0 and freq2 le 0) or freq2 le 0 or freq1 eq freq2) then begin
   print,"Illegal frequency values."
   return
endif else if freq1 le 0 then begin
   freq1 = dfreq
   print,'PDM2: Warning!  Starting frequency adjusted.'
endif

if (freq1 gt freq2) then begin
   tmp=freq1
   freq1=freq2
   freq2=tmp
endif

freq = findgen((freq2-freq1)/dfreq)
freq = freq/(n_elements(freq)-1)
freq = freq * (freq2-freq1) + freq1
period = 1.0/freq
chi2  = fltarr(n_elements(freq))
avgyes = fltarr(n_elements(freq))
nempty = intarr(n_elements(freq))

width = 1.0/(nbins/2.0)
pbinl = findgen(nbins)/float(nbins)
pbinr = pbinl + width

;Double the input data to eliminate phase wrapping
x=[x0,x0]
sigx=[sigx0,sigx0]
                     
cr = string("15b)  ;"
form='($,a,a,i)'
for i=0L,long(n_elements(freq)-1) do begin
   if not keyword_set(silent) then $
      print,cr,' left to do ',n_elements(freq)-i,format=form
   lphase = t * freq[i]
   lphase = lphase - fix(lphase)
   lphase = [lphase,lphase+1.0]

   samp = intarr(nbins)
   chi  = fltarr(nbins)

   for p=0,nbins-1 do begin
      z = where( lphase gt pbinl[p] and lphase le pbinr[p], count)
      if (count gt 1) then begin
         avgyes[i] = avgyes[i]+count
         meanerr,x[z],sigx[z],m,dummy,vals
         chi[p] = total(((x[z] - m)/sigx[z])^2)/(count-1)
         samp[p] = count
      endif
   endfor

   s = total(chi)/float(total(samp)-nbins)
   chi2[i] = s
   z = where(samp ne 0)
   avgyes[i] = avgyes[i] / float(n_elements(z))
   nempty[i] = nbins - n_elements(z)
endfor

if not keyword_set(silent) then begin
   print,cr,'Done                           ',format=form
   print,' '
endif

end
