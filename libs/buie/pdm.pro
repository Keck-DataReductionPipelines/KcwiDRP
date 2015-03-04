;+
; NAME:
;	pdm
; PURPOSE: (one line)
;	Period search by phase dispersion minimization ($\theta$ based).
; DESCRIPTION:
;	This routine computes the Theta statistic for period searching
;	in time-series data using the technique described by Stellingwerf,
;	ApJ, 224, pp. 953-960 (1978).
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;	pdm,t,x,sigx0,freq1,freq2,dfreq,freq,period,theta
; INPUTS:
;	t     - independent variable (usually time)
;	x0    - dependent variable (usually magnitude or intensity)
;	sigx0 - Uncertainty on x.
;	freq1 - Lower limit to frequency to compute statistic  (units=[1/t])
;	freq2 - Upper limit to frequency to compute statistic  (units=[1/t])
;	dfreq - Frequency interval.  (units=[1/t])
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;	freq  - Frequency vector.
;	period - inverse frequency.
;	theta - PDM statistic (1 ==> insignificant, 0 ==> significant).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992 Feb 11, by Marc W. Buie, Lowell Observatory
;-
pro pdm,t,x0,sigx0,freq1,freq2,dfreq,freq,period,theta

   self = 'PDM:'
   if badpar(t,[2,3,4,5],1,caller=self+' (t)',npts=npts_t) then return
   if badpar(x0,[2,3,4,5],1,caller=self+' (x0)',npts=npts_x0) then return
   if badpar(sigx0,[2,3,4,5],1,caller=self+' (sigx0)', $
                   npts=npts_sigx0) then return
   if badpar(freq1,[2,3,4,5],0,caller=self+' (freq1)') then return
   if badpar(freq2,[2,3,4,5],0,caller=self+' (freq2)') then return
   if badpar(dfreq,[2,3,4,5],0,caller=self+' (dfreq)') then return

   if npts_t le 1 then begin
      print,self,' ERROR! You must provide two or more input points'
      return
   endif

   if npts_x0 ne npts_t then begin
      print,self,' ERROR! Length of x0 vector must match the length of t'
      return
   endif

   if npts_x0 ne npts_sigx0 then begin
      print,self,' ERROR! Length of sigx0 vector must match the length of t'
      return
   endif

   if (freq1 le 0 or freq2 le 0 or freq1 eq freq2) then begin
      print,"Illegal frequency values."
      return
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
   theta = fltarr(n_elements(freq))

   nbins = 60
   width = 1.0/20.0
   pbinl = findgen(nbins)/float(nbins)
   pbinr = pbinl + width

   meanerr,x0,sigx0,mx,dummy,sigsamp
   varsamp = sigsamp^2
;   varsamp = x0 - mean(x0)
;   varsamp = varsamp^2
;   varsamp = total(varsamp)/(n_elements(x0)-1)
;   print,varsamp

;  Double the input data to eliminate phase wrapping
   x=[x0,x0]
   sigx=[sigx0,sigx0]

   for i=0,n_elements(freq)-1 do begin
      lphase = t * freq[i]
      lphase = lphase - fix(lphase)
      lphase = [lphase,lphase+1.0]

      samp = intarr(nbins)
      sig  = fltarr(nbins)
;      vars  = fltarr(nbins)

      for p=0,nbins-1 do begin
         z = where( lphase gt pbinl[p] and lphase le pbinr[p], count)
         if (count gt 1) then begin
            meanerr,x[z],sigx[z],m,dummy,vals
            sig[p] = vals
;            tmp = x[z) - mean(x[z])
;            vars[p] = total(tmp^2)/(count-1)
            samp[p] = count
;            print,m,mean(x[z]),dummy^2,sig[p]^2,vars[p]
         endif
      endfor

      z = where(samp gt 0)
;      s = total(vars[z]*(samp[z]-1))/float(total(samp[z])-nbins)
      s = total(sig[z]^2*(samp[z]-1))/float(total(samp)-nbins)
      theta[i] = s/varsamp
   endfor

end
