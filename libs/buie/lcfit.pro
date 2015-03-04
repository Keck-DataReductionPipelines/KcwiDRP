;+
; NAME: 
;  lcfit
; PURPOSE: 
;  Fit a lightcurve function (Fourier series plus phase coefficient).
; DESCRIPTION:
; CATEGORY:
;  Function fitting
; CALLING SEQUENCE:
;  lcfit,lon,phang,data,sig,nterms,c,csig
; INPUTS:
;  lon    - Longitude of sub-earth point (0 to 360).
;  phang  - Phase angle (Sun-Object-Earth angle in degrees).
;  data   - Measured values.
;  sig    - Uncertainties of the data.
;  nterms - Number of fourier terms to fit.  (2*nterms+1 unknowns)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NODISPLAY - Flag, if set suppresses all plots from this routine.
;  TITLE - Title to put on plot (default = no title)
;  DYMF - The normal magnitude range on the plots is +/- 0.2 mag about the
;           mean of the extrema of the data.  This keyword lets you affect
;           this scaling.  The range is +/- 0.2/dymf (default is dymf=1)
; OUTPUTS:
;  c - fourier series coefficients (see fourfunc)
;  csig - uncertainties of the coefficients
; KEYWORD OUTPUT PARAMETERS:
;  YFIT - Final resulting fitted position for each input data point.
;  CHISQ - Goodness-of-fit statistic for fit.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  94/10/10, Written by Marc W. Buie, Lowell Observatory
;  2008/03/16, MWB, added some keywords
;  2008/11/20, MWB, fixed bug that could cause returned value of CHISQ to be bad
;-
pro lcfit,lon,in_phang,data,sig,nterms,c,csig, $
      YFIT=yfit,CHISQ=chisq,TITLE=title,DYMF=dymf,NODISPLAY=nodisplay

   common lc_com,phang

   if badpar(title,[0,7],0,CALLER='LCFIT (title): ',default='') THEN return
   if badpar(dymf,[0,1,2,3,4,5,6],0,CALLER='LCFIT (DYMF): ',default=1.0) THEN return
   if badpar(nodisplay,[0,1,2,3],0,CALLER='LCFIT (NODISPLAY): ',default=0) THEN return

   phang = in_phang

   w = 1.0/sig^2
   guess = dblarr(nterms*2+2)
   guess[0] = mean(data)

   yfit = curvefit(double(lon),double(data),double(w),guess,sigterms,function_name="LCFUN")

   c=guess
   csig=sigterms

   lonran=[0,360]
   dym=[0.2,-0.2]
   synlon=findgen(361.0)
   lcfun,synlon,c,syn,phang=0.0
   lcfun,lon,c,synd,phang=0.0

   data_2 = data - synd + c[1]
   synphase = [0.,max(in_phang)]
   synp     = c[1] + c[0]*synphase

   chisq = total( ((data-yfit)/sig)^2 ) / float(n_elements(data)-n_elements(c))
   
   if not nodisplay then begin
      setwin,0,xsize=540,ysize=320
      data_1 = data - c[0]*in_phang
      ploterror,lon,data_1,sig,psym=4,xr=lonran,yr=mean(minmax(data_1))+dym, $
         xtit='East longitude (degrees)',ytit='Phase corrected magnitude', $
         tit=title
      oplot,synlon,syn

      setwin,1,xsize=540,ysize=320

      ploterror,in_phang,data_2,sig,psym=4, $
         xr=[0.,max(in_phang)],yr=[max(data_2+sig),min([c[1],data_2-sig])], $
         xtit='Phase angle (deg)',ytit='Rotation corrected magnitude', $
         tit=title
      oplot,[0],[c[1]],psym=5
      oplot,synphase,synp

      setwin,2,xsize=540,ysize=320
      ploterror,lon,data-yfit,sig,psym=4,xr=lonran, $
         yr=mean(minmax(data-yfit))+dym/dymf, $
         xtit='East longitude (degrees)',ytit='Residuals', $
         tit=title
   endif

end
