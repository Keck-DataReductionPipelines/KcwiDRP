;+
; Project     : SOHO - CDS
;
; Name        : FMEDIAN_SLOW
;
; Purpose     : FMEDIAN routine without CALL_EXTERNAL
;
; Explanation : See fmedian.pro/built-in median routine.
;
; Use         : RESULT = FMEDIAN(ARRAY, [, NW1 [, NW2]])
;
; Inputs      : See fmedian.pro
;
; Opt. Inputs : See fmedian.pro
;
; Outputs     : See fmedian.pro
;
; Opt. Outputs: See fmedian.pro
;
; Keywords    : See fmedian.pro
;
; Calls       : None.
;
; Common      : None.
;
; Restrictions: ARRAY must be either one or two dimensional. No parameters are
;               checked, since this routine is supposed to be called only from
;               within FMEDIAN().
;
; Side effects: None.
;
; Category    : Utilities, Arrays
;
; Prev. Hist. : Fortran version was a SERTS routine.
;
; Written     : William Thompson, August 1991 (Call_external version)
;
; Modified    : Version 1, S.V.H.Haugan, UiO, 9 October 1996
;                       Added MISSING keyword, added non-call_external code.
;               Version 2, S.V.H.Haugan, UiO, 9 January 2008
;                       Added ONLY_MISSING keyword
;		Version 3, richard.schwartz@nasa.gov, 18-jan-2013
;			Cleaned it up. Scan for missing values once. Create box indices vectorally
;			Removed repetitive operations from the double loop over dimensions.
;			Simple test runs, ~500 times faster with 1000 bad points in 1024x1024 array
;			EIS case can be made much faster if that's of interest
;
;-

FUNCTION fmedian_slow,array,n_w1,n_w2,missing=missing_in,only_missing=only_missing


  always = ~keyword_set(only_missing)

  dim = [size(/dim ,array),1]

  ;;
  ndim1 = dim[0]
  ndim2 = dim[1]
  nw1 = (n_w1-1)/2
  nw1p= n_w1-nw1-1
  nw2 = (n_w2-1)/2
  nw2p=  n_w2-nw2-1
  out = array
  mx1 = ndim1 - 1
  mx2 = ndim2 - 1
  ;
  ; Identify the missing points and apply the median filter to each
  ;
  zm = always ? where(array*0+1, nzm) : where( array eq missing_in, nzm)
  if nzm ge 1 then begin
  	ij = get_ij( zm, ndim1)
  	ilow = reform( ij[0,*] - nw1 > 0)
  	ihi  = reform( ij[0,*] + nw1p < (ndim1 - 1))
  	jlow = reform( ij[1,*] - nw2 > 0)
  	jhi  = reform( ij[1,*] + nw2p < (ndim2 - 1))
  	for i = 0L, nzm-1 do begin
  		sub  = array[ ilow[i] : ihi[i], jlow[i] :jhi[i]]
	  	good = where(sub ne missing_in, ngd)
	  	case ngd of
	  		0: ;can't fix anything, all missing
	  		1: out[ij[0,i], ij[1,i]] = sub[good]
	  		2: out[ij[0,i], ij[1,i]] = avg( sub[good] )
	  		else: out[ij[0,i], ij[1,i]] = median( sub[good] )
	  		end
	  	endfor
	  endif

return, out
end
;
;
;
;
;
;
;
;  FOR j = 0,ndim2-1 DO BEGIN
;     j1 = (j-nw2) > 0
;     j2 = ((j+n_w2-nw2-1) < (ndim2-1)) > 0
;     FOR i = 0,ndim1-1 DO BEGIN
;        I1 = (I-nw1) > 0
;        I2 = (I+N_W1-NW1-1) < (ndim1-1)
;
;        ;; Fetch data if necessary
;        IF always OR array[i,j] EQ MISSING_IN THEN BEGIN
;           sub = array(i1:i2,j1:j2)
;           good = sub([where(sub NE MISSING_IN,N)])
;
;           CASE n OF
;              0 : ;; Do nothing- missing already there
;              1 : out(i,j) = good(0)
;              2 : out(i,j) = (good(0)+good(1))*0.5
;              ELSE: out(i,j) = median(good)
;           END
;        END
;     END
;  END
;
;  return,out
;END
;
;
