;+
; NAME:
;  astterms
; PURPOSE:
;  Evaulate the independent vectors to match an astrometric polynomial function.
; DESCRIPTION:
;
;  This function is a support routine to ASTROM and handles part of the
;  transformation between pixel (x,y) coordinates and the tangent plane
;  coordinates (xi,eta).  The transformation from (ra,dec) to (xi,eta)
;  is not handled in this routine.  The premise is that the transformation
;  from the tangent plane to pixel coordinates can be done with a polynominal.
;  I have implemented all of the common terms found in typical astrometric
;  solutions.  In practice, the high order terms are probably not needed
;  except for very large fields or for highly distorted fields caused by
;  excessive optics.  Most CCD fields can be accurately modeled using just
;  the linear terms.
;
;  This function does NOT actually evaluate the transformation.  Instead,
;  the independent values for the polynominal are computed.  The result is
;  an array with (nterms,nvals) elements where nterms is the number of
;  non-zero terms and nvals is the number of input x and y values (which must
;  be of the same length.  The table below lists the contents of the i^th
;  column in the output array.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  val=astterms(x,y,terms)
; INPUTS:
;  x     - X - coordinate (scalar or vector)
;  y     - Y - coordinate (scalar or vector)
;  terms - Which terms are to be built, if this input is a 10-element
;             integer array, then a 1 means use the term,
;                                   0 means don't use it.
;              0 - const (always use this)
;              1 - x     (always use this)
;              2 - y     (always use this)
;              3 - r
;              4 - x^2
;              5 - y^2
;              6 - x^3
;              7 - y^3
;              8 - xy^2
;              9 - yx^2
;         Another input option is provide where terms is an string array
;            that contains the name of the term.  The terms can appear in
;            any order and any subset can be used.  There is NO error
;            checking to prevent duplicating a term (to save time).  The
;            terms that are supported are (the names must match, case is
;            ignored):
;            CONST, X, Y, XX, YY, XY, R (sqrt(x^2+y^2)), XXX, YYY, XYY, XXY,
;            XXXX, YYYY, XYYY, XXYY, XXXY, XXXXX, YYYYY, XYYYY, XXYYY, XXXYY,
;            XXXXY, T1, T2, T3, T4, T5, U1, U2, U3, U4, U5
;
;         The terms labeled 'T' and 'U' are Chebyshev polynomials, with order
;            begin given by the repetition of X or Y, (ie., TXX is second order
;            of degree 1; TXYYY is 1st order in X times 3rd order in Y for
;            degree 1.
;         Complete list: (CONST applied to all forms)
;            degree 1
;            TX, TY, TXX, TXY,TYY, TXXX, TXXY, TXYY, TYYY,
;            TXXXX, TXYYY, TXXYY, TXXXY, TYYYY,
;            TXXXXX, TXYYYY, TXXYYY, TXXXYY, TXXXXY, TYYYYY
;            degree 2
;            UX, UY, UXX, UXY, UYY, UXXX, UXXY, UXYY, UYYY,
;            UXXXX, UXYYY, UXXYY, UXXXY, UYYYY,
;            UXXXXX, UXYYYY, UXXYYY, UXXXYY, UXXXXY, UYYYYY
;
;         The terms labeled L are Legendre polynomials with M=0
;            PX, PY, PXX, PXY, PYY, PXXX, PXXY, PXYY, PYYY,
;            PXXXX, PXYYY, PXXYY, PXXXY, PYYYY,
;            PXXXXX, PXYYYY, PXXYYY, PXXXYY, PXXXXY, PYYYYY
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  return value - Dependent value(s), if x,y was 1-d then this will be scalar.
;
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
; MODIfICATION HISTORY:
;  1997/06/17, Written by Marc W. Buie, Lowell Observatory
;  2009/10/25, MWB, added string input option for terms
;  2009/12/10, MWB, added 4th order terms (names option only)
;  2010/02/24, Chris Sauro, added Chebyshev terms
;  2010/02/28, MWB, filled out Chebyshev and added Legendre
;  2010/03/31, MWB, added 6th order Legendre
;-
function astterms,x,y,terms

   self='ASTTERMS: '
   if badpar(x,[2,3,4,5],[0,1],caller=self+'(x) ') then return,0
   if badpar(y,[2,3,4,5],[0,1],caller=self+'(y) ') then return,0
   if badpar(terms,[2,3,4,5,7],1,caller=self+'(terms) ', $
                                 type=terms_type) then return,0

   if terms_type ne 7 then begin

      nterms=fix(total(terms))
      nvals =n_elements(x)

      ind = dblarr(nterms,nvals,/nozero)

      tpos=0

      if terms[0] eq 1 then begin
         ind[tpos,*] = 1.0
         tpos=tpos+1
      endif

      if terms[1] eq 1 then begin
         ind[tpos,*] = x
         tpos=tpos+1
      endif

      if terms[2] eq 1 then begin
         ind[tpos,*] = y
         tpos=tpos+1
      endif

      if terms[3] eq 1 then begin
         ind[tpos,*] = sqrt(x^2+y^2)
         tpos=tpos+1
      endif

      if terms[4] eq 1 then begin
         ind[tpos,*] = x^2
         tpos=tpos+1
      endif

      if terms[5] eq 1 then begin
         ind[tpos,*] = y^2
         tpos=tpos+1
      endif

      if terms[6] eq 1 then begin
         ind[tpos,*] = x^3
         tpos=tpos+1
      endif

      if terms[7] eq 1 then begin
         ind[tpos,*] = y^3
         tpos=tpos+1
      endif

      if terms[8] eq 1 then begin
         ind[tpos,*] = x*y^2
         tpos=tpos+1
      endif

      if terms[9] eq 1 then begin
         ind[tpos,*] = x^2*y
         tpos=tpos+1
      endif

   endif else begin

      nterms=n_elements(terms)
      nvals =n_elements(x)

      ind = dblarr(nterms,nvals,/nozero)

      for i=0,nterms-1 do begin
         case strlowcase(terms[i]) of
            'const': begin
               ind[i,*] = 1.0
               end
            'x': begin
               ind[i,*] = x
               end
            'y': begin
               ind[i,*] = y
               end
            'xy': begin
               ind[i,*] = x*y
               end
            'xx': begin
               ind[i,*] = x^2
               end
            'yy': begin
               ind[i,*] = y^2
               end
            'r': begin
               ind[i,*] = sqrt(x^2+y^2)
               end
            'xxx': begin
               ind[i,*] = x^3
               end
            'xxy': begin
               ind[i,*] = x^2 * y
               end
            'xyy': begin
               ind[i,*] = x * y^2
               end
            'yyy': begin
               ind[i,*] = y^3
               end
            'xxxx': begin
               ind[i,*] = x^4
               end
            'xxxy': begin
               ind[i,*] = x^3 * y
               end
            'xxyy': begin
               ind[i,*] = x^2 * y^2
               end
            'xyyy': begin
               ind[i,*] = x * y^3
               end
            'yyyy': begin
               ind[i,*] = y^4
               end
            'xxxxx': begin
               ind[i,*] = x^5
               end
            'xxxxy': begin
               ind[i,*] = x^4 * y
               end
            'xxxyy': begin
               ind[i,*] = x^3 * y^2
               end
            'xxyyy': begin
               ind[i,*] = x^2 * y^3
               end
            'xyyyy': begin
               ind[i,*] = x * y^4
               end
            'yyyyy': begin
               ind[i,*] = y^5
               end

            ; Chebyshev terms
            ; first degree
            'tx': begin
               ind[i,*] = x
               end
            'ty': begin
               ind[i,*] = y
               end
            'txx': begin
               ind[i,*] = 2 * x^2 - 1
               end
            'txy': begin
               ind[i,*] = x * y
               end
            'tyy': begin
               ind[i,*] = 2 * y^2 - 1
               end
            'txxx': begin
               ind[i,*] = 4 * x^3 - 3 * x
               end
            'txxy': begin
               ind[i,*] = (2 * x^2 - 1) * y
               end
            'txyy': begin
               ind[i,*] = x * (2 * y^2 - 1)
               end
            'tyyy': begin
               ind[i,*] = 4 * x^3 - 3 * x
               end
            'txxxx': begin
               ind[i,*] = 8 * x^4 - 8 * x^2 + 1
               end
            'txxxy': begin
               ind[i,*] = (4 * x^3 - 3 * x) * y
               end
            'txxyy': begin
               ind[i,*] = (2 * x^2 - 1) * (2 * y^2 - 1)
               end
            'txyyy': begin
               ind[i,*] = x * (4 * y^3 - 3 * y)
               end
            'tyyyy': begin
               ind[i,*] = 8 * y^4 - 8 * y^2 + 1
               end
            'txxxxx': begin
               ind[i,*] = 16 * x^5 - 20 * x^3 + 5 * x
               end
            'txxxxy': begin
               ind[i,*] = (8 * x^4 - 8 * x^2 + 1) * y
               end
            'txxxyy': begin
               ind[i,*] = (4 * x^3 - 3 * x) * (2 * y^2 - 1)
               end
            'txxyyy': begin
               ind[i,*] = (2 * x^2 - 1) * (4 * y^3 - 3 * y)
               end
            'txyyyy': begin
               ind[i,*] = x * (8 * y^4 - 8 * y^2 + 1)
               end
            'tyyyyy': begin
               ind[i,*] = 16 * y^5 - 20 * y^3 + 5 * y
               end

            ; second degree
            'ux': begin
               ind[i,*] = 2 * x
               end
            'uy': begin
               ind[i,*] = 2 * y
               end
            'uxx': begin
               ind[i,*] = 4 * x^2 - 1
               end
            'uxy': begin
               ind[i,*] = 4 * x * y
               end
            'uyy': begin
               ind[i,*] = 4 * y^2 - 1
               end
            'uxxx': begin
               ind[i,*] = 8 * x^3 - 4 * x
               end
            'uxxy': begin
               ind[i,*] = (4 * x^2 - 1) * (2 * y)
               end
            'uxyy': begin
               ind[i,*] = (2 * x) * (4 * y^2 - 1)
               end
            'uyyy': begin
               ind[i,*] = 8 * y^3 - 4 * y
               end
            'uxxxx': begin
               ind[i,*] = 16 * x^4 - 12 * x^2 + 1
               end
            'uxxxy': begin
               ind[i,*] = (8 * x^3 - 4 * x) * (2 * y)
               end
            'uxxyy': begin
               ind[i,*] = (4 * x^2 - 1) * (4 * y^2 - 1)
               end
            'uxyyy': begin
               ind[i,*] = (2 * x) * (8 * y^3 - 4 * y)
               end
            'uyyyy': begin
               ind[i,*] = 16 * y^4 - 12 * y^2 + 1
               end
            'uxxxxx': begin
               ind[i,*] = 32 * x^5 - 32 * x^3 + 6 * x
               end
            'uxxxxy': begin
               ind[i,*] = (16 * x^4 - 12 * x^2 + 1) * (2 * y)
               end
            'uxxxyy': begin
               ind[i,*] = (8 * x^3 - 4 * x) * (4 * y^2 - 1)
               end
            'uxxyyy': begin
               ind[i,*] = (4 * x^2 - 1) * (8 * y^3 - 4 * y)
               end
            'uxyyyy': begin
               ind[i,*] = (2 * x) * (16 * y^4 - 12 * y^2 + 1)
               end
            'uyyyyy': begin
               ind[i,*] = 32 * x^5 - 32 * x^3 + 6 * x
               end

            ; Legendre terms
            'px': begin
               ind[i,*] = legendre(x,1)
               end
            'py': begin
               ind[i,*] = legendre(y,1)
               end
            'pxx': begin
               ind[i,*] = legendre(x,2)
               end
            'pxy': begin
               ind[i,*] = legendre(x,1) * legendre(y,1)
               end
            'pyy': begin
               ind[i,*] = legendre(y,2)
               end
            'pxxx': begin
               ind[i,*] = legendre(x,3)
               end
            'pxxy': begin
               ind[i,*] = legendre(x,2) * legendre(y,1)
               end
            'pxyy': begin
               ind[i,*] = legendre(x,1) * legendre(y,2)
               end
            'pyyy': begin
               ind[i,*] = legendre(y,3)
               end
            'pxxxx': begin
               ind[i,*] = legendre(x,4)
               end
            'pxxxy': begin
               ind[i,*] = legendre(x,3) * legendre(y,1)
               end
            'pxxyy': begin
               ind[i,*] = legendre(x,2) * legendre(y,2)
               end
            'pxyyy': begin
               ind[i,*] = legendre(x,1) * legendre(y,3)
               end
            'pyyyy': begin
               ind[i,*] = legendre(y,4)
               end
            'pxxxxx': begin
               ind[i,*] = legendre(x,5)
               end
            'pxxxxy': begin
               ind[i,*] = legendre(x,4) * legendre(y,1)
               end
            'pxxxyy': begin
               ind[i,*] = legendre(x,3) * legendre(y,2)
               end
            'pxxyyy': begin
               ind[i,*] = legendre(x,2) * legendre(y,3)
               end
            'pxyyyy': begin
               ind[i,*] = legendre(x,1) * legendre(y,4)
               end
            'pyyyyy': begin
               ind[i,*] = legendre(y,5)
               end
            'pxxxxxx': begin
               ind[i,*] = legendre(x,6)
               end
            'pxxxxxy': begin
               ind[i,*] = legendre(x,5) * legendre(y,1)
               end
            'pxxxxyy': begin
               ind[i,*] = legendre(x,4) * legendre(y,2)
               end
            'pxxxyyy': begin
               ind[i,*] = legendre(x,3) * legendre(y,3)
               end
            'pxxyyyy': begin
               ind[i,*] = legendre(x,2) * legendre(y,4)
               end
            'pxyyyyy': begin
               ind[i,*] = legendre(x,1) * legendre(y,5)
               end
            'pyyyyyy': begin
               ind[i,*] = legendre(y,6)
               end
            'pxxxxxxx': begin
               ind[i,*] = legendre(x,7)
               end
            'pxxxxxxy': begin
               ind[i,*] = legendre(x,6) * legendre(y,1)
               end
            'pxxxxxyy': begin
               ind[i,*] = legendre(x,5) * legendre(y,2)
               end
            'pxxxxyyy': begin
               ind[i,*] = legendre(x,4) * legendre(y,3)
               end
            'pxxxyyyy': begin
               ind[i,*] = legendre(x,3) * legendre(y,4)
               end
            'pxxyyyyy': begin
               ind[i,*] = legendre(x,2) * legendre(y,5)
               end
            'pxyyyyyy': begin
               ind[i,*] = legendre(x,1) * legendre(y,6)
               end
            'pyyyyyyy': begin
               ind[i,*] = legendre(y,7)
               end

            else: begin
               print,self,'Illegal term name: ',terms[i]
               return,0
               end
         endcase
      endfor

   endelse

   return,trimrank(ind)

end
