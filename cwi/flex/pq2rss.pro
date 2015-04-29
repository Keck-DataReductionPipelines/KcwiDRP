
pro pq2rss,p,q,erot,exscl,eyscl,exshft,eyshft,enrss,nx1,ny1,center=center, $
           quiet=quiet,anchor=anchor,rotfirst=rotfirst

;+
;NAME:
;     PQ2RSS
;PURPOSE:
;     Converts the P and Q variables in a call to poly_2d into a rotation, 
;     scale change, and shift.
;CATEGORY:
;CALLING SEQUENCE:
;     pq2rss,p,q,erot,exscl,eyscl,exshft,eyshft,enrss,nx,ny
;INPUTS:
;     p,q = p,q for poly_2d.  P and Q should be 4-element vectors and represent
;           the first order transformation from poly_warp.
;OPTIONAL INPUT PARAMETERS:
;     nx,ny = the size of the image which is being warped.  These are required
;             to fix the center of the rotation.  If /center is set, they are
;             required.
;KEYWORD PARAMETERS
;      anchor = [x0,y0] defines the center of the rotation.
;     /center = rotate about the center rather thatn the lower left corner
;               This affects only the output value of exshft and eyshft.
;     /rotfirst = Do the rotation before the shift.  Not normally a good idea.
;                 Ignored unless center or anchor is set.
;     /quiet = don't print the answer
;OUTPUTS:
;     erot = equivalent rotaion in degrees
;     exscl = equivalent x scale as a fraction of the original sacle
;     eyscl = equivalent y scale as a fraction of the original sacle
;     exshft = equivalent x shift in pixels
;     eyshft = equivalent y shift in pixels
;     enrss = warping ... this is whatever bit of p,q does not fit into the
;             simple rotation/scale/shift.  fltarr(4,2)
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;     The first order P,Q transformation also includes a warping (x*y term)
;     which is not really included here.  enrss returns the leftover P,Q
;     which represent this term to some extent.
;PROCEDURE:
;EXAMPLES:
;     IDL> pq2rss,[1,0,1,0],[1,1,0,0]
; 
;        rotation =  0.00000 degrees
;        scale x =  1.00000
;        scale y =  1.00000
;        shift x =  -1.00000
;        shift y =  -1.00000
;MODIFICATION HISTORY:
;     Written by T. Metcalf  March 30, 1993
;     TRM 27-June-93   Sepearted this code into a separate program from 
;                      align_img.pro
;     TRM 1994-11-10   Added anchor keyword.
;     TRM 2001-09-26   Added rotfirst keyword so that the default call to
;                      pq2rss is the inverse of the default call to rss2pq.
;     TRM 2001-09-27   Changed to double precision since there are matrix
;                      inversions to do.
;     TRM 2005-01-25   /center now implies an anchor point of
;                      [(nx-1)/2,(ny-1)/2] rather than [nx/2,ny/2].
;-

   if NOT keyword_set(center) then begin
      if n_elements(anchor) NE 2 then begin
         anchor = [0.0d0,0.0d0]  ; Default is to rotate about corner
      endif
   endif
   nanchor = n_elements(anchor)
   tsp = dblarr(4,2)
   tsp(*,0) = p
   tsp(*,1) = q
   erot = atan(tsp(1,0),tsp(2,0))
   erot= (erot+arange(atan(-tsp(2,1),tsp(1,1)),erot))/2.0d0
   ct = cos(double(erot))
   st = sin(double(erot))
   determ = tsp(2,0)*tsp(1,1) - tsp(1,0)*tsp(2,1)
   exscl = sqrt(determ*tsp(2,0)/tsp(1,1))
   eyscl = determ/exscl
   exscl = 1.d0/exscl
   eyscl = 1.d0/eyscl

   if nanchor EQ 2 then begin
      nx12 = double(anchor(0))
      ny12 = double(anchor(1))
   endif else begin
      nx12 = double((nx1-1)/2.d0)
      ny12 = double((ny1-1)/2.d0)
   endelse

   halfshft1 = [ [1.d0,0.d0,-nx12], $
                 [0.d0,1.d0,-ny12], $
                 [0.d0,0.d0,1.d0]]
   halfshft2 = [ [1.d0,0.d0,nx12], $
                 [0.d0,1.d0,ny12], $
                 [0.d0,0.d0,1.d0]]
   rtn = halfshft1 # [ [ct,st,0.d0], $
                       [-st,ct,0.d0], $
                       [0.d0,0.d0,1.d0]] # halfshft2 
   scl = [ [1.d0/exscl, 0.d0, 0.d0], $
           [0.d0, 1.d0/eyscl, 0.d0], $
           [0.d0, 0.d0, 1.d0 ]]
   tran = transpose([[double(p[2]),double(q[2]),0.d0], $
                     [double(p[1]),double(q[1]),0.d0], $
                     [double(p[0])/exscl,double(q[0])/eyscl,1.d0]])
   if keyword_set(rotfirst) then begin
      shft=invert(rtn,rtnstatus,/double)#(tran#invert(scl,sclstatus,/double))
   endif else begin
      shft=(tran#invert(scl,sclstatus,/double))#invert(rtn,rtnstatus,/double)
   endelse
   exshft = -shft[2,0]
   eyshft = -shft[2,1]
   if rtnstatus GT 0 OR sclstatus GT 0 then $
      message,/info,'WARNING: bad matrix inversion.  ' + $
                    'xshift and yshift may not be accurate.'

   erot = erot * (180.0d0/!dpi)
   enrss = tsp - rss2pq(xshift=exshft,yshift=eyshft,xscale=exscl, $
                        yscale=eyscl,rot12=erot,center=center,nx1,ny1, $
                        anchor=anchor,rotfirst=rotfirst)

   if NOT keyword_set(quiet) then begin
      print
      print,'   rotation = '+strcompress(string(float(erot)))+' degrees'
      print,'   scale x = '+strcompress(string(float(exscl)))
      print,'   scale y = '+strcompress(string(float(eyscl)))
      print,'   shift x = '+strcompress(string(float(exshft)))
      print,'   shift y = '+strcompress(string(float(eyshft)))
   endif
end

