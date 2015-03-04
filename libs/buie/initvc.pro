;+
; NAME:
;   initvc
; PURPOSE:
;   Compute unit vectors for a spherical surface map.
; DESCRIPTION:
; CATEGORY:
;   Image display
; CALLING SEQUENCE:
;   initvc,nx,ny,xn,yn,zn
; INPUTS:
;   nx  - Size of map in x direction (same as longitude)
;   ny  - Size of map in y direction (same as latitude)
;         If nx & ny are non-zero, the map is assumed to be a rectangular
;         projection map.  If ny is zero, then nx is the total number of
;         annuli and the "optimal" tiling scheme is assumed.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   xn,yn,zn - Unit vector for each tile in the map.  These will have the
;              same dimensionality as the map, ie., nx by ny or just nx.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   Written by Marc W. Buie, Lowell Observatory, 1998 Sep 7
;   2003/09/18, MWB, added rectangular map option code
;-
pro initvc,nx,ny,xn,yn,zn,area

   ; Rectangular map
   if ny ne 0 then begin

      phi = (findgen(nx)+0.5)/float(nx) * 2.0 * !pi
      ones = replicate(1.0,ny)
      phi = phi#ones
      theta   = (findgen(ny)+0.5)/float(ny) * !pi
      area = 2.0*!pi*(cos(theta-!pi/ny/2.0)-cos(theta+!pi/ny/2.0))
      ones = replicate(1.0,nx)
      area = ones#area
      theta = ones#theta
      ct = cos(theta)
      st = sin(theta)
      xn = st*cos(phi)
      yn = st*sin(phi)
      zn = ct

   ; Optimal tiling map.
   endif else begin

      ; compute the number of elements in ONE hemisphere
      n = nx/2
      nelem = 2*n*(n+1)

      ; Allocate the vector arrays.
      xn = dblarr(nelem*2)
      yn = dblarr(nelem*2)
      zn = dblarr(nelem*2)

      ; compute the area for the surface elements.
      area = 2.0*!dpi/double(nelem)

      ; Generate the vectors for the Northern Hemisphere.  The width of the
      ; each annulus is adjusted so as to keep the area of each element
      ; constant.  THETA1 and THETA2 specify the lower and upper bounds on
      ; the annulus.
      k=0
      theta1=0.0d0
      for i=1,n do begin

         ; Compute the area for the entire annulus (depends only on the
         ;    number of tiles)
         da = 4.0*double(i)*area

         ; Compute the upper bound on the annulus that gives DA.
         theta2=acos( cos(theta1) - da/2.0/!dpi )

         ; Compute theta for the center of the annulus.
         theta0=(theta2+theta1)/2.0

         ; Compute theta for the center of the annulus.
         theta0 = (theta2+theta1)/2.0d0
         st = sin(theta0)
         ct = cos(theta0)

         ; Compute the step size in phi, and the offset for the first tile.
         ; I don't want the elements starting from a coherent place.
         ; Therefore, add a stagger which depends on the annulus number.
         ; The offset ranges linearly from zero at the pole to almost DPHI
         ; at the equator.  The southern hemisphere will be treated in the
         ; same way, zero at the pole and almost DPHI at the equator.
         ; This helps reduce coherent numerical noise at certain look angles
         ; when the integral over the disk is performed.
         dphi = !dpi/2.0/double(i)
         offset = double(i-1)/double(n)-1.0d0
         for j=1,4*i do begin
            phi=dphi*(double(j)+offset)
            xn[k] = st*cos(phi)
            yn[k] = st*sin(phi)
            zn[k] = ct
            k=k+1
         endfor

         theta1=theta2

      endfor

      ; Copy the vectors for the Southern Hemisphere.  Only the Z component
      ; needs to be altered.  Copy the annuli in reverse order.
      j=k
      for i=n,1,-1 do begin
         j=j-4*i
         for l=1,4*i do begin
            xn[k] =  xn[j+l-1]
            yn[k] =  yn[j+l-1]
            zn[k] = -zn[j+l-1]
            k=k+1
         endfor
      endfor

      ; Set the true number of surface elements for the entire sphere.
      nelem = 2 * nelem

   endelse
   
end
