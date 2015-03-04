;+
; NAME: 
;  render
; PURPOSE: 
;  Render a rectangular projection map to a sphere.
; DESCRIPTION:
;  The most confusing aspect of this program is by far the image orientation.
;  I can't make the conventions natural for everyone (not even myself), but
;  here's what this program does:
;
;  In the output image there is a natural coordinate system, (x,y).  Each
;  row, ie., image[*,i] corresponds to a fixed value of y.  image[*,0]
;  is the row with the LOWEST value of y.  This may seem strange, but it
;  permits a "normal" orientation of the image if displayed with !order=0
;  (the usual IDL default).  The x direction (image[i,*]) works in the
;  obvious way, image[0,*] is the column with the LOWEST value of x.  So,
;  viewed with !ORDER=0, this is an image that has up at the top.  The
;  top is also considered NORTH when dealing with images that one sees in
;  the sky (or relative to your point of view).
;
;  The position angle of the pole sets the rotation of the sphere in the
;  plane of the sky (image).  North is to the top (+y) and East is to the
;  left (-x).  The position angle (POLE) is the angle of the apparent
;  North pole of the sphere relative to up (North) in the image, measured
;  From up (North) counter-clockwise (also known as eastward from North).
;
;  Visualizing the indexing of the input map is equally confusing.  In
;  the coordinate system containing the map, North is +y, East is -x, as
;  before.  These coordinates are plane-of-sky on the object.  The map
;  has its own coordinates, latitude and longitude.  The first index into
;  the map is treated like x but is actually East Longitude.  The left edge
;  of image[0,*] is precisely at 0 degrees longitude.  As the first index
;  increases (increasing x?), the longitude increases.  The right edge of
;  the last column in the map is 360 degrees.
;
;  Ok, here is the weird part.  The first row in the map is the NORTHERN-most
;  set of pixels.  The last row is the SOUTHERN-most set.  So, map[0,0]
;  touches the prime meridian and the north pole.
;      Thus map[i,j]:
;           longitude = (i+0.5)/nl * 360.0
;           latitude  = 90.0 - (j+0.5)/nt * 180.0
;        where nl is the width of map and nt is the height of the map.
;      These formulas give the lat,lon of the CENTER of the pixel in degrees.
;
;  Now, understanding the sub-"earth" and sub-solar longitudes becomes
;  easy.  The sub-"earth" point is the lat,lon of the map that is in the
;  center of the projected disk.  The sub-solar point is the lat,lon of
;  the map that is nearest to the sun (normal solar illumination).  Note,
;  that as the object rotates, the sub-earth longitude will decrease, not
;  increase.
;
; CATEGORY:
;  Image display
; CALLING SEQUENCE:
;  render,map,radius,scale,pole,selat,selon,sslat,sslon,nx,ny,image
; INPUTS:
;  map - Array containing a full map of surface
;  radius - Radius of object (eg., in km)
;  scale  - Scale of image (eg., km/pixel)
;  pole   - Position angle of pole, east from north (degrees)
;  selat  - Sub-earth latitude (degrees)
;  selon  - Sub-earth longitude (degrees)
;  sslat  - Sub-solar latitude (degrees).  Only used for Hapke functions.
;  sslon  - Sub-solar longitude (degrees).  Only used for Hapke functions.
;  nx     - X size of output image in pixels.
;  ny     - Y xize of output image in pixels.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  SILENT - Flag, if set suppresses all printed output
;  NODISPLAY - Flag, if set suppresses graphical output
;  GEOM - Undefined (default), no special action.
;         If defined but not a structure, then upon return geom will contain
;            all the geometric information needed to render the image with a
;            new map.
;         If defined and is a structure, then the contents are taken to be
;            that needed to do the final image calculation without redoing
;            all the geometric computation.
;  XOFFSET - x offset, of object from center of image (in pixels) default=0
;  YOFFSET - y offset, of object from center of image (in pixels) default=0
;
;  Limb darkening model to use (set one of these, at most):
;     
;     LIN   - Linear limb-darkening coeff, map is normal albedo
;     MIN   - Minnaert limb-darkening coeff, map is normal albedo
;     HAPKE - Hapke scattering model, map is single scattern albedo
;                HAPKE[0] = h (old style, circa 1981)
;                HAPKE[1] = P(0)
;     HAP2  - 2nd generation Hapke scattering model, 1986 and the book,
;               "Theory of Reflectance and Emittance Spectroscopy"
;                HAP2[0] = h (new style, circa 1986, p. 226 in book)
;                HAP2[1] = P(g)
;                HAP2[2] = B0, Emperical backscatter factor (p. 228)
;                HAP2[3] = Theta(bar), surface roughness parameter.
;     H93   - When used with HAP2, will select which approximation to the
;                H-function is used.  Default is the simplest (and fastest)
;                formula from the 1981 Hapke paper.  H93, when set, will
;                force the use of eqn. 8.57 from the Hapke book (from 1993).
;
; OUTPUTS:
;  image  - Rendered image of object.  Return is a traditional I/F value.
; KEYWORD OUTPUT PARAMETERS:
;  XARR - Optional return of the array of plane-of-sky x positions for each
;           pixel in the image.  These values are in units of the object
;           radii.
;  YARR - Optional return of the array of plane-of-sky y positions for each
;           pixel in the image.  These values are in units of the object
;           radii.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   95/07/24, Written by Marc W. Buie, Lowell Observatory
;   96/04/11, MWB, fixed numerous bugs, now works for sun and viewpoint
;                    being different
;   97/08/21, MWB, added HAP2 keyword
;   97/08/27, MWB, added GEOM keyword
;   97/09/18, MWB, repaired incorrect mapping of map to surface.
;   2003/04/03, MWB, added XOFFSET and YOFFSET keywords
;   2009/02/01, MWB, improved parameter validation and some documentation
;-

pro render,map,radius,scale,in_pole,selat,selon,sslat,sslon,nx,ny,image, $
      LIN=xlimb,MIN=k,HAPKE=hapke,HAP2=hapke2,H93=h93, $
      NODISPLAY=nodisplay,SILENT=silent,GEOM=geom,XARR=xarr,YARR=yarr, $
      XOFFSET=xoffset, YOFFSET=yoffset

   self = 'RENDER: '

   if badpar(map,[4,5],2,caller=self+'(map) ') then return
   if badpar(radius,[2,3,4,5],0,caller=self+'(radius) ') then return
   if badpar(scale,[2,3,4,5],0,caller=self+'(scale) ') then return
   if badpar(in_pole,[2,3,4,5],0,caller=self+'(pole) ') then return
   if badpar(selat,[2,3,4,5],0,caller=self+'(selat) ') then return
   if badpar(selon,[2,3,4,5],0,caller=self+'(selon) ') then return
   if badpar(sslat,[2,3,4,5],0,caller=self+'(sslat) ') then return
   if badpar(sslon,[2,3,4,5],0,caller=self+'(sslon) ') then return
   if badpar(nx,[2,3],0,caller=self+'(nx) ') then return
   if badpar(ny,[2,3],0,caller=self+'(ny) ') then return

   if badpar(nodisplay,[0,1,2,3],0,caller=self+'(NODISPLAY) ', $
                default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                default=0) then return
   if badpar(xoffset,[0,1,2,3,4,5],0,caller=self+'(XOFFSET) ', $
                default=0.0) then return
   if badpar(yoffset,[0,1,2,3,4,5],0,caller=self+'(YOFFSET) ', $
                default=0.0) then return

   nx = round(nx)
   ny = round(ny)

   ; Save size of input map
   sz=size(map)
   nlon = sz[1]
   nlat = sz[2]

   ; Display input map
   if not nodisplay then begin
      setwin,1,xsize=long(nlon),ysize=long(nlat)
      tvscl,map,/order
   endif

   ; Set up output image array
   image=fltarr(nx,ny)

   ; Check on the GEOM keyword and find out what to do.
   szg=size(geom)
   typecode = szg[n_elements(szg)-2]

   fullcompute = n_elements(geom) le 1

   IF fullcompute THEN BEGIN

      ; Set up image that contains the y coordinate of each pixel
      ;   (relative to center)
      y = (((findgen(ny) - float(ny-1)/2.0) - yoffset) * scale)/radius
      onex = replicate(1.0,nx)
      yarr = onex#y

      ; Set up image that contains the x coordinate of each pixel
      ;   (relative to cntr)
      x = (((findgen(nx) - float(nx-1)/2.0) - xoffset) * scale)/radius
      oney = replicate(1.0,ny)
      xarr = x#oney

      ; Image of pixel distance from center.
      rsq = xarr^2 + yarr^2

      ; Convert input angles to radians
      pole   = in_pole / !radeg   ; position angle of pole
      theta  = selat   / !radeg   ; sub-earth latitude
      phi    = selon   / !radeg   ; sub-earth longitude
      stheta = sslat   / !radeg   ; sub-solar latitude
      sphi   = sslon   / !radeg   ; sub-solar longitude

      ; Unit vector that points toward observer
      xv = cos(theta)*cos(phi)
      yv = cos(theta)*sin(phi)
      zv = sin(theta)

      ; Unit vector that points toward sun
      xs = cos(stheta)*cos(sphi)
      ys = cos(stheta)*sin(sphi)
      zs = sin(stheta)

      ; Index into IMAGE that fall on disk of object.
      zon = where(rsq lt 1.0,count_zon)

      if count_zon gt 0 then begin

         ; 3-D coordinates of image plane ON the object.  (this is also the set
         ;   of unit vectors for each of the points on the surface of the object)
         x = xarr[zon]
         y = yarr[zon]
         z = sqrt( 1.0 - rsq[zon] )

         ; Rotate the surface vectors to the viewpoint
         rotpoint,x,y,z,'z',-pole,xp,yp,zp
         rotpoint,xp,yp,zp,'x',-theta,xpp,ypp,zpp
         rotpoint,xpp,ypp,zpp,'y',phi,xppp,yppp,zppp

         ; Compute dot products with viewpoint and illumination directions
         mu  = zppp*xv + xppp*yv + yppp*zv
         smu = zppp*xs + xppp*ys + yppp*zs
         g = acos(xv*xs + yv*ys + zv*zs)

         ; Compute map coordinates for each surface pixel
         colat = acos(yppp)
         lon   = atan(xppp,zppp)

         ; Convert negative longitudes to positive.
         zl = where(lon lt 0,count)
         if count ne 0 then lon[zl]=lon[zl]+!pi*2.0

         ; Convert from lat,lon to indicies into the map, these arrays now point
         ;   to the visbile pixels, but they are not necessarily illuminated.
         xidx = long(lon/!pi/2.0*nlon)
         yidx = long(colat/!pi*nlat)

         ; colat can be exactly !pi, must map that back down one pixel
         zc = where(yidx eq nlat, count)
         if count ne 0 then yidx[zc]=nlat-1

         ; Find only those pixels that are illuminated
         ill = where(smu gt 0.,ill_count)

         ; compute image pixels for relevant reflectance "law"
         if ill_count gt 0 then begin

            ; Linear limb darkening case (Marcialis)
            if keyword_set(xlimb) then begin
               if not silent then print,'Linear limb darkening'
               image[zon[ill]] = map[xidx[ill],yidx[ill]]*(1.0-xlimb+xlimb*mu[ill])

            ; Minnaert limb darkening
            endif else if keyword_set(k) then begin
               if not silent then print,'Minnaert limb darkening'
               image[zon[ill]] = map[xidx[ill],yidx[ill]]*mu[ill]^(2*k-1.0)

            ; Hapke scattering function.
            endif else if keyword_set(hapke) then begin
               if not silent then print,'Hapke theory used.'
               image[zon[ill]] = bidr(map[xidx[ill],yidx[ill]],mu[ill],smu[ill],g[ill], $
                                      hapke[0],hapke[1])*!pi

            ; Hapke scattering function, second generation.
            endif else if keyword_set(hapke2) then begin
               if not silent then print,'Hapke theory used, new formalism.'
               image[zon[ill]] = bidr2(map[xidx[ill],yidx[ill]],mu[ill],smu[ill],g[ill], $
                                       hapke2[0],hapke2[1],hapke2[2],hapke2[3],h93=h93)*!pi

            ; No limb darkening at all, just wrap the map as is.
            endif else begin
               if not silent then print,'No limb darkening used.'
               image[zon[ill]] = map[xidx[ill],yidx[ill]]
            endelse

            if typecode ne 0 then begin
               geom = [xidx[ill], $
                       yidx[ill], $
                       mu[ill], $
                       smu[ill], $
                       g[ill], $
                       zon[ill]    ]
            endif

         endif else begin
            geom=0
         endelse

      endif ; end of main computation block

      ; Now, we need to modify any pixel whose center falls on the disk but
      ;  is not totally enclosed within the disk.  These pixels will have
      ;  their flux reduced by the fraction of their area not on the disk.

      rdiff = (sqrt(rsq) - 1.0) * radius/scale
      zlimb = where(rdiff le 0.0 and rdiff gt -1.0/sqrt(2.0), count)

      ; zlimb is now the list of pixels that involve the limb and whose
      ;  centers are inside the disk.  These are easy becuase there is
      ;  already a flux calculation image and we just need to adjust that
      ;  flux by the included area of the disk in the pixel.
      if count ne 0 then begin
         factor = pixwt(0.0,0.0,radius/scale, $
                         xarr[zlimb]*radius/scale,yarr[zlimb]*radius/scale)
         image[zlimb] *= factor
      endif

      ; The next bit is harder.  We need to get a list of pixels whose centers
      ;   fall outside the disk but whose corners can come inside.  Here we
      ;   must use an approximation to figure out what the flux should be
      ;   before applying the pixel inclusion factor.
      zon = where(rdiff lt 1.0/sqrt(2.0) and rdiff gt 0.0, count_zon)
      if count_zon ne 0 then begin
         xt1 = xarr[zon]
         yt1 = yarr[zon]
         rt1 = sqrt(rsq[zon])
         x = xt1/rt1
         y = yt1/rt1
         z = replicate(0.0,count_zon)
         ; Rotate the surface vectors to the viewpoint
         rotpoint,x,y,z,'z',-pole,xp,yp,zp
         rotpoint,xp,yp,zp,'x',-theta,xpp,ypp,zpp
         rotpoint,xpp,ypp,zpp,'y',phi,xppp,yppp,zppp
         ; Compute dot products with viewpoint and illumination directions
         mu  = zppp*xv + xppp*yv + yppp*zv
         smu = zppp*xs + xppp*ys + yppp*zs
         g = acos(xv*xs + yv*ys + zv*zs)
         ; Compute map coordinates for each surface pixel
         colat = acos(yppp)
         lon   = atan(xppp,zppp)
         ; Convert negative longitudes to positive.
         zl = where(lon lt 0,count)
         if count ne 0 then lon[zl]=lon[zl]+!pi*2.0
         ; Convert from lat,lon to indicies into the map, these arrays now point
         ;   to the visbile pixels, but they are not necessarily illuminated.
         xidx = long(lon/!pi/2.0*nlon)
         yidx = long(colat/!pi*nlat)
         ; colat can be exactly !pi, must map that back down one pixel
         zc = where(yidx eq nlat, count)
         if count ne 0 then yidx[zc]=nlat-1
         ; Find only those pixels that are illuminated
         ill = where(smu gt 0.,ill_count)
         ; compute image pixels for relevant reflectance "law"
         if ill_count gt 0 then begin
            ; Linear limb darkening case (Marcialis)
            if keyword_set(xlimb) then begin
               if not silent then print,'Linear limb darkening'
               image[zon[ill]] = map[xidx[ill],yidx[ill]]*(1.0-xlimb+xlimb*mu[ill])
            ; Minnaert limb darkening
            endif else if keyword_set(k) then begin
               if not silent then print,'Minnaert limb darkening'
               image[zon[ill]] = map[xidx[ill],yidx[ill]]*mu[ill]^(2*k-1.0)
            ; Hapke scattering function.
            endif else if keyword_set(hapke) then begin
               if not silent then print,'Hapke theory used.'
               image[zon[ill]] = bidr(map[xidx[ill],yidx[ill]],mu[ill],smu[ill],g[ill], $
                                      hapke[0],hapke[1])*!pi
            ; Hapke scattering function, second generation.
            endif else if keyword_set(hapke2) then begin
               if not silent then print,'Hapke theory used, new formalism.'
               image[zon[ill]] = bidr2(map[xidx[ill],yidx[ill]],mu[ill],smu[ill],g[ill], $
                                       hapke2[0],hapke2[1],hapke2[2],hapke2[3],h93=h93)*!pi
            ; No limb darkening at all, just wrap the map as is.
            endif else begin
               if not silent then print,'No limb darkening used.'
               image[zon[ill]] = map[xidx[ill],yidx[ill]]
            endelse
         endif

         factor = pixwt(0.0,0.0,radius/scale, $
                         xarr[zon]*radius/scale,yarr[zon]*radius/scale)
         image[zon] *= factor

      endif

   endif else begin ; GEOM has geometry

      illc = n_elements(geom)/6
      xidx = 0
      yidx = 1
      mu   = 2
      smu  = 3
      g    = 4
      zon  = 5

      ; compute image pixels for relevant reflectance "law"
      ; Linear limb darkening case (Marcialis)
      if keyword_set(xlimb) then begin
         if not silent then print,'Linear limb darkening'
         image[geom[zon*illc:(zon+1)*illc-1]] = $
            map[geom[xidx*illc:(xidx+1)*illc-1], $
                geom[yidx*illc:(yidx+1)*illc-1]] * $
            (1.0-xlimb+xlimb*geom[mu*illc:(mu+1)*illc-1])

      ; Minnaert limb darkening
      endif else if keyword_set(k) then begin
         if not silent then print,'Minnaert limb darkening'
         image[geom[zon*illc:(zon+1)*illc-1]] = $
            map[geom[xidx*illc:(xidx+1)*illc-1], $
                geom[yidx*illc:(yidx+1)*illc-1]] * $
            geom[mu*illc:(mu+1)*illc-1]^(2*k-1.0)

      ; Hapke scattering function.
      endif else if keyword_set(hapke) then begin
         if not silent then print,'Hapke theory used.'
         image[geom[zon*illc:(zon+1)*illc-1]] = $
            bidr(map[geom[xidx*illc:(xidx+1)*illc-1], $
                     geom[yidx*illc:(yidx+1)*illc-1]], $
                 geom[mu*illc:(mu+1)*illc-1], $
                 geom[smu*illc:(smu+1)*illc-1], $
                 geom[g*illc:(g+1)*illc-1], $
                 hapke[0],hapke[1])*!pi

      ; Hapke scattering function, second generation.
      endif else if keyword_set(hapke2) then begin
         if not silent then print,'Hapke theory used, new formalism.'
         image[geom[zon*illc:(zon+1)*illc-1]] = $
            bidr2(map[geom[xidx*illc:(xidx+1)*illc-1], $
                      geom[yidx*illc:(yidx+1)*illc-1]], $
                  geom[mu*illc:(mu+1)*illc-1], $
                  geom[smu*illc:(smu+1)*illc-1], $
                  geom[g*illc:(g+1)*illc-1], $
                  hapke2[0],hapke2[1],hapke2[2],hapke2[3],h93=h93)*!pi

      ; No limb darkening at all, just wrap the map as is.
      endif else begin
         if not silent then print,'No limb darkening used.'
         image[geom[zon*illc:(zon+1)*illc-1]] = $
            map[geom[xidx*illc:(xidx+1)*illc-1], $
                geom[yidx*illc:(yidx+1)*illc-1]]
      endelse

   ENDELSE

   ; Display final image.
   if not nodisplay then begin
      setwin,0,xsize=long(nx),ysize=long(ny)
      tvscl,image
   endif

end
