;+
; NAME:
;   plotglob
; PURPOSE:
;   Plot a globe projection with latitude and longitude lines
; DESCRIPTION:
; CATEGORY:
;   Miscellaneous
; CALLING SEQUENCE:
;   plotglob,lon,lat,polang
; INPUTS:
;   lon    - East longitude of center of disk (degrees)
;   lat    - Latitude of center of disk (degrees)
;   polang - Position angle of North (CCW from +y, in degrees)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;   BORD     - Flag, if set, put a ticked border around plot.
;   DLAT     - Spacing between latitude lines (default=30 degrees)
;   DLON     - Spacing between longitude lines (default=30 degrees)
;   DP       - Angular step size for plot lines (default=5 degrees)
;   COLORS   - 4 element vector that specifies color indicies for
;                 various line elements in the plot.  The default
;                 is a white line for displays and a black line
;                 for postscript output.
;              colors[0] - Prime meridian and equator
;              colors[1] - 180 deg longitude meridian
;              colors[2] - Other grid lines
;              colors[3] - Limb
;   LINESTYLE - Linestyle for wire grid, default=0
;   NOERASE  - Flag, if true, suppresses initial erase of plotting
;              area before globe is drawn.  Used for overplotting
;              the grid on an image.
;   POSITION - Optional 4-element vector that sets location of
;              plot on the plotting device (this is a standard PLOT keyword)
;   DEVICE   - Flag, if true, means POSITION is in DEVICE units, otherwise,
;              POSITION is taken to be in normal units. (standard PLOT
;              keyword)
;   PAD      - Pad amount in the plot window.  The width of the plot is
;              2*(1+pad) where 1 is the radius of the globe.  The default
;              value is pad=0.15
;   NOLIMB   - Flag, if true, supresses plotting the limb of the globe.
;   LATPTS   - Latitude vector of positions to compute X,Y on plot for.
;   LONPTS   - Longitude vector of positions to compute X,Y on plot for.
;                 Both of LATPTS and LONPTS must be provided and must be the same length.
;                 If this condition is met, then XPTS,YPTS will contain the x,y positions
;                 of these lat/lon pairs on the plot.  If the condition is not met then
;                 any input through these keywords is silently ignored.
;
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;   XPTS     - X position of LATPTS/LONPTS input
;   YPTS     - Y position of LATPTS/LONPTS input
;   ZPTS     - Z position of LATPTS/LONPTS input (positive means it's on the visible part
;                 of the globe, negative means it is on the back side).
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   96/01/26, Written by Marc W. Buie, Lowell Observatory
;   96/02/12, MWB, added POSITION and DEVICE keyword support
;   96/05/10, MWB, added NOLIMB keyword
;   2002/03/15, MWB/Joel Parker, added BORD, LATPTS, LONPTS, XPTS, YPTS
;                  keywords.  Also, the /ISOTROPIC keyword is set on the
;                  initial plot call.
;   2009/01/16, MWB, added LINESTYLE keyword
;-
pro plotglob,plon0,plat0,p0, $
      DLAT=dlat,DLON=dlon,DP=dp,COLORS=colors,NOERASE=noerase, $
      POSITION=position,DEVICE=device,PAD=pad,NOLIMB=nolimb, $
      LONPTS=lonpts,LATPTS=latpts,XPTS=xpts,YPTS=ypts,ZPTS=zpts,BORD=bord, $
      LINESTYLE=linestyle

   if !d.name eq 'PS' then defcolor=0 else defcolor=!d.n_colors-1

   if badpar(dlat,[0,2,3,4,5],0,CALLER='PLOTGLOB: (dlat) ',default=30.0) then return
   if badpar(dlon,[0,2,3,4,5],0,CALLER='PLOTGLOB: (dlon) ',default=30.0) then return
   if badpar(dp,[0,2,3,4,5],0,CALLER='PLOTGLOB: (dp) ',default=5.0) then return
   if badpar(pad,[0,2,3,4,5],0,CALLER='PLOTGLOB: (pad) ',default=0.15) then return
   if badpar(noerase,[0,2,3],0,CALLER='PLOTGLOB: (noerase) ',default=0) then return
   if badpar(colors,[0,2,3],1,CALLER='PLOTGLOB: (colors) ', $
         default=replicate(defcolor,4)) then return
   if badpar(device,[0,2,3],0,CALLER='PLOTGLOB: (device) ',default=0) then return
   if badpar(position,[0,2,3,4,5],1,CALLER='PLOTGLOB: (position) ',default=-1) then return
   if badpar(nolimb,[0,2,3],0,CALLER='PLOTGLOB: (nolimb) ',default=0) then return

   if badpar(latpts,[0,2,3,4,5],1,CALLER='PLOTGLOB: (LATPTS) ',npts=nlatpts) then return
   if badpar(lonpts,[0,2,3,4,5],1,CALLER='PLOTGLOB: (LONPTS) ',npts=nlonpts) then return
   if badpar(linestyle,[0,1,2,3],0,CALLER='PLOTGLOB: (LINESTYLE) ',default=0) then return

   npts = ceil(360.0/dp)

   nlon = fix(360.0/dlon+0.5)+1
   nlat = fix(180.0/dlat+0.5)
   dolon=(findgen(nlon)*dlon)/!radeg
   dolat=((findgen(nlat)-nlat/2)*dlat)/!radeg

   if keyword_set(bord) then sty=1 else sty=5
   if n_elements(position) ne 4 then begin
      plot,[0],[1],/nodata,xr=[-1.0-pad,1.0+pad],yr=[-1.0-pad,1.0+pad], $
         xstyle=sty,ystyle=sty,xmargin=[0,0],ymargin=[0,0],noerase=noerase, $
         linestyle=linestyle,/iso
   endif else begin
      plot,[0],[1],/nodata,xr=[-1.0-pad,1.0+pad],yr=[-1.0-pad,1.0+pad], $
         xstyle=sty,ystyle=sty,xmargin=[0,0],ymargin=[0,0],noerase=noerase, $
         position=position,device=device,/iso,linestyle=linestyle
   endelse

   lon = findgen(npts+1)/npts*2.0*!pi

   ; Plot the lines of latitude
   for i=0,n_elements(dolat)-1 do begin
      lat = !pi/2.0 - dolat[i]
      x = cos(lon)*sin(lat)
      y = sin(lon)*sin(lat)
      z = replicate(cos(lat),n_elements(lon))
      rotpoint,x,y,z,'z',-plon0,xp,yp,zp,/deg
      rotpoint,xp,yp,zp,'y',plat0,xpp,ypp,zpp,/deg
      rotpoint,xpp,ypp,zpp,'x',p0,xppp,yppp,zppp,/deg
      notvis=where(xppp lt 0)
      if (notvis[0] ne -1) then zppp[notvis] = 10.0
      oplot,yppp,zppp,max_value=2.0,color=colors[2],linestyle=linestyle
   endfor

   ; Plot the equator.
   lat = !pi/2.0
   x = cos(lon)*sin(lat)
   y = sin(lon)*sin(lat)
   z = replicate(cos(lat),n_elements(lon))
   rotpoint,x,y,z,'z',-plon0,xp,yp,zp,/deg
   rotpoint,xp,yp,zp,'y',plat0,xpp,ypp,zpp,/deg
   rotpoint,xpp,ypp,zpp,'x',p0,xppp,yppp,zppp,/deg
   notvis=where(xppp lt 0)
   if (notvis[0] ne -1) then zppp[notvis] = 10.0
   oplot,yppp,zppp,max_value=2.0,thick=2.0,color=colors[0],linestyle=linestyle

   lat = findgen(101)/100.0*!pi

   ; Plot the lines of longitude.
   for i=0,n_elements(dolon)-1 do begin
      x = cos(dolon[i])*sin(lat)
      y = sin(dolon[i])*sin(lat)
      z = cos(lat)
      rotpoint,x,y,z,'z',-plon0,xp,yp,zp,/deg
      rotpoint,xp,yp,zp,'y',plat0,xpp,ypp,zpp,/deg
      rotpoint,xpp,ypp,zpp,'x',p0,xppp,yppp,zppp,/deg
      notvis=where(xppp lt 0)
      if (notvis[0] ne -1) then zppp[notvis] = 10.0
      oplot,yppp,zppp,max_value=2.0,color=colors[2],linestyle=linestyle
   endfor

   ; Plot the prime meridian.
   npts=fix(npts/2)
   lat = findgen(npts+1)/npts*!pi
   x = cos(0.0)*sin(lat)
   y = sin(0.0)*sin(lat)
   z = cos(lat)
   rotpoint,x,y,z,'z',-plon0,xp,yp,zp,/deg
   rotpoint,xp,yp,zp,'y',plat0,xpp,ypp,zpp,/deg
   rotpoint,xpp,ypp,zpp,'x',p0,xppp,yppp,zppp,/deg
   notvis=where(xppp lt 0)
   if (notvis[0] ne -1) then zppp[notvis] = 10.0
   oplot,yppp,zppp,max_value=2.0,thick=2.0,color=colors[0],linestyle=linestyle

   ; Plot the 180 degree meridian.
   npts=fix(npts/2)
   lat = findgen(npts+1)/npts*!pi
   x = cos(!pi)*sin(lat)
   y = sin(!pi)*sin(lat)
   z = cos(lat)
   rotpoint,x,y,z,'z',-plon0,xp,yp,zp,/deg
   rotpoint,xp,yp,zp,'y',plat0,xpp,ypp,zpp,/deg
   rotpoint,xpp,ypp,zpp,'x',p0,xppp,yppp,zppp,/deg
   notvis=where(xppp lt 0)
   if (notvis[0] ne -1) then zppp[notvis] = 10.0
   oplot,yppp,zppp,max_value=2.0,thick=2.0,color=colors[1],linestyle=linestyle

   ; Plot the limb (if desired).
   if not nolimb then begin
      npts=npts*2
      angle = findgen(npts+1)/npts*2.0*!pi
      x = cos(angle)
      y = sin(angle)
      oplot,x,y,color=colors[3],linestyle=linestyle
   endif

   if nlonpts eq nlatpts and nlonpts gt 0 then begin
      ; Translate Lon/Lat points to X/Y.
      lon=lonpts/!radeg
      lat=(90.0-latpts)/!radeg
      x = cos(lon)*sin(lat)
      y = sin(lon)*sin(lat)
      z = cos(lat)
      rotpoint,x,y,z,'z',-plon0,xp,yp,zp,/deg
      rotpoint,xp,yp,zp,'y',plat0,xpp,ypp,zpp,/deg
      rotpoint,xpp,ypp,zpp,'x',p0,xppp,yppp,zppp,/deg
      xpts = yppp
      ypts = zppp
      zpts = xppp
   endif

end
