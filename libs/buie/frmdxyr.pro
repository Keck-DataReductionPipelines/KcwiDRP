;+
; NAME:
;  frmdxyr
; PURPOSE:
;  Given two lists of source on field, find the dx,dy,dr offset between lists.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  frmdxyr,x1,y1,x2,y2,thetamin,thetamax,dtheta,xoff,yoff,theta,error
; INPUTS:
;  x1 - X coordinate from list 1, in pixels.
;  y1 - Y coordinate from list 1, in pixels.
;            The first list is the one that is manipulated to get a match.
;            The second list is not touched.
;  x2 - X coordinate from list 2, in pixels.
;  y2 - Y coordinate from list 2, in pixels.
;  thetamin - Minimum angle in search range (degrees)
;  thetamax - Maximum angle in search range (degrees)
;  dtheta   - Angle step size (degrees).  A 0.1 degree step size is fine
;               for 2048x2048 data but smaller steps sizes may be necessary
;               for larger arrays.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  NX - maximum extent in X to consider (default is max([x1,x2]))
;  NY - maximum extent in Y to consider (default is max([y1,y2]))
;        NOTE: if either list of  x,y are negative, they are first shifted
;        so that nothing is negative.  The default will make the center of
;        the union of the lists be the center of rotation.
;  MAXERR - maximum error allowed in initial spread test of position.
;              (default=3)
;  SILENT - Flag, if set will suppress all printed output to the screen
;  TOLERANCE - tolerance on the best angle (degrees).  Default=0.01 deg.
;
; OUTPUTS:
;  xoff - X offset (2-1) between positions in each list.
;  yoff - Y offset (2-1) between positions in each list.
;  theta - Angle offset (2-1) between positions in each list (degrees).
;  error - Flag, set if something went wrong in correlating the lists.
; KEYWORD OUTPUT PARAMETERS:
;  FOM - Figure of merit, a number than can be used (differentially) to
;           measure how good the spatial correlation is.  This number is
;           approximately the fraction of objects in the shortest list that
;           ended up spatially correlated.  A number close to 1 should be
;           good.
;  INDEX- index into list 2 for points in list1, ie, list2[index[i]] is the
;         closest, or one of a group of closest points, in list 2 to the
;         ith element of list 1, given the xoff, yoff determined.
;  SPATIAL- spatial filtering parameters, passed transparently to FRMDXDY.
;  XOUT - The shifted and rotated X position from the best match.
;  YOUT - The shifted and rotated X position from the best match.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
; It is conventional (and more optimal speedwise) for list 1 to be the shorter
; of the two lists. However success is indepenedent of the order in which lists 
; are presented.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2007/11/28, Written by Marc W. Buie, Lowell Observatory
;  2009/07/23, MWB, modified so that x,y input arrays do not have to be
;                    positive definite.
;  2009/07/24, MWB, added XOUT,YOUT optional output.
;  2010/02/14, MWB, merged with alternate version from Peter Collins, this
;                     change brings in the INDEX and SPATIAL keywords.
;  2010/07/19, MWB, cleanup of error reporting output variable
;  2010/11/30, MWB, fixed busted internal check that made routine never succeed
;-
pro frmdxyr,in_x1,in_y1,in_x2,in_y2,thetamin,thetamax,dtheta, $
            xoff,yoff,theta,error, $
            NX=nx,NY=ny,MAXERR=maxerr,SILENT=silent,TOLERANCE=tolerance, $
            FOM=fom,XOUT=xout,YOUT=yout,INDEX=index,SPATIAL=spatial

   error=1
   self='FRMDXYR: '
   if badpar(in_x1,[2,3,4,5],1,caller=self+'(x1) ') then return
   if badpar(in_y1,[2,3,4,5],1,caller=self+'(y1) ') then return
   if badpar(in_x2,[2,3,4,5],1,caller=self+'(x2) ') then return
   if badpar(in_y2,[2,3,4,5],1,caller=self+'(y2) ') then return

   if badpar(thetamin,[2,3,4,5],0,caller=self+'(thetamin) ') then return
   if badpar(thetamax,[2,3,4,5],0,caller=self+'(thetamax) ') then return
   if badpar(dtheta,[2,3,4,5],0,caller=self+'(dtheta) ') then return

   minx = min([in_x1,in_x2])
   miny = min([in_y1,in_y2])
   x1 = in_x1-minx
   y1 = in_y1-miny
   x2 = in_x2-minx
   y2 = in_y2-miny

   if badpar(nx,[0,2,3],0,caller=self+'(NX) ', $
                default=long(max([x1,x2]))) then return
   if badpar(ny,[0,2,3],0,caller=self+'(NY) ', $
                default=long(max([y1,y2]))) then return
   if badpar(maxerr,[0,2,3,4,5],0,caller=self+'(MAXERR) ', $
                default=3.0) then return
   if badpar(tolerance,[0,2,3,4,5],0,caller=self+'(TOLERANCE) ', $
                default=0.01) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ', $
                default=0) then return

   ; This is the initial coarse sweep.  It is hoped that this will bound
   ;  the problem.
   spatial2 = [4.0, 3.0]
   if n_elements(spatial) eq 0 then spatial=spatial2
   npts = ceil((thetamax-thetamin)/dtheta)
   angle = findgen(npts)*dtheta + thetamin
   xoff  = fltarr(npts)
   yoff  = fltarr(npts)
   fom   = fltarr(npts)
   verror = intarr(npts)
   if not silent then print,'Initial sweep'
   for i=0,npts-1 do begin
      angle_r = angle[i]/!radeg
      x1r =  (x1-nx/2)*cos(angle_r)+(y1-ny/2)*sin(angle_r) + nx/2
      y1r = -(x1-nx/2)*sin(angle_r)+(y1-ny/2)*cos(angle_r) + ny/2
      ; leave the spatial filter off in this phase.
      frmdxdy,x1r,y1r,x2,y2,xoff0,yoff0,error0,nx=nx,ny=ny,fom=fom0, $
              index=indx, SPATIAL=spatial2
      xoff[i] = xoff0
      yoff[i] = yoff0
      fom[i]  = fom0
      if i eq 0 then index = indx else if fom0 gt max(fom[0:i-1]) then index=indx
      verror[i] = error0
      if not silent then print,angle[i],xoff[i],yoff[i],fom[i]
   endfor

   ; Make sure something worked here, at least one call must not return error
   if min(verror) eq 1 then begin
      xoff=0.
      yoff=0.
      theta=0.
      return
   endif

   ; Find the best angle
   z=where(fom eq max(fom))
   ibest = z[0]

   ; If this is at the edge, quit with error
   if ibest eq 0 or ibest eq npts-1 then begin
      xoff=0.
      yoff=0.
      theta=0.
      return
   endif

   ; set the boundaries
   angle  = angle[ibest+[-1,0,1]]
   fom    = fom[ibest+[-1,0,1]]
   xoff   = xoff[ibest+[-1,0,1]]
   yoff   = yoff[ibest+[-1,0,1]]
   delta = dtheta

   index3 = lonarr(3,n_elements(x1))
   index3[0,*] = index
   index3[1,*] = index
   index3[2,*] = index

   ; Now do a binary search until tolerance reached
   while delta gt tolerance do begin
      if fom[0] ge fom[2] then begin
         angle[2] = angle[1]
         fom[2] = fom[1]
         xoff[2] = xoff[1]
         yoff[2] = yoff[1]
         index3[2,*]=index3[1,*]
      endif else begin
         angle[0] = angle[1]
         fom[0] = fom[1]
         xoff[0] = xoff[1]
         yoff[0] = yoff[1]
         index3[0,*]=index3[1,*]
      endelse
      delta /= 2.0
      angle[1] = (angle[0]+angle[2])/2.0
      angle_r = angle[1]/!radeg
      x1r =  (x1-nx/2)*cos(angle_r)+(y1-ny/2)*sin(angle_r) + nx/2
      y1r = -(x1-nx/2)*sin(angle_r)+(y1-ny/2)*cos(angle_r) + ny/2
      frmdxdy,x1r,y1r,x2,y2,xoff0,yoff0,error0,nx=nx,ny=ny,fom=fom0, $
              SPATIAL=spatial,INDEX=indx
      index3[1,*]=indx
      xoff[1] = xoff0
      yoff[1] = yoff0
      fom[1]  = fom0
      if not silent then print,angle[1],xoff[1],yoff[1],fom[1]
   endwhile

   z=where(fom eq max(fom))
   z=z[0]

   xoff=xoff[z]
   yoff=yoff[z]
   theta=angle[z]
   fom=fom[z]
   index[*] = index3[z[0],*]
   error = fom le 0.0

   angle_r = angle[z]/!radeg
   xout =  (x1-nx/2)*cos(angle_r)+(y1-ny/2)*sin(angle_r) + nx/2 + minx + xoff
   yout = -(x1-nx/2)*sin(angle_r)+(y1-ny/2)*cos(angle_r) + ny/2 + miny + yoff

end

