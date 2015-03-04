;+
; NAME:
;  frmdxdy
; PURPOSE:
;  Given two lists of source on field, find the dx,dy offset between lists.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  frmdxdy,x1,y1,x2,y2,xoff,yoff,error
; INPUTS:
;  x1 - X coordinate from list 1, in pixels.
;  y1 - Y coordinate from list 1, in pixels.
;  x2 - X coordinate from list 2, in pixels.
;  y2 - Y coordinate from list 2, in pixels.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  NX - maximum extent in X to consider (default is max([x1,x2]))
;  NY - maximum extent in Y to consider (default is max([y1,y2]))
;  MAXERR - maximum error allowed in initial spread test of position.
;              (default=3)
;  FNDRAD - Size of the aperture used for the final offset measurement.
;              DEFAULT VALUE = 12 pixels
;           The default is provided based on its historical value.  In most
;              cases this appears to work pretty well and should generally
;              be left alone.  However, some data have been seen to get
;              confused with a value that is this big.  Changing this value
;              will require knowing a better value for a specific dataset.
;
; OUTPUTS:
;  xoff - X offset (2-1) between positions in each list.
;  yoff - Y offset (2-1) between positions in each list.
;  error - Code, set if something went wrong in correlating the lists.
;            0 - everything appears to be good.
;            1 - failure during input validation
;            2 - initial x offset is too big (>maxerr)
;            3 - initial y offset is too big (>maxerr)
;            4 - correlation spot has negative "flux" or fwhm
;            5 - Final pass on x offset excluded all points in robomean
;            6 - Final pass on y offset excluded all points in robomean
;            7 - All of the final pass x offsets were bigger than 1.5*xsize
; KEYWORD OUTPUT PARAMETERS:
;    FOM - Figure of merit, a number than can be used (differentially) to
;           measure how good the spatial correlation is.  This number is
;           approximately the fraction of objects in the shortest list that
;           ended up spatially correlated.  A number close to 1 should be
;           good.
;   INDEX- index into list 2 for points in list1, ie, list2[index[i]] is the
;         closest, or one of a group of closest points, in list 2 to the
;         ith element of list 1, given the xoff, yoff determined.
;         If SPATIAL is specified, elements of the index may be invalidated
;         by setting to -1- these represent invalid matches from list 1.
;         On return, if error is set, the index output should be ignored.
; SPATIAL- Filtering parameters to frmdxdy - a vector or scalar of either 1 
;           or 2 elements- the first is the max distance in pixels from the
;           mean correlation dx,dy for a match to be valid, and the 2nd is
;           the maximum threshold in sigma from the mean correlation dx,dy
;           for a match to be valid. If SPATIAL is specified, invalid matches
;           will be excluded (via an initial distance test and robomean) from 
;           both the IDX 
;           outputs and the final dx,dy result- if not specified, the distance
;           and sigma criteria will be defaulted by frmdxdy; in this case, 
;           invalid matches will be excluded from the final dx,dy result but
;           INCLUDED in the IDX output. Note that if only the first element of
;           SPATIAL is specified, the second is defaulted to 3.0. The default
;           assumed for spatial[0] is 3.0.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
; It is conventional (and more optimal speedwise) for list 1 to be the shorter
; of the two lists Success is independent of the order in which lists 
; are presented, although if list 1 is longer than list 2, the index generated
; will not be unique (many->1).
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  99/03/22, Written by Marc W. Buie, Lowell Observatory
;  2005/06/21, MWB, changed called to robomean to trap errors.
;  2007/11/21, MWB, merged with alternate code buried in astrom.pro
;  2009/07/23, MWB, modified so that x,y input arrays do not have to be
;                    positivie definite.
;  2009/07/24, MWB, added XOUT,YOUT optional output.
;  2010/02/14, MWB, merged with alternate version from Peter Collins, this
;                     brings in the INDEX and SPATIAL keywords.
;  2010/07/19, MWB, minor tweak to ensure that the error flag is set for
;                     all cases of premature return.  Added FNDRAD keyword.
;-
pro frmdxdy,in_x1,in_y1,in_x2,in_y2,xoff,yoff,error, $
      NX=nx,NY=ny,MAXERR=maxerr,FNDRAD=fndrad, $
      FOM=fom,XOUT=xout,YOUT=yout,INDEX=index,SPATIAL=spatial

   error=1

   self='FRMDXDY: '
   if badpar(in_x1,[2,3,4,5],1,caller=self+'(x1) ') then return
   if badpar(in_y1,[2,3,4,5],1,caller=self+'(y1) ') then return
   if badpar(in_x2,[2,3,4,5],1,caller=self+'(x2) ') then return
   if badpar(in_y2,[2,3,4,5],1,caller=self+'(y2) ') then return

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
   if badpar(spatial,[4,5,0],[0,1],caller=self+'(SPATIAL) ', $
                npts=nspatial) then return
   if badpar(fndrad,[0,2,3,4,5],0,caller=self+'(FNDRAD) ', $
                default=12.0) then return

   if nspatial gt 2 then begin
      print,self,'SPATIAL keyword accepts at most 2 parameters.'
      return
   endif

   n1 = n_elements(x1)
   n2 = n_elements(x2)
   normfac = float(min([n1,n2]))
   index=lonarr(n1)
   if nspatial lt 2 then sigthresh = 3.0 else sigthresh = spatial[1]
   if nspatial lt 1 then drthresh = 3.0 else drthresh = spatial[0]*spatial[0]

   ; The first step is to generate a cross-correlation image and find the
   ;    peak, this gives a crude offset.
   dxdy=intarr(nx,ny)
   for i=0L,n1-1 do begin
      dx = fix(x2-x1[i]+0.5+nx/2.0)
      dy = fix(y2-y1[i]+0.5+ny/2.0)
      zd=where(dx ge 0 and dx lt nx and $
               dy ge 0 and dy lt ny,countzd)
      if countzd gt 0 then begin
         dxdy[dx[zd],dy[zd]]=dxdy[dx[zd],dy[zd]]+1
      endif
   endfor
   zd=where(dxdy ge max(dxdy)-1,countmax)
   xoff=(zd mod nx)-nx/2.0
   yoff=zd/nx-ny/2.0
   if countmax gt 1 then begin
      if max(xoff)-min(xoff) gt maxerr then begin
         fom = 0.
         error=2
         xoff=0.
         yoff=0.
         return
      endif
      if max(yoff)-min(yoff) gt maxerr then begin
         fom = 0.
         xoff=0.
         yoff=0.
         error=2
         return
      endif
      xoff = mean(xoff)
      yoff = mean(yoff)
   endif else begin
      xoff=xoff[0]
      yoff=yoff[0]
   endelse

   basphote,1,dxdy,1.,xoff+nx/2.0,yoff+ny/2.0,fndrad, $
      fndrad+10,fndrad+40,/nolog,/silent,xcen=nxc,ycen=nyc, $
      fwhm=fwhm,rdnoise=0.1,flux=flux,flerr=flerr,skymean=sky,skysig=skysig
   nxc = nxc - nx/2.0
   nyc = nyc - ny/2.0

   if flux le 0. or fwhm le 0. then begin
      error=4
;print,xoff+nx/2.0,yoff+ny/2.0,nxc,nyc,fwhm,flux,sky
;itool,dxdy,/block
      fom = 0.0
   endif else begin
      error=0
      fom = flux/fwhm/normfac
      xoff = nxc
      yoff = nyc

      ; Using the rough offset, compute a good offset.
      gdx=fltarr(n1)
      gdy=fltarr(n1)
      for i=0L,n1-1 do begin
         tdx=(x2-xoff)-x1[i]
         tdy=(y2-yoff)-y1[i]
         tdr=tdx^2+tdy^2
         zt=where(tdr eq min(tdr))
         zt=zt[0]
         index[i]=zt
         if tdr[zt] lt drthresh then begin
            gdx[i]=x2[zt[0]]-x1[i]
            gdy[i]=y2[zt[0]]-y1[i]
         endif else begin
            gdx[i]=nx*2
            gdy[i]=ny*2
         endelse
      endfor
      zt=where(gdx lt nx*1.5,countzt,COMPLEMENT=unzt)
      if countzt ne 0 then begin
         badx = bytarr(countzt)
         robomean,gdx[zt],sigthresh,0.5,avgxoff,error=xerror,BAD=badx
         if xerror then begin
            error=5
            return
         endif
         bady = bytarr(countzt)
         robomean,gdy[zt],sigthresh,0.5,avgyoff,error=yerror,BAD=bady
         if yerror then begin
            error=6
            return
         endif
      endif else begin
         error = 7
         fom = 0.0
         return
      endelse

      xoff = avgxoff
      yoff = avgyoff

      xout = in_x1 + xoff
      yout = in_y1 + yoff

      znomat=where(badx ne 0 or bady ne 0,nnomat)
      if nspatial gt 0 then begin
         if countzt ne n1 then index[unzt] = -1
         if nnomat gt 0 then index[zt[znomat]] = -1
      endif

   endelse

end

