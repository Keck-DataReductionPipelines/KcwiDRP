;+
; NAME:
;  lclxtrem
;
; PURPOSE:
;  Find local minima or maxima in a 1-d vector.
;
; DESCRIPTION:
;  Scan a vector and identify all maxima or minima separated by more than
;    WIDTH values.
;
; CATEGORY:
;  Spectroscopy
;
; CALLING SEQUENCE:
;  idx=lclxtrem(vec,width,[/MAXIMA])
;
; INPUTS:
;  vec - Input vector of data points.
;
; OPTIONAL INPUT PARAMETERS:
;  width - size of zone to search, minima (or maxima) separated by less than
;            width are never returned.  (Default = 5)
;
; KEYWORD INPUT PARAMETERS:
;  MAXIMA  - Flag, if set, causes program to search for local maxima, the default
;            is to search for local minima
;
; OUTPUTS:
;  Returns indicies into vec that give local extrema.  This returned vector
;    is also sorted by decreasing absolute value of vec.
;
; KEYWORD OUTPUT PARAMETERS:
;  COUNT  - Number of extrema returned.  If 0, there was an error.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  The first and last points in the array will NEVER be flagged as extrema
;    since there is no information to decide if the endpoint is an inflection
;    or not.  There is one exception to this condition.  If the data are
;    smooth and there are no inflection points, one or the other of the
;    endpoints will be the absolute extreme.  In this case only, will an
;    endpoint be returned.
;
; PROCEDURE:
;  This procedure scans the vector and looks for local maxima or local minima.
;  In the first pass, all such extrema are flagged, if you want all of them
;  Just set width to 0 if you want _all_ local extrema.  The second pass
;  filters our extrema that are too close together.  The width of the filter
;  can be provided, the default is 5.  The extrema are scanned in decreasing
;  absolute value.  For each extremum, all other extrema that are fainter and
;  are closer than width away are removed from the list.  The returned vector
;  of indicies is sorted by decreasing size of the peak/trough.
;
; MODIFICATION HISTORY:
;  97/12/5, Written by Marc W. Buie, Lowell Observatory
;  99/11/24, MWB, total rewrite to speed it up and make it work properly.
;-
function lclxtrem,vec,in_width, $
                  MAXIMA=maxima,COUNT=count,POINT_ORDER=point_order

   count = 0
   if badpar(vec,[2,3,4,5],1,caller='LCLXTREM: (vec)',npts=n) then return,-1
   if badpar(in_width,[0,2,3,4,5],0,caller='LCLXTREM: (width)', $
                                    default=5) then return,-1
   if badpar(maxima,[0,1,2,3],0,caller='LCLXTREM: (MAXIMA)', $
                                    default=0) then return,-1
   if badpar(point_order,[0,1,2,3],0,caller='LCLXTREM: (POINT_ORDER)', $
                                    default=0) then return,-1

   ; make sure width is positive
   width = abs(in_width)

   ; Compute the first derivative of the vector
   vecp = vec[1:n-1] - vec[0:n-2]
   np = n - 1

   ; Collapse the derivative to just +1, 0, or -1
   vecps = intarr(np)
   z = where(vecp gt 0.0,count)
   if count ne 0 then vecps[z]=1
   z = where(vecp lt 0.0,count)
   if count ne 0 then vecps[z]=-1

   ; Compute the derivative of just the sign vectors (vecps)
   vecpps = vecps[1:np-1] - vecps[0:np-2]

   ; Keep the appropriate extremum
   if maxima then begin
      z = where(vecpps lt 0,nidx)
   endif else begin
      z = where(vecpps gt 0,nidx)
   endelse

   ; Create an index vector with just the good points.
   if nidx eq 0 then begin
      if maxima then begin
         idx = where(vec eq max(vec))
      endif else begin
         idx = where(vec eq min(vec))
      endelse
   endif else begin
      idx = z+1
   endelse

   ; Sort the extrema (actually, the absolute value)
   sidx = reverse(sort(abs(vec[idx])))

   ; Scan down the list of extrema, start with the brightest and take out
   ;   all extrema within width of the position.  Any that are too close should
   ;   be removed from further consideration.
   if width ge 1 then begin
      i=0
      nsidx=nidx
      while i lt nsidx-1 do begin
         z=where(abs(idx[sidx[i+1:nsidx-1]]-idx[sidx[i]]) le width,count)
         if count ne 0 then begin
            idx[sidx[z+i+1]] = -1
            z=where(idx[sidx] ge 0, nsidx)
            if nsidx eq 0 then return,-1 ; this should never happen
            sidx=sidx[z]
         endif
         i = i + 1
      endwhile
   endif

   ; The ones that survive are returned.
   count = nsidx
   if point_order then begin
      idx=idx[sidx]
      idx=idx[sort(idx)]
      return,idx
   endif else begin
      return,idx[sidx]
   endelse

end
