;+
; NAME:
;  msrcor
; PURPOSE:   (one line only)
;  Spatial correlation souce positions found in multiple lists
; DESCRIPTION:
;  This routine is a conceptual extension of the Astronomy Users Library
;    routine, srcor.pro.  This extension of this routine is that you
;    can correlate more than two lists of positions.
;  Each call of this routine will serve to add one list to the description
;    of the union of all the prior lists.  The lists are copied into the
;    information collected about the set of lists so you don't need to
;    maintain a copy of the original lists.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  msrcor,set,x,y,dcr
; INPUTS:
;  set - This is the description of the set of all lists seen thus far.
;          There are three ways to signal the first call to this routine.
;          1) set is undefined
;          2) set is not a structure
;          3) set is a valid structure but set.nlists is equal to zero.
;        Note that there is absolutely no performance advantage to maintaining
;          a previous structure and then setting set.nlists=0.  If you want
;          to restart it is just as effective to simply say set=0 prior to
;          an initilizing call.
;        If the structure exists already and set.nlists!=0 then the information
;          will be added to and the anonymous structure will be modified
;          before returning.
;  x,y - Array of x and y coordinates for a new list to add to the set.
;          The order in which these array are successively presented to msrcor
;          will determine the ordinal number of the list within the set.
;  dcr - Critical radious outside which correlations are rejected.
;          x,y,dcr can be in any units that make sense to your problem but
;          they must all be the same units.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  set - Information structure, the following tags are defined:
;         x - X position of source (all lists are concatenated here)
;         y - Y position of source (all lists are concatenated here)
;         objid - vector that matches the length of x and y.  These values
;                   are unique object id numbers, starting at 0.
;         objcnt - vector that indicates how many times each object
;                     appears in the lists (<= nlists)
;         lidx   - vector (same length as x,y) that indicates which list
;                     the position appears in.
;         nobj   - scalar, number of unique objects found.
;         nlists - scalar, total number of lists included.
;
;  There may be other related information you might like to keep along with
;    this information, such as magnitude or FWHM.  If you concatentate
;    everything in list order you will have a vector whose structure matches
;    set.x and set.y and the indexing for all will be the same.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2008/06/18, Written by Marc W. Buie with algorithmic input from Leslie Young,
;     Southwest Research Institute.
;-
pro msrcor,set,in_x,in_y,dcr

   self='MSRCOR: '
   if badpar(set,[0,1,2,3,8],[0,1],caller=self+'(set) ',type=stype) then return
   if badpar(in_x,[2,3,4,5],1,caller=self+'(x) ',npts=nx) then return
   if badpar(in_y,[2,3,4,5],1,caller=self+'(y) ',npts=ny) then return
   if badpar(dcr,[2,3,4,5],0,caller=self+'(dcr) ') then return

   if nx ne ny then begin
      print,self,' Error!  Input x and y vectors must be the same length. Aborting.'
      return
   endif

   if stype eq 8 then begin
      if set.nlists eq 0 then newstart=1 else newstart=0
   endif else begin
      newstart=1
   endelse

   if newstart then begin
      x = in_x
      y = in_y
      objid = lindgen(nx)
      objcnt = replicate(1,nx)
      lidx = replicate(0L,nx)
      nobj = nx
      nlists = 1
   endif else begin

      ; Here's the real work in this program.

      ; Step through the lists already in the set.  For each list, only
      ;  consider the objects in the new set that have NOT been matched up
      ;  yet.  Of course, that means we always check the entirety of the
      ;  new list against the very first list in the set.  After that
      ;  the amount checked against subsequent lists should dwindle (or
      ;  disappear completely).  Anything that isn't matched after all old
      ;  lists are checked become new object ids that have no links.

      ; This array keeps track of the object id number as we proceed.
      ;   The starting value indicates this object hasn't been linked up yet.
      objid = replicate(-1,nx)

      for i=0,set.nlists-1 do begin

         ; Find the unmatched objects from the new list.  Exit loop if none.
         znew = where(objid eq -1, countnew)
         if countnew eq 0 then break

         ; list 1 (from new list)
         x1 = in_x[znew]
         y1 = in_y[znew]

         ; list 2 (from old lists)
         z = where(set.lidx eq i,count)
         x2 = set.x[z]
         y2 = set.y[z]

         srcor,x1,y1,x2,y2,dcr,ind1,ind2,option=1

         if ind1[0] ne -1 then $
            objid[znew[ind1]] = set.objid[z[ind2]]

      endfor

      ; deal with the unmatched objects at the end
      objcnt = set.objcnt
      z=where(objid eq -1,count)
      if count ne 0 then begin
         objid[z] = lindgen(count)+set.nobj
         objcnt = [objcnt,replicate(0,count)]
      endif

      objcnt[objid] += 1

      x = [set.x,in_x]
      y = [set.y,in_y]
      objid = [set.objid,objid]
      lidx = [set.lidx,replicate(set.nlists,nx)]
      nobj = set.nobj + count
      nlists = set.nlists+1
   endelse

   set = { $
      x: x, $
      y: y, $
      objid: objid, $
      objcnt: objcnt, $
      lidx: lidx, $
      nobj: nobj, $
      nlists: nlists $
      }

end
