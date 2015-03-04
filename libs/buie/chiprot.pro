;+
; NAME:
;  chiprot
; PURPOSE:   (one line only)
;  Find the best rotation and offset for one chip against monolith coordinates
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  chiprot,astinfo,ref,xoffset,yoffset,rotang
; INPUTS:
;  astinfo - Anonymous structure that contains an astrometric description
;               of a image-based coordinate system and its mapping to the
;               celestial sphere.  (see mkastinfo for more information)
;  ref     - Anonymous structure that contains a set of measurements of
;               sources on an image with a corresponding linkage to
;               celestial coordinates (ra,dec).  This structure is normally
;               created by using rdref.pro.
;  xoffset - Initial guess of the X offset between the coordinates in ref
;               and the coordinates described in astinfo.
;  xoffset - Initial guess of the Y offset between the coordinates in ref
;               and the coordinates described in astinfo.
;  rotang  - Initial guess of the rotation between the coordinates in ref
;               and the coordinates described in astinfo.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  xoffset - Best fitting X offset
;  yoffset - Best fitting Y offset
;  rotang  - Best fitting rotation angle (radians)
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2010/01/13
;-
pro chiprot,astinfo,ref,xoffset,yoffset,rotang, $
       VERBOSE=verbose

   self='chiprot: '
   if badpar(astinfo,8,1,caller=self+'(astinfo) ') then return
   if badpar(ref,8,1,caller=self+'(ref) ') then return
   if badpar(xoffset,[2,3,4,5],0,caller=self+'(xoffset) ') then return
   if badpar(yoffset,[2,3,4,5],0,caller=self+'(yoffset) ') then return
   if badpar(rotang,[2,3,4,5],0,caller=self+'(rotang) ') then return
   if badpar(verbose,[0,1,2,3],0,caller=self+'(VERBOSE) ',default=0) then return

   ; using the provided solution, compute the predicted x,y of the input
   ;   set based on the ra,dec.
   astrd2xy,ref.ra,ref.dec,astinfo,xpred,ypred,/full

   xalpha=[1.0,0.0]
   yalpha=[0.0,1.0]
   ralpha=[1.0,1.0]

   todo='r'
   order=['x','y','r']
   nmax=50

   xchange=1
   ychange=1
   rchange=1

   for i=0,3*nmax-1 do begin

      todo = order[i mod 3]

      if todo eq 'x' then begin
         alpha=xalpha
         bigstep=1.5
         dval = 0.01
         xchange = 0
      endif else if todo eq 'y' then begin
         alpha=yalpha
         bigstep=1.5
         dval = 0.01
         ychange = 0
      endif else begin
         alpha=ralpha
         bigstep=0.00015
         dval = 0.000001
         rchange = 0
      endelse

      ; compute initial chisq
      mval=0.0
      mchi = astchi1(ref.xpos,ref.ypos,xpred,ypred,xoffset,yoffset,rotang,alpha)

      if verbose then $
         print,'Initial ',todo,mval,xoffset,yoffset,rotang*!radeg,mchi

      ; Find the left bound --  a point to the left that has a higher chi-sq
      lval=0.0
      xoff=xoffset
      yoff=yoffset
      rota=rotang
      repeat begin
         lval -= bigstep
         if todo eq 'x' then begin
            xoff = xoffset+lval
         endif else if todo eq 'y' then begin
            yoff = yoffset+lval
         endif else if todo eq 'r' then begin
            rota = rotang+lval
         endif
         lchi = astchi1(ref.xpos,ref.ypos,xpred,ypred,xoff,yoff,rota,alpha)
         if verbose then $
            print,'Left    ',todo,lval,xoff,yoff,rota*!radeg,lchi
      endrep until lchi gt mchi

      ; Find the right bound -- a point to the right that has a higher chi-sq
      rval=0.0
      xoff=xoffset
      yoff=yoffset
      rota=rotang
      repeat begin
         rval += bigstep
         if todo eq 'x' then begin
            xoff = xoffset+rval
         endif else if todo eq 'y' then begin
            yoff = yoffset+rval
         endif else if todo eq 'r' then begin
            rota = rotang+rval
         endif
         rchi = astchi1(ref.xpos,ref.ypos,xpred,ypred,xoff,yoff,rota,alpha)
         if verbose then $
            print,'Right   ',todo,rval,xoff,yoff,rota*!radeg,rchi
      endrep until rchi gt mchi

      ; The problem is now bounded, look for lowest chi-squared until change is
      ;   too small.
      while (rval-lval) gt dval do begin

         ; Left value is worst, replace with middle
         if lchi gt rchi then begin
            lval = mval
            lchi = mchi
         ; otherwise, right value is worst, replace with middle
         endif else begin
            rval = mval
            rchi = mchi
         endelse

         mval = (lval+rval)/2.0
         if todo eq 'x' then begin
            xoff = xoffset + mval
         endif else if todo eq 'y' then begin
            yoff = yoffset + mval
         endif else if todo eq 'r' then begin
            rota = rotang + mval
         endif

         mchi = astchi1(ref.xpos,ref.ypos,xpred,ypred,xoff,yoff,rota,alpha)
         if verbose then $
            print,'search  ',todo,mval,xoff,yoff,rota*!radeg,mchi

      endwhile

      if abs(mval) gt dval then begin
         if todo eq 'x' then begin
            xoffset = xoffset + mval
            xchange=1
         endif else if todo eq 'y' then begin
            yoffset = yoffset + mval
            ychange=1
         endif else if todo eq 'r' then begin
            rotang = rotang + mval
            rchange=1
         endif
         if verbose then $
            print,'----->  ',todo,0.0,xoffset,yoffset,rotang*!radeg,mchi
      endif

      if xchange eq 0 and ychange eq 0 and rchange eq 0 then break

   endfor

   if xchange or ychange or rchange then begin
      print,self,' Warning, incomplete convergence.  ', $
            strn(xchange),strn(ychange),strn(rchange)
   endif

end
