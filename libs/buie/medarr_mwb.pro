;+
; NAME:
;	medarr_mwb
; PURPOSE: (one line)
;	Combine arrays with a median average.
; DESCRIPTION:
;       This will combine a series of arrays into a single array by filling each
;       pixel in the output array with the median of the corresponding pixels
;       in the input arrays.
; CATEGORY:
;	CCD data processing
; CALLING SEQUENCE:
;       medarr_mwb, inarr, outarr
; INPUTS:
;       inarr  -- A three dimensional array containing the input arrays to
;                 combine together.  Each of the input arrays must be two
;                 dimensional and must have the same dimensions.  These arrays
;                 should then be stacked together into a single 3-D array,
;                 creating INARR.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;     BAD- a badmask, an byte or integer array with the dimensions of inarr.
;       Non-zero values will correspond to pixels NOT to be included in the
;       median stack. If all pixels for a stack are deleted, the corresponding 
;       position of the output array is zero.
; OUTPUTS:
;       outarr -- The output array.  It will have dimensions equal to the
;                 first two dimensions of the input array.  The type of the
;                 output is always floating point.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	This will run VERY slow if inarr and outarr won't fit into real memory
;	on your computer.  Don't try this using virtual memory.
; PROCEDURE:
;       The output array is created and then each pixel is extracted from the
;	cube.  Once extracted, the pixel stack is sorted and the middle value
;	is put into the output array.
; MODIFICATION HISTORY:
;	Written by Marc W. Buie, Lowell Observatory, 1992 Jan 17
;  2000/11/19, MWB, renamed to avoid name conflict with Astron library
;  2007/09/07, MWB & PLC, added BAD keyword support
;-
pro medarr_mwb,inarr,outarr,BAD=bad

   self='MEDARR_MWB: '

   ; Check parameters
   if n_params(0) lt 2 then begin
      print,self,"Syntax:  medarr_mwb, inputarr, outputarr"
      return
   endif

   if badpar(inarr,[1,2,3,4,5,12,13,14,15],3,caller=self+'(inarr) ', $
                npts=nin,type=type,dimen=dimen) then return
   if badpar(bad,[0,1,2,3],3,caller=self+'(inarr) ', $
                default=0,npts=nbad) then return

   if nbad ne 0 and nbad ne nin then begin
      print,self,'BAD array must match size of input array'
      return
   endif

   ; Create the output array.
   ncol = dimen[0]
   nrow = dimen[1]
   narr = dimen[2]
   ;outarr = make_array(dimen=[s(1),s(2)],type=type,/nozero)
   outarr = fltarr(ncol,nrow,/nozero)

   usebad = n_elements(bad) gt 0

   ;Make the plane indexing vector
   planes=indgen(narr)

   even = fix(narr/2)*2 eq narr

   ; Combine the input arrays.
   for row = 0,nrow-1 do begin
      for col = 0,ncol-1 do begin
         if usebad then planes = where( bad[col,row,*] eq 0, narr)
         if narr gt 0 then begin
            allpix=inarr[col,row,planes]
            ord = sort(allpix)
            even = fix(narr/2)*2 eq narr
            if even then begin 
               val = [narr/2, narr/2-1]
               outarr[col,row] = total(allpix[ord[val]])/2
            endif else begin
               val = narr/2 
               outarr[col,row] = allpix[ord[val]]
            endelse
         endif else begin
            outarr[col,row] = 0
         endelse
      endfor
   endfor

end
