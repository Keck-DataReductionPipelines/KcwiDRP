;+
; NAME:
;    sshift2d
; PURPOSE: (one line)
;    Shift a 2-D array using a damped sinc function for the fractional part.
; DESCRIPTION:
;
; CATEGORY:
;    Mathematical
; CALLING SeqUENCE:
;    result = sshift2d( array, shiftvec )
; INPUTS:
;    array    : Array to be shifted.
;    shiftvec : Vector of two elements: [ xshift, yshift ].
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;     EDGE_ZERO: Flag, if set will force the wrapped edges to zero.
;     
; OUTPUTS:
;    The shifted array is returned as the function value.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODifICATION HISTORY:
;    February, 1993:
;    Copied from "sincshift.pro" written by John Spencer, Lowell Observatory.
;    Very minor modifications by Doug Loucks, Lowell Observatory.
;    2010/05/01, Florian Scheulen, Harvey Mudd College, added EDGE_ZERO keyword
;    2010/05/05, MWB, style editing.
;-

;******************************************************************************
; Begin support functions.
; ------------------------------------------------------------------------------
; Function Padarray
; Returns original 2-D array padded by margin replication
; on all sides.  x margin = pad( 0 ), y margin=pad( 1 )
; ------------------------------------------------------------------------------
function padarray, array, pad
   sizearr = size(array)
   xs = sizearr[1]
   ys = sizearr[2]

   parray = fltarr(xs+2*pad[0],ys+2*pad[1])
   ; Middle:
   parray[pad[0],pad[1]] = array
   ; low-x side:
   parray[0:pad[0]-1,pad[1]:pad[1]+ys-1] = rebin(array[0,*],pad[0],ys)
   ; high-x side:
   parray[pad[0]+xs:*,pad[1]:pad[1]+ys-1]= rebin(array[xs-1,*],pad[0],ys)
   ; low-y side:
   parray[*,0:pad[1]-1] = rebin(parray[*,pad[1]],xs+2*pad[0],pad[1])
   ; high-y side:
   parray[*,pad[1]+ys:*]= rebin(parray[*,pad[1]+ys-1],xs+2*pad[0],pad[1])

   return, parray
end

; ------------------------------------------------------------------------------
; Function Shiftrep
; Shifts an image by integer numbers of pixels, replicating border pixels
; instead of wrapping around
; ------------------------------------------------------------------------------
function shiftrep, image, xshift, yshift, EDGE_ZERO=edge_zero
   imsize = size(image)
   xsize = imsize[1]
   ysize = imsize[2]

   ; Nint is the "nearest integer" function found in the Astron_misc library,
   ix = nint(xshift)
   iy = nint(yshift)

   temp=shift(image,ix,iy)
   if edge_zero then begin
      if ix gt 0 then temp[0:ix-1,*] = fltarr(ix,ysize)
      if ix lt 0 then temp[xsize+ix:*,*] = fltarr(-ix,ysize)
      if iy gt 0 then temp[*,0:iy-1] = fltarr(xsize,iy)
      if iy lt 0 then temp[*,ysize+iy:*] = fltarr(xsize,-iy)
   endif else begin
      if ix gt 0 then temp[0:ix-1,*] = rebin(temp[ix,*],ix,ysize)
      if ix lt 0 then temp[xsize+ix:*,*] = rebin(temp[xsize+ix-1,*],-ix,ysize)
      if iy gt 0 then temp[*,0:iy-1] = rebin(temp[*,iy],xsize,iy)
      if iy lt 0 then temp[*,ysize+iy:*] = rebin(temp[*,ysize+iy-1],xsize,-iy)
   endelse
   return, temp
end

; ------------------------------------------------------------------------------
; Function sshift2d
; Shifts a 2-D array by shiftvec[0] along x and shiftvec[1] along y,
; replicating the margins unless edge_zero is set in which case zeros are used.
; ------------------------------------------------------------------------------
function sshift2d, array, shiftvec, EDGE_ZERO=edge_zero

   self='sshift2d: '
   if badpar(edge_zero,[0,1,2,3],0,caller=self+'(EDGE_ZERO) ', $
                                   default=0) then return, 0
   sizearr=size(array)
   xs = sizearr[1]
   ys = sizearr[2]

   ; Separate and do integer shift first:
   ishift = nint(shiftvec)
   fshift = shiftvec-ishift
   sarray = shiftrep(array,ishift[0],ishift[1],EDGE_ZERO=edge_zero)

   ; Return if there's no fractional shift:
   if fshift[0] eq 0 and fshift[1] eq 0 then return,sarray

   dampfac = 3.25
   sincrad = 10
   sincsize = 2*sincrad+1

   ; Pad the array (replicating margins) in preparation for convolution:
   sarray = padarray(sarray,[sincrad,sincrad])

   ; Generate the x and y sinc functions:
   sinc = fltarr(sincsize,2)
   kernel = fltarr(sincsize,sincsize)

   for iindex=0,1 do begin
      if fshift[iindex] ne 0 then begin
         s = findgen(2*sincrad+1)-sincrad+fshift[iindex]
         sinc[*,iindex] = exp( -(s/dampfac)^2 )*sin(!pi*s)/(!pi*s)
      endif else begin
         sinc[*,iindex] = 0.0
         sinc[sincrad,iindex ] = 1.0
      endelse
   endfor

   ; Multiply the sinc functions to generate 2-D kernel
   for i=0,sincsize-1 do begin
      kernel[*,i] = sinc[*,0]*sinc[i,1]
   endfor

   sarray = convol(sarray,kernel,CENTER=1)

   ;Trim back to original size:
   sarray = sarray[sincrad:sincrad+xs-1,sincrad:sincrad+ys-1]

   return,sarray

end
