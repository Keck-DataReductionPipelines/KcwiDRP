;+
; NAME:
;    digit
; PURPOSE: (one line)
;    Digitize from a displayed gif file.
; DESCRIPTION:
; CATEGORY:
;    Image display
; CALLING SEQUENCE:
;    digit, image, outfile
; INPUTS:
;    image   : Image to be displayed.  May be a string scalar containing the
;              name of an image file to be loaded and displayed, or an image
;              array to be displayed.  File is read with READ_IMAGE.
;    outfile : File into which collected points are saved.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;    NATURAL    = If set, the natural screen coordinates are used.  No
;                 control points are solicited and no coordinate
;                 transformations are computed.
;    NOSCALE    = If set, the image is displayed without being scaled.
;    ROTATE     = Rotation angle (in degrees) for the gif image.  Allowed
;                 values are 90, 180, and 270 counterclockwise.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
;    XVEC, YVEC = If both of these keywords are present, the collected
;                 coordinates are returned as vectors to the variables
;                 specified in the keywords.  The collected coordinates are
;                 always written to the output file.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;    Unless the 'NATURAL' keyword is specified, four control points and four
; associated values are requested in order to define the x and y axes and
; scale factors.
;    The first two points are the x-axis control points (and their values) and
; the second two are the y-axis control points (and their values).
;    Control vectors are computed from the requested control points and
; coordinate transformations are computed using simple vector algebra.
; MODIFICATION HISTORY:
;  Written by Doug Loucks, Lowell Observatory, April, 1993.
;  94/10/04 - MWB - moved cr to front of print statement (v3.6 bug workaround)
;  2000/01/31, MWB, fixed bug that was introduced by some IDL version that
;                      changed the way CURSOR worked.
;  2009/01/13, MWB, changed to use READ_IMAGE to read external file.
;-
pro digit, image, outfile, NATURAL=natural, NOSCALE=noscale, ROTATE=in_angle, $
    XVEC=xvec, YVEC=yvec

   ; Check for required parameters.
   if n_params() ne 2 then begin
      message, 'digit, image, outfile', /info
      return
   endif

   image_size = size( image )
   if image_size[0] eq 0 and image_size[1] eq 7 then begin
      ; parameter is a scalar string.  if the file exists, load it into a local
      ; variable.
      if not exists( image ) then begin
         message, 'file ' + image + ' does not exist.', /info
         return
      endif
      read_gif, image, l_image
      l_image=read_image(image)
      sz=size(l_image,/n_dimen)
      if sz eq 3 then l_image=reform(l_image[0,*,*],/overwrite)
      type = 7
   endif else begin
      ; parameter should be an array of rank 2.
      if badpar( image, [1,2,3,4,5], 2, caller='% digit: (image) ', $
      type=type ) then return
   endelse

   ; check the output file name.
   if outfile eq '' or outfile eq ' ' then begin
      message, 'outfile parameter must be non-blank.', /info
      return
   endif

   ; define a few ascii control characters.
   bel = string( 7b )
   cr  = string( 13b )
   lf  = string( 10b )

   ; initialize the coordinate vectors.
   xvec = [ 0.0 ]
   yvec = [ 0.0 ]

   if type eq 7 then begin
      ; the image parameter is a scalar string containing a gif filename.
      image_size = size( l_image )
help,l_image
print,image_size
   endif else begin
      image_size = size( image )
   endelse

   wsize = float( [ !d.x_size, !d.y_size ] )

   if keyword_set( in_angle ) then angle=in_angle else angle=0
   case angle of
       90  : begin
         isize = [ image_size[2], image_size[1] ]
         direction = 1
      end
      180  : begin
         isize = [ image_size[1:2] ]
         direction = 2
      end
      270  : begin
         isize = [ image_size[2], image_size[1] ]
         direction = 3
      end
      else : begin
         isize = [ image_size[1:2] ]
         direction = 0
      end
   endcase

   sf = min( wsize / float(isize) )
   ;print, 'scale factor: ', sf

   ; display the image.
   nscale = keyword_set( noscale )
   case nscale of
      0 : begin
         ; display the image, scaled to fit into the idl draw window.
         if type eq 7 then begin
            ; image parameter is a string scalar containing a gif filename.  the
            ; file was loaded into a local image array.
            tvscl,congrid( rotate(l_image,direction), isize[0]*sf, isize[1]*sf, $
            /interp )
         endif else begin
            ; image parameter is an image array.
            tvscl,congrid( rotate(image,direction), isize[0]*sf, isize[1]*sf, $
            /interp )
         endelse
      end
      1 : begin
         ; display the image without scaling.
         if type eq 7 then begin
            ; image parameter is a string scalar containing a gif filename.  the
            ; file was loaded into a local image array.
            tvscl, rotate( l_image, direction )
         endif else begin
            ; image parameter is an image array.
            tvscl, rotate( image, direction )
         endelse
      end
   endcase


   if not keyword_set( natural ) then begin
      ; coordinate transformations will be computed.
      ; ask for the x-axis control points and their associated values.
      !mouse.button = 0
      while !mouse.button ne 1 do begin
         print, 'first x-axis control point (left=accept, right=exit).' + bel
         cursor, xa1, ya1, wait=3, /device
         if !mouse.button eq 4 then return
      endwhile
      read, '... x-value for this point>' + bel, x1
      print, ''

      !mouse.button = 0
      while !mouse.button ne 1 do begin
         print, 'second x-axis control point (left=accept, right=exit).' + bel
         cursor, xa2, ya2, wait=3, /device
         if !mouse.button eq 4 then return
      endwhile
      read, '... x-value for this point>' + bel, x2
      print, ''

      ; compute the x-axis control vectors and magnitudes.
      va1 = [ xa2-xa1, ya1-ya1 ]
      vb1 = [ xa2-xa2, ya2-ya1 ]
      vc1 = va1 + vb1
      ;
      magva1 = sqrt( total( va1*va1 ) )
      magvb1 = sqrt( total( vb1*vb1 ) )
      magvc1 = sqrt( total( vc1*vc1 ) )

      ; compute the x-axis scale factor.
      sx = ( x2-x1 ) / magvc1

      ; ask for the y-axis control points and their associated values.
      !mouse.button = 0
      while !mouse.button ne 1 do begin
         print, 'first y-axis control point (left=accept, right=exit).' + bel
         cursor, xb1, yb1, wait=3, /device
         if !mouse.button eq 4 then return
      endwhile
      read, '... y-value for this point>' + bel, y1
      print, ''

      !mouse.button = 0
      while !mouse.button ne 1 do begin
         print, 'second y-axis control point (left=accept, right=exit).' + bel
         cursor, xb2, yb2, wait=3, /device
         if !mouse.button eq 4 then return
      endwhile
      read, '... y-value for this point>' + bel, y2
      print, ''

      ; compute the y-axis control vectors and magnitudes.
      va2 = [ xb2-xb1, yb2-yb2 ]
      vb2 = [ xb1-xb1, yb2-yb1 ]
      vc2 = va2 + vb2

      magva2 = sqrt( total( va2*va2 ) )
      magvb2 = sqrt( total( vb2*vb2 ) )
      magvc2 = sqrt( total( vc2*vc2 ) )

      ; compute the y-axis scale factor.
      sy = ( y2-y1 ) / magvc2
   endif

   print, 'Left mouse button enters points, middle button enters a blank line.'
   print, '... right mouse button to exit.'

   on_error, 2
   fmt  = "($,a,'x=',g20.4,4x,'y=',g20.4)"
   fmt1 = "(a,'x=',g20.4,4x,'y=',g20.4)"

   ; begin the main loop for tracking the mouse cursor and collecting the points.
   npts = 0
   !mouse.button = 0
   while !mouse.button ne 4 do begin
      ; track the cursor position in device coordinates.
      cursor, xm, ym, 2, /device
      if keyword_set( natural ) then begin
         ; use actual device coordinates.
         x = xm
         y = ym
      endif else begin
         ; compute the coordinates from the coordinate transformation vectors.
         va = [ xm-xa1, ya1-ya1 ]
         magva = sqrt( total( va*va ) )
         magvb = magva * ( magvb1 / magva1 )
         vb = [ xm-xm, magvb ]
         v = va + vb
         magv = sqrt( total( v*v ) )
         if xm-xa1 lt 0 then x=x1-(sx*magv) else x=x1+(sx*magv)

         vb = [ xb1-xb1, ym-yb1 ]
         magvb = sqrt( total( vb*vb ) )
         magva = magvb * ( magva2 / magvb2 )
         va = [ magva, ym-ym ]
         v = vb + va
         magv = sqrt( total( v*v ) )
         if ym-yb1 lt 0 then y=y1-(sy*magv) else y=y1+(sy*magv)
      endelse

      if !mouse.button eq 1 or !mouse.button eq 2 then begin
         ; left (or middle) mouse button pressed.
         if !mouse.button eq 1 then begin
            ; left mouse button pressed.
            ; save the position into the output file and move to a new print line.
            print, cr, x, y, format=fmt1
            get_lun, lu
            openw, lu, outfile, /append
            printf, lu, x, y, format='(g20.4,4x,g20.4)'
            free_lun, lu
            xvec = [ xvec, x ]
            yvec = [ yvec, y ]
            npts = npts + 1
         endif else begin
            ; middle mouse button pressed.  move to a new print line and put a
            ; blank line into the output file.
            print, '                                                  '
            openw, lu, outfile, /append
            printf, lu
            free_lun, lu
         endelse
         while ( !mouse.button ne 0 ) do begin
            cursor, xx, yy, 4, /device,/change
         endwhile
      endif
      print, cr, x, y, format=fmt
   endwhile

   print, format='(/)'

   print, 'Selected points saved to file ' + outfile + '.'
   if keyword_set( xvec ) and keyword_set( yvec ) and npts gt 0 then begin
      xvec = xvec[ 1 : npts ]
      yvec = yvec[ 1 : npts ]
   endif

end
