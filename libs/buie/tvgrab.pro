;+
; NAME:
;  tvgrab
; PURPOSE:   (one line only)
;  Grab plot window and save to a portable image file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  tvgrab,fn,win
; INPUTS:
;  fn  - file name for output JPG file
;  win - window number to grab and save
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PNG - flag, if set saves image in PNG format, otherwise saved as JPG
;  TIFF - flaf, if set save image in TIFF format, otherwise saved as JPG
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/10/04
;  2005/07/27, MWB, added PNG keyword
;  2010/02/01, MWB, added TIFF keyword
;  2010/06/23, MWB, changed setwin call to wset
;-
pro tvgrab,fn,win,PNG=png,TIFF=tiff

   if n_params() eq 0 then begin
      print,'tvgrab,filename,window'
      return
   endif

   self='TVGRAB: '
   if badpar(win,[0,1,2,3],0,caller=self+'(win)',default=-1) then return
   if badpar(png,[0,1,2,3],0,caller=self+'(PNG)',default=0) then return
   if badpar(tiff,[0,1,2,3],0,caller=self+'(TIFF)',default=0) then return

   if png and tiff then begin
      png=0
      tiff=0
      print,self,'PNG and TIFF cannot both be set, reverting to JPG.'
   endif

   if win ge 0 then wset,win

   img=tvrd(/true)
   if png then begin
      write_png,fn,img
   endif else if tiff then begin
      img=reverse(img,3)
      write_tiff,fn,img,orientation=1,compression=1
   endif else begin
      write_jpeg,fn,img,true=1,quality=100
   endelse

end
