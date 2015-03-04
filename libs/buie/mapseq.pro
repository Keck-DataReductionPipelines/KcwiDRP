;+
; NAME:
;	mapseq
; PURPOSE: (one line)
;	Animate a seqence of PLUTOMEM maps.
; DESCRIPTION:
; CATEGORY:
;       Miscellaneous
; CALLING SEQUENCE:
;	mapseq,root,frame1,frame2,scale,step=step
; INPUTS:
;	root   - String containing a four character root for the map file name.
;	frame1 - Starting frame number to animate.
;	frame2 - Ending frame number to animate.
;	scale  - Zoom factor for the image.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;	step   - Step size between frames to be displayed (default = 1).
; OUTPUTS:
;	Visual output only.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;	The number of frames to animate (frame2-frame1)/step must be less than
;	256.  This is a limitation imposed by Xinteranimate as of Jan. 1992.
; PROCEDURE:
; MODIFICATION HISTORY:
;	Written 1992 Jan. by Marc W. Buie, Lowell Observatory.
;-

pro mapseq,root,frame1,frame2,scale,step=step

  pmulti=!p.multi
  if n_elements(step) eq 0  then step = 1

  readtil,root+string(frame1,format='(i4.4)')+'.til',pluto,charon

  binwidth=0.01
  plhist=histogram(pluto,min=0.0,max=1.0,binsize=binwidth)
  w=float(indgen(n_elements(plhist)))/float(n_elements(plhist))

  nann1 = fix(sqrt(n_elements(pluto)/4))
  plann=nann1*2*scale
  nann1 = fix(sqrt(n_elements(charon)/4))
  chann=nann1*2*scale

  width = 4*plann
  height = ( 2*plann + 2*chann ) * 2

  nframes = fix((frame2 - frame1)/step) + 1

  mb = float(width)*height*nframes/1024./1024.
  print,'Size of animation cube is ',mb,' Mbytes with ',nframes,' frames

  window,1,xsize=width,ysize=height,xpos=1000-width,ypos=800-height


  xinteranimate,set=[width,height,nframes],title='Pluto-Charon maps for sequence '+root

  j=0
  for i=frame1,frame2,step do begin
     tvmaps,root+string(i,format='(i4.4)'),scale,pl,ch,pldef,chdef
     plhist=histogram(pl,min=0.0,max=1.0,binsize=binwidth)
     pldhis=histogram(pldef,min=0.0,max=1.0,binsize=binwidth)
     chhist=histogram(ch,min=0.0,max=1.0,binsize=binwidth)
     chdhis=histogram(chdef,min=0.0,max=1.0,binsize=binwidth)
     !p.multi=[4,1,4]
     plot,w,plhist,xmargin=[5,1],ymargin=[1,1]
     oplot,w,pldhis*max(plhist)/max(pldhis),color=!d.n_colors/2
     oplot,w,plhist
     plot,w,chhist,xmargin=[5,1],ymargin=[1,1]
     oplot,w,chdhis*max(chhist)/max(chdhis),color=!d.n_colors/2
     oplot,w,chhist
     xyouts,10,10,string(i,format='(i4.4)'),/device,charsize=scale
     xinteranimate,frame=j,window=1
     j = j+1
  endfor

  wdelete,1

  xinteranimate

  !p.multi=pmulti

end
