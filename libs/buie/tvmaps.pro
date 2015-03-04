;+
; NAME: 
;  tvmaps
; PURPOSE:
;  Display a full set of Pluto/Charon model .til maps on the current display.
; DESCRIPTION:
; CATEGORY:
;  Miscellaneous
; CALLING SEQUENCE:
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;-
pro tvmaps,root,scale,pluto,charon,pldef,chdef

  erase,0
  t=!d.n_colors-1

  readtil,root+'.til',pluto,charon
  readtil,root+'.def',pldef,chdef

  nann1 = fix(sqrt(n_elements(pluto)/4))
  plann=nann1*2*scale
  nann1 = fix(sqrt(n_elements(charon)/4))
  chann=nann1*2*scale

  plmin=min(pluto)
  plmax=max(pluto)
  chmin=min(charon)
  chmax=max(charon)

  tvtil,bytscl(pluto,min=0,max=1,top=t),0,plann+2*chann,scale
  tvtil,bytscl(pluto,min=plmin,max=plmax,top=t),2*plann,plann+2*chann,scale

  tvtil,bytscl(charon,min=0,max=1,top=t),plann,plann+chann,scale
  tvtil,bytscl(charon,min=chmin,max=chmax,top=t),3*plann,plann+chann,scale

  tvtil,bytscl(pldef,min=0,max=1,top=t),0,chann,scale
  tvtil,bytscl(pldef,min=plmin,max=plmax,top=t),2*plann,chann,scale

  tvtil,bytscl(chdef,min=0,max=1,top=t),plann,0,scale
  tvtil,bytscl(chdef,min=chmin,max=chmax,top=t),3*plann,0,scale

end
