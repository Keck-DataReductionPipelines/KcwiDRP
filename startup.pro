; $Id: startup.pro,v 1.10 2014/01/09 17:32:47 neill Exp $
;
; KCWI DRP IDL startup file
;
; Astrolib setup
astrolib
;
; plotting system variables
defsysv,'!top_color',0
defsysv,'!top_colorx',0
defsysv,'!top_colorps',255
defsysv,'!colorstr',''
defsysv,'!colorstr_ps',''
defsysv,'!colorstr_x',''
;
; KCWI system variables:
; EDIT THIS to point to data dir in current release!!!
defsysv,'!KCWI_DATA','/Users/neill/kcwi/pipeline/releases/kderp/data/'
defsysv,'!CWI_DATA','/Users/neill/kcwi/pipeline/releases/kderp/cwi/'
;
