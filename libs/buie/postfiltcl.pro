;+
; NAME:
;  postfiltcl
; PURPOSE:   (one line only)
;  Post-filter the vclass mySQL table removing uninteresting objects
; DESCRIPTION:
; This script takes care of sifting through the vclass table and striking out
;   objects that are flagged by orbclass as interesting but have a very low
;   probability of really being interesting.  This program is needed to handle
;   using the results from a joined query and then updating the vclass table
;   based on one or more queries.  I have not found a way to do this directly
;   in mySQL even though this would seem to be a reasonable thing to do.  So,
;   here is the kludge....
;
;  des=2 means it was flagged by orbclass but could also be a Jupiter Trojan
;  des=3 means it was flagged by orbclass, but has a rate>15"/hr
;  des=4 means it was flagged by orbclass but sel<135 and cen>0
; CATEGORY:
;  Database
; CALLING SEQUENCE:
;  postfiltcl
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Requires transparent read/write access to the database des.
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/06/23
;  2004/07/29, MWB, changed Centaur/MBO inner exclusion elongation from 97 to 85
;-

pro postfiltcl,in_rundate

   self = 'postfiltcl: '
   if badpar(in_rundate,[0,7],0,caller=self+'(rundate) ',default='') then return

   openmysql,lun,'des'
   t="'"

   ; Jupiter Trojans that aren't interesting to DES
   query = [ $
   'select vclass.lookerid,vclass.rundate,obj.rate,obj.sel', $
   '   from vclass,obj', $
   '   where vclass.des=1 and vclass.jt>0 and', $
   '         vclass.lookerid=obj.lookerid and vclass.rundate=obj.rundate']
   
   if in_rundate ne '' then $
      query = [query,'and vclass.rundate='+t+in_rundate+t]

   query = [query,'   order by vclass.rundate,vclass.lookerid;' ]

print,query
   mysqlquery,lun,query,lookerid,rundate,rate,sel, $
      format='(a,a,f,i,i,i,i,i,i,i,i)'

   nobjs = n_elements(lookerid)

   if nobjs eq 1 and lookerid[0] eq '' then nobjs=0

   print,strn(nobjs),' Jupiter Trojans removed'

   for i=0,nobjs-1 do begin
      cmd = 'update vclass set des=2 where lookerid='+t+lookerid[i]+t+ $
               ' and rundate='+t+rundate[i]+t+';'
               
      mysqlcmd,lun,cmd,answer,nlines
      if nlines ne 0 then begin
         print,cmd
         print,answer
      endif
   endfor

   ; fast movers
   query = [ $
   'select vclass.lookerid,vclass.rundate,obj.rate,obj.sel', $
   '   from vclass,obj', $
   '   where vclass.des=1 and obj.rate>15 and', $
   '         vclass.lookerid=obj.lookerid and vclass.rundate=obj.rundate']

   if in_rundate ne '' then $
      query = [query,'and vclass.rundate='+t+in_rundate+t]

   query = [query,'   order by vclass.rundate,vclass.lookerid;' ]

   mysqlquery,lun,query,lookerid,rundate,rate,sel, $
      format='(a,a,f,i,i,i,i,i,i,i,i)'

   nobjs = n_elements(lookerid)

   if nobjs eq 1 and lookerid[0] eq '' then nobjs=0

   print,strn(nobjs),' fast moving objects removed (more than 15"/hr)'

   for i=0,nobjs-1 do begin
      cmd = 'update vclass set des=3 where lookerid='+t+lookerid[i]+t+ $
               ' and rundate='+t+rundate[i]+t+';'
               
      mysqlcmd,lun,cmd,answer,nlines
      if nlines ne 0 then begin
         print,cmd
         print,answer
      endif
   endfor

   ; low elongation Centaurs that are most likely main-belt
   query = [ $
   'select vclass.lookerid,vclass.rundate,obj.rate,obj.sel', $
   '   from vclass,obj', $
   '   where vclass.des=1 and obj.sel>=85 and obj.sel<=131 and vclass.cen1>0 and', $
   '         vclass.lookerid=obj.lookerid and vclass.rundate=obj.rundate']
   
   if in_rundate ne '' then $
      query = [query,'and vclass.rundate='+t+in_rundate+t]

   query = [query,'   order by vclass.rundate,vclass.lookerid;' ]

   mysqlquery,lun,query,lookerid,rundate,rate,sel, $
      format='(a,a,f,i,i,i,i,i,i,i,i)'

   nobjs = n_elements(lookerid)

   if nobjs eq 1 and lookerid[0] eq '' then nobjs=0

   print,strn(nobjs),' main-belt asteroids masquerading as low elongation Centaurs'

   for i=0,nobjs-1 do begin
      cmd = 'update vclass set des=4 where lookerid='+t+lookerid[i]+t+ $
               ' and rundate='+t+rundate[i]+t+';'
               
      mysqlcmd,lun,cmd,answer,nlines
      if nlines ne 0 then begin
         print,cmd
         print,answer
      endif
   endfor

   ; low elongation Centaurs that are most likely main-belt
   query = [ $
   'select vclass.lookerid,vclass.rundate,obj.rate,obj.sel', $
   '   from vclass,obj', $
   '   where vclass.des=1 and obj.sel>=114 and obj.sel<=127 and vclass.cen2>0 and', $
   '         vclass.lookerid=obj.lookerid and vclass.rundate=obj.rundate']
   
   if in_rundate ne '' then $
      query = [query,'and vclass.rundate='+t+in_rundate+t]

   query = [query,'   order by vclass.rundate,vclass.lookerid;' ]

   mysqlquery,lun,query,lookerid,rundate,rate,sel, $
      format='(a,a,f,i,i,i,i,i,i,i,i)'

   nobjs = n_elements(lookerid)

   if nobjs eq 1 and lookerid[0] eq '' then nobjs=0

   print,strn(nobjs),' main-belt asteroids masquerading as low elongation Centaurs'

   for i=0,nobjs-1 do begin
      cmd = 'update vclass set des=5 where lookerid='+t+lookerid[i]+t+ $
               ' and rundate='+t+rundate[i]+t+';'
               
      mysqlcmd,lun,cmd,answer,nlines
      if nlines ne 0 then begin
         print,cmd
         print,answer
      endif
   endfor

   free_lun,lun

end
