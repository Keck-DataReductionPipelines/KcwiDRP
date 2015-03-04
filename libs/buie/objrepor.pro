;+
; NAME:
;  objrepor
; PURPOSE:
;  Generate a report about the contents of the object files in a directory.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1999/2/18, Written by Marc W. Buie, Lowell Observatory
;  2000/11/9, MWB, removed Str_sep call.
;  2004/9/21, MWB, removed obsolete call to Findfile
;
;-
pro objrepor,in_path,PSCALE=pscale

   if badpar(in_path,[0,7],0,caller='objrepor: (path) ',default='.') then return
   IF badpar(pscale,[0,4,5],0, $
         CALLER='objrepor: (PATH) ',DEFAULT=0.26) THEN RETURN

   if in_path eq '.' then $
      path = '' $
   else $
      path = addslash(in_path)

   fmt1='(a12,1x,a2,1x,a1,6(1x,f7.1))'
   fmt2='(17x,6(1x,f7.1))'
   blanks='            '
   fobj = file_search(path+'*.obj',count=nobjf)

   print,'There are a total of',strcompress(nobjf),' files in directory ',path

   countunk=0
   totalobj=0
   for i=0,nobjf-1 do begin
      rdoblist,fobj[i],nobj,filelist,dt,offset,pos,flags,idstr,nfiles,ver=ver
      if ver eq 'OBJLIST v0' then begin
         print,'Object file ',fobj[i],' is still a version 0 file.  It must'
         print,'  be upgraded with objprmt before this program can be run.'
         return
      endif
      z=where(flags eq '?',count)
      if count ne 0 then begin
         print,count,' objects marked as unknown on field ',fobj[i]
         print,fobj[i],' --> ',count,' objects marked as unknown', $
            format='(a15,a,i4,a)'
         countunk=countunk+count
      endif

      ; Check each object against others in the list to see if any are
      ;   duplicate measurements.
      for k=0,nobj-1 do begin
         if flags[k] ne 'n' then begin
            pos1=reform(pos[*,k],2,nfiles)
            x1=pos1[0,*]
            y1=pos1[1,*]
            for j=k+1,nobj-1 do begin
               if flags[j] ne 'n' then begin
                  pos2=reform(pos[*,j],2,nfiles)
                  x2=pos2[0,*]
                  y2=pos2[1,*]
                  dist =(x1-x2)^2 + (y1-y2)^2
                  z=where(dist lt 1.0 and x1 ne -1.0 and x2 ne -1.0 and $
                                          y1 ne -1.0 and y2 ne -1.0, count)
                  if count ne 0 then begin
                     print,fobj[i],strb36(k,pad=2),flags[k],pos1,format=fmt1
                     print,' ',strb36(j,pad=2),flags[j],pos2,format=fmt1
                     print,dist,format=fmt2
                  endif
               endif
            endfor
         endif
      endfor

      ; Collect all 'y' measurements into the master list
      if nobj gt 0 then begin
         z=where(flags eq 'y',count)
         if count ne 0 then begin
            rate0=fltarr(count,nfiles-1)
            dir0 =fltarr(count,nfiles-1)
            objtag=strarr(count)
            idx = indgen(nobj)
            words=strsplit(fobj[i],'.',/extract)
            words=strsplit(words[0],'x',/extract)
            field0 = strmid(words[0]+blanks,0,8)
            if n_elements(words) eq 2 then $
               exttag = words[1] $
            else $
               exttag = ''
            words=strsplit(filelist[0],'.',/extract)
            objroot=field0+strmid(words[0],strlen(words[0])-2,2) + $
                   words[1] + exttag
            for j=1,nfiles-1 do begin
               dx=pos[j*2,z]   - pos[0,z]
               dy=pos[j*2+1,z] - pos[1,z]
               rate0[*,j-1]=sqrt(dx^2+dy^2)/dt[j-1]*pscale
               dir0[*,j-1] =atan(dy,dx) * !radeg
            endfor
            for j=0,count-1 do begin
               objtag[j] = objroot + strb36(idx[z[j]],pad=2)
            endfor
            if totalobj eq 0 then begin
               rate = rate0[*,0]
               dir  = dir0[*,0]
               obid = objtag
               totalobj = n_elements(rate)
            endif else begin
               rate = [rate,rate0[*,0]]
               dir  = [dir, dir0[*,0] ]
               obid = [obid,objtag]
               totalobj = totalobj + n_elements(rate0[*,0])
            endelse
         endif
      endif

   endfor

   print,'Slow moving objects. (<5)'
   if totalobj gt 0 then begin
      idx=sort(rate)
      z=where(rate[idx] lt 5.0,count)
      if count ne 0 then begin
         for i=0,count-1 do begin
            print,i,obid[idx[z[i]]],rate[idx[z[i]]],fix(dir[idx[z[i]]]+0.5), $
               format='(i6,1x,a,2x,f4.1,2x,i4)'
         endfor
      endif
   endif

   if countunk ne 0 then $
      print,'There are a total of',strcompress(countunk),' objects marked as ?.'
end
