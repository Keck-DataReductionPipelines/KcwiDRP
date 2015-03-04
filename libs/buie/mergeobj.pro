;+
; NAME:
;  mergeobj
; PURPOSE:
;  Merge a pair of object lists.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  mergeobj,file1,file2,outfile
; INPUTS:
;  file1 - First object list file.
;  file2 - Second object list file.
;  outfile - Name of file to write merged list to (can be the same as file1 or 2).
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
;  1999/2/16, Written by Marc W. Buie, Lowell Observatory
;  2002/09/03, MWB, changed Str_sep call to strsplit
;
;-
pro mergeobj,file1,file2,numnew,numdup,nodice

   if not exists(file1) then begin
      print,'MERGEOBJ: File ',file1,' not found.'
      return
   endif

   if not exists(file2) then begin
      print,'MERGEOBJ: File ',file2,' not found.'
      return
   endif

   rdoblist,file1,nobj1,filelist1,dt1,offset1,pos1,flags1,idstr1,nfiles1
   rdoblist,file2,nobj2,filelist2,dt2,offset2,pos2,flags2,idstr2,nfiles2

   if nfiles1 ne nfiles2 then begin
      print,'MERGEOBJ:  The number of files referred to in each object list'
      print,'   must be the same.'
      print,'file1 - ',file1
      print,'  filelist: ',filelist1
      print,'file2 - ',file2
      print,'  filelist: ',filelist2
      return
   endif

   if min(filelist1 eq filelist2) eq 0 then begin
      print,'MERGEOBJ:  The files referred to in the object lists must be identical.'
      print,'file1 - ',file1
      print,'  filelist: ',filelist1
      print,'file2 - ',file2
      print,'  filelist: ',filelist2
      return
   endif

   if nobj2 eq 0 then begin
      spawn,'rm '+file2
      return
      numnew=0
      numdup=0
      nodice=0
   endif

   ; Scan through the objects in the second list, looking for a positional
   ;   match on the first postion.
   copyit=intarr(nobj2)
   duploc=lonarr(nobj2)
   for i=0,nobj2-1 do begin
      if flags2[i] ne 'n' then begin
         x = pos2[0,i]
         y = pos2[1,i]
         dist = (pos1[0,*]-x)^2 + (pos1[1,*]-y)^2
         z=where(dist lt 1.0 and flags2 ne 'n',count)
         if count eq 0 then begin
            copyit[i] = 1
         endif else if count eq 1 then begin
            pos=reform(pos2[*,i],2,nfiles2)
            x2=pos[0,*]
            y2=pos[1,*]
            pos=reform(pos1[*,z[0]],2,nfiles1)
            x1=pos[0,*]
            y1=pos[1,*]
            dist =(x1-x2)^2 + (y1-y2)^2
            if max(dist) lt 1.0 then begin
               copyit[i] = 2
               duploc[i] = z[0]
            endif else begin
               copyit[i] = 3
               print,i,' close dup',dist[*],' no action programmed.'
            endelse
         endif else begin
            copyit[i] = 4
         endelse
      endif else begin
         copyit[i] = 5
      endelse
   endfor

   for i=0,nobj2-1 do begin
      case copyit[i] OF

         0: begin
            print,i,' no action recorded, this should not happen.'
         end

         ; New object, just add to the list.
         1: begin
            pos1=[[pos1],[pos2[*,i]]]
            flags1=[flags1,flags2[i]]
            idstr1=[idstr1,idstr2[i]]
            nobj1=nobj1+1
         end

         2: begin
            if flags1[duploc[i]] eq 'y' or flags2[i] eq '?' then begin
               list1=strsplit(idstr1[duploc[i]],',',/extract)
if n_elements(list1) ne 1 then begin
print,i,' dup'
print,idstr1[duploc[i]],'  <-- ',idstr2[i]
endif
               list2=strsplit(idstr2[i],',',/extract)
            endif else begin
               list1=strsplit(idstr2[i],',',/extract)
               list2=strsplit(idstr1[duploc[i]],',',/extract)
               flags1[duploc[i]] = flags2[i]
            endelse

            if n_elements(list1) eq 1 then begin
               list1 = [list1,list2,list1]
            endif else begin
               list1 = [list1[0:n_elements(list1)-2],list2,list1[n_elements(list1)-1]]
            endelse
            list1 = list1[uniq(list1)]
            idstr1[duploc[i]] = list1[0]
            for j=1,n_elements(list1)-1 do $
               idstr1[duploc[i]] = idstr1[duploc[i]]+','+list1[j]
         end

         3: begin
; no action yet
         end
         4: begin
            print,i,' multiple hit, no action programmed.'
         end
         5: begin
            ; this is a no, no action required
         end
         else: begin
            print,i,' illegal case, this should not happen.'
         end
      endcase
   endfor

   z=where(copyit eq 1,c1)
   z=where(copyit eq 2,c2)
   z=where(copyit eq 3,c3)
   z=where(copyit eq 4,c4)
   z=where(copyit eq 5,c5)

   str=strcompress(string(nobj2,c1,c2,c3,c4,c5,nobj2-c1-c2-c3-c4-c5))
   print,file1,' <-- ',file2,'  ',str
   wroblist,file1,nobj1,filelist1,dt1,offset1,pos1,flags1,idstr1,nfiles1
;   wroblist,'test1.obj',nobj1,filelist1,dt1,offset1,pos1,flags1,idstr1,nfiles1

   numnew = c1
   numdup = c2
   nodice = nobj2-c1-c2-c5

   z=where(copyit eq 3 or copyit eq 4,count)
   if count eq 0 then begin
      spawn,'rm '+file2
   endif else begin
      wroblist,file2,count,filelist2,dt1,offset2,pos2[*,z],flags2[z],idstr2[z],nfiles2
;      wroblist,'test2.obj',count,filelist2,dt1,offset2,pos2[*,z],flags2[z],idstr2[z],nfiles2
   endelse

end
