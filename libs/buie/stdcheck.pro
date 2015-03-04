;+
; NAME:
;  stdcheck
; PURPOSE:
;  Determine if object is in a given standard catalog.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  stdcheck,file,names,filter,std
; INPUTS:
;  file   - Name of catalog file to read.
;  name   - Name of the stars.
;  filter - Filter indicies for each name. (vector or (npts x n))
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  std    - Results of search (vector):
;             0 - not listed in file
;             1 - listed and is a good standard
;             2 - listed and is flagged as a variable
;             3 - listed and but is not good enough to be a standard.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/11/22, Written by Marc W. Buie, Lowell Observatory
;-
pro stdcheck,file,names,in_filter,std

   if badpar(file,7,0,CALLER='STDCHECK: (file) ') then return
   if badpar(names,7,1,CALLER='STDCHECK: (names) ',npts=len) then return
   if badpar(in_filter,[2,3],[1,2],CALLER='STDCHECK: (filter) ') then return

   rdphocat,file,cname,mags,codes,filname,nfil

   sz=size(in_filter)

   ; Promote filter array to rank=2 if just a vector
   if sz[0] eq 1 then begin
      filter = rebin(in_filter,sz[3],1)
   endif else filter=in_filter

   sz=size(filter)
   nsf = sz[2]   ; Number of filters to be scanned.

   ; Locate the names in the catalog
   idx=replicate(-1,len)
   for i=0,len-1 do begin
      z=where(names[i] eq cname)
      idx[i]=z[0]
   endfor

   ; this flags for in or out of file
   std=idx ne -1

   ; Check against all the filters.
   for i=0,nsf-1 do begin
      good=where(std eq 1,count)
      if count gt 0 then begin
         z0=where(codes[filter[good,i],idx[good]] eq 0,count0)
         if count0 ne 0 then std[good[z0]]=3
         z2=where(codes[filter[good,i],idx[good]] eq 2,count2)
         if count2 ne 0 then std[good[z2]]=2
      endif
   endfor

end
