;+
; NAME:
;  numtoflist
; PURPOSE:   (one line only)
;  Convert array of integer numbers to roboccd style filenames and back.
; DESCRIPTION:
; If the input list is numbers you get filenames out. If the input list is
; filenames you get numbers out.  It is also possible to input filenames and get
; a string out parseable by rangepar. 
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  numtoflist, inlist,outlist
; INPUTS:
;  inlist - list of integers or file name strings in 'roboccd' format.
;              To get such a list you must strip off the rundate from a normal
;              file name.  Thus, if the file name is 030506.010, you would
;              provide '.010'.  if the file name as 030506a.010, you would
;              provide 'a.010'.  The easiest way to do this is to put
;              strmid(fn,6) on the input command line if fn is the list
;              of file names (with no path).
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  DIRECT - flag, if set will force flist to numeric conversion- thus,
;            an inlist of integers is an error.
;  REVERSE - flag, if set will force  numeric to flist conversion- thus,
;            an inlist of strings is an error.
;  RANGEPAR- flag, if set numeric output is in the form of a rangepar string
;            instead of an array of integers.
;  ROBOCCD-  flag, indicating roboccd style naming, currently for documentary
;            purposes only.
; OUTPUTS:
;  outlist -list of file name strings in 'roboccd' format or integers. 
; KEYWORD OUTPUT PARAMETERS:
; ERROR-    set if one or more conversion errors occurred.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; The 'roboccd' style of image naming allows frame numbers between 0 and
; 26999  inclusive, where the file name is .000 through .999 for the
; first 1000, then a.000 thru a.999 up until z.999. This requires, in the
; roboccd tool set, that this style of file name be IMMEDIATELY preceded
; by the 6 digit date WITHOUT an intervening '.'. So 080125.999 is followed
; by 080125a.000. The 'filenames' in this example are '.999' and 'a.000' and
; are converted by NUMTOFLIST to [999, 1000].
; MODIFICATION HISTORY:
;  Written by Peter L. Collins, Lowell Observatory, 2008/01/25
;-

; this turns an array of integers into a rangepar- no claims about optimality.
; the 'x' construct is not used.
; It handles negative numbers but will produce output like -5--3 which
; rangepar can't parse. 
pro numtoflist_torangepar,inlist, outpar
   ; created sorted list without duplicates
   list = inlist[uniq(inlist,sort(inlist))]
   nlist=n_elements(list)
   outpar=''
   if nlist gt 1 then begin
      ; array of increment from ith to i+1th
      neighbor = list[1:*]-list[0:nlist-2]
      ; array of where ith is -end- of a run (which could be a run of 1).
      zspace =  where(neighbor ne 1,nsp)
      ; a final "implicit" zspace entry for the very end of list.
      if nsp++ eq 0 then zspace = nlist-1 else zspace = [zspace, nlist-1]
      srange=0  
      for i=0, nsp-1 do begin
         if srange gt zspace[i]-1 then  begin
            ; this is a run of 1.
            outpar += ( strn(list[srange]) + ',' )
            srange++
         endif else begin
            ; this is a run of 2 or more
            outpar += (strn(list[srange]) + '-' + strn(list[zspace[i]]) + ',')
            srange = zspace[i]+1
         endelse
      endfor
   endif else begin
      outpar = strjoin(string(list, FORMAT='(I5, ",")'))
   endelse
   outpar = strmid(outpar, 0, strlen(outpar)-1) ; kill trailing comma
end

pro numtoflist,inlist,outlist, DIRECT=direct, $
    REVERSE=reverse, RANGEPAR=rangepar, ERROR=error, DEBUG=debug, $
    ROBOCCD=roboccd

   self='NUMTOFLIST: '
   if badpar(inlist,[2,3,7], [0,1],caller=self+'(INLIST)', $
             npts=nlist, type=intype) then return
   if badpar(direct,[0,1,2,3],0,caller=self+'(DIRECT) ', $
                                   default=0) then return
   if badpar(reverse,[0,1,2,3],0,caller=self+'(REVERSE) ', $
                                   default=0) then return
   if badpar(rangepar,[0,1,2,3],0,caller=self+'(RANGEPAR) ', $
                                   default=0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ', $
                                   default=0) then return

   error=0
   maxrobo=26999   ; largest number possible in this system
   basebyte= byte('a')-1B
   basebyte=basebyte[0] ; make scalar
   if intype eq 7 then begin
      ; converting fn to number
      if direct gt 0 then begin
         print, self, '/DIRECT requires numeric inlist.'
         error = 1
      endif
      if error eq 0 then begin
         ; every string must have a '.'
         dotpos = strpos(inlist, '.')
         z = where( dotpos lt 0, error)
      endif
      if error eq 0 then begin
         outlist = intarr(nlist)
         ; find fn with 'a' etc prefix
         zprefix = where (dotpos eq 1, nprefix, COMPLEMENT=znoprefix)
         if nprefix gt 0 then begin 
            outlist[zprefix] += $
              (byte(strmid(inlist[zprefix],0,1)) - basebyte)*1000 ; prefix
            outlist[zprefix] += fix(strmid(inlist[zprefix], 2,3)) ; + suffix
         endif
         if nprefix lt nlist then outlist[znoprefix] = $
                          fix(strmid(inlist[znoprefix], 1,3)) ; just suffix
         if rangepar gt 0 then  begin
            ; convert the whole list to a rangepar string.
            numtoflist_torangepar,outlist, outpar
            outlist=outpar
         endif
      endif
   endif else begin
      ; convert number to fn.
      if reverse gt 0 then begin
         print, self, '/REVERSE requires string inlist.'
         error = 1
      endif
      ; find values out of range.
      if error le 0 then zrange = where(inlist lt 0 or inlist gt maxrobo, error)
      if error le 0 then begin
         ; generate all the suffixes.
         outlist = '.' + string(inlist MOD 1000, FORMAT='(I3.3)')
         zprefix = where(inlist ge 1000, nprefix)
         ; generate prefixes for those that need them.
         for i=0, nprefix -1 do outlist[zprefix[i]] =  string(basebyte + $
                  byte(inlist[zprefix[i]]/1000)) + outlist[zprefix[i]]
      endif
   endelse
end
