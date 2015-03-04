;+
; NAME:
;  finddata
; PURPOSE: 
;  Find valid file or directory names, using groups of path strings.
; DESCRIPTION:
;  Searches for targets which are valid files and/or directories.
;  Various combinations of path names are searched for a named item.
;  Specifically, paths of the form   a/r/t are checked, where 'a' is taken from 
;  the array of abspaths, 'r' from the relpaths, and 't' is a scalar
;  string, the actual target,
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;
;  finddata, abspaths,relpaths,target,results
;
; INPUTS:
; abspaths -  An array of strings comprising initial path components for search.
;                It may also be a single string with entries separated by :
;                It may be a null string.
;                The abspaths are used as the lefthand parts of the trial
;                search paths. They usually start with a '/' but this is 
;                not a requirement. 
; relpaths  - An array of strings comprising final path components for search.
;                It may also be a single string with entries separated by :
;                It may be a null string.
;                The relpaths are used as the righthand parts of the trial
;                search paths. They usually do not start with a '/' but this is 
;                not a requirement. 
;  target -   String representing the target of search.
;                Target is normally a single path component and does not include
;                '/' characters but this is not a requirement. A target is
;                 considered valid and returned, by default, if it is a readable
;                 file or directory.
;                 If target is an empty string, a recursive search will
;                 return all files and/or directories under the search
;                 paths- a non-recursive search returns those concantenations
;                 of abspath and relpaths that are locatable. Using a
;                 null string for target and including a null string in
;                 both abspaths and relpaths returns everything eligible
;                 within the current working directory.

; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; CACHE    - Path of file to save list cache. The default is for there to
;               be no cache. If /ONEONLY is not set the cache is  updated only.
; DEBUG    - Flag, if set, will generate verbose debugging output.
; FILEONLY-  Flag, if set, returns readable file targets only.
; DIRONLY-   Flag, if set, returns readable directory targets only.
; ONEONLY  - Flag, if set, forces the search to end as soon as the first 
;               target is found. Otherwise the entire range of paths generated
;               is checked for multiple matches.
; RECURSIVE -Flag, if set the search will be recursive on each of the paths
;               to find any target of the form a/../../....../r/t where 'a' is 
;               taken from the abspaths, 'r' from the relpaths and 't' is a 
;               target. Otherwise the paths selected are of the form a/r/t
; OUTPUTS:
; results  - String array, one or more pathnames to the targets located.
;               If the search found nothing, this will contain one
;               element equal to ''
;
; KEYWORD OUTPUT PARAMETERS:
; COUNT  -   Count of matches found.
; COMMON BLOCKS:
; SIDE EFFECTS:
; writes a cache list file if enabled to do so by specifying the CACHE keyword.
; RESTRICTIONS:
; Can be very slow.
; Because of the underlying file search used, paths returned may be slightly
; edited relative to the expected concatenation of paths, eg leading
; './'  may be elided.
; The cache is not currently working  and is silently turned off if invoked.
; This routine does not currently locate symbolic links or file names beginning
; with '.'
; This routine could usefully look for other combinations of permissions.
; PROCEDURE:
; MODIFICATION HISTORY:
;       2006/08/15, Written by Peter L. Collins, Lowell Observatory.
;       2006/08/25, PLC, generalized functionality
;-

pro finddata, abspaths,relpaths,target,results, $
            CACHE=cache,ONEONLY=oneonly,COUNT=count,RECURSIVE=recursive, $
            DEBUG=debug,FILEONLY=fileonly,DIRONLY=dironly


   self = 'FINDDATA: '
   if badpar(target,7,           0, caller=self + '(TARGET)') then return
   if badpar(abspaths,[7],   [0,1], caller=self + '(ABSPATHS)', $
                                   RANK=r_abspaths, NPTS=nabspaths)  then return
   if badpar(relpaths,[7],   [0,1], caller=self + '(RELPATHS)', $
                                   RANK=r_relpaths,NPTS=n_relpaths) then return
   if badpar(oneonly, [0,1,2,3], 0,caller=self + '(ONEONLY)', $
             default=0) then return
   if badpar(recursive,[0,1,2,3],0,caller=self + '(RECURSIVE)', $
             default=0) then return
   if badpar(cache,   [0,7],     0,caller=self + '(CACHE) ',  $
             default='')    then return
   if badpar(debug, [0,1,2,3],   0,caller=self + '(DEBUG)', $
             default=0) then return
   if badpar(fileonly, [0,1,2,3],0,caller=self + '(FILEONLY)', $
             default=0) then return
   if badpar(dironly, [0,1,2,3], 0,caller=self + '(DIRONLY)', $
             default=0) then return

   results = ''
   count=0

   if r_abspaths eq 0 then abspaths=strsplit(abspaths,':', $
                                             /EXTRACT,COUNT=n_abspaths)
   if r_relpaths eq 0 then relpaths=strsplit(relpaths,':', $
                                             /EXTRACT,COUNT=n_relpaths)
   nocache = 1
   pathtotal = fulljoin(abspaths,relpaths,/NOUNIQ)
   solidleft = where( pathtotal[0,*] ne '')
   if solidleft[0] ge 0 then $
      pathtotal[0,solidleft] = pathtotal[0,solidleft] + '/'
   solidright = where( pathtotal[1,*] ne '')
   if solidright[0] ge 0 then $
      pathtotal[1,solidright] = pathtotal[1,solidright] + '/'

   pathleft = pathtotal[0,*] + pathtotal[1,*]

   ; find it in the cache if possible
   if not nocache and oneonly then begin
      openr, luncache,cache,/get_lun
      prevhit=''
      while(not eof(luncache)) do begin
         readf, luncache,prevhit,FORMAT="(a)"
         hit = where ( strcmp(pathleft,prevhit, strlen(pathleft)) and $
                       strcmp(target, strmid(prevhit, strlen(target), $
                                         /REVERSE_OFFSET)),count)
         if count ne 0 then begin
            results = pathleft[hit[0]] + target
         endif else begin
            fullpath =  pathleft+target
            hit = where ( fullpath eq prevhit, count)
            if count ne 0 then $
               results = fullpath[hit[0]]
         endelse
         if count ne 0 then begin 
            count = 1
            if exists(results[0]) then begin
               free_lun, luncache
               return
            endif
         endif
      endwhile
      free_lun, luncache
   endif

   
   ; this badly supports oneonly, which should do the file_search's in a loop
   testregular = not dironly
   testdirectory = not fileonly
   if recursive then begin
      if testdirectory then begin
          print, ' test_dir', pathleft, target
          candidates=file_search(pathleft,target,/TEST_DIRECTORY, /TEST_READ, $
                                 COUNT=c1)
      endif else c1 = 0
      count = c1
      if testregular then begin
          print, ' test_reg', pathleft, target
          candidates2= file_search(pathleft,target,/TEST_REGULAR,/TEST_READ, $
                                  COUNT=c1)  
      endif else c1 = 0
   endif else begin
      if testdirectory then $
          candidates=file_search(pathleft + target, /TEST_READ, $
                                /TEST_DIRECTORY, COUNT=c1) $
      else c1 = 0
      count = c1
      if testregular then $
          candidates2=file_search(pathleft + target,/TEST_REGULAR, /TEST_READ, $
                                  COUNT=c1) $ 
      else c1 = 0
   endelse
   if c1 ne 0 then  $
      if count eq 0 then candidates = candidates2 else candidates = [ $
                                                candidates,candidates2]
   count += c1

    

   if debug then help, pathleft
   if debug then print, pathleft[*]+target
   if debug then print, 'candidates: ' , candidates
   
   if count ge 1 then begin
      if oneonly then results = candidates[0] $
      else results = candidates
   endif

   if not nocache then begin  ; update cache
      openw,luncache,cache,/APPEND,/GET_LUN
      for i = 0, n_elements(results)-1 do begin
         if results[i] ne '' then printf, luncache, results[i]
      endfor
      free_lun, luncache
   endif

   if n_elements(results) eq 1 then results=results[0]

end
