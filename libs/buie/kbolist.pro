;+
; NAME:
;  kbolist
; PURPOSE:
;  Create a summary listing of KBOs in a single directory of DES data
; DESCRIPTION:
;  Reads .info files
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  kbolist, dir, dirname, rawcode, firstfile, rmag,rmagerr,elong,nelements
; INPUTS:
;  dir - directory to look in for kbo object list (relative to redpath)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ASTPATH - path to astrometry data
;            default is /net/frakir/raid/buie/astrometry
;  LOGFILE - name of error log file to use, default is 'kbolist.log'
;  REDPATH - Root of the reduction directory tree, default is
;              /net/frakir/raid/reduced
; OUTPUTS: 
;  dirname - directory where the objectlist for the kbo exists
;  rawcode - object code for kbo, can be looker code, mpc code or designation 
;  firstnight - first frame of night discovery in survey images
;  rmag - 'r' magnitude of reduction frame
;  rmagerr - error in 'r' magnitude of reduction frame
;  rate - derived rate of motion (arcsec/hour)
;  direct - direction of motion between frames
;  dr - total motion between frames (arcsec)
;  dt - time interval between frames (hours)
;  elong - solar elongation of object
;  nelements - number of objects found, this is 0 if no KBO data is found
;              in this case, other outputs are undefined.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
; 01/01/02, Written by Susan Kern
; 01/04/06, DBT - Fixed bug in rate
; 02/07/02, SDK, added REDPATH keyword
; 2004/02/09, MWB, changed default for ASTPATH
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro kbolist,dir,dirname,rawcode,firstnight,rmag,rmagerr,rate,direct,dr,dt, $
   elong,nelements,ASTPATH=astpath,LOGFILE=logfile, REDPATH=redpath

   nelements=0

   ; check to see that the parameters are valid
   if badpar(dir,7,0,caller='kbolist:(dir)') then return
   if badpar(astpath,[0,7],0,caller='kbolist:(ASTPATH)', $
            default='/net/frakir/raid/buie/astrometry') then return
   if badpar(logfile,[0,7],0,caller='kbolist:(LOGFILE)', $
            default='kbolist.log') then return
   if badpar(redpath,[0,7],0,caller='kbolist:(REDPATH) ', $
            default='/net/frakir/raid/reduced') then return

   ; check to see if the data directory is a KBO Search directory 
   list=file_search(addslash(redpath)+addslash(dir)+'infox?.log', count=count) 
   if count ne 8 then return

   ; check to see if there is a .info file in the submitted directory
   filepath=addslash(astpath)+addslash('Submitted')+dir+'.info'
   if not exists(filepath) then begin
      logerror,logfile,caller='kbolist', $
         '*** error:'+filepath+' not found'
      return
   endif

   ; get all objects with a rate < 10.0 and elong > 150
   linecount = 0
   line = ''    ;init to string
   openr, lun, filepath, /get_lun

   ; the first line is a header
   if not eof(lun) then readf, lun, line, format='(A)'

   while not eof(lun) do begin
      readf, lun, line, format='(A)'
      line = strcompress(strtrim(temporary(line),2))
      elements = strsplit(line, ' ', /extract)

      ;if (size(elements))[1] lt 11 then begin
      if n_elements(elements) lt 11 then begin
         logerror,logfile,caller='kbolist', $
            'parse error:'+line 
         continue
      endif

      if float(elements[6]) gt 10.0 then break
      if float(elements[11]) lt 150 then continue

      ;rawcode,firstnight,??,rmag,??,rmagerr,rate,direct,dr,dt,??,elong
      if linecount eq 0 then begin
         rawcode    =elements[0]
         firstnight =elements[1]
         rmag       =float(elements[3])
         rmagerr    =float(elements[5])
         rate       =float(elements[6])
         direct     =float(elements[7])
         dr         =float(elements[8])
         dt         =float(elements[9])
         elong      =float(elements[11])
      endif else begin
         rawcode    =[rawcode,elements[0]]
         firstnight =[firstnight,elements[1]]
         rmag       =[rmag,float(elements[3])]
         rmagerr    =[rmagerr,float(elements[5])]
         rate       =[rate,float(elements[6])]
         direct     =[direct,float(elements[7])]
         dr         =[dr,float(elements[8])]
         dt         =[dt,float(elements[9])]
         elong      =[elong,float(elements[11])]
      endelse
      linecount = linecount + 1
   endwhile
   free_lun, lun 

   if linecount gt 0 then dirname=replicate(dir, linecount)
   nelements = linecount
end
