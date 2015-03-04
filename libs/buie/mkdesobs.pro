;+
; NAME:
;   mkdesobs
; PURPOSE:
;   Create data file to post into des\_obs MySQL databse from KBO search data
; DESCRIPTION:
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;  mkdesobs,root
; INPUTS:
;  root - String, 6 digit date code for observation.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  OVERWWRITE - Flag to force overwrite of pre-existing output file.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/03/08
;  2003/04/30, MWB, added OVERWRITE keyword
;-
pro mkdesobs,root,OVERWRITE=overwrite

   self='mkdesobs:'
   if badpar(overwrite,[0,1,2,3],0,caller=self+' (OVERWRITE) ',default=0) then return

   kred = '/net/frakir/raid/reduced/'
   keyfile = kred+'mosaic.key'
   info = kred+root+'/infox2.log'
   outfile = root+'.des_obs'

   ; Before starting, check to see if the output file already exists, if
   ;  it does, do not continue unless the override is set.
   if exists(outfile) and not overwrite then begin
      print,'Output file, ',outfile,' already exists.  Cannot continue.'
      print,'Use /OVERWRITE flag if you really want to proceed.'
      return
   endif

   ; First, read in the infox2.log file, don't do anything if this file
   ;  does not exist.
   if not exists(info) then begin
      print,info,' not found.'
      print,'Unable to continue.'
      return
   endif

   loadkeys,keyfile,keylist

   ; Figure out where the image data are located.
   machine = ['amber','frakir','spikard']
   sname   = ['raid','data','data']
   first   = [1,1,1]
   last    = [1,2,2]
   res=0
   for i=0,n_elements(machine)-1 do begin
      for j=first[i],last[i] do begin
         imdir = '/net/'+machine[i]+'/'+sname[i]+strn(j)+'/buie/rawfits/des/'+root

         res = file_test(imdir,/dir)
         if not res then imdir='' else break
      endfor
      if res then break
   endfor

   print,'Image data found in --> ',imdir

   readcol,info,fn,fwhm,maglim,format='a,x,x,x,f,x,x,x,x,x,x,x,f'
   fn = strmid(fn,0,10)
   nimages = n_elements(fwhm)

   openw,lout,outfile,/get_lun
   for i=0,nimages-1 do begin
      imfile = imdir+'/'+fn[i]
      ft=''
      if exists(imfile+'.fits') then ft = '.fits'
      if not exists(imfile+ft) then begin
         print,imfile,' not found.'
      endif else begin
         hdr = headfits(imfile+ft,exten=0)
         if strmid(root,0,1) eq '9' then $
            keylist[2,2] = 'DD/MM/YY' $
         else $
            keylist[2,2] = 'YYYY/MM/DD'
         parsekey,hdr,keylist,info
         jdstr,info.jd,0,jds
         date=strmid(jds,0,4)+'-'+strmid(jds,5,2)+'-'+strmid(jds,8,2)
         ut=strmid(jds,11,99)
         objname=strupcase(nobname(strtrim(info.object,2)))
         if strlen(objname) eq 5 and strmid(objname,0,1) eq 'F' then $
            objname = 'F0'+strmid(objname,1,99)
         str = string(fn[i],objname, $
            info.jd,date,ut, $
            info.ra,info.dec,fix(info.exptime),fwhm[i],maglim[i], $
            format='(a,1x,a,1x,f13.5,2(1x,a),2(1x,f12.9),1x,i3,1x,f5.2,1x,f5.2)')
         str=strcompress(str)
         printf,lout,str
      endelse
   endfor
   free_lun,lout

end
