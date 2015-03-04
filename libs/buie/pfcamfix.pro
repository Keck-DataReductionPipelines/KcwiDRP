;+
; NAME:
;  pfcamfix
; PURPOSE:   (one line only)
;  Fix and reformat Lick 120$''$ prime focus camera data.
;
; DESCRIPTION:
;  This program is used to reformat the PFCam raw data into a form that can
;    be handled by the normal image processing programs in this library.  The
;    following is done to each image:
;
;    1 - Rename file from dNNN.ccd to root.NNN
;    2 - Create new header keyword, EXPTIME that has the exposure in seconds.
;    3 - Read date/time and compute decimal year to put in EQUINOX (of date).
;    4 - Compute airmass and put in header.
;    5 - Watch for bad ra/dec/ha, if all 0, then ask for correct RA/Dec.
;    6 - Put new file name in header under CCDFNAME
;    7 - Convert the file into a multi-group FITS image, one per amp.
;
;  Note that no attempt has been made to make this general.  It works on full
;    frame data that has been binned 2x2.
;
;  When processing, it looks for all .ccd files in the input directory.  The
;    file names are converted and then looked for in the destination directory.
;    Any destination file not found will be created.  Things that already
;    appear in the destination are not re-created unless the FORCE keyword
;    is set.
;
; CATEGORY:
;  CCD data processing
;
; CALLING SEQUENCE:
;  pfcamfix,source,dest,root
;
; INPUTS:
;  source - String, source directory to read from
;  dest   - String, destination directory to read from, do NOT make these
;              two directories the same.
;  root   - String, root of output file name.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/03/13
;  2004/9/21, MWB, removed obsolete call to Findfile
;-
pro pfcamfix,source,dest,root,FORCE=force

   if badpar(source,7,0,caller='PFCAMFIX (source) ') then return
   if badpar(dest,7,0,caller='PFCAMFIX (dest) ') then return
   if badpar(root,7,0,caller='PFCAMFIX (root) ') then return
   if badpar(force,[0,1,2,3],0,caller='PFCAMFIX (FORCE) ',default=0) then return

   if source eq dest then begin
      print,'Source and destination directories must not be the same.'
      return
   endif

   lat = (37.0+20.579/60.0)/180.0*!pi
   lon = (121.0+38.245/60.0)/180.0*!pi

   print,'Reading files from ',source
   print,'Writing files to   ',dest

   ; Get a list of all files in the source directory.
   str=addslash(source)+'*.ccd'
   fnlist = file_search(str,count=nfiles)

   if nfiles eq 0 then begin
      print,'No raw data files found.  Quitting.'
      return
   endif

   print,strn(nfiles),' files found in the source directory.'

   ; bust up the file names and get a list of image numbers.
   str = strmid(fnlist,strlen(addslash(source)),99)
   imnum=intarr(nfiles)
   reads,str,imnum,format='(1x,i,4x)'
   z=sort(imnum)
   imnum=imnum[z]
   fnlist=fnlist[z]

   ; If the file doesn't exist, process it, if it does, skip it.
   for i=0,nfiles-1 do begin
      infile = fnlist[i]
      ccdfname = root+'.'+string(imnum[i],format='(i3.3)')
      outfile = addslash(dest)+ccdfname
      if not exists(outfile) or force then begin
         image=readfits(infile,hdr,/noscale)
         exptime = sxpar(hdr,'EXPOSURE')
         exptime = float(exptime)/1000.0
         sxaddpar,hdr,'EXPTIME',exptime,after='EXPOSURE',format='F6.1', $
            ' Exposure time in seconds.'
         str=sxpar(hdr,'DATE-OBS')
         strput,str,' ',10
         jd=jdparse(str)
         jd2year,jd,year
         sxaddpar,hdr,'EQUINOX',year,after='DEC',format='F7.2', $
            ' Equinox of coordinates (of date)'
         str=sxpar(hdr,'RA')
         ra=raparse(strtrim(sxpar(hdr,'RA'),2))
         dec=decparse(sxpar(hdr,'DEC'))
         ha=haparse(sxpar(hdr,'HA'))
         if ra eq 0.0 and dec eq 0.0 and ha eq 0.0 then begin
            print,'Bad telescope coordinates detected, manual input', $
                  ' required for ',ccdfname
            input=''
            read,input,prompt='RA > ',format='(a)'
            ra = raparse(input)
            rastr,ra,1,ras
            sxaddpar,hdr,'RA',ras
            read,input,prompt='DEC> ',format='(a)'
            dec = decparse(input)
            decstr,dec,0,decs
            sxaddpar,hdr,'DEC',decs
            print,ras,' ',decs
         endif
         am = (airmass(jd,ra,dec,lat,lon))[0]
         sxaddpar,hdr,'AIRMASS',am,after='EQUINOX',format='F5.2'
         chk = intarr(n_elements(hdr))
         z=where(strtrim(hdr,2) ne '')
         hdr = hdr[z]
         sxaddpar,hdr,'CCDFNAME',ccdfname,after='OBSNUM'
         left  = [image[20:531,*],image[1044:1053,*]]
         right = [image[532:1043,*],image[1054:1063,*]]
         hdr0=hdr
         sxaddpar,hdr0,'EXTEND','T',after='NAXIS',' File contains extensions'
         sxaddpar,hdr0,'NEXTEND',2,after='EXTEND',' Number of extensions'
         sxaddpar,hdr0,'BITPIX',8,' Bits per pixel (not used)'
         sxaddpar,hdr0,'NAXIS',0,' PHU contains no image matrix'
         sxdelpar,hdr0,'NAXIS1'
         sxdelpar,hdr0,'NAXIS2'
         mkhdr,hdr1,left,/image
         sxaddpar,hdr1,'BZERO',32768L
         sxaddpar,hdr1,'BSCALE',1L
         rastr,ra,1,ras
         decstr,dec,0,decs
         print,'Writing ',outfile,'   ',ras,' ',decs
         writefits,outfile,0,hdr0
         writefits,outfile,left,hdr1,/append
         writefits,outfile,right,hdr1,/append
      endif else begin
         print,'Skip    ',outfile
      endelse
   endfor

end
