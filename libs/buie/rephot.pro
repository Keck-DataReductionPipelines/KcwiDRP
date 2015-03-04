;+
; NAME:
;  rephot
; PURPOSE: (one line)
;  Reprocess photometry data set by re-extracting from images.
; DESCRIPTION:
;  This program will use a previously created photometry log file (new
;  format as read and defined by RDPHALT.PRO) to reprocess the photometry
;  allowing a change to the CCD gain, photometric aperture, and more.
;  It also normally is used to reprocess photometry with new calibration
;  information but this is not required.
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  rephot,oldlog,newlog,calibs
; INPUTS:
;  oldlog - Filename of an alternate format basphot log file.
;  newlog - Filename for the new format file (must be different).
;  calibs - Filename for the calibration information to be use.
;                     (See LDCALIB.PRO for full details.)
;              If this name is set to 'none', then all calibration steps
;                  are bypassed.
;
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
;  CALIBPATH-   Directory path pointing to calibration information,
;                  default=PATH+'calib/'
;  EXACT      - Flag, if true: take position as exact; otherwise (re)find it.
;  PATH       - Directory path pointing to location for raw images.
;  NEWGAIN    - New value of the gain to reduce with, e-/ADU
;  NEWRAD     - New value of the object aperture radius in pixels.
;  NEWRDNOISE - New value of the readout noise, e-/pixel.
;  NEWSKY1    - New value of the inner sky annulus radius in pixels.
;  NEWSKY2    - New value of the outer sky annulus radius in pixels.
;  PSCALE     - Provide plate scale of data (default = 0.726 arcsec/pixel).
;  SILENT     - Do not generate voluminous BASPHOTE output to the screen.
;  NSTART     - Starting point number to measure (default is beginning).
;  NSTOP      - Ending point number to measure (default is end).
;                 Note: NSTART and NSTOP are zero-indexed.
;   The following set of keywords controls a feature known as Multi-Aperture
;     Photometry, where a number of different apertures are used on the
;     same object on the same frame.
;  MAPNAME - String array list of object names to apply multi-aperture
;               photometry processing to (default=none).  If you want to
;               run the multiple apertures on everything, set this to '[[all]]'
;               In this case, mapserno is ignored.
;  MAPSERNO - Integer scalar or vector giving object serial numbers to
;               apply serial numbers to (default=0).  If set to -1, then
;               all serial numbers are processed with multiple apertures.
;  MAPERTURE - Vector list of apertures to apply to all of MAPNAME/MAPSERNO.
;  MAPSKY1   - Inner sky annulus radius.  May be scalar or same length as
;                 MAPERTURE.  Default = NEWSKY1
;  MAPSKY2   - Outer sky annulus radius.  Must be same length as MAPSKY1.
;                 Default = NEWSKY2
; OUTPUTS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  Rephot will read the log file from start to finish and process those
;    points in the range from NSTART to NSTOP.  There can be many extractions
;    from a single image so the point range does not usually correspond to
;    a frame number.  All of the information required to re-extract the
;    photometry from the image is read from the photometry log file or comes
;    from the optional override numbers.  This program does NOT read the
;    image headers to get its information.  This means that if the log file
;    information is wrong, you need only edit (and fix) the log file to get
;    the right information.  When editing a log file you need to be a little
;    careful but the format is pretty flexible and works by fields, not by
;    absolute position.
;
;  If MAPNAME is not set, then each entry is recomputed.
;
;  If MAPNAME and MAPERTURE are set, then the processing considerably more
;    complicated.
;
; MODIFICATION HISTORY:
;  Written 1993 Feb 4 by Marc W. Buie, Lowell Observatory
;  93/05/10, MWB, Added LORAL instrument option.
;  93/11/02, MWB, added PSCALE keyword, fixed overscan code for LORAL
;  94/03/16, MWB, added version 2 calibration file support.
;  95/03/03, MWB, fixed bug the didn't handle -1 -1 in calib files properly
;  95/03/07, MWB, added version 3 calibration file support.
;  95/06/12, MWB, added CALIBPATH support.
;  95/08/08, MWB, split out calibration info loading to separate routine.
;  96/10/31, MWB, added "bad" flag support from rdphalt
;  97/10/31, MWB, added calibs='none' option.
;  2000/02/08, MWB, added EXACT keyword
;  2000/01/24, MWB, added [[all]] option to MAPNAME.
;  2006/05/12, Peter L. Collins, Lowell Observatory
;                 added read nose keyword parameter and processing.
;-
pro rephot,oldlog,newlog,calibs, $
   CALIBPATH=calibpath,EXACT=exact, $
   PATH=path,NEWGAIN=newgain,NEWRAD=newrad,NEWRDNOISE=newrdnoise, $
   NEWSKY1=newsky1,NEWSKY2=newsky2, $
   NSTART=nstart,NSTOP=nstop,PSCALE=pscale,SILENT=silent,MAPNAME=mapname, $
   MAPERTURE=maperture,MAPSERNO=mapserno,MAPSKY1=mapsky1,MAPSKY2=mapsky2

   ;Validate parameters.
   if n_params() ne 3 then begin
      print,'rephot,oldlog,newlog,calibs'
      return
   endif

   self='REPHOT: '
   if badpar(oldlog,7,0,caller=self + '(oldlog) ') then return
   if badpar(newlog,7,0,caller=self + '(newlog) ') then return
   if badpar(calibs,7,0,caller=self + '(calibs) ') then return

   if badpar(path,[0,7],0,caller=self + '(path) ',default='') then return
   path=addslash(path)
   defcalib=addslash(path+'calib')
   if badpar(calibpath,[0,7],0,caller=self + '(calibpath) ', $
                               default=defcalib) then return
   if badpar(newgain,[0,2,3,4,5],0,caller=self + '(newgain) ') then return
   if badpar(newrad,[0,2,3,4,5],0,caller=self + '(newrad) ') then return
   if badpar(newrdnoise,[0,2,3,4,5],0,caller=self + '(newdnoise) ', $
                                      default=-1.0) then return
   if badpar(newsky1,[0,2,3,4,5],0,caller=self + '(newsky1) ') then return
   if badpar(newsky2,[0,2,3,4,5],0,caller=self + '(newsky2) ') then return
   if badpar(pscale,[0,4,5],0,caller=self + '(pscale) ', $
                              default=0.726) then return

   if badpar(mapname,[0,7],[0,1],caller=self +'(MAPNAME) ', $
                                 default='[[none]]') then return

   multiap = mapname[0] ne '[[none]]'

   if not exists(oldlog) then begin
      print,'Old log file ',oldlog,' does not exist.  Cannot continue.'
      return
   endif

   ; Validate Multi-aperture input (if supplied).
   if multiap then begin

      if badpar(mapserno,[0,2,3],[0,1],caller=self +'(MAPSERNO) ', $
                                       default=0) then return
      if badpar(maperture,[2,3,4,5],1,caller=self +'(MAPERTURE) ') then return
      if badpar(mapsky1,[0,2,3,4,5],[0,1],caller=self +'(MAPSKY1) ', $
                                          default=newsky1) then return
      if badpar(mapsky2,[0,2,3,4,5],[0,1],caller=self +'(MAPSKY2) ', $
                                          default=newsky2) then return

      ms1def=size(mapsky1)
      ms1def = ms1def[0] ne 0
      ms2def=size(mapsky2)
      ms2def = ms2def[0] ne 0
      naper  = n_elements(maperture)
      print,'Multi-aperture photometry mode enabled with ', $
         strn(naper),' apertures.'

      if n_elements(mapsky1) gt 1 and $
         n_elements(maperture) ne n_elements(mapsky1) then begin
         print,'MAPSKY1 must be scalar or match length of MAPERTURE'
         return
      endif

      if n_elements(mapsky2) ne n_elements(mapsky1) then begin
         print,'MAPSKY2 must match length of MAPSKY1.'
         return
      endif

   endif

   ; Load calibration information
   if calibs ne 'none' then begin
      ldcalib,calibs,calib,valid,calibpath=calibpath
      if not valid then return
   endif

   ;Second, read in the old log file and get all the information.

   print,'Load old photometry from ',oldlog

   rdphalt,oldlog,filename,obj,fil,jd,exptime,gain,rad,sky1,sky2, $
               serial,xpos,ypos,fwhm,maxcnt,sky,skyerr,mag,err,bad,rdnoise

   ; Sort the old photometry data
   tag = strarr(n_elements(filename))
   for i=0,n_elements(filename)-1 do begin
;      tag[i] = string(filename[i],obj[i],serial[i],format='(a,1x,a,1x,i4.4)')
      tag[i] = string(filename[i],serial[i],obj[i],format='(a,1x,i4.4,1x,a)')
   endfor
   idx=sort(tag)
   filename = filename[idx]
   obj      = obj[idx]
   fil      = fil[idx]
   jd       = jd[idx]
   exptime  = exptime[idx]
   gain     = gain[idx]
   rad      = rad[idx]
   sky1     = sky1[idx]
   sky2     = sky2[idx]
   serial   = serial[idx]
   xpos     = xpos[idx]
   ypos     = ypos[idx]
   fwhm     = fwhm[idx]
   maxcnt   = maxcnt[idx]
   sky      = sky[idx]
   skyerr   = skyerr[idx]
   mag      = mag[idx]
   err      = err[idx]
   bad      = bad[idx]
   rdnoise  = rdnoise[idx]

   if keyword_set(newgain) then gain[*]=newgain
   if keyword_set(newrad)  then rad[*]=newrad
   if newrdnoise ne -1.0 then   rdnoise[*]=newrdnoise
   if keyword_set(newsky1) then sky1[*]=newsky1
   if keyword_set(newsky2) then sky2[*]=newsky2

   nobs = n_elements(obj)

   if calibs ne 'none' then begin
      print,'Validate calibration files against photometry logfile.'
      ;Validate the filter codes and calibrations files before doing photometry.
      flatcode=intarr(nobs)
      for i=0,nobs-1 do begin
         flatcode0 = where(fil[i] eq calib.filter,count)

         if count eq 0 then begin
            print,'ABORTING: A flat field for filter code [',fil[i],'] was not found.'
            print,'You must fix the photometry file or the calibration file before proceeding.'
            return
         endif

         if count ge 2 then begin
            print,'ABORTING: the calibration file has a multiple flat definition for filter ',fil[i]
            return
         endif

         flatcode[i] = flatcode0

      endfor
   endif

   if badpar(nstart,[0,2,3],0,caller='REPHOT: (nstart) ',default=0) then return
   if badpar(nstop,[0,2,3],0,caller='REPHOT: (nstop) ',default=nobs-1) then return

   if nstart lt 0 then begin
      print,'WARNING: nstart less than zero.  Reset to start.'
      nstart=0
   endif

   if nstop ge nobs then begin
      print,'WARNING: nstop larger than number of points in file.  Reset to end.'
      nstop=nobs-1
   endif

   if nstart gt nstop then begin
      print,'ERROR: nstart is bigger than nstop, unable to continue.'
      return
   end

   print,'Ok, Start doing photometry, points ',nstart,' to ',nstop
   i=nstart
   while (i le nstop) do begin

      ; Get image.
      if not exists(path+filename[i]) then begin
         print,'Image file ',path+filename[i],' could not be found.  Aborting.'
         return
      endif
      image = readfits(path+filename[i],/silent)

      ; Process image (if needed).
      if calibs ne 'none' then begin
         ; Overscan correction.
         if calib.xl ge 0 and calib.xr ge 0 then $
            os = mean( image[calib.xl:calib.xr,*] ) $
         else $
            os = 0.0
         if n_elements(calib.dark) eq 1 then $
            image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - os) / $
                     calib.flat[*,*,flatcode[i]] $
         else $
            image = (image[calib.x1:calib.x2,calib.y1:calib.y2] - calib.bias - $
                                                      calib.dark*exptime[i] - os ) / $
                                 calib.flat[*,*,flatcode[i]]
      endif

      for j=i+1,nstop do begin
         if filename[j] ne filename[i] then begin
            last=j-1
            goto,found_range
         endif
      endfor

      last=nstop   ; This is the loop fall through condition (end of list).

      ; The current image runs from point I to LAST

      found_range:

      if multiap then begin

         ; With multiap, must look for start and stop on Object/Serial#
         j=i
         while j le last do begin

            ; Find range for this object/serial number, j will point to the
            ;   first line for this object/serial number.  After exiting the
            ;   loop, k will point to the last line for this object/serial
            ;   number.
            for k=j,last do begin
               if k eq last then $
                  break $
               else if obj[j] ne obj[k+1] and serial[j] ne serial[k+1] then $
                  break
            endfor

            ; check to see if this needs to be multi-app'ed
            domulti=0
            if mapname eq '[[all]]' then begin
               domulti=1
            endif else begin
               zob = where(obj[j] eq mapname)
               zse = where(serial[j] eq mapserno)
               if mapserno[0] eq -1 and zob[0] ne -1 then domulti=1
               if zse[0] ne -1 and zob[0] ne -1 then domulti=1
            endelse

            if domulti then begin

               if sky2[j] lt 0.0 and calibs ne 'none' then begin
                  print,j,'Skipping ',filename[j],obj[j],serial[j], $
                     ' manual sky not allowed with recalibration'
                  goto,skipit
               endif
               if ms1def or sky2[j] lt 0.0 then $
                  ms1=replicate(sky1[j],naper) $
               else begin
                  if n_elements(mapsky1) eq 1 then $
                     ms1=replicate(mapsky1,naper) $
                  else $
                     ms1=mapsky1
               endelse
               if ms2def or sky2[j] lt 0.0 then $
                  ms2=replicate(sky2[j],naper) $
               else begin
                  if n_elements(mapsky2) eq 1 then $
                     ms2=replicate(mapsky2,naper) $
                  else $
                     ms2=mapsky2
               endelse
               mserial=replicate(serial[j],naper)
               print,j,filename[i],obj[j],serial[j], $
                  format='("Point ",i4,"  file ",a,"  object [",a,":",i4.4,"]")'
               ; Use first aperture from maperture and the information from the
               ;   first of this list and call basphote.
               basphote,gain[j],image,exptime[j],xpos[j],ypos[j],maperture[0], $
                        ms1[0],ms2[0],newlog,mserial[0],/altlog,exact=exact, $
                        fname=filename[j],filter=fil[j],jd=jd[j],name=obj[j], $
                        rdnoise=rdnoise[j], $
                        pscale=pscale,silent=silent,bad=bad[j],xcen=xc0,ycen=yc0

               ; Now, do the rest, using the position from the first call and
               ;   forcing _that_ to be the location.
               basphote,gain[j],image,exptime[j],xc0,yc0,maperture[1:*], $
                        ms1[1:*],ms2[1:*],newlog,mserial[1:*],/altlog, $
                        fname=filename[j],filter=fil[j],jd=jd[j],name=obj[j], $
                        rdnoise=rdnoise[j], $
                        pscale=pscale,silent=silent,bad=bad[j],/exact

            endif else begin
               print,j,k-j,filename[j],obj[j],nstop-j, $
                  format='("Point ",i4,1x,i2,"  file ",a,"  object [",a,"]   left:",i4)'
               basphote,gain[j],image,exptime[j],xpos[j:k-1],ypos[j:k-1], $
                        rad[j:k-1],sky1[j:k-1],sky2[j:k-1], $
                        newlog,serial[j:k-1],/altlog,fname=filename[j], $
                        filter=fil[j],jd=jd[j], name=obj[j:k-1], $
                        pscale=pscale,rdnoise=rdnoise[j], $
                        silent=silent,bad=bad[j:k-1],exact=exact

            endelse

            skipit:
            ; Done, adjust pointer.
            j=k+1
         endwhile

      endif else begin

         print,i,last-i+1,filename[i],obj[i],nstop-i, $
            format='("Point ",i4,1x,i2,"  file ",a,"  object [",a,"]   left:",i4)'

         ;Do the photometry
         basphote,gain[i],image,exptime[i],xpos[i:last],ypos[i:last], $
                  rad[i:last],sky1[i:last],sky2[i:last],newlog, $
                  serial[i:last],/altlog, fname=filename[i],filter=fil[i], $
                  jd=jd[i],name=obj[i:last], rdnoise=rdnoise[i], $
                  pscale=pscale,silent=silent,bad=bad[i:last],exact=exact

      endelse

      i=last+1

   endwhile

end
