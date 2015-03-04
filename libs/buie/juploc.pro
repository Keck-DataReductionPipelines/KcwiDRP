;+
; NAME:
;  juploc
; PURPOSE:
;  Find and extract Jupiter in images and create PDS FITS headers on output.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  juploc,root,imnum, $
;       PATH=path, OUTPATH=outpath, PANG=in_pang, PSCALE=pscale
; INPUTS:
;  root  - String, contains the date root of the file name to be read.
;  imnum - Integer value or array of image number to process.
;             File names are root+'m.'+imnum  (imnum converted to 3 digit string)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BORDER  - Border around Jupiter for extracted image (default=15).
;  LATITUDE - String containing latitude for observatory, this value overrides
;             the value expected in the header.  Format MUST exactly match this
;             example:      '+37:20:36.0'
;  LONGITUDE - String containing longitude for observatory, this value overrides
;              the value expected in the header.  Format MUST exactly match this
;              example:      'W 121:38:12.0'    (use E for East longitude)
;  ALTITUDE  - String containing altitude for observatory, this value overrides
;              the value expected in the header.  Format MUST exactly match this
;              example:      '1290.0'    (in meters)
;  PATH    - Directory in which to look for data (default = current directory)
;  OUTPATH - Directory in which to write data (default = current directory)
;  PANG    - Position angle of Jupiter's north pole on image, this is the
;             standard astronomical position angle measured eastward from
;             North but in this case, take the top of the image as "north"
;             and image must have the proper handedness on input.
;  PSCALE  - Plate scale of image in arcsec/pixel.  Default=0.5 but this
;             default is fairly meaningless.  This needs to be the right
;             value or cropped image will be the wrong size.
;  AUTO    - Flag, if true suppresses request for mouse confirmation.
;             Be careful with this, there is no clean way to interrupt
;             things if something goes wrong.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  96/07/14, Written by Marc W. Buie, Lowell Observatory
;  96/08/12, MWB, added BORDER, LATITUDE, LONGITUDE, ALTITUDE  keywords.
;  96/09/18, MWB, invert PCCD images, plus /AUTO keyword.
;-
pro juploc,root,imnum, $
       PATH=path, OUTPATH=outpath, PANG=in_pang, PSCALE=pscale, BORDER=border, $
       LATITUDE=in_lat, LONGITUDE=in_lon, ALTITUDE=in_alt, AUTO=auto

if badpar(root,7,0,CALLER='JUPLOC: (root) ') then return
if badpar(imnum,[2,3],[0,1],CALLER='JUPLOC: (imnum) ',npts=nframes) then return
if badpar(path,[0,7],0,CALLER='JUPLOC: (path) ',default='./') then return
if badpar(outpath,[0,7],0,CALLER='JUPLOC: (outpath) ',default='./') then return
if badpar(in_pang,[0,2,3,4,5],0,CALLER='JUPLOC: (pang) ',default=0.) then return
if badpar(pscale,[0,2,3,4,5],0,CALLER='JUPLOC: (pscale) ',default=0.5) then return
if badpar(border,[0,2,3],0,CALLER='JUPLOC: (border) ',default=15) then return
if badpar(in_lat,[0,7],0,CALLER='JUPLOC: (latitude) ',default='blank') then return
if badpar(in_lon,[0,7],0,CALLER='JUPLOC: (longitude) ',default='blank') then return
if badpar(in_alt,[0,7],0,CALLER='JUPLOC: (altitude) ',default='blank') then return

; Start up Larry's ephemeris generator (this assumes it's in the path).
spawn,'geteph',unit=pipe
eph = dblarr(2)

; Hard coded ephemeris data for Jupiter for time of SL9 impacts
day   = [  7   , 11   , 15   , 19   , 23   , 27   , 31    ]
selat = [ -3.46, -3.45, -3.43, -3.41, -3.40, -3.39, -3.37 ]
pan   = [ 20.72, 20.70, 20.67, 20.62, 20.57, 20.50, 20.42 ]
prad  = [ 36.95, 36.52, 36.09, 35.67, 35.25, 34.84, 34.44 ] / 2.0

jdcnv,1994,7,day,0.,jdeph
time=jdeph-jdeph[0]

smofac = 7
angle=findgen(101)/100.0*2.0*!pi
x0=120.0
y0=120.0
e  = 0.063
pang = in_pang/!radeg

for f=0,nframes-1 do begin

   suffix = string(imnum[f],format='(i3.3)')
   inname = addslash(path)+root+'m.'+suffix
   if outpath eq path then $
      outname = addslash(outpath)+root+'c.'+suffix $
   else $
      outname = addslash(outpath)+root+'.'+suffix

   if exists(inname) then begin
      image = readfits(inname,hdr,/silent)
      mkhdr,newhdr,image
      sxdelpar,newhdr,'DATE'   ; don't need this info
      sz = size(image)
      if sz[0] ne 2 then begin
         print,'JUPLOC:  Error, file ',inname,'is not a 2-d file.'
      endif else begin

         ; Get header info that is independent of instrument
         sttime=sxpar(hdr,'STRTTIME')
         dateobs=sxpar(hdr,'DATE-OBS')
         instrument = strtrim(sxpar(hdr,'INSTRUME'),2)

         ; Get instrument specific information from header
         if instrument eq 'SNAPSHOT' then begin
            filter=sxpar(hdr,'OPTICSL')
            exptime=float(sxpar(hdr,'SHUTTER'))
            if in_lat eq 'blank' then lat='+37:20:36.0' else lat=in_lat
            if in_lon eq 'blank' then lon='W 121:38:12.0' else lon=in_lon
            if in_alt eq 'blank' then alt='1290.0' else alt=in_alt
            chkdate='19'+strmid(root,0,2)+'/'+strmid(root,2,2)+'/'+strmid(root,4,2)
            if chkdate ne dateobs then begin
               print,'Warning header date does not match file name root.'
               print,'Overriding date of ',dateobs,' changing to ',chkdate
               dateobs=chkdate
            endif
            gain=1.0
            flip=0
         endif else begin
            exptime=float(sxpar(hdr,'EXPTIME'))
            if in_lat eq 'blank' then lat=sxpar(hdr,'LAT-TEL') else lat=in_lat
            if in_lon eq 'blank' then lon=sxpar(hdr,'LONG-TEL') else lon=in_lon
            if in_alt eq 'blank' then alt=sxpar(hdr,'ALT-TEL') else alt=in_alt
            filter=sxpar(hdr,'FILTNAME')
            gain=2.4  ; e-/ADU
            flip=1
         endelse

         if exptime lt 0.0019 then begin
            print,'File ',inname,' ',strtrim(filter,2)
            read,newexp,prompt='Exposure time is ZERO!   Correct value? '
            exptime=newexp
         endif

         ; Manipulate info for new format
         lat=strmid(lat,0,11)
         if strmid(lon,0,2) eq 'E ' then $
            lon='+' + strmid(lon,2,11) $
         else if strmid(lon,0,2) eq 'W ' then $
            lon='-' + strmid(lon,2,11) $
         else $
            lon='?' + strmid(lon,2,11)
         alt=float(strmid(alt,0,8))
         print,inname,'   ',lat,' ',lon
         case instrument of
            'SNAPSHOT': begin
               obsid='Lick'
               observer='E. Dunham, J. Bell, R. Thompson, D. Toublanc'
               observat='Lick Observatory'
               telescop='Crossley 1-m'
               end
            'A': begin
               obsid='CTIO'
               observer='C. Ford'
               observat='CTIO'
               telescop='Planetary Patrol 0.6-m'
               end
            'Z': begin
               obsid='Reunion'
               observer='W. Hubbard, H. Reitsema, F. Roques, A. Peyrot'
               observat='Portable Station on Reunion Island'
               telescop='Portable C-14 (0.36-m)'
               end
            'L': begin
               obsid='QLD/Aus'
               observer='M. Buie, L. Wasserman'
               observat='Portable Station in Queensland Australia'
               telescop='Portable C-14 (0.36-m)'
               end
            'M': begin
               obsid='MKO'
               observer='D. Patten, J. Faust'
               observat='Mauna Kea Observatory'
               telescop='Planetary Patrol 0.6-m'
               end
            else: begin
               print,'No matches for INSTRUMENT [',instrument,']'
               obsid='???'
               observe=' '
               end
         endcase
         reads,dateobs,yr,mh,dy,format='(f4.0,1x,f2.0,1x,f2.0)'
         reads,sttime,hr,mn,sc,format='(f2.0,1x,f2.0,1x,f2.0)'
         dhr=hr+mn/60.0+sc/3600.0+exptime/2.0/3600.0
         jdcnv,yr,mh,dy,dhr,jdobs
         tim=jdobs-jdeph[0]
         jdstr,jdobs,0,rawstr
         midtim=strmid(rawstr,11,8)
         dateobs=strmid(rawstr,5,5)+'/'+strmid(rawstr,2,2)
         interp,time,pan,[tim],jn
         interp,time,prad,[tim],rad
         jn=jn[0]
         rad=rad[0]/pscale
         reads,lat,dd,mm,ss,format='(f3.0,1x,f2.0,1x,f4.1)'
         if dd ge 0.0 then sign = 1 else sign = -1
         dmstorad,sign,abs(dd),mm,ss,lat_r
         reads,lon,dd,mm,ss,format='(f4.0,1x,f2.0,1x,f4.1)'
         if dd ge 0.0 then sign = 1 else sign = -1
         dmstorad,sign,abs(dd),mm,ss,lon_r
         lon_r = -1.0 * lon_r  ; convert from east to west longitude

         printf,pipe,jdobs,500,0,'P5',format='(f13.5,1x,i3,1x,i2,1x,a)'
         readf,pipe,eph
         ra=eph[0]
         dec=eph[1]

         ; values for the rotated jupiter
         crota1 = prival((270.0 + jn)/!radeg)*!radeg
         crota2 = prival((  0.0 + jn)/!radeg)*!radeg
         crota1 = prival((270.0 + jn + in_pang)/!radeg)*!radeg
         crota2 = prival((  0.0 + jn + in_pang)/!radeg)*!radeg
         airmass=airmass[jdobs,ra,dec,lat_r,lon_r]

         ; Copy header info to the new header
         sxaddpar,newhdr,'OBJECT',  'Jupiter'
         sxaddpar,newhdr,'FILTER',  filter, ' Name of filter used'
         sxaddpar,newhdr,'DATE-OBS',dateobs,' UT Date at mid-time of observation'
         sxaddpar,newhdr,'TIMESTR', midtim, ' UT Time at mid-time of observation'
         sxaddpar,newhdr,'DATE-REL','08/15/96',' Date released to archive'
         sxaddpar,newhdr,'LONG-OBS',lon,    ' East longitude of telescope'
         sxaddpar,newhdr,'LAT--OBS',lat,    ' Latitude of telescope'
         sxaddpar,newhdr,'ELEV-OBS',alt,    ' Elevation of telescope in meters'
         sxaddpar,newhdr,'OBSERVER',observer,' Observers names'
         sxaddpar,newhdr,'SUBMITTR','M. Buie, R. Thompson',' Submitters names'
         sxaddpar,newhdr,'DAT-FORM','IMAGE',' Type of data'
         sxaddpar,newhdr,'OBSVTORY',observat,' Observatory Name'
         sxaddpar,newhdr,'TELESCOP',telescop,' Telescope name or description.'
         sxaddpar,newhdr,'CROTA1',  crota1, ' position angle of NAXIS1'
         sxaddpar,newhdr,'CROTA2',  crota2, ' position angle of NAXIS2'
         sxaddpar,newhdr,'PIXSCALE',pscale, ' Image scale in arcsec/pixel'
         sxaddpar,newhdr,'EXPTIME', exptime,' Duration of exposure in seconds'
         sxaddpar,newhdr,'AIRMASS', airmass,' airmass at time of observation'

         print,dateobs,midtim,strtrim(filter,2),exptime,airmass, $
            format='(3x,a,1x,a,1x,a,1x,f7.3,"s",1x,"X=",f4.2)'

         ; Invert image for PCCD data.
         if flip then image=rotate(image,7)

         ; Create unsharpmasked image for display (not saved)
         usmim = 3.0*image - 2.5*smooth(image,smofac)

         ; Find the "background" signal
         z = where(image lt min(image) + 2000)
         setwin,7,xsize=300,ysize=300
         stats,image[z],/robo,/silent,mean=skyback
         image=(image-skyback)*gain
         skyback=skyback*gain

         ; Compute image moments (position and FHWM) from image (not unsharp)
         xsum=fltarr(sz[1])
         ysum=fltarr(sz[2])
         for i=0,sz[2]-1 do xsum=xsum+image[*,i]
         for i=0,sz[1]-1 do ysum=ysum+image[i,*]
         xwd=total(xsum)/max(xsum)
         xloc=total(xsum*findgen(sz[1]))/total(xsum)
         ywd=total(ysum)/max(ysum)
         yloc=total(ysum*findgen(sz[2]))/total(ysum)

         ; Plot X sum
         setwin,1,xsize=200,ysize=200
         plot,xsum/max(xsum),xmargin=[4,1],ymargin=[2,1]
         oplot,[-xwd,xwd]/2.0+xloc,[0.5,0.5]
         xyouts,0.2,0.8,'X',charsize=1.5,/normal

         ; Plot Y sum
         setwin,2,xsize=200,ysize=200
         plot,ysum/max(ysum),xmargin=[4,1],ymargin=[2,1]
         oplot,[-ywd,ywd]/2.0+yloc,[0.5,0.5]
         xyouts,0.2,0.8,'Y',charsize=1.5,/normal

         xlast=-1
         ylast=-1

         !mouse.button=0
         while (!mouse.button ne 2 ) and (!mouse.button ne 4) do begin
            if xloc ne xlast or yloc ne ylast then begin
            
               ; Display unsharped image with overlay of limb and some "belts"
               setwin,0,xsize=sz[1],ysize=sz[2],/show
               tvscl,usmim
               plot,[0],[1],/nodata,/noerase,position=[0,0,1,1], $
                  xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5
               xyouts,0,0,suffix,/normal

               ; Take centroided location as the center and overplot limb with
               ;  a couple of "band" lines.
               x0=xloc
               y0=yloc
               x = rad *           cos(angle)
               y = rad * (1.0-e) * sin(angle)
               xp =  x*cos(pang) - y*sin(pang) + x0
               yp =  x*sin(pang) + y*cos(pang) + y0
               oplot,xp,yp
               yb = [-0.37,0.0,0.5]
               xb1= sqrt( 1.0 - yb^2 )
               xb2= -xb1
               x1 = rad * xb1
               x2 = rad * xb2
               y1 = rad * yb  * (1.0-e)
               xp1 =  x1*cos(pang) - y1*sin(pang) + x0
               yp1 =  x1*sin(pang) + y1*cos(pang) + y0
               xp2 =  x2*cos(pang) - y1*sin(pang) + x0
               yp2 =  x2*sin(pang) + y1*cos(pang) + y0
               for i=0,n_elements(yb)-1 do oplot,[xp1[i],xp2[i]],[yp1[i],yp2[i]]

               ; Set size of output image, size will be a multiple of 2 and provides
               ;  a border around Jupiter.
               wd=fix(2.0*(rad+border)+0.5)
               wd=wd/2*2

               ; Compute location of sub-image, extract, and display
               x1=fix(xloc-wd/2.0+0.5)
               y1=fix(yloc-wd/2.0+0.5)
               x1=max([x1,0])
               y1=max([y1,0])
               x2=x1+wd-1
               y2=y1+wd-1
               if x2 ge sz[1] then begin
                  x2 = sz[1]-1
                  x1 = x2 - wd + 1
               endif
               if y2 ge sz[2] then begin
                  y2 = sz[2]-1
                  y1 = y2 - wd + 1
               endif
               print,x1,':',x2,'  ',y1,':',y2
               imsub=image[x1:x2,y1:y2]
               imsubm=usmim[x1:x2,y1:y2]
               setwin,5,xsize=wd,ysize=wd
               tvscl,imsub
               setwin,6,xsize=wd,ysize=wd
               tvscl,imsubm

               ; Display a rotated image so that Jupiter N is up.
               imrot=rot(usmim,in_pang,1.0,xloc,yloc,missing=0)
               x1=fix((sz[1]-wd)/2.0+0.5)
               y1=fix((sz[2]-wd)/2.0+0.5)
               x2=x1+wd-1
               y2=y1+wd-1
               imrot=imrot[x1:x2,y1:y2]
               setwin,3,xsize=wd,ysize=wd
               tvscl,imrot

               ; Show a special image with the left half of Jupiter mirrored
               ;   on the right side.  If PANG is not correct, then the bands
               ;   will show as v or ^
               setwin,4,xsize=wd,ysize=wd
               imflp=imrot/max(imrot)
               imflp[wd/2:wd-1,*]=rotate(imflp[0:wd/2-1,*],5)
               tvscl,imflp

               setwin,0
               if not keyword_set(auto) then $
                  print,'Left click on center, middle to abort, right click when ok.'
            endif

            if keyword_set(auto) then begin
               !mouse.button=4
            endif else begin
               cursor,x,y,/change,/nowait,/device
               if (!mouse.button and 1) ne 0 then begin
                  while (!mouse.button ne 0) do begin
                     cursor,xx,yy,/up,/wait,/device
                  endwhile
                  xloc = x
                  yloc = y
               endif else begin
                  xlast = xloc
                  ylast = yloc
               endelse
            endelse
         endwhile

         if !mouse.button eq 2 then begin
            print,'ABORT!'
         endif else begin

            print,skyback,min(imsub),max(imsub), $
               format='("   sky background = ",f7.1,3x,"new minmax",2x,f7.1,2x,f10.1)'

            ; Compute BSCALE and BZERO and scale image
            bscale = (max(imsub)-min(imsub)) / 65535.0
            bzero  = max(imsub) - 32767.0*bscale
            outsub = fix( ( imsub - bzero ) / bscale + 0.5 )

            ; Set some more keywords
            sxaddpar,newhdr,'BSCALE',bscale,' Real = file * BSCALE + BZERO'
            sxaddpar,newhdr,'BZERO',bzero
            sxaddpar,newhdr,'DATAMAX',max(imsub),' Maximum value in image'
            sxaddpar,newhdr,'DATAMIN',min(imsub),' Minimum value in image'
            sxaddpar,newhdr,'BITPIX',16
            sxaddpar,newhdr,'NAXIS1',wd
            sxaddpar,newhdr,'NAXIS2',wd

            ; Save data
            print,'   Saving ',strtrim(string(wd),2),'x', $
               strtrim(string(wd),2),' sub-array to ',outname
            writefits,outname,outsub,newhdr
         endelse

      endelse

   endif else begin
      print,'JUPLOC:  Error, file ',inname,' could not be found.'
   endelse

endfor

; Close the pipe
free_lun,pipe

end
