;+
; NAME:
;  linkobj2
; PURPOSE:
;  Cross check source lists from one field and identify moving objects.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  linkobj2,tag,files
; INPUTS:
;  tag   - name of the field, should not contain any blanks
;  files - string array of file names for source lists to be linked.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
;  BADCOL    - Array of column numbers, any source found within 2.5 pixels of
;                these columns will be discarded.  Default = no bad columns.
;
;  CLEANAUTO - Flag, if set, all objects previously identified by "auto"
;                that are flagged with "?" and appear at the end of the list
;                will be removed from the list before any new linked objects
;                are added.
;
;  KEYLIST=  - Name of a file containing a correspondence list. This list
;                 associates a set of standard names with the actual keyword
;                 names found in a FITS file header. If this keyword is
;                 omitted, a default list is used, as if a file with the
;                 following contents had been supplied:
;                  AIRMASS   K  AIRMASS
;                  DATE      K  DATE-OBS
;                  DATETMPL  T  DD-MM-YYYY
;                  EXPDELTA  V  0.0
;                  EXPTIME   K  EXPTIME
;                  FILTER    K  FILTERS
;                  FILENAME  K  CCDFNAME
;                  OBJECT    K  OBJECT
;                  UT        K  UT 
;                 The middle column is a flag. It may be K, for Keyword,
;                 T, for Template, or V, for Value. If it is V, the contents
;                 of the third field on that line should make sense for the
;                 name in the first field.
;
;  KILLSRD -   Flag, if set, this program will delete the pair of offending
;                 srd files if the mean offset between the fixed sources is
;                 out of range.  When this happens, the .obj file is not
;                 altered, an error message is printed on the screen, and
;                 time-stamped information is written to linkobj2.err in
;                 the current directory.
;
;  MAXDUPLERATE - Maximum motion rate permitted for a duple, in arcsec/hour.
;                The default is 5.0 arcsec/hour.  Warning, making this too
;                big can create a very large number of false positive
;                detections.
;
;  MAXINCLINATION - Maximum motion angle from the ecliptic to be allowed.
;                      Used for duple only.  Default=no limit
;
;  MAXMAGDIFF - Maximum magnitude difference allowed between two measurements
;                   to be tagged as a moving object.  This is used ONLY for
;                   checks of duples.  The default value is 1.0 mag.  However,
;                   the allowed difference can be larger if the photometric
;                   errors are large.  The test is to keep objects whose
;                   variation is 3-sigma or less or magdiff less than maxmagdiff
;
;  MAXOBJ    - Maximum number of objects to report as linkages.  Default
;                is to report everything.  Only the first MAXOBJ objects
;                will be reported.  This can safeguard against the situation
;                where a bad astrometric solution leads to a very large number
;                of linkages.
;
;  MAXOFFSET - Maximum offset post-correlation in pixels to flag a bad
;                overlay.  Default is 1.5 pixels.  Use the feature with
;                extreme caution.  It's very easy to let garbage through if
;                you make this too big.
;
;  MAXRATE   - Maximum motion rate permitted for a tripet, in arcsec/hour.
;                The default is 50.0 arcsec/hour.
;
;  MINHITS   - When linking 3 or more frames, this variable controls the
;                minimum number of coincident hits that are required to
;                flag the link as valid.  Default=3.
;
;  MINRATE   - Minimum motion rate permitted for a tripet, in arcsec/hour.
;                The default is 1.0 arcsec/hour.
;
;  PATH      - Optional path for original image directory.
;                If not specified, the current directory is used.
;
;  SNRTHRESH - Signal-to-noise ratio threshold.  Default is to use all objects
;                in the input lists.  If you provide a threshold, all objects
;                with SNR less than this will be excluded from the linking.
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
;   98/03/11, Written by Marc W. Buie, Lowell Observatory
;   2000/02/08, MWB, numerous bug fixes and enhancements.
;   2000/03/09, MWB, minor bug fix for n-tuples and added MINHITS
;   2000/05/01, MWB, added MAXOFFSET keyword
;   2000/07/31, MWB, added MAXOBJ keyword
;   2000/10/18, MWB, added SNRTHRESH keyword
;   2003/10/01, MWB, converted my Delfile call to system file_delete routine
;   2004/06/24, MWB, added MAXMAGDIFF keyword
;   2010/07/19, MWB, minor tweak to accomodate change to frmdxdy
;-
pro linkobj2,tag,files, $
       MAXRATE=maxrate, MINRATE=minrate, KEYLIST=keylist, PATH=path, $
       BADCOL=badcol,MAXDUPLERATE=maxduplerate, $
       MAXINCLINATION=maxinclination,MAXMAGDIFF=maxmagdiff,KILLSRD=killsrd, $
       CLEANAUTO=cleanauto,MINHITS=minhits,MAXOFFSET=maxoffset, $
       MAXOBJ=maxobj,SNRTHRESH=snrthresh

   self='LINKOBJ2: '
   IF badpar(tag,7,0,CALLER=self+'(tag) ') THEN return
   IF badpar(files,7,1,CALLER=self+'(files) ',npts=nfiles) THEN return
   IF badpar(keylist,[7,0],0,CALLER=self+'(KEYLIST) ', $
                             DEFAULT='[[DEFAULT]]') THEN return
   IF badpar(maxrate,[0,2,3,4,5],0,CALLER=self+'(MAXRATE) ', $
                                   DEFAULT=50.0) THEN return
   IF badpar(maxduplerate,[0,2,3,4,5],0,CALLER=self+'(MAXDUPLERATE) ', $
                                        DEFAULT=5.0) THEN return
   IF badpar(maxinclination,[0,2,3,4,5],0,CALLER=self+'(MAXINCLINATION) ', $
                                          DEFAULT=380.0) THEN return
   IF badpar(maxmagdiff,[0,2,3,4,5],0,CALLER=self+'(MAXMAGDIFF) ', $
                                          DEFAULT=1.0) THEN return
   IF badpar(minrate,[0,2,3,4,5],0,CALLER=self+'(MINRATE) ', $
                                   DEFAULT=1.0) THEN return
   IF badpar(badcol,[0,2,3],[0,1],CALLER=self+'(BADCOL) ', $
                                  DEFAULT=-1) THEN return
   IF badpar(killsrd,[0,1,2,3],0,CALLER=self+'(KILLSRD) ', $
                                 DEFAULT=0) THEN return
   IF badpar(cleanauto,[0,1,2,3],0,CALLER=self+'(CLEANAUTO) ', $
                                   DEFAULT=0) THEN return
   IF badpar(minhits,[0,1,2,3],0,CALLER=self+'(MINHITS) ', $
                                 DEFAULT=3) THEN return
   IF badpar(maxoffset,[0,2,3,4,5],0,CALLER=self+'(MAXOFFSET) ', $
                                     DEFAULT=1.5) THEN return
   IF badpar(path,[0,7],0,CALLER=self+'(PATH) ',DEFAULT='') THEN RETURN
   IF badpar(maxobj,[0,2,3],0,CALLER=self+'(MAXOBJ) ', $
                                     DEFAULT=-1) THEN return
   IF badpar(snrthresh,[0,2,3,4,5],0,CALLER=self+'(SNRTHRESH) ', $
                                     DEFAULT=-1) THEN return

   if path ne '' then path=addslash(path)

   if nfiles le 1 then begin
      print,'LINKOBJ: You must specify two or more input files.'
      return
   endif

   left='                              '
   fmt='($,a)'

   loadkeys,keylist,hdrlist

   ; break apart the first file name
   pos=strpos(files[0],'.srd')
   if pos eq -1 then begin
      print,'LINKOBJ2:  ',files[0],' is an invalid file name.'
      return
   endif

   exttag = strmid(files[0],pos+4,999)

   fnobj=strlowcase(tag)+exttag+'.obj'

   data = ptrarr(nfiles)

   ; collect the information from each list
   print,tag+exttag+': (',format=fmt
   fnimlist = strarr(nfiles)
   for i=0,nfiles-1 do begin

      ; Get header information
      pos=strpos(files[i],'.srd')
      fnim=strmid(files[i],0,pos)
      fnimlist[i]=fnim
      if exists(path+fnim+'.fits') then ft='.fits' else ft=''
      if not exists(path+fnim+ft) then begin
         print,'Unable to locate image: ',path+fnim+ft
         goto,cleanup
      endif
      hdr=headfits(path+fnim+ft)
      parsekey,hdr,hdrlist,info

      ; Get the list
      if not exists(files[i]) then begin
         print,'Unable to locate source list: ',files[i]
         goto,cleanup
      endif
      list = readfits(files[i],hdrsrc,/silent)
      idx=sort(list[*,3])
      nobj = n_elements(list)/9
      if nobj gt 1000 then begin
         sel = idx[0:999]
         nsel = 1000
      endif else begin
         nsel=nobj
         sel=indgen(nobj)
      endelse
      maglimit=sxpar(hdrsrc,'MAGLIMIT')
      meanfwhm=sxpar(hdrsrc,'MEANFWHM')

      ; Gather all the information about this list into a structure
      collect = { $
         x:    list[*,0], $      ; raw x pixel coordinate from file
         y:    list[*,1], $      ; raw y pixel coordinate from file
         fwhm: list[*,2], $      ; fwhm of source in pixels
         rawmag: list[*,3], $    ; instrumental magnitude of source
         err:  list[*,4], $      ; Uncertainty on magnitude
         ra:   list[*,5], $      ; RA of source, J2000, in radians
         dec:  list[*,6], $      ; Dec of source, J2000, in radians
         snr:  list[*,7], $      ; SNR of source
         mag:  list[*,8], $      ; Calibrated magnitude of source.
         xi:   dblarr(nobj), $   ; Xi tangent plane coordinate, arcsec
         eta:  dblarr(nobj), $   ; Eta tangent plane coordinate, arcsec
         xa:   fltarr(nobj), $   ; x coordinate shifted to match frame 0
         ya:   fltarr(nobj), $   ; y coordinate shifted to match frame 0
         nobj: nobj, $           ; number of objects in this list
         meanfwhm: meanfwhm, $   ; Mean image size in pixels for this list.
         maglimit: maglimit, $   ; Faint limiting magnitude for this list.
         idx:  idx, $            ; Index into source list sorted by magnitude
         sel:  sel, $            ; Bright subset of idx
         nsel: nsel, $           ; Number of objects in bright subset
         type: intarr(nobj), $   ; object type flag
                                 ; -1 = ignore
                                 ;  0 = unknown
                                 ;  1 = fixed objects
                                 ;  2 = moving objects
         serno: intarr(nobj), $  ; Serial number of matches
         dmag: list[*,3], $      ; magnitude on frame 0 zeropoint
         jd:   info.jd, $        ; JD of observation for this list
         fnim: fnim, $           ; File name of original image
         zp:   0.0, $            ; zero point relative to frame 0
         zpsig: 1.0, $           ; uncertainty on the zero point relative to 0
         maxdist: 0.0, $         ; Maximum search distance for moving objects
                                 ;   equals maxrate*dt
         dx:   0.0, $            ; x offset from frame 0 to this frame
         dy:   0.0, $            ; y offset from frame 0 to this frame
         dt:   0.0 $             ; time offset in hours from frame 0
         }

      ; Push the structure into the master data array
      data[i] = ptr_new(collect,/no_copy)

      ; Compute dt and maxdist for this list
      (*data[i]).dt = ((*data[i]).jd-(*data[0]).jd)*24.0
      (*data[i]).maxdist = maxrate * (*data[i]).dt

      ; determine the ra and dec range for all of the lists
      if i eq 0 then begin
         rarange  = minmax(list[*,5])
         decrange = minmax(list[*,6])
      endif else begin
         rarange  = minmax([rarange, list[*,5]])
         decrange = minmax([decrange,list[*,6]])
      endelse

      ; print some eye candy
      print,strn((*data[i]).dt,format='(f10.1)'),format=fmt
      print,':'+strn((*data[i]).nobj),format=fmt
      if i ne nfiles-1 then print,',',format=fmt

   endfor
   print,')',format=fmt

   serno = 0L

   ; If there is already an obj file, read it in.  That way there is no need
   ;   to find the frame offset.  Note that the input file list information
   ;   must match the obj file exactly for this program to proceed.
   if exists(fnobj) then begin
      rdoblist,fnobj,nobj,filelist,dt,offset,pos,flags,idstr,nfiles0
      if nfiles0 ne nfiles then begin
         print,'Pre-existing obj file does not have ',strn(nfiles), $
               ' file entries.  Unable to proceed.'
         goto,cleanup
      endif
      for i=0,nfiles-1 do begin
         if filelist[i] ne (*data[i]).fnim then begin
            print,'The file list in the pre-existing obj file does not match'
            print,' the input file list.'
            print,'obj -> ',filelist,format='(14(1x,a))'
            print,'arg -> ',files,format='(14(1x,a))'
            goto,cleanup
         endif
      endfor
      if nobj gt 0 and cleanauto then begin
         ; Locate index of last non-auto/? field
         zlast=max(where(idstr ne 'auto' and flags ne '?'))
         if zlast eq -1 then begin
            nobj=0
         endif else if zlast ne nobj-1 then begin
            nobj  = zlast+1
            pos   = pos[*,0:zlast]
            flags = flags[0:zlast]
            idstr = idstr[0:zlast]
         endif
      endif
      for i=0,nfiles-1 do begin
         if i ne 0 then begin
            print,' '+strn(i+1)+'-1 ',format=fmt
            xoff = offset[(i-1)*2]
            yoff = offset[(i-1)*2+1]
            (*data[i]).xa = (*data[i]).x - xoff
            (*data[i]).ya = (*data[i]).y - yoff
            str=strcompress(string('[',xoff,',',yoff,']', $
                                     format='(a,f8.1,a,f8.1,a)'),/remove_all)
            print,str,format=fmt
         endif else begin
            (*data[i]).xa = (*data[i]).x
            (*data[i]).ya = (*data[i]).y
         endelse
      endfor
   endif else begin
      filelist=fnimlist
      nfiles = nfiles
      nobj = 0
      (*data[0]).xa = (*data[0]).x
      (*data[0]).ya = (*data[0]).y
      for i=1,nfiles-1 do begin
         print,' '+strn(i+1)+'-1 ',format=fmt
         frmdxdy,(*data[0]).x[(*data[0]).sel],(*data[0]).y[(*data[0]).sel], $
                 (*data[i]).x[(*data[i]).sel],(*data[i]).y[(*data[i]).sel], $
                 xoff,yoff,error
         if error ne 0 then begin
            print,''
            print,'LINKOBJ2: Error in finding frame offsets.'
            goto,cleanup
         endif
         if i eq 1 then begin
            offset = [xoff,yoff]
            dt     = [(*data[i]).dt]
         endif else begin
            offset = [offset,xoff,yoff]
            dt     = [dt,(*data[i]).dt]
         endelse
         (*data[i]).dx = xoff
         (*data[i]).dy = yoff
         (*data[i]).xa = (*data[i]).x - xoff
         (*data[i]).ya = (*data[i]).y - yoff
         str=strcompress(string('(',xoff,',',yoff,')', $
                                  format='(a,f8.1,a,f8.1,a)'),/remove_all)
         print,str,format=fmt
      endfor
   endelse

;; Find the magic target for error tracking
;tmpdist = ((*data[0]).x - 429.38)^2 + ((*data[0]).y - 1800.78)^2
;zz1 = where(tmpdist eq min(tmpdist))
;zz1 = zz1[0]
;tmpdist = ((*data[1]).x - 426.88)^2 + ((*data[1]).y - 1700.91)^2
;zz2 = where(tmpdist eq min(tmpdist))
;zz2 = zz2[0]
;print
;print,'index ',zz1,zz2
;print,'type, initial        ',(*data[0]).type[zz1],(*data[1]).type[zz2]

   ; Find the "center" of the positions
   if rarange[1]-rarange[0] gt !pi then begin
      racen = (rarange[1]-2.0d0*!dpi+rarange[0])/2.0d0
   endif else begin
      racen = (rarange[1]+rarange[0])/2.0d0
   endelse
   if racen lt 0.0 then racen = racen + 2.0d0*!dpi
   deccen = (decrange[1]+decrange[0])/2.0d0

   ; Convert to tangent plane coordinates
   for i=0,nfiles-1 do begin
      astrd2sn,(*data[i]).ra,(*data[i]).dec,racen,deccen,xi,eta
      (*data[i]).xi  = xi  * 180.0d0/!dpi * 3600.0d0
      (*data[i]).eta = eta * 180.0d0/!dpi * 3600.0d0
   endfor

   ; Block all positions on bad columns
   if badcol[0] ne -1 then begin
      print,' Bx ',format=fmt
      for i=0,n_elements(badcol)-1 do begin
         for j=0,nfiles-1 do begin
            z=where(abs((*data[j]).x-badcol[i]) lt 2.5,count)
            if count ne 0 then (*data[j]).type[z]=-1
         endfor
      endfor
   endif
;print,'type, post bad block ',(*data[0]).type[zz1],(*data[1]).type[zz2]

   ; Block low snr objects
   if snrthresh gt 0.0 then begin
      for j=0,nfiles-1 do begin
         z=where((*data[j]).snr lt snrthresh,count)
         if count ne 0 then (*data[j]).type[z]=-1
      endfor
   endif
;print,'type, post snr thrsh ',(*data[0]).type[zz1],(*data[1]).type[zz2]

   print,''
   print,'  Xf ',format=fmt
   ; Locate all the fixed sources.  To be complete, this requires multiple
   ;   passes through the lists.  At each pass the dao_phot rountine srcor
   ;   is used to find matches.
   for i=0,nfiles-2 do begin
;print,'type, position 1     ',(*data[0]).x[zz1],(*data[0]).y[zz1]
;print,'type, position 2     ',(*data[1]).x[zz2],(*data[1]).y[zz2]
      ; Get the index of all presently unmatched sources in the i_th list.
      ;   This becomes the list of object to try and find matches for.  The
      ;   list of matched objects starts out empty
      zn = where( (*data[i]).type eq 0, countn )
      zm = -1
      countm = 0

      ; Now, loop over the remaining lists to see if any of the sources
      ;   match.   In the process sources are shuffled from the no list to
      ;   the matched list.
      for j=i+1,nfiles-1 do begin

         ; matching can only be done against objects not already matched.
         zc = where( (*data[j]).type eq 0, countc)

         ; match the current list of matched objects against the new list.
         ;   this does not create new matches but uses old ones instead.
         if countm ne 0 and countc ne 0 then begin
            srcor,(*data[i]).xi[zm],(*data[i]).eta[zm], $
                  (*data[j]).xi[zc],(*data[j]).eta[zc], $
                  1.5,ind1,ind2,option=1

            if n_elements(ind1) gt 0 then begin

               ; Do a quick check of the offsets here, find the mean x-y
               ;   offset between the two lists.
               tdx = (*data[j]).xa[zc[ind2]] - (*data[i]).xa[zm[ind1]]
               tdy = (*data[j]).ya[zc[ind2]] - (*data[i]).ya[zm[ind1]]

               if mean(tdx) gt maxoffset or mean(tdy) gt maxoffset then $
                  goto,badoffset

               ; Flag the matched objects as fixed objects
               (*data[j]).type[zc[ind2]]  = 1
               (*data[j]).serno[zc[ind2]] = (*data[i]).serno[zm[ind1]]
;print,'type, match 1        ',(*data[0]).type[zz1],(*data[1]).type[zz2]

               ; Remove the indicies of all matches from the unmatched list
               if n_elements(ind2) ne countc then begin
                  remove,ind2,zc
                  countc=n_elements(zc)
               endif else $
                  countc=0

            endif

         endif

         ; create new matches and add to the list of matched objects
         if countn ne 0 and countc ne 0 then begin
            srcor,(*data[i]).xi[zn],(*data[i]).eta[zn], $
                  (*data[j]).xi[zc],(*data[j]).eta[zc], $
                  1.5,ind1,ind2,option=1

            if ind1[0] ne -1 then begin

               ; Do a quick check of the offsets here, find the mean x-y
               ;   offset between the two lists.
               tdx = (*data[j]).xa[zc[ind2]] - (*data[i]).xa[zn[ind1]]
               tdy = (*data[j]).ya[zc[ind2]] - (*data[i]).ya[zn[ind1]]

               if n_elements(ind1) gt 1 then begin
                  if mean(tdx) gt maxoffset or mean(tdy) gt maxoffset then goto,badoffset
               endif

               ; Flag the matched objects as fixed objects
;print,'type, match 2 (pre)  ',(*data[0]).type[zz1],(*data[1]).type[zz2]
               (*data[i]).type[zn[ind1]]  = 1
               (*data[j]).type[zc[ind2]]  = 1
;print,'type, match 2        ',(*data[0]).type[zz1],(*data[1]).type[zz2]

;setwin,0
;plot,(*data[0]).xa,(*data[0]).ya,psym=4, $
;   xr=[-50,50]+(*data[0]).xa[zz1], $
;   yr=[-50,50]+(*data[0]).ya[zz1]
;oplot,(*data[1]).xa+1,(*data[1]).ya+1,psym=4,color='70ff00'xl

               ; Set the serial number of the object
               (*data[i]).serno[zn[ind1]] = serno + countm + $
                                                lindgen(n_elements(ind1))
               (*data[j]).serno[zc[ind2]] = (*data[i]).serno[zn[ind1]]

               ; Adjust the indicies of the matched list
               if countm eq 0 then zm = zn[ind1] $
               else                zm = [zm,zn[ind1]]
               countm = n_elements(zm)

               ; Remove the indicies of all matches from the unmatched list
               if n_elements(ind1) ne countn then begin
                  remove,ind1,zn
                  countn=n_elements(zn)
               endif else $
                  countn=0
               if n_elements(ind2) ne countc then begin
                  remove,ind2,zc
                  countc=n_elements(zc)
               endif else $
                  countc=0
            endif

         endif

         print,' '+strn(serno+countm),format=fmt

      endfor ; inner file list for loop

      serno = serno + countm

   endfor ; outer file list for loop
;print
;print,'type, post fixed tst ',(*data[0]).type[zz1],(*data[1]).type[zz2]

   print,' nf ',format=fmt
   for i=0,nfiles-1 do begin
      z=where((*data[i]).type eq 0,count)
      print,' '+strn(count),format=fmt
   endfor

   if count eq 0 then return

   print,' fwhm (',format=fmt
   for i=0,nfiles-1 do begin
      z=where((*data[i]).type eq 1,count)
      if count eq 0 then return
      robomean,(*data[i]).fwhm[z],3.0,0.5,mfwhm
      print,strn(mfwhm,format='(f10.1)'),format=fmt
      if i ne nfiles-1 then print,',',format=fmt
   endfor
   print,')',format=fmt

   hit=0
   print,' MM ...' ,format=fmt

   dupnoauto = 0
   dupauto = 0
   numnewobj = 0

   rate=fltarr(nfiles-1)
   for i=0,(*data[0]).nobj-1 do begin
      if (*data[0]).type[i] eq 0 then begin

         ; Get rate and direction for the first duple.

         ; select non-matched objects
         z=where( (*data[1]).type eq 0, count)
         if count eq 0 then goto,quitloop

         ; Xi distance
         z1=where( (*data[1]).xi[z] gt (*data[0]).xi[i]-(*data[1]).maxdist and $
                   (*data[1]).xi[z] lt (*data[0]).xi[i]+(*data[1]).maxdist, count )
         IF count eq 0 THEN goto,nextobj
         z=z[z1]

         ; Eta distance
         z1=where( (*data[1]).eta[z] gt (*data[0]).eta[i]-(*data[1]).maxdist and $
                   (*data[1]).eta[z] lt (*data[0]).eta[i]+(*data[1]).maxdist, count )
         IF count eq 0 THEN goto,nextobj
         z=z[z1]

         ; Rate limit check
         drate=sqrt( ( (*data[1]).xi[z] -(*data[0]).xi[i] )^2 + $
                     ( (*data[1]).eta[z]-(*data[0]).eta[i])^2 ) / $
                        (*data[1]).dt
         z1=where(drate gt minrate and drate lt maxrate,count)
         IF count eq 0 THEN goto,nextobj
         z=z[z1]
         drate=drate[z1]

         ; Compute direction of motion of surviving sources
         ddir = atan( (*data[1]).xi[z]  - (*data[0]).xi[i], $
                      (*data[1]).eta[z] - (*data[0]).eta[i]   )

         ; Convert the ra and dec of the candidates to ecliptic
         ;   (Note: euler assumes B1950 and ra,dec are J2000, this may
         ;    need an extra precession call to get it right.)
         euler, (*data[0]).ra[i]*!radeg, (*data[0]).dec[i]*!radeg, $
                      ecllon0,ecllat0,3
         euler, (*data[1]).ra[z]*!radeg, (*data[1]).dec[z]*!radeg, $
                      ecllon1,ecllat1,3

         ; This is the position angle of motion in ecliptic coordinates.  This
         ;   has been set up to yield 0 degrees for retrograde motion along
         ;   the ecliptic.
         edir = atan( ecllat0 - ecllat1, ecllon0-ecllon1 )

         ; At this point in the loop, z is an index vector that points into
         ;   the second frame and is a list of all objects that when paired
         ;   with the i_th object in the first frame, are within the maxrate
         ;   limit.  This index vector is not supposed to be modified any
         ;   more within this loop.

         ; This vector will record the number of times an object in the
         ;    n_th frame matches up with the vector established by the
         ;    first and second frames.
         hits=intarr(count)

         ; This array holds indicies of all the matches recorded in "hits"
         ;   The length of the array matches the number of objects in "z".
         ;   The width is one less than the number of lists.  The first column
         ;   is the "z" array.
         lidx = replicate(-1L,nfiles-1,count)
         lidx[0,*] = z

         ; This array holds the distance by which the possible match errs from
         ;   matching the prediction based on the list0 and list1 matches.
         ;   This has the same structure as the lidx array.  The first column
         ;   is identically zero since list1 matches the prediction by definition.
         ldist = fltarr(nfiles-1,count)

         ; At this point we have a list of duples for one object in the first
         ;   list that match a number of objects in the second list.  For all
         ;   the rest of the files in the list, we must use the duples to predict
         ;   where the object would be in the other list.  Record any matches and
         ;   their indicies.  This loop will be skipped if there are only two images.

         for j=2,nfiles-1 do begin

            ; select non-matched objects
            z1=where( (*data[j]).type eq 0, count1)
            if count1 eq 0 then goto,nextfile

            ; Xi distance
            z2=where( (*data[j]).xi[z1] gt (*data[0]).xi[i]-(*data[j]).maxdist and $
                      (*data[j]).xi[z1] lt (*data[0]).xi[i]+(*data[j]).maxdist, count2 )
            IF count2 eq 0 THEN goto,nextfile
            z1=z1[z2]

            ; Eta distance
            z2=where( (*data[j]).eta[z1] gt (*data[0]).eta[i]-(*data[j]).maxdist and $
                      (*data[j]).eta[z1] lt (*data[0]).eta[i]+(*data[j]).maxdist, count2 )
            IF count2 eq 0 THEN goto,nextfile
            z1=z1[z2]

            ; Rate limit check
            crate=sqrt( ( (*data[j]).xi[z1] -(*data[0]).xi[i] )^2 + $
                        ( (*data[j]).eta[z1]-(*data[0]).eta[i])^2 ) / $
                           (*data[j]).dt
            z2=where(crate gt minrate and crate lt maxrate,count2)
            IF count2 eq 0 THEN goto,nextfile
            z1=z1[z2]
            crate=crate[z1]

            ; use drate and ddir to predict xi,eta for this frame.
            cdist    = drate * (*data[j]).dt
            cpredeta = cdist*cos(ddir)+(*data[0]).eta[i]
            cpredxi  = cdist*sin(ddir)+(*data[0]).xi[i]

            for k=0,count-1 do begin

               dist = ((*data[j]).xi[z1] -cpredxi[k])^2 + $
                      ((*data[j]).eta[z1]-cpredeta[k])^2
               idx  = where(dist eq min(dist))
               idx  = idx[0]
               dist = sqrt(dist[idx])

               if dist lt 2.5 then begin
                  hits[k]     = hits[k]+1
                  lidx[j-1,k] = z1[idx]
                  ldist[j-1,k] = dist
               endif

            endfor

nextfile:
         endfor

         ; This block will only execute when there are three or more frames
         ;   being linked.  hits will be identically zero for the case of just
         ;   two frames.
         if max(hits) gt 0 then begin
            zh=where(hits eq max(hits) and hits ge (minhits-1),counth)
            if counth le 1 then begin
               zh=zh[0]
            endif else begin
               mdist=fltarr(counth)
               for j=0,counth-1 do begin
                  if nfiles gt 3 then $
                     mdist[j] = mean(ldist[1:nfiles-2,zh[j]]) $
                  else $
                     mdist[j] = ldist[1,zh[j]]
               endfor
               zh1 = where(mdist eq min(mdist))
               zh=zh[zh1[0]]
            endelse

         ; This is for the case of just two files and we have to do the more
         ;   restrictive duple matching.
         endif else if nfiles eq 2 then begin

;if i eq zz1 then print,'duple check'
            ; Rate limit check
            z1=where(drate le maxduplerate,count)
;if i eq zz1 then print,'count      ',count
            IF count eq 0 THEN goto,nextobj
;if i eq zz1 then print,z1,z[z1]
;if i eq zz1 then print,'edir, maxinc ',edir[z1[*]]*!radeg,maxinclination

            ; Impose a limit on direction of motion in ecliptic coordinates.
            if maxinclination gt 0 and maxinclination lt 180 then begin
               z2=where(abs(edir[z1])*!radeg le maxinclination,count)
;if i eq zz1 then print,'edir test N',count
               if count eq 0 then goto,nextobj
               z1=z1[z2]
;if i eq zz1 then print,z1,z[z1]
            endif

            ; Additional limits.  There should not be many objects at this
            ;   point so the examination is done in a scalar mode because
            ;   the logic is a bit more complicated.
            for k=0,count-1 do begin

               ; Impose a limit on the magnitude difference.  Compare against
               ;   the brighter of the two and require that it be within 2 sigma
               ;   using the sigma from the brighter of the two.
;if i eq zz1 then begin
;   print,zz1,zz2,i,z[z1[k]]
;   print,(*data[0]).mag[i],(*data[1]).mag[z[z1[k]]]
;   print,(*data[0]).err[i],(*data[1]).err[z[z1[k]]]
;endif
               ; first measurement is brighter (higher SNR)
               magdiff = abs((*data[1]).mag[z[z1[k]]] - (*data[0]).mag[i])
               if (*data[0]).mag[i] le (*data[1]).mag[z[z1[k]]] then begin
                  magsig = (*data[0]).err[i]
               ; or, second measurement is brighter
               endif else begin
                  magsig = (*data[1]).err[z[z1[k]]]
               endelse
;if i eq zz1 then print,'magdiff ',magdiff,magsig,magdiff/magsig

               if magdiff gt 3.0*magsig and magdiff gt maxmagdiff then z1[k]=-1

;if i eq zz1 then print,' post mag test'
;if i eq zz1 then print,z1,z[z1]

               ; Impose an additional limit that says if the source is brighter
               ;   than 1 magnitude above the limiting magnitude, then its fwhm
               ;   can't be more than 0.5 pixels broader than the mean fwhm.
;if i eq zz1 then print,' fwhm ',(*data[0]).fwhm[i],(*data[1]).fwhm[z[z1[k]]], $
;   (*data[0]).meanfwhm,(*data[1]).meanfwhm
               if z1[k] ne -1 then begin
                  if ( (*data[0]).maglimit-(*data[0]).mag[i] gt 1.0 and $
                       (*data[0]).fwhm[i]-(*data[0]).meanfwhm gt 0.5 ) or $
                     ( (*data[1]).maglimit-(*data[1]).mag[z[z1[k]]] gt 1.0 and $
                       (*data[1]).fwhm[z[z1[k]]]-(*data[1]).meanfwhm gt 0.5 ) then z1[k]=-1
               endif
;if i eq zz1 then print,' post fwhm test'
;if i eq zz1 then print,z1,z[z1]

            endfor
            z2 = where(z1 ge 0,count)
            if count eq 0 then goto,nextobj
            z1=z1[z2]

            if count eq 0 then $
               goto,nextobj $
            else if count eq 1 then $
               zh = z1[0] $
            else begin
               magdiff = abs((*data[0]).mag[i] - (*data[1]).mag[z[z1]])
               z2=where( magdiff eq min(magdiff), count)
               z1=z1[z2]
               zh = z1[0]
            endelse
;print,long(i),(*data[0]).mag[i],(*data[0]).err[i],(*data[0]).fwhm[i]
;print,z[zh],(*data[1]).mag[z[zh]],(*data[1]).err[z[zh]],(*data[1]).fwhm[z[zh]]

         endif else begin
            zh=-1
         endelse

         ; Check to make sure this hit has not already been recorded.
         if zh ge 0 then begin
            newpos   = [(*data[0]).x[i],(*data[0]).y[i]]
            for j=1,nfiles-1 do begin
               if lidx[j-1,zh] eq -1 then $
                  newpos = [newpos,-1,-1] $
               else $
                  newpos = [newpos,(*data[j]).x[lidx[j-1,zh]],(*data[j]).y[lidx[j-1,zh]]]
            endfor
            for j=0,nobj-1 do begin
               if zh ge 0 then begin
                  diff = abs(pos[*,j]-newpos)
                  if max(diff) lt 1.5 then begin
                     if strpos(idstr[j],'auto') ge 0 then $
                        dupauto = dupauto + 1 $
                     else $
                        dupnoauto = dupnoauto + (flags[j] eq 'y')
                     (*data[0]).type[i] = -1
                     for j=1,nfiles-1 do $
                        (*data[j]).type[lidx[j-1,zh]] = -1
                     zh=-1
                  endif
               endif
            endfor
         endif

         ; Add to list
         if zh ge 0 then begin
;            magchk   = [(*data[0]).mag[i]]
;            widchk   = [(*data[0]).fwhm[i]]
            (*data[0]).type[i] = 2
            for j=1,nfiles-1 do begin
               if lidx[j-1,zh] ne -1 then $
                  (*data[j]).type[lidx[j-1,zh]] = 2
;                  magchk = [magchk,(*data[j]).mag[lidx[j-1,zh]]]
;                  widchk = [widchk,(*data[j]).fwhm[lidx[j-1,zh]]]
            endfor

            if nobj eq 0 then begin
               pos   = newpos
               flags = '?'
               idstr = 'auto'
               nobj  = 1
            endif else begin
               pos   = [ [[pos]], [newpos] ]
               flags = [flags,'?']
               idstr = [idstr,'auto']
               nobj  = nobj+ 1
            endelse

            if hit eq 0 then print,''
            print,strn(hit,length=3)+' ',format=fmt
            print,' ('+strb36(nobj-1)+'):',format=fmt
            print,' '+strn(drate[zh],len=5,format='(f10.1)')+',',format=fmt
            print,' '+strn(fix(ddir[zh]*!radeg+0.5), len=5)+',',format=fmt
            print,' '+strn(fix(edir[zh]*!radeg+0.5), len=5)+',',format=fmt
            hit=hit+1
            print,''

         endif

      endif

      if maxobj gt 0 and nobj eq maxobj then begin
         print,' Maximum number of objects reached, aborting.'
         goto, quitloop
      end

nextobj:
   endfor
quitloop:

   IF hit eq 0 THEN print,' None!'

   if dupnoauto ne 0 then $
      print,' --> ',dupnoauto,' triplets previously noted (not auto)'

   if dupauto ne 0 then $
      print,' --> ',dupauto,' triplets previously noted (auto)'

   wroblist,fnobj,nobj,filelist,dt,offset,pos,flags,idstr,nfiles

   if maxobj gt 0 and nobj eq maxobj then begin
      jdstr,systime(/julian,/utc),0,curtimestr
      openw,lun,'linkobj2.err',/get_lun,/append
      printf,lun,curtimestr,' ',files[0],' ',files[1],' more than ', $
         strn(maxobj),' objects.'
      free_lun,lun
   endif

cleanup:

   for i=0,nfiles-1 do begin
      ptr_free,data
   endfor
   return

badoffset:

   print,''
   print,'ERROR!  Excessive mean offset of fixed stars ',mean(tdx),mean(tdy)

   jdstr,systime(/julian,/utc),0,curtimestr

   openw,lun,'linkobj2.err',/get_lun,/append
   printf,lun,curtimestr,files[i],files[j],mean(tdx),mean(tdy), $
      format='(a,1x,a,1x,a,"  star offset",2(1x,f10.2))'

   if killsrd then begin
      print,'Deleting ',files[i],' ',files[j]
      printf,lun,curtimestr,'   Deleting ',files[i],' ',files[j]
      file_delete,files[i],/quiet,/noexpand_path
      file_delete,files[j],/quiet,/noexpand_path
   endif

   free_lun,lun

   goto,cleanup

end
