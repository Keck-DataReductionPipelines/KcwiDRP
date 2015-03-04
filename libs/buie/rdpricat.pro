;+
; NAME:
;     rdpricat
; PURPOSE: (one line)
;     Read data from a private star catalog.
; DESCRIPTION:
;
;     This procedure reads a semi-standard file containing a private format
;     star catalog.
;
;     The Version 1 format of the file is as follows:
;        Line 1 - Id line, must be 'PRICAT v1'
;        Line 2 - Number of magnitudes stored in file for each star.
;                    You can also list the names for the magnitudes here.  They
;                    aren't read but the documentation can be stored here.
;        Line 3 to end -
;           id name  h  m  s   sign  d  m  s   dra ddec epoch/equinox mag_1 mag_2 ... mag_n
;           i3  a16 i2 i2 f6.3  a1  i2 i2 f5.2  d   d       f           f     f        f
;
;        The RA and Dec must be in the epoch and equinox stated on the same line.
;        The RA proper motion is in seconds of time per century (not including
;        the cos(dec) factor.  The Dec proper motion is in seconds of arc
;        per century.  If the epoch is listed as 2000.0 it is assumed to be
;        in the J2000 coordinate system.  Any other epoch is treated as if
;        it were in the B1950 system (regardless of epoch and equinox).
;        Therefore, going to J2000 requires precession to B1950 then shifting
;        to the J2000 system.
;
;        Example -
;       201 1989 Comp      12:22:34.455 +10:34:45.56 +0.0020 -0.0030 1950.0 14.567
;
;     The values stored for the magnitudes are completely left up the the user.
;     This program doesn't use it.
;
; CATEGORY:
;     File I/O
; CALLING SEQUENCE:
;     rdpricat,id,name,ra,dec,mags
; INPUTS:
;     None.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     EPOCH   - JD of epoch to correct for proper motion to.
;                 Default = catalog epoch.
;     FILE    - Name of star catalog file to read.
;                 Default = 'private.cat'  in current directory.
;
;     Only one of the following is allowed to be set.  If none are set,
;        the coordinates will be returned in the equinox of the catalog.
;     B1950   - Coordinates should be referred to equinox of B1950.
;     J2000   - Coordinates should be referred to equinox of J2000.
;     OFDATE  - Coordinates should be referred to equinox of date.
;                 If selected, EPOCH must be provided.
;
; OUTPUTS:
;	   id      - Identification number of the stars.
;	   object  - Name of the stars.
;	   ra      - Right Ascension in radians.
;	   dec     - Declination in radians.
;	   mags    - Stellar magnitude(s)
;
; KEYWORD OUTPUT PARAMETERS:
;     DRA     - Proper motion in "/year for right ascension.
;     DDEC    - Proper motion in "/year for declination.
;     NMAGS   - number of magnitudes for each object.
;     NPTS    - number of catalog entries.
;
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;
;   WARNING!!!! Do not put ANY tabs in the catalog file.  The program will
;     very likely crash on that line.  v3.1.0 restriction?
;
; PROCEDURE:
; MODIFICATION HISTORY:
;     93/03/31, Written by Marc W. Buie, Lowell Observatory
;     93/09/30, MWB, fixed erroneous precession for OFDATE case.
;  2000/11/08, MWB, removed use of obsolete ()
;-
pro rdpricat,id,name,ra,dec,mags, $
             DRA=dra, DDEC=ddec, EPOCH=epoch, FILE=file, $
             NMAGS=nmags, NPTS=npts, $
             B1950=B1950, J2000=J2000, OFDATE=ofdate

if badpar(epoch,[0,2,3,4,5],0,CALLER='RDPRICAT: (epoch) ') then return
if badpar(file,[0,7],0,CALLER='RDPRICAT: (file) ',default='private.cat') then return
if badpar(B1950,[0,1,2,3],0,CALLER='RDPRICAT: (B1950) ') then return
if badpar(J2000,[0,1,2,3],0,CALLER='RDPRICAT: (J2000) ') then return
if badpar(OFDATE,[0,1,2,3],0,CALLER='RDPRICAT: (OFDATE) ') then return

if not exists(file) then begin
   msgstr='File '+file+' could not be found.  Aborting.'
   message,msgstr
endif

set=0
if keyword_set(B1950)  then set = set+1
if keyword_set(J2000)  then set = set+1
if keyword_set(OFDATE) then set = set+1
if set gt 1 then begin
   print,'RDPRICAT: Error. Only one of B1950, J2000, or OFDATE can be selected.'
   return
endif

if keyword_set(OFDATE) and not keyword_set(epoch) then begin
   print,'RDPRICAT: Error. Epoch must be provided with OFDATE request.'
   return
endif

openr,lun,file,/get_lun

newline=''
npts=0

while (not eof(lun)) do begin

   if npts eq 0 then begin
      ; Read and verify file version and identification.
      readf,lun,newline,format='(a)'
      if newline ne 'PRICAT v1' then begin
         print,'Incorrect file version, expecting PRICAT V1, got ',newline
         goto,bad
      endif
      readf,lun,nmags
      nmags=fix(nmags)
      t_mags=fltarr(nmags)
      t_id = 0
      t_name=''
      pm=''
      fmt='(i3,1x,a16,2(1x,i2),1x,f6.3,1x,a1,2(i2,1x),f5.2,1x,f6.3,1x,f6.3,1x,f6.1,' + $
             strcompress(string(nmags),/rem)+'(1x,f7.4))'
   endif

   readf,lun,t_id,t_name,rh,rm,rs,pm,dd,dm,ds,t_dra,t_ddec,t_epoch,t_mags,format=fmt

   t_ra  = (rh + rm/60.0d0 + rs/3600.0d0) * 15.0d0 / 180.0d0 * !dpi
   t_dec = (dd + dm/60.0d0 + ds/3600.0d0) / 180.0d0 * !dpi
   if pm eq '-' then t_dec = -1.0d0 * t_dec

   if npts eq 0 then begin
      id   = t_id
      name = t_name
      ra   = t_ra
      dec  = t_dec
      mags = t_mags
      dra  = t_dra
      ddec = t_ddec
      in_epoch = t_epoch
   endif else begin
      id   = [ id   , t_id   ]
      name = [ name , t_name ]
      ra   = [ ra   , t_ra   ]
      dec  = [ dec  , t_dec  ]
      mags = [ [mags] , [t_mags] ]
      dra  = [ dra  , t_dra  ]
      ddec = [ ddec , t_ddec ]
      in_epoch = [ in_epoch , t_epoch ]
   endelse

   npts = npts + 1

endwhile

; Convert proper motion vectors to radians per century.
dra  = dra  /  43200.0d0 * !dpi
ddec = ddec / 648000.0d0 * !dpi

; Now that we have the catalog, convert to the correct epoch and equinox.

; First, correct for proper motion.
if keyword_set(epoch) then begin
   jd2year,epoch,out_year
   year = fix(in_epoch)
   jdcnv,year,1,1,0.,in_jd
   in_jd = in_jd + (in_epoch-year)*365.25d0
   dt = (epoch - in_jd) / 36525.0d0          ; time difference in centuries.
   ra  = ra + dra*dt
   dec = dec + ddec*dt
endif

; Make a copy in degrees for the precession routines.
rad  = ra  * 180.0d0 / !dpi
decd = dec * 180.0d0 / !dpi

; Second, convert to requested equinox.
if keyword_set(B1950) then begin
   z2000 = where(in_epoch eq 2000,count_2000)
   zother = where(in_epoch ne 2000 and in_epoch ne 1950,count_other)
   if count_2000 ne 0 then begin
      for i=0,n_elements(z2000)-1 do begin
         bprecess,rad[z2000[i]],decd[z2000[i]],new_ra,new_dec
         rad[z2000[i]]  = new_ra
         decd[z2000[i]] = new_dec
      endfor
   endif
   if count_other ne 0 then begin
      for i=0,n_elements(zother)-1 do begin
         new_ra  = rad[zother[i]]
         new_dec = decd[zother[i]]
         precess,new_ra,new_dec,in_epoch[zother[i]],1950.0
         rad[zother[i]]  = new_ra
         decd[zother[i]] = new_dec
      endfor
   endif
endif

if keyword_set(J2000) then begin
   z1950 = where(in_epoch eq 1950,count_1950)
   zother = where(in_epoch ne 2000 and in_epoch ne 1950,count_other)
   if count_1950 ne 0 then begin
      for i=0,n_elements(z1950)-1 do begin
         jprecess,rad[z1950[i]],decd[z1950[i]],new_ra,new_dec
         rad[z1950[i]]  = new_ra
         decd[z1950[i]] = new_dec
      endfor
   endif
   if count_other ne 0 then begin
      for i=0,n_elements(zother)-1 do begin
         new_ra  = rad[zother[i]]
         new_dec = decd[zother[i]]
         precess,new_ra,new_dec,in_epoch[zother[i]],1950.0
         jprecess,new_ra,new_dec,new_ra1,new_dec1
         rad[zother[i]]  = new_ra1
         decd[zother[i]] = new_dec1
      endfor
   endif
endif

if keyword_set(OFDATE) then begin
   z1950 = where(in_epoch eq 1950,count_1950)
   z2000 = where(in_epoch eq 2000,count_2000)
   zother = where(in_epoch ne 2000 and in_epoch ne 1950,count_other)
   if count_1950 ne 0 then begin
      for i=0,n_elements(z1950)-1 do begin
         new_ra  = rad[z1950[i]]
         new_dec = decd[z1950[i]]
         precess,new_ra,new_dec,in_epoch[z1950[i]],out_year
         rad[z1950[i]]  = new_ra
         decd[z1950[i]] = new_dec
      endfor
   endif
   if count_2000 ne 0 then begin
      for i=0,n_elements(z2000)-1 do begin
         bprecess,rad[z2000[i]],decd[z2000[i]],new_ra,new_dec
         precess,new_ra,new_dec,1950.,out_year
         rad[z2000[i]]  = new_ra
         decd[z2000[i]] = new_dec
      endfor
   endif
   if count_other ne 0 then begin
      for i=0,n_elements(zother)-1 do begin
         new_ra  = rad[zother[i]]
         new_dec = decd[zother[i]]
         precess,new_ra,new_dec,in_epoch[zother[i]],out_year
         rad[zother[i]]  = new_ra
         decd[zother[i]] = new_dec
      endfor
   endif
endif

; Convert back to radians for output.
ra  = rad  / 180.0d0 * !dpi
dec = decd / 180.0d0 * !dpi

; Convert proper motion vectors back to "/year from radians per century.
dra  = dra  *  43200.0d0 / !dpi
ddec = ddec * 648000.0d0 / !dpi

free_lun,lun
return

bad:
   free_lun,lun

end

