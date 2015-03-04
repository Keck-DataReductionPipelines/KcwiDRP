;+
; NAME:
;    getobloc
; PURPOSE: (one line)
;    Fetch location of observatory given its code
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;    getobloc,obscode,obs
;
; INPUTS:
;  obscode - observatory code (integer or string)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  OBSFILE - Name of observatory code file to read.  Default=obscode.dat
;
; OUTPUTS:
;  obs - anonymous structure that contains information for the requested
;          observatory.  Four tags are defined:
;             name - string name of observatory
;             lat  - Latitude of observatory (radians)
;             lon  - Longitude of observatory (radians)
;             obscode - observatory code (input returned always as string)
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;  Note, if the observatory code file is NOT found, '688' (Lowell Observatory)
;    is used.
;
;  If the file is okay but the code is not recognized, the observatory
;    name is returned as 'unknown' and lat=lon=0.0
;
;  Observatory coordinate file is read each time this procedure is called.
;    This really needs to add code to read the file once and store its
;    contents in non-volatile memory (anonymous stucture in a common block)
;    and then re-read the file only when it's date changes.
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    02/03/14, Written by David Tucker
;  2002/12/09, MWB, converted to separate standalone routine.
;-
pro getobloc,obscode,obs,OBSFILE=obsfile

   if badpar(obscode,[1,2,3,7],0,caller='getobloc: (obscode) ', $
                                             type=codetype) then return

   if badpar(obsfile,[0,7],0,caller='getobloc: (OBSFILE) ', $
                                             default='obscode.dat') then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   obs = { $
      name:"",        $ ;Name of observatory
      lat:0.0,        $ ;Latitude of observitory
      lon:0.0,        $ ;Longitude of observatory
      obscode:obscode $ ;Unique observatory code
      }
  
   ; Try to load file coordinates, if failed, set to Lowell
   rdobscod, code,alllon,rhosinp,rhocosp,obsname,valid,FILE=obsfile
   if valid then begin
      idx=where(obscode eq code,count)
      idx=idx[0]
      if (count eq 1) then begin
         obs.lon = (360.0-alllon[idx])/180.0*!pi
         obs.lat = atan(rhocosp[idx],rhosinp[idx])
         obs.name=strtrim(obsname[idx],2)
      endif else begin
         obs.name='unknown'
      endelse
   endif else begin
      print,'Observatory code file ',obsfile,' not found.'
      print,'Using Lowell Observatory (688) built in default values.'
      obscode = '688'
      obs.name=''
   endelse

   ; Hardcoded position for 42" to get the program running on failure.
   if obscode eq '688' and obs.name eq '' then begin
      ; This is the GPS position for the 42", derived 1993 Sep 08
      obs.lat = (35.0+5.0/60.0+48.740/3600.0)/180.0*!pi
      obs.lon = (111.0+32.0/60.0+10.601/3600.0)/180.0*!pi
      obs.name= 'Lowell Observatory - Anderson Mesa Station'
      obs.obscode = obscode
   endif

end
