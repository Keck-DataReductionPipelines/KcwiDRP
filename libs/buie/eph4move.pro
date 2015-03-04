;+
; NAME: 
;  eph4move
; PURPOSE:
;  Generates ephemeris files for use by the MOVE computer.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  eph4move,objfile,outfile,TIME=time
; INPUTS:
;  objfile - String containing file name of objects to generate ephemerides
;              for.  This file is a correspondence list between offical
;              object codes (see EPHEM.PRO) and common names.  RDMATCH
;              Gives a description of the format of this file.
;  outfile - String containing file name for output, this is the file you
;              will give to the MOVE program.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  TIME    - By default, the program will read the system clock and generate
;              an output file consistent with the current date.  However,
;              if you wish to override the date, you give a time vector
;              here such as [1995,11,30,0,0,0].  You can also specify a
;              Julian date (scalar) for the starting time.
;  NDAYS   - Number of days to run ephemeris for, default = 1
;  DT      - Spacing between ephemeris points, default = 1 hour.
; OUTPUTS:
;  All information is written to the output file.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  This is setup to work for Anderson Mesa only.
; PROCEDURE:
; MODIFICATION HISTORY:
;  1995/11/30, Written by Marc W. Buie, Lowell Observatory
;  1995/12/20, MWB, Added NDAYS and DT keywords.
;  1996/05/02, MWB, Fixed pipe overflow problem when doing to many position.
;  1998/07/03, MWB, added JD option to TIME
;  2009/11/09, MWB, minor change to force object code to uppercase when sending
;                     to geteph.  Some codes aren't accepted as lowercase.
;-

pro eph4move,objfile,outfile,TIME=time,NDAYS=ndays,DT=dt,DEBUG=debug

   if n_params() ne 2 then begin
      print,'eph4move,objfile,outfile[,TIME=time]'
      return
   endif

   self='EPH4MOVE: '
   if (badpar(ndays,[0,2,3,4,5],0,CALLER=self+'(ndays) ', $
                                  default=1.0)) then return
   if (badpar(dt,[0,2,3,4,5],0,CALLER=self+'(dt) ', $
                               default=1.0)) then return
   if (badpar(time,[0,2,3,4,5],[0,1],CALLER=self+'(TIME) ', $
                                     rank=timerank,default=-1)) then return
   if (badpar(debug,[0,1,2,3],0,CALLER=self+'(DEBUG) ',default=0)) then return

   ; Grab the names
   rdmatch,objfile,obid,objname

   if n_elements(obid) gt 999 then $
      message,'ERROR! there are too many objects in the list.  Limit is 999'

   ; Start up Larry''s ephemeris generator (this assumes it''s in the path).
   spawn,'geteph',unit=pipe

   ; Open the output file for writing.
   openw,outlun,outfile,/get_lun

   ; Write the ID header
   printf,outlun,'MOVE EPHEMERIS V1.0'

   ; Construct time vector.
   if time[0] lt 0.0  then begin
      curjd = systime(/julian,/utc)
      jd0 = double(floor(curjd - 0.5d0)) + 7.0d0/24.0d0
      deljd = (curjd-jd0)*24.0
      if ( deljd gt 24.0 ) then jd0=jd0+1.0d0
   endif else if timerank eq 1 then begin
      jdcnv,time[0],time[1],time[2],time[3]+time[4]/60.0+time[5]/3600.0,jd0
   endif else begin
      jd0 = double(floor(time - 0.5d0)) + 7.0d0/24.0d0
      deljd = (time-jd0)*24.0
      if ( deljd gt 24.0 ) then jd0=jd0+1.0d0
   endelse
   jd = dindgen(ceil(ndays*24.0/dt)+1)*dt/24.0 + jd0

   ; print out first and last time for info
   jdstr,jd[0],0,str1
   jdstr,jd[n_elements(jd)-1],0,str2
   print,'Time span for ephemeris is ',str1,' to ',str2

   ; Allocate space for the returning ephemeris information
;   eph = dblarr(2,n_elements(jd))
   eph = dblarr(2)

   for j=0,n_elements(obid)-1 do begin

      print,objname[j],format="($,'   ',a)"
      if keyword_set(debug) then print,''
      first=1

      ; Get ephemeris data.
      for i=0,n_elements(jd)-1 do begin
         printf,pipe,jd[i],688,2+50,strupcase(obid[j]), $
            format='(f13.5,1x,i3,1x,i2,1x,a)'
         if keyword_set(debug) then $
            print,jd[i],688,2+50,strupcase(obid[j]), $
               format='(f13.5,1x,i3,1x,i2,1x,a)'
         readf,pipe,eph
         if first eq 1 then begin
            goodobj = eph[0,0] ge -90.0
            first=0
            if not goodobj then goto,badobj
         endif
         if goodobj then $
            printf,outlun,objname[j]+'                    ', $
               2000.0,jd[i],eph[0],eph[1], $
               format='(a20,1x,f6.1,1x,f13.5,1x,f10.8,1x,f11.8)'

      endfor

badobj:

      if goodobj then begin
         print,'  done.'
      endif else begin
         print,'  ****  Error ',obid[j],' is an invalid object code.'
      endelse

   endfor

   ; Close the pipe
   free_lun,pipe
   free_lun,outlun
end
