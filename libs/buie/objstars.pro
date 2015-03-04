;+
; NAME:
;  objstars
; PURPOSE:
;  Get a list of stars from the USNO A2.0 catalog centered on a solar system object.
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
;
; CALLING SEQUENCE:
;  objstars,starttime,ndays,objcode,outfile,OBSCODE=obscode,PAD=pad
;
; INPUTS:
;  starttime - Starting time for object ephemeris.  This can be provided as
;                a julian date (scalar), or a 3,4,5, or 6 element time vector
;                [y,m,d,h,m,s].  h, m, s, are optional in order of decreasing
;                significance.
;  ndays     - Number of days to run ephemeris over.
;  objcode   - Object code for solar system object (see EPHEM.PRO)
; OPTIONAL INPUT PARAMETERS:
;  outfile   - Output file name, default is objcode+'.'+FILETYPE
; KEYWORD INPUT PARAMETERS:
;  FILETYPE  - Specifies the output format for the file.  This type is also
;                used as the default suffix of the output file name.
;                edb - (default) ascii format needed for XEphem.
;  OBSCODE   - Standard M.P.C. observatory code, default is geocentric
;  PAD       - padding distance around path of object for extraction,
;                default = 240 arcsec
; OUTPUTS:
;
;  A file is created that is in the format specified.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  It is not practical to extract too large of a list.  Also, this program
;    does not have appropriate logic to handle the case where the object's
;    path crosses 0h RA.  There is an arbitrary limit of 2 degrees placed
;    on the extraction widths to prevent extracting the entire star catalog.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 1999 July 13
;  2002/09/09, MWB, added support for string obscode values
;  2003/10/01, MWB, converted my Delfile call to system file_delete routine
;-
pro objstars,starttime,ndays,objcode,outfile, $
       OBSCODE=obscode,PAD=pad,FILETYPE=filetype

   ;Verify correct number of parameters.
   if n_params() lt 3 then begin
      print,'objstars,starttime,ndays,objcode,[outfile],[OBSCODE=val],[PAD=pad]'
      return
   endif

   if badpar(starttime,[2,3,4,5],[0,1],CALLER='OBJSTARS: (starttime) ') then return
   if badpar(ndays,[2,3,4,5],0,CALLER='OBJSTARS: (ndays) ') then return
   if badpar(objcode,7,0,CALLER='OBJSTARS: (objcode) ') then return
   if badpar(obscode,[0,1,2,3,7],0,CALLER='OBJSTARS: (OBSCODE) ', $
                               default='500',type=codetype) then return
   if badpar(pad,[0,2,3,4,5],0,CALLER='OBJSTARS: (PAD) ', $
                               default=240.0) then return
   if badpar(filetype,[0,7],0,CALLER='OBJSTARS: (FILETYPE) ', $
                               default='edb') then return

   if codetype ne 7 then begin
      obscode = strn(obscode,length=3,padchar='0')
   endif else begin
      obscode = strupcase(obscode)
   endelse

   validtypes=['edb','app']
   z=where(filetype eq validtypes,count)
   if count ne 1 then begin
      print,'OBJSTARS: Filetype of [',filetype,'] is not supported.'
      print,'valid types are ',validtypes
      return
   endif

   if badpar(outfile,[0,7],0,CALLER='OBJSTARS: (outfile) ', $
                         default=objcode+'.'+filetype) then return

   case n_elements(starttime) OF
      1: begin
         jd0=starttime
      end
      3: begin
         jdcnv,starttime[0],starttime[1],starttime[2],0.,jd0
      end
      4: begin
         jdcnv,starttime[0],starttime[1],starttime[2],starttime[3],jd0
      end
      5: begin
         hr = float(starttime[3]) + float(starttime[4])/60.0
         jdcnv,starttime[0],starttime[1],starttime[2],hr,jd0
      end
      6: begin
         hr = float(starttime[3]) + float(starttime[4])/60.0 + $
              float(starttime[5])/3600.0
         jdcnv,starttime[0],starttime[1],starttime[2],hr,jd0
      end
      else: begin
         print,'OBJSTARS: Error!  starttime must be one of the following:'
         print,'   scalar julian date,'
         print,'   3 element time vector, [y,m,d]'
         print,'   4 element time vecotr, [y,m,d,h]'
         print,'   5 element time vecotr, [y,m,d,h,m]'
         print,'   6 element time vecotr, [y,m,d,h,m,s]'
         print,''
         return
      end
   endcase

   jd = jd0 + dindgen(ndays)

   starfile='tmp.refout'

   ephem,jd,obscode,2,objcode,eph

   rarange = minmax(eph[0,*])
   decrange = minmax(eph[1,*])

   ra = total(rarange)/2.0
   dec= total(decrange)/2.0

   dra = 2.0 * (rarange[1]-ra)/cos(dec) * !radeg * 3600.0 + 2.0*pad
   ddec= 2.0 * (decrange[1]-dec) * !radeg * 3600.0 + 2.0*pad

   rastr,ra,0,str1
   decstr,dec,0,str2
   print,'Extract stars centered at ',str1,' ',str2
   print,'   field width    ra=',dra/60.0,'  dec=',ddec/60.0,' arcmin', $
      format='(a,f5.1,a,f5.1,a)'

   if dra gt 7200.0 or ddec gt 7200.0 then begin
      print,'OBJSTARS:  Error!  The field extraction widths are too large.'
      print,'   dra = ',dra,'    ddec = ',ddec
      print,'           Operation aborted.'
      return
   endif

   refnet,ra,dec,dra,ddec,30.0,30.0,starfile

   readcol,starfile,hr,m1,s1,dgas,m2,s2,smag, $
      format='d,d,d,a,d,d,f',/silent

   print,strn(n_elements(smag)),' total stars found.'

   signas = strmid(dgas,0,1)
   dg = fix(strmid(dgas,1,2))
   hmstorad,hr,m1,s1,sra
   sign = replicate(1.0,n_elements(dg))
   z=where(signas eq '-',count)
   if count ne 0 then sign[z] = -1.0
   dmstorad,sign,abs(dg),m2,s2,sdec

   print,'Creating file ',outfile
   if filetype eq 'edb' then begin
      openw,lun,outfile,/get_lun
      for i=0L,n_elements(sra)-1 do begin
         rastr,sra[i],2,str1
         decstr,sdec[i],1,str2
         str = string(i,str1,str2,smag[i], $
                      format='("NV-",i10,",f|S,",a,",",a,",",f5.1,",2000.0")')
         str = strcompress(str,/remove_all)
         printf,lun,str
      endfor
   endif else if filetype eq 'app' then begin

   endif
   free_lun,lun

   file_delete,starfile,/quiet,/noexpand_path

end
