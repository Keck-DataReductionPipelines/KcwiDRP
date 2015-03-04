;+
; NAME:
;  des_sum
; PURPOSE:
;  Create a summary listing of KBOs in all directories of DES data
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  des_sum, rootid
; INPUTS:
;  rootid - input date for output file name, YYMMDD
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ASTPATH - path to astrometry data, '/net/frakir/raid/buie/astrometry' 
;  REDPATH - path to reduced data, default is '/net/frakir/raid/reduced'
;  RAWPATH - path to image data, default is a vector:
;             ['/net/frakir/data1/buie/rawfits',
;              '/gw/data7/buie/rawfits', '/gw/data3/buie/rawfits']
;  KBOPATH - location of bernstein files,
;             default is '/net/frakir/raid/buie/kbo/Bernstein'
;  OUTPATH - location for output file, current default is 
;             '/net/frakir/raid/reduced/Summary/Current/'
; OUTPUTS: 
;  outfile - output file for kbo list, default name is 'osYYMMDD.0.txt'
;            'os' stands for "object summary (file)" and '0' is version number
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;    The goal with this file is to have a list of all the kbos that belong to
;    DES as original discoveries. The orbital elements in this file are for the
;    day of file generation and can be updated for the given set of targets at
;    any time in the future using des_update. 
;    Outline of code:
;       (1) Get list of DES kbos (designated and undesignated): from desig.dat
;              and oblist.dat
;       (2) Get a list of the survey directories on frakir to be used later.
;       (3) Split up the object list into five options: MPC designation, MPC
;              name, preliminary designation, local code (MB) or temporary
;              code (looker)
;       (4) Generate a list of local codes and cross-references using newobj.dat
;       (5) Get looker ids by reading lpast.xrft files
;       (6) Read .info files to get magnitudes from discovery file 
;       (7) Read .ast files to get discovery date, time, RA and DEC
;       (8) Get current orbital element information using ephem
;       (9) Get survey field ID and filter information, truncate filter
;              information
;       (10) Calculate Hmag and Hmagerr for discovery filter
;              error based on orbit not photometry 
;       (11) Get quality of observation information -- needs to be added,
;              dummy variable for now
;       (12) Stuff information into output array
;       (13) Write output to file 'osYYMMDD.0.txt'
; MODIFICATION HISTORY:
; 01/01/04, Written by Susan Kern
; 01/04/06, DBT - Fixed bug in sort, Fixed bug in KBOName, cleand up code
; 02/06/10, SDK - took out original sort, revised and cleaned up code.
; 02/07/02, SDK - changed method of getting orbits to ephem from reading
;                    bernstein files.  
; 02/07/03, SDK - modified output statement, added error to log for missing 
;                    for filter information
; 02/07/09, SDK - fixed header problems, added back getinfo to get H,G.
; 02/08/03, SDK - fixed matching problem for linked objs, fixed search dir list
; 03/01/21, MWB - fixed loop counter on scanning Mosaic reduction directories
; 03/02/04, MWB - added a trap on reading lplast.xrft files to catch when those
;                   files don't exist or are empty.
; 04/01/19, SDK - modified magnitude input to come from .ast file rather than 
;                   .info file, values in all locations now for the most part  
; 04/02/18, SDK - added new data path, update KBOPATH, ASTPATH
;  2004/9/21, MWB, removed obsolete call to Findfile
;  2010/09/01, MWB, changed default rawpath.
;-
pro des_sum, rootid,ASTPATH=astpath, REDPATH=redpath, RAWPATH=rawpath, $
   KBOPATH=kbopath, OUTPATH=outpath, CACHE=cache

   ; check to see that the parameters are valid
   if badpar(astpath,[0,7],0,caller='des_sum:(ASTPATH) ', $
            default='/net/frakir/raid/buie/astrometry') then return

   if badpar(redpath,[0,7],0,caller='des_sum:(REDPATH) ', $
            default='/net/frakir/raid/reduced') then return

   if badpar(rawpath,[0,7],0,caller='des_sum:(RAWPATH) ', $
            default=['/net/amber/raid1/buie/rawfits/des', $
                     '/net/amber/raid1/buie/rawfits/des/followup']) then return

   if badpar(kbopath,[0,7],0,caller='des_sum:(KBOPATH) ', $
            default='/net/frakir/raid/buie/kbo') then return

   astpath=addslash(astpath)
   redpath=addslash(redpath)
   kbopath=addslash(kbopath)

   if badpar(outpath,[0,7],0,caller='des_sum:(OUTPATH) ', $
            default=redpath+addslash('Summary')+addslash('Current')) then return

   outpath=addslash(outpath)

   if badpar(cache,[0,1,2,3],0,caller='des_sum:(CACHE) ',default=0) then return

   ; Fire up ephemeris generator
   spawn,'geteph',unit=geteph_pipe

   logfile = 'des_sum'+rootid+'.log'

   logerror,logfile,'Generating master KBO list',caller='des_sum'

   ; read in current desig.dat file and oblist.dat files to get des object lists

   ; Read information from desig.dat for DES designated objects

   desigobj=''
   desigfile=kbopath+'desig.dat'
      readcol, desigfile, desigobj, format='(a)'
   
   ; Read in undesignated list
   undesigobj=''
   oblist=kbopath+'oblist.dat' 
   readcol, oblist, undesigobj, format='(a)'

   ; Combine lists
   desobjs=[strtrim(desigobj,2),strtrim(undesigobj,2)]
   nelements=n_elements(desobjs)

   if nelements eq 0 then begin 
      logerror,logfile,caller='des_sum','No entries for the KBO list'
      return
   endif
  
   ; get just the data directory from the entire path
   fdirtmp=file_search(redpath,COUNT=nfile)
   ndir=0 

   ; Just check directories that have six elements
   z=where(strlen(fdirtmp) eq 6 and $
           strmid(fdirtmp,0,1) ge '0' and strmid(fdirtmp,0,1) le '9', zcount)
   if zcount ne 0 then begin
      fdir=fdirtmp[z]
print, fdir
   endif
   nfile=zcount

   for i=0, nfile-1 do begin
 
      ; check to see if the data directory is a KBO Search directory 
      list=file_search(redpath+addslash(fdir[i])+'infox?.log', count=count) 

      if count eq 8 then begin
         ; check to see if there is a .info file in the submitted directory
         filepath=astpath+addslash('Submitted')+fdir[i]+'.info'
         if not exists(filepath) then begin
            logerror,logfile,caller='des_sum', $
               '*** error:'+filepath+' not found'
            ffdir=fdir[i]
         endif else begin
            if ndir eq 0 then begin
               ffdir=fdir[i] 
            endif else begin
               ffdir=[ffdir,fdir[i]]
            endelse
            ndir=ndir+1
         endelse   
      endif 

   endfor


   ; split up the object list into five options
   ; looker id (tmpcode), local id (lcode), and mpc designation (desig) 
   mpcdesig=strarr(nelements)
   mpcname=strarr(nelements)
   desig=strarr(nelements)
   lcode=strarr(nelements)
   tmpcode=strarr(nelements)
   firstfield=strarr(nelements)
   rmag=fltarr(nelements)
   rmagerr=fltarr(nelements)
   rate=fltarr(nelements)
   dir=strarr(nelements)
   dr=fltarr(nelements)
   dt=fltarr(nelements)
   elong=fltarr(nelements)
   hmag=fltarr(nelements)
  
   rawcode=strupcase(DESOBJS) 

   for i=0,nelements-1 do begin
      c0=strmid(rawcode[i],0,1)
      c1=strmid(rawcode[i],1,1)
      c2=strmid(rawcode[i],2,1)
      c3=strmid(rawcode[i],3,1)
      c4=strmid(rawcode[i],4,1)

      if c0 ge 'A' and c0 le 'Z' and $
         c1 ge 'A' and c1 le 'Z' then begin
         lcode[i]=rawcode[i]   
      endif else if strlen(rawcode[i]) ge 5 and $
         c0 ge '0' and c0 le '9' and $
         c1 ge '0' and c1 le '9' and $
         c2 ge '0' and c2 le '9' and $
         c3 ge '0' and c3 le '9' and $
         c4 ge 'A' and c4 le 'Z' then begin
         desig[i]=rawcode[i] 
      endif else begin
         mpcdesig[i]=rawcode[i]
      endelse
   endfor

   ; generate a list of local codes wih cross-reference information 
   logerror,logfile,caller='des_sum', $
      'Generating list of local codes'
   rdobjdes,'newobj.dat',ncodes,objcode,field,xref,xsec,COMMENTS=comments

   ; determine if any local codes have designation information available   
   for i=0,nelements-1 do begin
      if lcode[i] ne '' then begin
         z=where(lcode[i] eq objcode,count)
         if count eq 1 then begin
            desig[i]=xref[z[0]]
            firstfield[i]=field[z[0]]
         endif
      endif else begin
         z=where(desig[i] eq xref,count)
         if count eq 1 then begin
            lcode[i]=objcode[z[0]]
            firstfield[i]=field[z[0]]
         endif
         if count gt 1 then begin
            zz=where(xsec[z] eq 0, zcount)
            if zcount eq 1 then begin
               lcode[i]=objcode[z[zz]]
               firstfield[i]=field[z[zz]]
            endif
         endif 
     endelse
     if mpcdesig[i] ne '' then begin
         z=where(mpcdesig[i] eq xref, count)
        if count eq 1 then begin
           comment=strsplit(comments[z[0]], /extract)
           desig[i]=strtrim(comment[1] ,2)
           lcode[i]=objcode[z[0]]
           firstfield[i]=field[z[0]]
        endif
      endif 
   endfor

   ; get looker ids by reading lplast.xrft files
   alldir=strmid(firstfield,0,6)
   dirlist=alldir[uniq(alldir,sort(alldir))]
   ndir=n_elements(dirlist)

   ; Look for a cross-reference file.
   logerror,logfile,caller='des_sum', $
      'Reading lplast.xrft files'
   for i=0,ndir-1 do begin
      tmpdir=redpath+addslash(dirlist[i])
      rdlplast, lookerid, tmpid, ncount, /trim, path=tmpdir

      if ncount eq 0 then begin
         logerror,logfile,caller='des_sum', $
            'lplast.xrft file not found in '+tmpdir
      endif else begin
         z=where(dirlist[i] eq alldir,count)
         for j=0,count-1 do begin
            ;use lcode or desig to find tmpcode
            if tmpcode[z[j]] eq '' then begin
               zz=where(lcode[z[j]] eq tmpid or desig[z[j]] eq tmpid,count)
               if count eq 1 then begin
                  tmpcode[z[j]]=lookerid[zz[0]]
               endif
            endif else begin
            ;; Else look up lcode or desig form lookerid
               zz=where(tmpcode[z[j]] eq lookerid,count)
               if count ne 1 then begin
                  logerror,logfile,caller='des_sum', $
                     '*** lookerid '+tmpcode[z[j]]+' not found in file '+tmpdir
                  continue
               endif
               if tmpcode[z[j]] eq tmpid[zz[0]] then begin
                  logerror,logfile,caller='des_sum', $
                     '*** invalid lookerid '+tmpcode[z[j]]+' in file '+tmpdir
                  continue
               endif
               c0=strmid(tmpid[zz[0]],0,1)
               c1=strmid(tmpid[zz[0]],1,1)
               if c0 ge 'A' and c0 le 'Z' and $
                  c1 ge 'A' and c1 le 'Z' then begin
                  if strlen(tmpid[zz[0]]) ne 0 then lcode[z[j]]=tmpid[zz[0]]
                  ;; look up desig from new lcode
                  dtz=where(lcode[z[j]] eq objcode,count)
                  if count eq 1 then begin
                     if strlen(xref[dtz[0]]) ne 0 then $
                         desig[z[j]]=strmid(xref[dtz[0]],2)
                  endif
               endif else begin
                  if strlen(tmpid[zz[0]]) ne 0 then desig[z[j]]=tmpid[zz[0]]
               endelse
            endelse
         endfor
      endelse
   endfor 

   ;Read .info files and get rate/mag information out
   nelements=0
   logerror,logfile,caller='des_sum', $
      'Reading .info files'

   for i=1, ndir-1 do begin
      kbolist,dirlist[i],dirname0,rawcode0,firstnight0,rmag0,logfile=logfile, $
         rmagerr0,rate0,dir0,dr0,dt0,elong0,retcount,ASTPATH=astpath, $
         REDPATH=redpath

      if retcount gt 0 then begin
         if nelements eq 0 then begin
            tdirname=dirname0
            trawcode=rawcode0
            tfirstnight=firstnight0
            trmag=rmag0
            trmagerr=rmagerr0
            trate=rate0
            tdir=dir0
            tdr=dr0
            tdt=dt0
            telong=elong0
         endif else begin
            tdirname=[tdirname,dirname0]
            trawcode=[trawcode,rawcode0]
            tfirstnight=[tfirstnight,firstnight0]
            trmag=[trmag,rmag0]
            trmagerr=[trmagerr,rmagerr0]
            trate=[trate,rate0]
            tdir=[tdir,dir0]
            tdr=[tdr,dr0]
            tdt=[tdt,dt0]
            telong=[telong,elong0]
         endelse
         nelements=nelements+retcount
         logerror,logfile,caller='des_sum', $
            'Directory '+dirlist[i]+', new='+strn(retcount)+ $
            ', cumulative count '+strn(nelements)
      endif
   endfor


      for i=0,n_elements(trawcode)-1 do begin
      c0=strmid(trawcode[i],0,1)
      c1=strmid(trawcode[i],1,1)
      c2=strmid(trawcode[i],2,1)


      if c0 gt '0' and c0 le '9' and $
         c1 gt '0' and c1 le '9' and $
         c2 ge 'A' and c2 le 'Z' then begin
         trawcode[i]='19'+trawcode[i]   
      endif else if c0 eq '0' and $
         c1 ge '0' and c1 le '9' and $
         c2 ge 'A' and c2 le 'Z' then begin
         trawcode[i]='20'+trawcode[i]   
      endif
   endfor

   ; Compare limited object list to compiled list and get necessary info.
   for i=0, n_elements(rawcode)-1 do begin
      z=where(lcode[i] eq trawcode, tcount)
      
      if tcount gt 0 then begin
         zz=where(firstfield[i] eq tfirstnight[z],fcount)
         if fcount gt 0 and fcount lt 2 then begin
            rmag[i]=trmag[z[zz]]
            rmagerr[i]=trmagerr[z[zz]]
            rate[i]=trate[z[zz]]
            dir[i]=tdir[z[zz]]
            dr[i]=tdr[z[zz]]
            dt[i]=tdt[z[zz]]
            elong[i]=telong[z[zz]]
         endif 
      endif else begin
         z=where(desig[i] eq trawcode,rawcount)
         if rawcount gt 0 then begin
            zz=where(firstfield[i] eq tfirstnight[z],fcount)
            if fcount gt 0 and fcount lt 2 then begin
               rmag[i]=trmag[z[zz]]
               rmagerr[i]=trmagerr[z[zz]]
               rate[i]=trate[z[zz]]
               dir[i]=tdir[z[zz]]
               dr[i]=tdr[z[zz]]
               dt[i]=tdt[z[zz]]
               elong[i]=telong[z[zz]]  
            endif 
         endif else begin       
           z=where(tmpcode[i] eq trawcode, tmpcount)
            if tmpcount gt 0 then begin
               zz=where(firstfield[i] eq tfirstnight[z],fcount)
               if fcount gt 0 and fcount lt 2 then begin
                  rmag[i]=trmag[z[zz]]
                  rmagerr[i]=trmagerr[z[zz]]
                  rate[i]=trate[z[zz]]
                  dir[i]=tdir[z[zz]]
                  dr[i]=tdr[z[zz]]
                  dt[i]=tdt[z[zz]]
                  elong[i]=telong[z[zz]]  
               endif
            endif
        endelse
     endelse
     if rmag[i] eq 0 then rmag[i]=999999. 
     if rmagerr[i] eq 0 then rmag[i]=999999. 
     if rate[i] eq 0 then rmagerr[i]=999999.
     if dir[i] eq 0 then dir[i]=999999. 
     if dr[i] eq 0 then dr[i]=999999. 
     if dt[i] eq 0 then dt[i]=999999. 
     if elong[i] eq 0 then elong[i]=999999. 
   endfor


   ntmpcode=tmpcode
   nlcode=lcode
   ndesig=desig
   nfirstfield=firstfield
   nrawcode=rawcode
   nrmag=rmag
   nrmagerr=rmagerr
   nrate=rate
   ndir=dir
   ndr=dr
   ndt=dt
   nelong=elong
   nhmag=hmag

   nelements=n_elements(nhmag)

   ;read information from .ast file in reduced directory for each object
   ; to get discovery jdt and ra/dec
   logerror,logfile,caller='des_sum', $
      'Reading .ast files'
   nra = strarr(nelements) 
   ndec = strarr(nelements) 
   njd = strarr(nelements) 
   tmpjd = dblarr(nelements) 
   rmagnitude = fltarr(nelements) 
   rmagnitudeE = fltarr(nelements) 

   dirlist=strmid(nfirstfield,0,6)
   for i=0,nelements-1 do begin
      ; Fill in KBOname
      if strlen(ndesig[i]) ne 0 then begin
         kboname=ndesig[i]
      endif else if strlen(nlcode[i]) ne 0 then begin
         kboname=nlcode[i]
      endif else if strlen(ntmpcode[i]) ne 0 then begin
         kboname=ntmpcode[i]
      endif else begin
         kboname='*undefined*'
      endelse

      tmpdir=redpath+addslash(dirlist[i])
      if exists (tmpdir) then begin
         tmpfile=tmpdir+ntmpcode[i]+'.ast'

         if exists(tmpfile) then begin
 
            ; read in astrometry file
            rdrawast,tmpfile,fn,jd,ra,dec,mag,nobs
            rastr,ra,1,ra0
            decstr,dec,1,dec0
            jdstr,jd,1,jd0 
            tmpjd[i]=jd[0]
            njd[i]=jd0[0]
            nra[i]=ra0[0]
            ndec[i]=dec0[0]
            rmagnitude[i]=mag[0]
            rmagnitudeE[i]='0.0'
         endif else begin
            ; initialize fields to default values
            logerror,logfile,caller='des_sum', $
               '*** error astfile='+tmpfile+' not found,    KBOName='+kboname
            nra[i]='*'
            ndec[i]='*'
            njd[i]='* *'
            rmagnitude[i]='0.0'
            rmagnitudeE[i]='0.0'
            tmpjd[i]=systime(/julian)
         endelse
      endif else begin
         logerror,logfile,caller='des_sum', $
            '*** error astdir='+tmpdir+' not found,    KBOname='+kboname
         nra[i]='*'
         ndec[i]='*'
         njd[i]='* *'
         rmagnitude[i]='0.0'
         rmagnitudeE[i]='0.0'
         tmpjd[i]=systime(/julian)
      endelse
   endfor

   time=strarr(nelements)
   tmpdate=strarr(nelements)
   datetmpl='YYYY/MM/DD'
   tm = strarr(nelements)
   year = intarr(nelements)
   month = intarr(nelements)
   day = intarr(nelements)
   hour=fltarr(nelements)

   for i=0, nelements-1 do begin
         tmparr=strsplit(njd[i], /extract)
         tmpdate[i]=tmparr[0]
         time[i]=tmparr[1]

      if njd[i] ne '* *' then begin
         ; parse the date (lifted from jdparse,parsekey.pro)
 
         errflg = 0
         ;Determine the date order and delimiter locations in the date template.
         ;  Only the letters M, D, and Y are considered token characters. All
         ;  others are treated as delimiters.
         x = byte( datetmpl )
         w = where( (x ne 68) and (x ne 77) and (x ne 89), count )
         if count eq 2 then begin
            toktmpl = [ strmid( datetmpl, 0, 1 ), $
                        strmid( datetmpl, w[0]+1, 1 ), $
                        strmid( datetmpl, w[1]+1, 1 ) ]
            result=stregex(toktmpl,'[YMD]',/boolean)
         endif

        ;Parse the date, according to the template.  
        ; Make copy of string
        str = tmpdate[i]

        for j=0,2 do begin

            result=stregex(str,'[^0-9]')
            if result gt 0 then begin
               val = fix(strmid(str,0,result))
               str = strmid(str,result+1,999)
               case strmid( toktmpl[j], 0, 1 ) of
                  'D' : begin
                     if val ge 1 and val le 31 then begin
                        day[i] = val
                     endif else begin
                        day[i]=1
                        print,'JDPARSE: Day out of range! element ',strn(i), $
                              ' is ',strn(val)
                     endelse
                  end
                  'M' : begin
                     if val ge 1 and val le 12 then begin
                        month[i]=val
                     endif else begin
                        month[i]=1
                        print,'JDPARSE: Month out of range! element ',strn(i), $
                              ' is ',strn(val)
                     endelse
                  end
                  'Y' : begin
                     year[i]=val
                  end
                  else : begin
                     print,'JDPARSE: Impossible token! ', $
                           strmid( toktmpl[j], 0, 1 )
                  end
               endcase
            end

         endfor ; run over date fields

         tm[i] = str


         ; parse the time
         x = BYTE( time[i] )
         w = WHERE( x EQ 58, count )  ; 58 is a colon (:)

         IF count EQ 2 THEN BEGIN
            n  = STRLEN( time[i] )
            hh = FLOAT( STRMID( time[i], 0, w[0] ) )
            mm = FLOAT( STRMID( time[i], w[0]+1, w[1]-w[0]-1 ) )
            ss = FLOAT( STRMID( time[i], w[1]+1, n-w[1]-1 ) )
         ENDIF ELSE BEGIN
            hh = 0.0
            mm = 0.0
            ss = 0.0
         ENDELSE

        ;Convert to decimal hours.
        hour[i] = hh + ( mm / 60 ) + ( ss / 3600 )

      endif


   endfor

   ;Compute the Julian Date.
   Jdcnv, year, month, day, hour, jdvalue
;   print, jdvalue

       
   ; get orital elements for the objects from Bernstein files.
   logerror,logfile,caller='des_sum', $
      'Getting orbital elements for the objects'

   ; call information using geteph
   
   noobs=dblarr(nelements)
   arc=dblarr(nelements)
   anom=dblarr(nelements)
   anomerr=dblarr(nelements)
   peri=dblarr(nelements)
   perierr=dblarr(nelements)
   node=dblarr(nelements)
   nodeerr=dblarr(nelements)
   inc=dblarr(nelements)
   incerr=dblarr(nelements)
   e=dblarr(nelements)
   eerr=dblarr(nelements)
   a=dblarr(nelements)
   aerr=dblarr(nelements)
   timeperi=dblarr(nelements)
   timeperierr=dblarr(nelements)
   lastobs=dblarr(nelements)
   perihelion=dblarr(nelements)
   perihelionerr=dblarr(nelements)
   aphelion=dblarr(nelements)
   aphelionerr=dblarr(nelements)
   epoch=dblarr(nelements)
   x=dblarr(nelements)
   xerr=dblarr(nelements)
   y=dblarr(nelements)
   yerr=dblarr(nelements)
   zcoord=dblarr(nelements)
   zerr=dblarr(nelements)
   xdot=dblarr(nelements)
   xdoterr=dblarr(nelements)
   ydot=dblarr(nelements)
   ydoterr=dblarr(nelements)
   zdot=dblarr(nelements)
   zdoterr=dblarr(nelements)
   heliodist=dblarr(nelements)
   heliodisterr=dblarr(nelements)
   geodist=dblarr(nelements)
   geodisterr=dblarr(nelements)
   hvmag=dblarr(nelements)
   phang=fltarr(nelements)
   phangerr=fltarr(nelements)
   gval=fltarr(nelements)

   currentjd=systime(/julian)

   ; Start up Larry's info fetch program
   spawn,'getinfo',unit=pipe


   for i=0,nelements-1 do begin
      ; Fill in KBOname
      if strlen(mpcdesig[i]) ne 0 then begin
         kboname='A'+mpcdesig[i]
      endif else if strlen(ndesig[i]) ne 0 then begin
         kboname='A'+strmid(ndesig[i],2)
      endif else if strlen(nlcode[i]) ne 0 then begin
         kboname='A'+nlcode[i]
      endif else if strlen(ntmpcode[i]) ne 0 then begin
         kboname='A'+ntmpcode[i]
      endif else begin
         kboname=''
         logerror,logfile,caller='des_sum', $
            '***** KBONAME UNDEFINED *****'
      endelse


      ; If no name don't call
      if strlen(kboname) ne 0 then begin
         ephem,currentjd,500,11,kboname,ephems,cache=cache,pipe=geteph_pipe
         ephem,currentjd,500,12,kboname,ephemerrs,cache=cache,pipe=geteph_pipe
         
         if strmid(strtrim(string(jdvalue[i]),2),0,1) eq '1' then $
            jdvalue[i]=currentjd
         ephem,jdvalue[i],500,34,kboname,hephems,cache=cache,pipe=geteph_pipe
         ephem,jdvalue[i],500,34,'P3',geo0,cache=cache,pipe=geteph_pipe
         
         printf,pipe,kboname
         object=''
         readf,pipe,object,h,g0,format='(a32,1x,f6.2,1x,f5.2)'
         object=strtrim(object,2)
         object=strcompress(object)
         words=strsplit(object,/EXTRACT)
         object=words[0]
         if strmid(object,0,10) ne 'XXXXXXXXXX' then begin
            hvmag[i]=h
            gval[i]=g0  
         endif else begin
            hvmag[i]=999999.
            gval[i]=0.150  
         endelse 

         noobs[i]=ephems[16]
         arc[i]=ephems[15]
         anom[i]=ephems[0]
         anomerr[i]=ephemerrs[0]
         peri[i]=ephems[1]
         perierr[i]=ephemerrs[1]
         node[i]=ephems[2]
         nodeerr[i]=ephemerrs[2]
         inc[i]=ephems[3]
         incerr[i]=ephemerrs[3]
         e[i]=ephems[4]
         eerr[i]=ephemerrs[4]
         a[i]=ephems[5]
         aerr[i]=ephemerrs[5]
         lastobs[i]=ephems[14]
         perihelion[i]=ephems[6]
         perihelionerr[i]=ephemerrs[6]
         aphelion[i]=ephems[7]
         aphelionerr[i]=ephemerrs[7]
         epoch[i]=ephems[9]
         x[i]=hephems[0]
         xerr[i]=hephems[6]
         y[i]=hephems[1]
         yerr[i]=hephems[7]
         zcoord[i]=hephems[2]
         zerr[i]=hephems[8]
         xdot[i]=hephems[3]
         xdoterr[i]=hephems[9]
         ydot[i]=hephems[4]
         ydoterr[i]=hephems[10]
         zdot[i]=hephems[5]
         zdoterr[i]=hephems[11]
         heliodist[i]=sqrt(hephems[0]*hephems[0]+hephems[1]*hephems[1]+ $
                      hephems[2]*hephems[2])
         heliodisterr[i]=sqrt(hephems[9]*hephems[9]+hephems[10]*hephems[10]+ $
                         hephems[11]*hephems[11])
         geodist[i]=sqrt((hephems[0]-geo0[0])^2.0+(hephems[1]-geo0[1])^2.0+ $
                    (hephems[2]-geo0[2])^2.0)
         geodisterr[i]=sqrt((hephems[9]-geo0[9])^2.0+(hephems[10]-geo0[10])^2.0+$
                       (hephems[11]-geo0[11])^2.0)

         earthdist=sqrt(geo0[0]*geo0[0]+geo0[1]*geo0[1]+geo0[2]*geo0[2])
         earthdisterr=sqrt(geo0[9]*geo0[9]+geo0[10]*geo0[10]+geo0[11]*geo0[11])
         phang[i]=acos((cos(earthdist)-cos(heliodist[i])*cos(geodist[i]))/ $
                       (sin(heliodist[i])*sin(geodist[i])))
         
         sub=1-((cos(earthdist)-cos(heliodist[i])*cos(geodist[i]))/ $
               (sin(heliodist[i])*sin(geodist[i])))^2.0
         dda=-sin(earthdist)/(sin(heliodist[i])*sin(geodist[i]))
         ddb=cos(earthdist)*cos(heliodist[i])/((sin(heliodist[i])*sin(geodist[i]))^2.0)- $
             (-sin(heliodist[i])*cos(geodist[i])-cos(heliodist[i])*sin(geodist[i]))/ $
             ((sin(heliodist[i])*sin(geodist[i]))^2.0)
         ddc=cos(earthdist)*cos(geodist[i])/((sin(heliodist[i])*sin(geodist[i]))^2.0)- $
             (cos(heliodist[i])*(-sin(geodist[i]))-sin(heliodist[i])*cos(geodist[i]))/ $
             ((sin(heliodist[i])*sin(geodist[i]))^2.0)
         phangerr[i]=sqrt((((((-1)/sqrt(1-sub))*dda)^2.0)*earthdisterr^2.0)+$
                     (((((-1)/sqrt(1-sub))*ddb)^2.0)*heliodisterr[i]^2.0)+$
                     (((((-1)/sqrt(1-sub))*ddc)^2.0)*geodisterr[i]^2.0))

         ; calculate time of perihelion
         meanmotion=(180.0/!pi)*0.01720209895*a[i]^(-1.5)
         meanmotionerr=sqrt(((-1.5)*(180.0/!pi)*0.01720209895*a[i]^(-2.5))^2.0*aerr[i]^2.0)
         if anom[i] gt 180.0 then begin
            degtoperi=360.0-anom[i]
            degtoperierr=anomerr[i]
            timeperi[i]=(degtoperi/meanmotion)+jdvalue[i]
            timeperierr[i]=sqrt(((1/meanmotion)*degtoperierr)^2.0+ $
                           ((degtoperi/meanmotion^2.0)*meanmotionerr)^2.0)
         endif else begin
            degtoperi=anom[i]
            degtoperierr=anomerr[i]
            timeperi[i]=jdvalue[i]-(degtoperi/meanmotion)
            timeperierr[i]=sqrt(((1/meanmotion)*degtoperierr)^2.0+ $
                           ((degtoperi/meanmotion^2.0)*meanmotionerr)^2.0)
         endelse
      endif else begin
         noobs[i]=999999.
         arc[i]=999999.
         anom[i]=999999.
         anomerr[i]=999999.
         peri[i]=999999.
         perierr[i]=999999.
         node[i]=999999.
         nodeerr[i]=999999.
         inc[i]=999999.
         incerr[i]=999999.
         e[i]=999999.
         eerr[i]=999999.
         a[i]=999999.
         aerr[i]=999999.
         timeperi[i]=999999999.
         timeperierr[i]=999999.
         lastobs[i]=999999.
         perihelion[i]=999999.
         perihelionerr[i]=999999.
         aphelion[i]=999999.
         aphelionerr[i]=999999.
         epoch[i]=999999999.
         x[i]=999999.
         xerr[i]=999999.
         y[i]=999999.
         yerr[i]=999999.
         zcoord[i]=999999.
         zerr[i]=999999.
         xdot[i]=999999.
         xdoterr[i]=999999.
         ydot[i]=999999.
         ydoterr[i]=999999.
         zdot[i]=999999.
         zdoterr[i]=999999.
         geodist[i]=999999.
         geodisterr[i]=999999.
         heliodist[i]=999999.
         heliodisterr[i]=999999.
         phang[i]=0.0
         phangerr[i]=0.0
      endelse
   endfor

   logerror,logfile,caller='des_sum', $
      'Getting FieldID and Filter info'

   ; Get field id and filter infomation for each object
   alldir=strmid(nfirstfield,0,6)
   numdir=n_elements(alldir)
   allfiles=strmid(nfirstfield,0,10)
    
   
   ; Get the header decoding information
   loadkeys,redpath+'mosaic.key',hdrlist
   fieldid=strarr(nelements)
   filter=strarr(nelements)
   npath=n_elements(rawpath)

   for i=0,numdir-1 do begin
      for j=0,npath-1 do begin
      fn=addslash(rawpath[j])+addslash(alldir[i])+allfiles[i]
         if exists(fn+'.fits') then ft='.fits' else ft=''
         if exists(fn+ft) then begin
            hdr=headfits(fn+ft)
            parsekey,hdr,hdrlist,info 
            fieldid[i]=info.object
            filter[i]=info.filter
            break
         endif else begin
            fieldid[i]=['***']
            filter[i]=['NAN'] 
         endelse
      endfor
   endfor
   

   ; truncate filter information
   for i=0,n_elements(filter)-1 do begin
      if strmid(filter[i],0,2) eq 'VR' then filter[i]='VR'
      if strmid(filter[i],0,2) eq 'Be' then filter[i]='VR'
      if strmid(filter[i],0,1) eq 'I' then filter[i]='I'
      if filter[i] eq 'Sloan_r' then filter[i]='rp'
      if strmid(filter[i],0,2) eq 'Wh' then filter[i]='W'
      if strmid(filter[i],0,7) eq 'r_prime' then filter[i]='rp'
      if strmid(filter[i],0,3) eq '???' then filter[i]='*'
      if strmid(filter[i],0,3) eq 'NAN' then begin
         logerror,logfile,caller='des_sum', $
            '*** error:filter for '+nfirstfield[i]+' not found'
      endif
      if nfirstfield[i] eq '' then nfirstfield[i]='*'
   endfor


   logerror,logfile,caller='des_sum', $
      'Cleaning up data'

   ; Calculate Hmag and Hmag error for filter observed, using bernstein information

   hmagerr=fltarr(nelements)

   for i=0,nelements-1 do begin
      newh0=0.0
      hmagerr0=0.0
      G=gval[i]
      if strtrim(string(phang[i]),2) ne 'NaN' then begin
            disphase,nrmag[i],heliodist[i],geodist[i],phang[i],G,newh0
            
            ; calculate errors
            phi1=exp(-3.33*(tan(0.5*phang[i]))^0.63)
            phi1err=sqrt(((exp(-3.33*(tan(0.5*phang[i]))^0.63))*$
            (-3.33*(0.63/2)*(1/cos(0.5*phang[i]))*tan(0.5*phang[i])^(-0.37))*phangerr[i])^2)

            phi2=exp(-1.87*(tan(0.5*phang[i]))^1.22)
            phi2err=sqrt(((exp(-1.87*(tan(0.5*phang[i]))^1.22))*$
            (-1.87*(1.22/2)*(1/cos(0.5*phang[i]))*tan(0.5*phang[i])^(0.22))*phangerr[i])^2)

            magerr=sqrt((-5*(1/heliodist[i])*heliodisterr[i])^2+ $
                   (-5*(1/geodist[i])*geodisterr[i])^2)


            hmagerr0=sqrt((magerr^2)+((2.5*(1-G)/((1-G)*phi1+phi2*G))*phi1err)^2+$
             ((2.5*G/((1-G)*phi1+G*phi2))*phi2err)^2)
 
      endif
      
      if newh0 eq 0.0 then begin
         hmag[i]=999999.
      endif else begin
         hmag[i] =newh0
      endelse
      
      if hmagerr0 eq 0.0 or hmagerr0 eq 'NaN' then begin
         hmagerr[i]=999999.
      endif else begin
         hmagerr[i]=hmagerr0
      endelse

   endfor


   nelements=n_elements(ndesig)
   
   ; Get quality information, image and orbit
   iq=fltarr(nelements)

   ; Put asterisk where there is object name for desig, lcode or tmpcode
   for i=0,nelements-1 do begin
      if strlen(mpcname[i]) eq 0 then mpcname[i]='*'
      if strlen(mpcdesig[i]) eq 0 then mpcdesig[i]='*'
      if strlen(ndesig[i]) eq 0 then ndesig[i]='*'
      if strlen(nlcode[i]) eq 0 then nlcode[i]='*'
      if strlen(ntmpcode[i]) eq 0 then ntmpcode[i]='*'     
   endfor     


   ; print a file with all the output, iterate version number
   ; put all output into one array
   header = [ $
                 ['Number',     '*'], $
                 ['Name',       '*'], $
                 ['ProvID',     '*'], $
                 ['LocalID',    '*'], $
                 ['LookerID',   '*'], $
                 ['QC',         '*'], $
                 ['MPEC',       '*'], $
                 ['DES',        '*'], $
                 ['Field',      '*'], $
                 ['FrameID',    '*'], $
                 ['Date',       'yyyy/mm/dd'], $
                 ['Time',       'hh:mm:ss.s'], $
                 ['RA_(J2000)', 'hh:mm:ss.s'], $
                 ['Dec_(J2000)','dd:mm:ss'], $
                 ['Fil',        '*'], $
                 ['FMag',       '*'], $
                 ['FMErr',    '*'], $
                 ['Fhmag',      '*'], $
                 ['FnmErr',   '*'], $
                 ['HMag',       '*'], $
                 ['HMErr',    '*'], $
                 ['Type',       '*'], $
                 ['nObs',       '*'], $
                 ['orbDateL',   'JD'], $
                 ['Arc',        'd'], $
                 ['Epoch',      'JD'], $
                 ['semi',       'AU'], $
                 ['semiErr',    'AU'], $
                 ['ecc',        '*'], $
                 ['eccErr',     '*'], $
                 ['inc',        'deg'], $
                 ['incErr',     'deg'], $
                 ['Node',       'deg'], $
                 ['NodeErr',    'deg'], $
                 ['arg',        'deg'], $
                 ['argErr',     'deg'], $
                 ['Anom',       'deg'], $
                 ['AnomErr',    'deg'], $
                 ['Peri_Time',  'JD'], $
                 ['ptErr',      'd'], $
                 ['q',          'AU'], $
                 ['qErr',       'AU'], $
                 ['Q',          'AU'], $
                 ['QErr',       'AU'], $
                 ['Eclip_X',    'AU'], $
                 ['exErr',      'AU'], $
                 ['Eclip_Y',    'AU'], $
                 ['eyErr',      'AU'], $
                 [' Eclip_Z',   'AU'], $
                 ['ezErr',      'AU'], $
                 ['X_dot',      'AU_d^-1'], $
                 ['xdErr',      'AU_d^-1'], $
                 ['Y_dot',      'AU_d^-1'], $
                 ['ydErr',      'AU_d^-1'], $
                 ['Z_dot',      'AU_d^-1'], $
                 ['zdErr',      'AU_d^-1'], $
                 ['GeoDist',    'AU'], $
                 ['gdistErr',   'AU'], $
                 ['HelioDist',  'AU'], $
                 ['hdistErr',   'AU'], $
                 ['Notes',      '*'] $
            ]


   headers = header[0,*]
   units   = header[1,*]

   ;Stuff information into outputinfo array
   outputinfo=strarr(61,nelements+2) 
   len=[9,11,10,9,10,3,9,4,10,13,11,11,12,12,4,5,6,6,7,6,6,5,5,10,8,10,9,8,7,7,7,7, $  
         8,8,8,8,8,8,10,9,8,7,8,7,8,6,8,6,9,6,8,8,8,8,8,8,8,9,10,9,6]  


   for i=0, n_elements(headers)-1 do begin 
      outputinfo[i,0]=strn(headers[i], padtype=1, format='(A)', length=len[i]) 
      outputinfo[i,1]=strn(units[i], padtype=1, format='(A)', length=len[i]) 
   endfor

   for i=2, nelements+1 do begin
      outputinfo[0,i]=strn(mpcdesig[i-2], padtype=0, format='(A8)', length=9)
      outputinfo[1,i]=strn(mpcname[i-2], padtype=0, format='(A10)', length=11)
      outputinfo[2,i]=strn(ndesig[i-2], padtype=0, format='(A9)', length=10)
      outputinfo[3,i]=strn(nlcode[i-2], padtype=0, format='(A8)', length=9)
      outputinfo[4,i]=strn(ntmpcode[i-2], padtype=0, format='(A9)', length=10)
      outputinfo[5,i]=strn('*', padtype=0, format='(A2)', length=3)
      outputinfo[6,i]=strn('*', padtype=0, format='(A8)', length=9)
      outputinfo[7,i]=strn('Y', padtype=0, format='(A2)', length=4)
      outputinfo[8,i]=strn(strupcase(fieldid[i-2]), padtype=0, format='(A9)', length=10)
      outputinfo[9,i]=strn(nfirstfield[i-2], padtype=0, format='(A12)', length=13)
      outputinfo[10,i]=strn(tmpdate[i-2], padtype=0, format='(A10)', length=11)
      outputinfo[11,i]=strn(time[i-2], padtype=0, format='(A10)', length=11)
      outputinfo[12,i]=strn(nra[i-2], padtype=0, format='(A11)', length=12)
      outputinfo[13,i]=strn(ndec[i-2], padtype=0, format='(A9)', length=12)
      outputinfo[14,i]=strn(filter[i-2], padtype=0, format='(A3)', length=4)
      outputinfo[15,i]=strn(rmagnitude[i-2], padtype=1, format='(f4.1)', length=5)
      outputinfo[16,i]=strn(rmagnitudeE[i-2], padtype=1, format='(f3.1)', length=6)
      outputinfo[17,i]=strn(hmag[i-2], padtype=1, format='(f5.2)', length=6)
      outputinfo[18,i]=strn(hmagerr[i-2], padtype=1, format='(f4.2)', length=7)
      outputinfo[19,i]=strn(hvmag[i-2], padtype=1, format='(f5.2)', length=6)
      outputinfo[20,i]=strn('*', padtype=1, format='(A3)', length=6)
      outputinfo[21,i]=strn('*', padtype=1, format='(A4)', length=5)
      outputinfo[22,i]=strn(noobs[i-2], padtype=1, format='(i4)', length=5)
      outputinfo[23,i]=strn(lastobs[i-2], padtype=1, format='(f9.1)', length=10)
      outputinfo[24,i]=strn(arc[i-2], padtype=1, format='(f7.2)', length=8)
      outputinfo[25,i]=strn(epoch[i-2], padtype=1, format='(f9.1)', length=10)
      outputinfo[26,i]=strn(a[i-2], padtype=1, format='(f8.3)', length=9)
      outputinfo[27,i]=strn(aerr[i-2], padtype=1, format='(f6.3)', length=8)
      outputinfo[28,i]=strn(e[i-2], padtype=1, format='(f6.4)', length=7)
      outputinfo[29,i]=strn(eerr[i-2], padtype=1, format='(f6.4)', length=7)
      outputinfo[30,i]=strn(inc[i-2], padtype=1, format='(f6.3)', length=7)
      outputinfo[31,i]=strn(incerr[i-2], padtype=1, format='(f6.2)', length=7)
      outputinfo[32,i]=strn(node[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[33,i]=strn(nodeerr[i-2], padtype=1, format='(f6.2)', length=8)
      outputinfo[34,i]=strn(peri[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[35,i]=strn(perierr[i-2], padtype=1, format='(f6.2)', length=8)
      outputinfo[36,i]=strn(anom[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[37,i]=strn(anomerr[i-2], padtype=1, format='(f6.2)', length=8)
      outputinfo[38,i]=strn(timeperi[i-2], padtype=1, format='(f9.1)', length=10)
      outputinfo[39,i]=strn(timeperierr[i-2], padtype=1, format='(f8.1)', length=9)
      outputinfo[40,i]=strn(perihelion[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[41,i]=strn(perihelionerr[i-2], padtype=1, format='(f6.3)', length=7)
      outputinfo[42,i]=strn(aphelion[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[43,i]=strn(aphelionerr[i-2], padtype=1, format='(f6.3)', length=7)
      outputinfo[44,i]=strn(x[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[45,i]=strn(xerr[i-2], padtype=1, format='(f5.3)', length=6)
      outputinfo[46,i]=strn(y[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[47,i]=strn(yerr[i-2], padtype=1, format='(f5.3)', length=6)
      outputinfo[48,i]=strn(zcoord[i-2], padtype=1, format='(f8.3)', length=9)
      outputinfo[49,i]=strn(zerr[i-2], padtype=1, format='(f5.3)', length=6)
      outputinfo[50,i]=strn(xdot[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[51,i]=strn(xdoterr[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[52,i]=strn(ydot[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[53,i]=strn(ydoterr[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[54,i]=strn(zdot[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[55,i]=strn(zdoterr[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[56,i]=strn(geodist[i-2], padtype=1, format='(f7.3)', length=8)
      outputinfo[57,i]=strn(geodisterr[i-2], padtype=1, format='(f6.3)', length=9)
      outputinfo[58,i]=strn(heliodist[i-2], padtype=1, format='(f9.3)', length=10)
      outputinfo[59,i]=strn(heliodisterr[i-2], padtype=1, format='(f6.3)', length=9)
      outputinfo[60,i]=strn('*', padtype=1, format='(A5)', length=6)
   endfor

   outfile = outpath+'os'+strtrim(string(rootid),2)+'.0.txt'

   logerror,logfile,caller='des_sum', $
      'Writing output file '+outfile

   openw, lunout, outfile, /get_lun

    for i=0, nelements+1 do begin
      printf, lunout, outputinfo[0:*,i], format='(64(A))'
    endfor

   ; Close the file
   free_lun, lunout    

   free_lun,geteph_pipe
   free_lun,pipe

   logerror,logfile,caller='des_sum','All done'

end
