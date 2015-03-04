;+
; NAME:
;  xdspec
; PURPOSE:
;  End-to-end reduction tool for OSIRIS XD spectral data.
; DESCRIPTION:
;
;  This program handles all steps of processing raw OSIRIS XD spectral data
;    to create final averaged output spectra.  This will work quite well for
;    data on single point sources.  If the objects are extended or if there
;    are more than one object, you need to do the reductions by hand.
;
; CATEGORY:
;  Spectroscopy
;
; CALLING SEQUENCE:
;  xdspec
;
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  Unless otherwise indicated, these options are all flags.
;
;  ABSPOSCAL  - Run through the absolute position calibration step.
;
;  ADDPATH    - String of directory name to prepend to the DIRLIST for a
;                  location to look for the raw data.  The same restriction
;                  applies to this as to DIRLIST.
;
;  ALLCAL     - Run through all the calibration steps.
;
;  ALLSPEC    - (Re)compute all spectral extractions.
;
;  AVGSPEC    - Force (re)computation of final average spectra.
;
;  DIRLIST    - String array list of directories for where to search for
;                  raw data.  Do NOT include the final directory that is
;                  the same as the current directory and the root of the
;                  raw data files.
;
;  FINAL      - Enable final spectral extraction, this step combines any
;                  bad pixel masks and creates final mask (if not already
;                  present) and then extracts all spectra.
;
;  FLUSH_INFO - Flush all remembered information.
;
;  GAUSSCOR- Correction factor from the directly computed image FWHM (per order)
;               See OPTSPEC for a more detailed explanation.
;
;  GROUPCHECK - Flag, generate a summary plot of all spectra against their
;                  group leader.
;
;  MASKTHRESH - When using a final mask image, you can adjust the mask
;                  threshold with this keyword.  The mask image records the
;                  fraction of the time that a given pixel is recorded as
;                  being "bad".  This fraction varies between 0.0 and 1.0.
;                  The default is to mark pixels bad that are seen to be bad
;                  more than 4% of the time.  In other words, MASKTHRESH=0.04
;                  is the default.
;
;  NOSPEC     - Inhibit any spectral extraction processing.
;
;  PLOTRANGE  - Two element vector, [start,stop], frame numbers to generate
;                  postscript plots for.
;
;  PLOTS      - How many and how much to plot.  (only for optspec plots)
;                  0 - All plots generated.
;                  1 - No plots at all.
;                  2 - Show strip image, average profile, order profiles (default)
;
;  QUEUE      - (String) Print queue to send postscript plots to.  If this
;                  is not provided, then the idl.ps file is not spooled to
;                  a printer and is left behind when xdspec finishes.  If you
;                  give a queue name, the idl.ps file is deleted after the
;                  file is spooled for printing.
;
;  RELPOSCAL  - Run through the relative position calibration step.
;
;  SLOPECAL   - Run through centerline slope calibration step.
;
;  WAVECAL    - Run the wavelength calibration steps on each order.
;
; OUTPUTS:
;
;  All output is either to the various plot screens or to files.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;  COM_XDSPEC - Used to store information from one invocation of xdspec to
;                 the next.
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
;  If you use an 's' flag to eliminate an image and its spectrum from the
;    output products, you will not necessarily get this image as a mate in
;    cases where you might like it.  You will almost always need to tweak
;    the mate in the .tbl file by hand in these cases.
;
; PROCEDURE:
;
;  This program assumes (and requires) that the current directory is NOT where
;    the raw frames are to be found.  The raw frames are in some other location
;    and have no other files with them.  This allows you to have the raw data
;    in a location where you do not have write permission (ie., CDROM).  All
;    of the resulting files from running xdspec are written to the current
;    directory and directories beneath there.
;
;  Also, for simplicity of bookkeeping, I have assumed that the root of the
;    raw data file names is the same as the directory where you are running
;    xdspec and where the data live.  For example:
;       Data files of the form:   980115.001
;       Current directory:     /usr/results/980115/
;       Data directory:        /usr/data/980115/
;    In this case '/usr/data' should be in the DIRLIST or added with ADDPATH
;    (if it isn't already present).
;
;  If there are no flat field calibration frames on this night, you must use
;    calibration specifications and flat fields from another night in that
;    run.  In this case, as you start XDSPEC you get a flat field selection
;    popup window.  Use this tool to browse through other nights until you
;    find an appropriate calibration file.  In selecting the flat you are
;    actually selecting that flat and its associated files.cal specification
;    file.  In this case you probably shouldn't fiddle with _anything_ in
;    the files.cal and if the original ever changes, you should probably start
;    all over from scratch on any other nights that use the same stuff.
;    You _can_ re-run the wavelength calibration steps in this case but it's
;    probably not a good idea.  Once you select the flat field, processing
;    continues and there's very little that needs to be done on the calibration
;    steps (actually nothing at all).
;
;  During the wavelength calibration steps, here are the mouse operation
;   options:          (1st:sky spectrum)                 (2nd:synthetic)
;          left      measure nearest line           grab nearest strong line
;          middle    delete nearest point                   nothing
;          right           done                         abort this line
;
;  Here's the typical "flow" of running this program:
;   xdspec        <-- Initial call, sets up calibration information, and
;                        extracts initial spectra.  Run this as many times
;                        as needed to get a complete set of initial spectra.
;
;   xdspec,plotrange=[1,NNN],queue='printer name'
;                     this will generate the summary plots for all the
;                     spectra.  Do only when all are done.  You then review
;                     the plots with an eye toward spectra that should be
;                     removed from the averages.  You mark one to be skipped
;                     by replacing the second column flag character
;                     by an 'x' (to exclude).  After all have
;                     been marked, proceed to the next step ...
;
;   xdspec,/final  <-- Final processing, combines bad pixel maps, reextracts
;                      the spectra and generates final averages.  You are done
;                      with this program at this point.
;
;   Note, you can ignore any spectrum in the tbl file by marking it with an
;     'x' in the second column in the table.  It will do no good to delete the
;     line from the table as the line will come back.  'x' marks it to be
;     ignored.  If any extraction/reduction products exist for such a spectrum
;     they will be deleted.
;
; MODIFICATION HISTORY:
;  97/12/11, Written by Marc W. Buie, Lowell Observatory
;  98/03/09, MWB, cleaned up logic needed when data are added to raw directory.
;  98/04/01, MWB, added some generalization items, and ADDPATH, DIRLIST
;  98/06, MWB, numerous changes to incorporate a numerical image profile for
;            the optimal extraction on the FINAL pass.  Also added support
;            for those cases where more than one data directory can be found.
;  98/06/30, MWB, added user selection if data could be found in more than one
;            directory.  Also, some logic was cleaned up that dealt with
;            spectra marked to be ignored.
;  98/08/25, MWB, changed the default plotting option, PLOTS=2 so that only
;            three windows are automatically created and used.
;  98/09/17, MWB, fixed a minor bookkeeping problem with 's' flagged frames.
;            The output files from these frames were still hanging around and
;            this has been changed so the output products are deleted.
;  99/11/24, MWB, changed lclxtrem call to accomodate new version.
;  2000/11/08, MWB, removed use of obsolete code constructs
;  2002/09/03, MWB, changed Str_sep call to strsplit
;  2003/10/01, MWB, converted to IDL file_mkdir calls
;
;-
pro xdspec,FLUSH_INFO=flush_info,WAVECAL=wavecal, $
           ALLCAL=allcal,SLOPECAL=slopecal,RELPOSCAL=relposcal, $
           ABSPOSCAL=absposcal,NOSPEC=nospec,ALLSPEC=allspec,FINAL=final, $
           AVGSPEC=avgspec,PLOTRANGE=plotrange,QUEUE=queue,DIRLIST=in_dirlist, $
           ADDPATH=addpath,GROUPCHECK=groupcheck,GAUSSCOR=gausscor,PLOTS=plots, $
           MASKTHRESH=maskthresh

   common com_xdspec, info, calib, tbl

   if badpar(flush_info,[0,1,2,3],0,caller='XDSPEC: (FLUSH_INFO) ', $
                                                          default=0) then return
   if badpar(wavecal,   [0,1,2,3],0,caller='XDSPEC: (WAVECAL) ', $
                                                          default=0) then return
   if badpar(allcal,    [0,1,2,3],0,caller='XDSPEC: (ALLCAL) ', $
                                                          default=0) then return
   if badpar(slopecal,  [0,1,2,3],0,caller='XDSPEC: (SLOPECAL) ', $
                                                          default=0) then return
   if badpar(relposcal, [0,1,2,3],0,caller='XDSPEC: (RELPOSCAL) ', $
                                                          default=0) then return
   if badpar(absposcal, [0,1,2,3],0,caller='XDSPEC: (ABSPOSCAL) ', $
                                                          default=0) then return
   if badpar(nospec,    [0,1,2,3],0,caller='XDSPEC: (NOSPEC) ', $
                                                          default=0) then return
   if badpar(allspec,   [0,1,2,3],0,caller='XDSPEC: (ALLSPEC) ', $
                                                          default=0) then return
   if badpar(final,     [0,1,2,3],0,caller='XDSPEC: (FINAL) ', $
                                                          default=0) then return
   if badpar(avgspec,   [0,1,2,3],0,caller='XDSPEC: (AVGSPEC) ', $
                                                          default=0) then return
   if badpar(plotrange, [0,2,3],  1,caller='XDSPEC: (PLOTSPEC) ', $
                                                    default=[-1,-1]) then return
   if badpar(queue,     [0,7],    0,caller='XDSPEC: (QUEUE) ', $
                                                    default='[none]') then return
   if badpar(in_dirlist,   [0,7],[0,1],caller='XDSPEC: (DIRLIST) ', $
                                                    default='[none]') then return
   if badpar(addpath,   [0,7],    0,caller='XDSPEC: (ADDPATH) ', $
                                                    default='[none]') then return
   if badpar(groupcheck,[0,2,3],  1,caller='XDSPEC: (GROUPCHECK) ', $
                                                    default=[-1,-1]) then return
   if badpar(plots,     [0,1,2,3],0,caller='XDSPEC: (PLOTS) ', $
                                                          default=2) then return
   if badpar(maskthresh,[0,4,5],  0,caller='XDSPEC: (MASKTHRESH) ', $
                                                          default=0.04) then return

   blanks='                '
   setusym,1  ; force user symbol to filled circle.
   lscmd = '"ls" '
   rmcmd = '"rm" '
   loadct,5,/silent

; If common is empty, fill in the structure with empty stuff.
   if flush_info then info=0
   sz_info=size(info)
   if sz_info[n_elements(sz_info)-2] ne 8 then begin
      info = { $
         cwd:        '', $
         ddir:       '', $
         root:       '', $
         dirroot:    '', $
         bsrefn:     -1, $
         flaton:     -1, $
         flatoff:    -1, $
         caldate:    '', $
         tbldate:    ''  $
         }
   endif

; Is the directory the same as the last time?  If so, reset some of the info.
   cd,'.',current=cwd
   if cwd ne info.cwd then begin
      info.caldate=''
      info.tbldate=''
      info.ddir = ''
      info.bsrefn = -1
      info.flaton = -1
      info.flatoff= -1

      ; root, derived from current directory.
      pos = rstrpos(cwd,'/')
      info.root = strmid(cwd,pos+1,99)

      info.cwd = cwd
   endif

   ; Locate the raw data.
   if in_dirlist[0] eq '[none]' then begin
      dirlist=['/gryll/data2/buie/rawfits/', $
               '/gryll/data3/buie/rawfits/', $
               '/gryll/data4/buie/rawfits/', $
               '/gryll/data5/buie/rawfits/', $
               '/gryll/data6/buie/rawfits/', $
               '/cdrom/cdrom0/', $
               '/usr1/data1/', $
               '/usr1/data2/', $
               '/usr1/data3/', $
               '/usr1/data4/']
   endif else begin
      dirlist=[in_dirlist]
   endelse

   ; Pre-pend a special location
   if addpath ne '[none]' then begin
      dirlist= [addslash(addpath),dirlist]
   endif

   ; Find where the data frames are located.  First, look for dirlist/root,
   ;   If nothing found look for just dirlist.
   dpath='not found'
   for i=0,n_elements(dirlist)-1 do begin
      if exists(dirlist[i]+info.root) then dpath=[dpath,dirlist[i]+info.root+'/']
   endfor
;   if n_elements(dpath) eq 1 then begin
      for i=0,n_elements(dirlist)-1 do begin
         if exists(dirlist[i]+info.root+'.001') then $
            dpath=[dpath,dirlist[i]]
      endfor
;   endif
   if n_elements(dpath) eq 1 then begin
      print,'Raw data for ',info.root,' could not be found. Aborting.'
      return
   endif else if n_elements(dpath) eq 2 then begin
      info.ddir = dpath[1]
   endif else begin
      dpath=dpath[1:n_elements(dpath)-1]
      print,'There appear to be multiple data directories for this date.'
      dpath=picker(dpath)
      info.ddir = dpath[0]
   endelse

   rootloc = strpos(info.ddir,info.root)
   info.dirroot=info.ddir
   if rootloc ne -1 then $
      info.ddir = strmid(info.ddir,0,rootloc)

   ; Take a census of the raw data
   cd,info.dirroot,current=currentdir
   spawn,lscmd+info.root+'.???',rawlist
   cd,currentdir
   if rawlist[0] eq '' then begin
      print,'Raw data could not be found. Aborting.'
      return
   endif

   ; Look for the information table for this dataset.  If not found, build it.
   tablname=info.root+'.tbl'
   if not exists(tablname) then begin
      print,'   ---> generating information table file.'
      tcal={height:2,nor:1,por:1,slope:0.,x1:0,x2:255,y0:128, $
            cof:[1,0.001],npts:256,o:[0,255],w:findgen(256), $
            flatname:'[none]',flat:1}
      openw,lun,tablname,/get_lun
      for i=0,n_elements(rawlist)-1 do begin
         getstrip,tcal,info.dirroot+'/'+info.root, $
            fix(strmid(rawlist[i],strlen(info.root)+1,3)),strip,hdr
;         hdr=headfits(info.dirroot+rawlist[i])
         objname=nobname(strtrim(strcompress(sxpar(hdr,"OBJECT")),2))
         objname=repstr(objname,'/','_')
         repeat begin
            if objname eq '' then begin
               ans=''
               read,prompt='File '+rawlist[i]+' has no object name.  ' + $
                           'Enter name> ',ans
               objname=nobname(strtrim(strcompress(ans),2))
            endif
         endrep until objname ne ''
         exptime=sxpar(hdr,"EXPTIME")
         avego  =sxpar(hdr,"AVEGO")
         dateobs=strtrim(sxpar(hdr,"DATE-OBS"),2)
         ut     =strtrim(sxpar(hdr,"UT"),2)
         airmass=sxpar(hdr,"AIRMASS")
         imtype =strmid(sxpar(hdr,"IMAGETYP"),0,1)
         reads,dateobs,day,mon,yr,format='(i2,1x,i2,1x,i4)'
         reads,ut,hr,mn,sc,format='(i2,1x,i2,1x,i4)'
         hr = float(hr)+float(mn)/60.0+float(sc)/3600.0
         jdcnv,yr,mon,day,hr,jd
         jd = jd + (avego*exptime/2.0)/3600.0d0/24.0d0
         printf,lun,imtype,rawlist[i],objname+blanks,jd,airmass,exptime,avego,-1,-1, $
            format='(a1," - ",a,1x,a16,1x,f13.5,1x,f5.3,1x,f8.3,1x,i3,2(1x,i3))'
      endfor
      free_lun,lun
   endif

   ; Check to see if table is newer than what is in memory, if it is, reload.
   spawn,lscmd+'-l '+tablname,result
   if result[0] ne info.tbldate then begin
      rdtbl,tablname,tbl
      info.tbldate = result[0]
   endif

   ; Check all the spectra flagged as bad.  Make sure the mate and relsig are
   ;   blanked out.  Also make sure no products exist for anything marked 'x'.
   z=where(tbl.imflag eq 'x',countz)
   if countz ne 0 then begin
      for i=0,countz-1 do begin
         if tbl.mate[z[i]] ne -1 then begin
            tbl.mate[z[i]] = -1
            tbl.relsig[z[i]] = -1.0
            tbl.dirty=1
         endif
         outmask='mask.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outbadp='Spec/'+info.root+'b.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outspec='Spec/'+info.root+'s.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outprof='Spec/'+info.root+'p.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outscor='Spec/'+info.root+'c.'+string(tbl.imnum[z[i]],format='(i3.3)')
         cmd=rmcmd
         if exists(outmask) then cmd=cmd+' '+outmask
         if exists(outbadp) then cmd=cmd+' '+outbadp
         if exists(outspec) then cmd=cmd+' '+outspec
         if exists(outprof) then cmd=cmd+' '+outprof
         if exists(outscor) then cmd=cmd+' '+outscor
         if cmd ne rmcmd then begin
            print,'x-spec cleanup: ',cmd
            spawn,cmd
         endif
      endfor
   endif

   ; Extra check, don't keep any files for stuff marked 's'.
   z=where(tbl.imflag eq 's',countz)
   if countz ne 0 then begin
      for i=0,countz-1 do begin
         outmask='mask.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outbadp='Spec/'+info.root+'b.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outspec='Spec/'+info.root+'s.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outprof='Spec/'+info.root+'p.'+string(tbl.imnum[z[i]],format='(i3.3)')
         outscor='Spec/'+info.root+'c.'+string(tbl.imnum[z[i]],format='(i3.3)')
         cmd=rmcmd
         if exists(outmask) then cmd=cmd+' '+outmask
         if exists(outbadp) then cmd=cmd+' '+outbadp
         if exists(outspec) then cmd=cmd+' '+outspec
         if exists(outprof) then cmd=cmd+' '+outprof
         if exists(outscor) then cmd=cmd+' '+outscor
         if cmd ne rmcmd then begin
            print,'s-spec cleanup: ',cmd
            spawn,cmd
         endif
      endfor
   endif

   ; Force all frames flagged with 't' to '-'
   z=where(tbl.imflag eq 't',countz)
   if countz ne 0 then tbl.imflag[z]='-'

   ; Scan the list of files and make sure there is information for all the
   ;   frames, if any are missing, collect the information and add to the
   ;   information structure.
   for i=0,n_elements(rawlist)-1 do begin
      z=where(rawlist[i] eq tbl.fn)
      if z[0] eq -1 then begin
         print,'   ---> adding ',rawlist[i],' to information table.'
         tcal={height:2,nor:1,por:1,slope:0.,x1:0,x2:255,y0:128, $
               cof:[1,0.001],npts:256,o:[0,255],w:findgen(256), $
               flatname:'[none]',flat:1}
         getstrip,tcal,info.dirroot+'/'+info.root, $
            fix(strmid(rawlist[i],strlen(info.root)+1,3)),strip,hdr
;         hdr=headfits(info.dirroot+rawlist[i])
         imnum=intarr(n_elements(rawlist))
         parts = strsplit(rawlist[i],'.',/extract)
         imnum = fix(parts[1])
         objname=nobname(strtrim(strcompress(sxpar(hdr,"OBJECT")),2))
         exptime=sxpar(hdr,"EXPTIME")
         avego  =sxpar(hdr,"AVEGO")
         dateobs=strtrim(sxpar(hdr,"DATE-OBS"),2)
         ut     =strtrim(sxpar(hdr,"UT"),2)
         airmass=sxpar(hdr,"AIRMASS")
         imtype =strmid(sxpar(hdr,"IMAGETYP"),0,1)
         imflag ='-'
         reads,dateobs,day,mon,yr,format='(i2,1x,i2,1x,i4)'
         reads,ut,hr,mn,sc,format='(i2,1x,i2,1x,i4)'
         hr = float(hr)+float(mn)/60.0+float(sc)/3600.0
         jdcnv,yr,mon,day,hr,jd
         jd = jd + (avego*exptime/2.0)/3600.0d0/24.0d0
         imtype = [tbl.imtype,imtype]
         imflag = [tbl.imflag,imflag]
         fn     = [tbl.fn,rawlist[i]]
;print,'tbl.imnum',tbl.imnum
;print,'pre imnum',imnum
         imnum  = [tbl.imnum,imnum]
;print,'pst imnum',imnum
         objnam = [tbl.objnam,objname]
         jd     = [tbl.jd,jd]
         airmas = [tbl.airmas,airmass]
         exptim = [tbl.exptim,exptime]
         avego  = [tbl.avego,avego]
         mate   = [tbl.mate, -1]
         relsig = [tbl.relsig, -1.0]
         z=uniq(fn,sort(fn))
         tbl = { $
            nobs:   n_elements(z), $
            imtype: imtype[z], $
            imflag: imflag[z], $
            fn:     fn[z], $
            imnum:  imnum[z], $
            objnam: objnam[z], $
            jd:     jd[z], $
            airmas: airmas[z], $
            exptim: exptim[z], $
            avego:  avego[z], $
            mate:   mate[z], $
            relsig: relsig[z], $
            dirty:  1 $
            }
      endif
   endfor

   ; Look for calibration/extraction info.  If not found, must generate it.
   if not exists('files.cal') then begin
      info.caldate=''

      ; If there are no flat frames we must get the calibration information
      ;   from another night.
      z=where(tbl.imtype eq 'f',count)
      if count eq 0 then begin
         print,''
         print,"This night's data does not contain any flat fielding data.  This means that"
         print,"you must use a flat field and calibration information from another night."
         print,"Using the pop-up dialog that just appeared, you must locate a flat field"
         print,"image that will work for the data on this night.  If you think there is"
         print,"relevant calibration data on this night, the click cancel and review the"
         print,"information table that can be found in ",info.root,".tbl before running this"
         print,"program again."
         print,''
         flatname=dialog_pickfile(filter='*.flat',title='Select Flat Field', $
                                  /noconfirm,/must_exist,get_path=fpath)
         if flatname eq '' then return
         spawn,'cp -p '+flatname+' .'
         spawn,'cp -p '+fpath+'files.cal .'

      ; Flats are present, create new calibration information.
      endif else begin
         print,'   ---> Creating new calibration support information.'
         ans=''
         read,prompt='Create new calibraton (N) or copy from old (O)? ',ans
         if strlowcase(strmid(ans,0,1)) eq 'n' then begin
            openw,lun,'files.cal',/get_lun
            printf,lun,'OSIRIS XD v2.0'
            printf,lun,'36'
            nor=0
            read,prompt='How many orders to extract? (3 or 4) ',nor,format='(i)'
            printf,lun,nor,format='(i1)'
            printf,lun,'2'
            if nor eq 4 then begin
               printf,lun,'96 255 251.85 -0.329 2.3042016 -0.0019167348 -1.6455014e-07'
               printf,lun,' 0 255 151.97 -0.252 1.8679308 -0.0014568482  0.'
               printf,lun,' 0 255  91.56 -0.205 1.4937664 -0.0011471715 -7.1757334e-8'
               printf,lun,' 0  80  50.45 -0.161 1.1742377 -0.0008556351 -1.4269414e-6'
            endif else begin
               printf,lun,' 45 255 245.13 -0.328   2.3617096e+00  -1.9464493e-03   1.9557774e-08'
               printf,lun,'  0 255 146.87 -0.249   1.8353825e+00  -1.4372170e-03  -6.9383532e-08'
               printf,lun,'  0 255  87.97 -0.205   1.4681654e+00  -1.1459589e-03  -8.2887709e-08'
            endelse
            printf,lun,'[none]'
            free_lun,lun
         endif else begin
            print,''
            print,'Choose an existing files.cal file from a previous night as a'
            print,'starting point for the calibration information.'
            print,''
            calname=dialog_pickfile(filter='files.cal',title='Select Old File', $
                                     /noconfirm,/must_exist,get_path=fpath)
            if calname eq '' then return
            spawn,'cp -p '+calname+' .'
            ldcalir,'files.cal',calib,valid,/noflat,/silent
            wrcalir,calib,'files.cal'
         endelse
         allcal=1
      endelse
   endif

   spawn,lscmd+'-l files.cal',result
   if result[0] ne info.caldate then begin
      ldcalir,'files.cal',calib,valid,/silent
      if not valid then begin
         print,'calibration support file, files.cal, is invalid.'
         return
      endif
      info.caldate=result[0]
   endif

   imx = 1000 ; top scaling for bright star

   ; Get object reference for slope stuff
   if allcal or slopecal or relposcal then begin
      print,''
      print,'When determining the calibration information this program needs to have'
      print,'access to an image which contains a single bright star.  The most'
      print,'important consideration is the need for a image with a single strong'
      print,'point source with little to no sky signal.  The dialog box that has'
      print,'just appeared allows you an opportunity to select the needed image.'
      print,''
      if info.bsrefn lt 0 then begin
         frlist=strarr(tbl.nobs)
         for i=0,tbl.nobs-1 do begin
            frlist[i] = tbl.fn[i] + ' ' + tbl.objnam[i]
         endfor
         bs=picker(frlist,title='Select bright object',index=bsref,ysize=40)
         if bs eq '[[[CANCEL]]]' then return
         bsimage=readfits(info.dirroot+tbl.fn[bsref],/silent)
         setwin,0,xsize=256,ysize=256
         tv,bytscl(bsimage,min=0,max=imx,top=!d.n_colors-1)
         words=strsplit(tbl.fn[bsref],'.',/extract)
         info.bsrefn=fix(words[1])
      endif
   endif

   ; Get flat field images
   if allcal or absposcal or calib.flatname eq '[none]' then begin
      print,''
      print,'You must also identify the flat field image pair (one with lights on'
      print,'and one with lights off).  The dialog box that has just appeared allows'
      print,'you an opportunity to select the needed image pair.'
      print,''
      if info.flaton lt 0 or info.flatoff lt 0 then begin
         frlist=strarr(tbl.nobs)
         for i=0,tbl.nobs-1 do begin
            frlist[i] = tbl.fn[i] + ' ' + tbl.objnam[i]
         endfor
         ffs=picker(frlist,title='Flat with lights',ysize=40,index=ff1)
         if ffs eq '[[[CANCEL]]]' then return
         ffs=picker(frlist,title='Flat no lights',ysize=40,index=ff2)
         if ffs eq '[[[CANCEL]]]' then return
         words=strsplit(tbl.fn[ff1],'.',/extract)
         info.flaton=fix(words[1])
         words=strsplit(tbl.fn[ff2],'.',/extract)
         info.flatoff=fix(words[1])
      endif
   endif

   ; Set slope of each order (if requested)
   if allcal or slopecal then begin
      print,''
      print,'The first determination of the calibration information is to find the slope'
      print,'of each spectral order.  You will proceed through each order to set this'
      print,'slope, one order at a time.  Window 1 will contain a plot of the current'
      print,"order's centerline.  Also, the program will print out the current slope"
      print,'of the extracted centerline.  If the extraction slope is correct, then this'
      print,'slope should be zero.  The commands during this step are s, x, y, or q'
      print,''
      print,'    s  - Compute current slope and set new slope to the new value.'
      print,'    x  - Set the x bounds for the extraction in this order.'
      print,'    y  - Set the y location for this order.'
      print,'    q  - Quit, go on to the next step.'
      print,''
      print,'Normally, you would just use "s" on each order and you would be done.'
      print,'Occasionally, you may need to tweak the x location.  You only need to'
      print,'tweak the y location if the centerline is not located at all.   As long'
      print,'as you can see the bright star spectrum in this step you are close enough.'
      print,''
      dirtycal=0
      for i=0,calib.nor-1 do begin
         choice=''
         title='Order '+string(i,form='(i1)')
         repeat begin
            getstrip,calib,info.dirroot+'/'+info.root,info.bsrefn,strip
            setwin,1,xsize=256,ysize=calib.height
            erase
            tv,bytscl(strip[calib.o[i,0]:calib.o[i,1],*], $
                         min=0,max=imx,top=!d.n_colors-1)
            maxloc,strip[calib.o[i,0]:calib.o[i,1],*],/x,xpos,ypos
            xpos=xpos+calib.x1[i]
            coeff=goodpoly(xpos,ypos,1,2.0,yfit,newx,newy)
            setwin,2,xsize=400,ysize=250
            setusym,-1
            plot,xpos,ypos,psym=-8,yr=[0,calib.height-1],title=title, $
               xtitle='Column number',ytitle='Row number'
            setusym,1
            oplot,newx,newy,psym=8
            oplot,xpos,yfit
            print,title,'   slope = ',coeff[1]
            read,prompt='Action? (s,x,y or  q)  ',choice
            case choice of
               's': begin
                     calib.slope[i] = calib.slope[i]+coeff[1]
                     dirtycal = 1
                  end
               'x': begin
                     print,'Current x bounds: ',calib.x1[i],calib.x2[i]
                     newx1=0
                     newx2=0
                     read,prompt='New x bounds? ',newx1,newx2
                     calib.x1[i]=newx1
                     calib.x2[i]=newx2
                     dirtycal = 1
                  end
               'y': begin
                     print,'Current y position: ',calib.y0[i]
                     newy=0.0
                     read,prompt='New y position? ',newy
                     calib.y0[i] = newy
                     dirtycal = 1
                  end
               else: begin
                     if choice ne 'q' then begin
                        print,''
                        print,'  s - Adjust slope based on fitted line'
                        print,'  x - Provide new x boundaries for the order'
                        print,'  y - Provide new y position for the order'
                        print,''
                        print,'  q - Quit, no more changes to this order'
                        print,''
                     endif
                  end

            endcase

            if dirtycal then begin
               wrcalir,calib,'files.cal'
               ldcalir,'files.cal',calib,valid,/silent
               spawn,lscmd+'-l files.cal',result
               info.caldate=result[0]
               dirtycal=0
            endif

         endrep until choice eq 'q'
      endfor

   endif ; end slope adjustment block

   ; Set the relative shifts between orders
   if allcal or relposcal then begin
      print,''
      print,'The next step is to get the relative shifts between the orders set so that'
      print,'the centerline of the star spectrum is in the same place in all orders'
      print,'(after extraction).  To set the relative location, you will be asked to'
      print,'pick a reference order.  It does not really matter which order you pick if'
      print,'you have picked a good reference star.  After selecting the reference order'
      print,'the locations of the other orders are shifted to make them all have the same'
      print,'location as the reference order.'
      print,''
      dirtycal=0
      choice=''
      repeat begin
         getstrip,calib,info.dirroot+'/'+info.root,info.bsrefn,strip
         setwin,2,xsize=400,ysize=250
         plot,[0],[0],xr=[0,calib.height-1],yr=[-0.1,1.1]
         backsub,strip,/col,order=1
         diff=strip
         idx=findgen(calib.height)
         avgcol=fltarr(calib.height)
         for i=0,calib.npts-1 do begin
            diff[i,*]=diff[i,*]/max(diff[i,*])
         endfor
         yloc=fltarr(calib.nor)
         for i=0,calib.nor-1 do begin
            for j=0,calib.height-1 do begin
               robomean,diff[calib.o[i,0]:calib.o[i,1],j],3,.5,avg
               avgcol[j]=avg
            endfor
            yfit=gaussfit(idx,avgcol,a)
            yloc[i] = a[1]
            if i eq 0 then begin
              aref=a[1]
              print,'order ',i,a[1]
            endif else begin
              print,'order ',i,a[1],calib.y0[i]+a[1]-aref
            endelse
            oplot,idx,avgcol,psym=-8,symsize=0.5,color=!d.n_colors-1-i*20
            z=where(avgcol eq max(avgcol))
            xyouts,idx[z],avgcol[z],string(i,format='(i1)'),align=0.5
         endfor

         ;Just look and make sure object spectrum lines up between orders.
         setwin,1,xsize=calib.npts,ysize=calib.height
         tv,bytscl(strip,min=-20,max=70,top=!d.n_colors-1)

         read,prompt='Reference order for shift? (or q to quit) ',choice
         case choice of
            '0': begin
                  oref = fix(choice)
                  dirtycal = 1
               end

            '1': begin
                  oref = fix(choice)
                  dirtycal = 1
               end

            '2': begin
                  oref = fix(choice)
                  dirtycal = 1
               end

            '3': begin
                  oref = fix(choice)
                  dirtycal = 1
               end

            else: begin
               end
         endcase

         if dirtycal then begin
            if oref lt calib.nor then begin
               calib.y0=calib.y0+(yloc-yloc[oref])
               wrcalir,calib,'files.cal'
               ldcalir,'files.cal',calib,valid,/silent
               spawn,lscmd+'-l files.cal',result
               info.caldate=result[0]
            endif
            dirtycal=0
         endif

      endrep until choice eq 'q'

   endif ; end relative shift block

   ; Set the absolute positions of all orders (together)
   if allcal or absposcal then begin
      print,''
      print,'The final location step is the set the absolute location of the slit on'
      print,'the image.  To do this you are shown a plot of some random columns in the'
      print,'image as traces across the raw flat field image.  You want to set the'
      print,'slit location such that you are seeing just those pixels illuminated by'
      print,'the spectrograph.  Each shift value you give is relative to the current'
      print,'location so you can move things around by successively smaller amounts'
      print,'until the extraction parameters look right.'
      print,''
      choice=''
      dirtycal=0
      repeat begin

         setwin,1,xsize=calib.npts,ysize=calib.height
         getpair,calib,info.dirroot+info.root,info.flaton, $
            info.flatoff,strip,/raw
         erase
         tvscl,strip,0
         setwin,2,xsize=400,ysize=250
         plot,strip[100,*]/max(strip[100,*]),yr=[0.,1.]
         oplot,strip[101,*]/max(strip[101,*])
         oplot,strip[102,*]/max(strip[102,*])
         oplot,strip[250,*]/max(strip[250,*]),color=100
         oplot,strip[251,*]/max(strip[251,*]),color=100
         oplot,strip[252,*]/max(strip[252,*]),color=100
         oplot,strip[480,*]/max(strip[480,*]),color=80
         oplot,strip[481,*]/max(strip[481,*]),color=80
         oplot,strip[482,*]/max(strip[482,*]),color=80
         oplot,strip[680,*]/max(strip[680,*]),color=60
         oplot,strip[681,*]/max(strip[681,*]),color=60
         oplot,strip[682,*]/max(strip[682,*]),color=60

         read,prompt='Shift amount (or q to quit) ',choice

         if choice ne 'q' then begin
            calib.y0=calib.y0+float(choice)
            wrcalir,calib,'files.cal'
            ldcalir,'files.cal',calib,valid,/silent
            spawn,lscmd+'-l files.cal',result
            info.caldate=result[0]
         endif

      endrep until choice eq 'q'

   endif ; end absolute position block

   ; Construct flat field
   if calib.flatname eq '[none]' then begin
      print,''
      print,'The final step in the creation of the flat field is its normalization.'
      print,'You should select an intensity level from the plot in window 2 that'
      print,'roughly corresponds to the average peak signal.'
      print,''
      getpair,calib,info.dirroot+info.root,info.flaton, $
         info.flatoff,strip,/raw
      setwin,2,xsize=400,ysize=250
      plot,strip[*,calib.height/2],yr=[0,max(strip[*,calib.height/2])]
      for i=0,calib.height-1 do oplot,strip[*,i]

      ; Get normalization constant
      print,'Click on window 2 at the normalization level'
      cursor,x,ynorm,3,/data

      ; Normalize and clip very low values.
      setwin,1,xsize=calib.npts,ysize=calib.height*3
      erase
      tvscl,strip,0
      flat = strip/ynorm
      mflat=median(flat,7)
      z=where(mflat lt 0.005,count)
      if count ne 0 then mflat[z]=0.005
      tvscl,mflat,1
      z=where(abs(flat/mflat - 1.0) gt 0.2 or (flat lt 0.005),count)
      if count ne 0 then flat[z]=mflat[z]
      tvscl,flat,2

      ; Plot the normalized flat field.
      setwin,3,xsize=400,ysize=250
      plot,flat[*,calib.height/2],yr=[0.,max(flat)]
      for i=0,calib.height-1 do oplot,flat[*,i]
      setwin,4,xsize=400,ysize=250
      plot,[0],[1],/nodata,xr=[0,calib.npts-1],yr=[0,max(flat/mflat)]
      for i=0,calib.height-1 do oplot,flat[*,i]/mflat[*,i]

      ; Save data to FITS file.
      calib.flatname = '+'+info.root+'.flat'
      writefits,info.root+'.flat',flat
      print,'New flat field saved to ',info.root+'.flat'
      wrcalir,calib,'files.cal'
      ldcalir,'files.cal',calib,valid,/silent
      spawn,lscmd+'-l files.cal',result
      info.caldate=result[0]

   endif

   ; Wavelength calibration
   if allcal or wavecal then begin
      if not exists('sky.spec') then begin

         print,''
         print,'Determining the wavelength calibration requires the generation of a sky'
         print,'spectrum.  This is best done from a set of images on an object that is'
         print,'faint and thus has a considerable sky signal in the data.  Just select'
         print,'an appropriate object the program will automatically construct a sky'
         print,'spectrum from the block of observations you selected.'
         print,''

retry_sky:
         ; Pick an object that has lots of sky signal.
         frlist=strarr(tbl.nobs)
         for i=0,tbl.nobs-1 do frlist[i] = tbl.fn[i] + ' ' + tbl.objnam[i]
         skys=picker(frlist,title='Select frame for sky',index=ski,ysize=40)
         if skys eq '[[[CANCEL]]]' then return

         ; Scan to find range of contiguous frames for this object
         i10=ski
         i20=ski
         repeat i10=i10-1 until tbl.objnam[i10] ne tbl.objnam[ski] or i10 eq 0
         repeat i20=i20+1 until tbl.objnam[i20] ne tbl.objnam[ski] or i20 eq tbl.nobs-1
         if i10 ne 0 then i10=i10+1
         if i20 ne tbl.nobs-1 then i20=i20-1
         words=strsplit(tbl.fn[i10],'.',/extract)
         i1=fix(words[1])
         words=strsplit(tbl.fn[i20],'.',/extract)
         i2=fix(words[1])

         ; Check for any excluded frames
         zg=where(tbl.imflag[i10:i20] eq '-',countzg)
         if zg[0] eq -1 then begin
            print,'All frames for this object set are excluded, try another object set.'
            goto,retry_sky
         endif
         zidx=indgen(i20-i10+1)+i10

         ; Read in entire stack of frames
         stack=fltarr(calib.npts,calib.height,countzg,/nozero)
         print,'   ---> Loading frames ',i1,i2
         j=i10
         k=0
         for i=i1,i2 do begin
            if tbl.imflag[j] eq '-' then begin
               getstrip,calib,info.dirroot+info.root,i,strip
               stack[*,*,k] = strip
               k=k+1
            endif
            j=j+1
         endfor

         ; Combine stack into a single frame.
         print,'   ---> Averaging stack of frames '
         avgclip,stack,avg,/silent
         setwin,1,xsize=calib.npts,ysize=calib.height*3
         tvscl,avg,0

         ; Get an averaged sky signal
         nonsky=avg
         backsub,nonsky,/col
         tvscl,nonsky,1
         sky=avg-nonsky
         tvscl,sky,2
         skyspec=sky[*,15]
         print,'   ---> Saving sky spectrum to sky.spec '
         writefits,'sky.spec',skyspec

      endif ; end sky spectrum creation block

      sky=readfits('sky.spec',/silent)
      gotlines=0

      if exists('/opt/local/iraf/iraf/noao/lib/linelists/ohlines.dat') then begin
         readcol,'/opt/local/iraf/iraf/noao/lib/linelists/ohlines.dat', $
            ohlam,flux,/silent
         gotlines=1
      endif

      if exists('/usr/local/iraf/noao/lib/linelists/ohlines.dat') then begin
         readcol,'/usr/local/iraf/noao/lib/linelists/ohlines.dat', $
            ohlam,flux,/silent
         gotlines=1
      endif

      if not gotlines then begin
         print,'FATAL ERROR!  Unable to find the IRAF ohlines.dat file.'
         return
      endif

      ohlam=ohlam/10000.0
      dd=[-2,-1,0,1,2] ; centroiding aperture
      dirtycal=0

      print,''
      print,'You need to calibrate the wavelength scale for each order.  You will be'
      print,'shown the measured sky spectrum (on top in white) and a synthetic OH line'
      print,'spectrum (on the bottom in orange).  You will identify corresponding lines'
      print,'in each spectrum.  As the number of lines identified increases, the'
      print,'wavelength calibration is recomputed and replotted accordingly.  During this'
      print,'step the mouse buttons have the following functions:'
      print,''
      print,'              (1st:sky spectrum)              (2nd:synthetic)'
      print,'   left      measure nearest line        grab nearest strong line'
      print,'   middle    delete nearest point                nothing'
      print,'   right           done                      abort this line'
      print,''

      for i=0,calib.nor-1 do begin
         ordfile='order'+string(i,format='(i1)')+'.dat'

         title='Order '+string(i,form='(i1)')

         ;Extract working copy for this order and sort by increasing wavelength
         osky=sky[calib.o[i,0]:calib.o[i,1]]
         wsky=calib.w[calib.o[i,0]:calib.o[i,1]]
         n = calib.x2[i] - calib.x1[i] + 1
         xsky=indgen(n)
         idx=sort(wsky)
         wsky=wsky[idx]
         osky=osky[idx]
         xsky=xsky[idx]

         ; Find local minima to define a spline-based continuum and remove.
         zcont=lclxtrem(osky,5,/point_order)
         tmpval=spl_init(wsky[zcont],osky[zcont])
         cont=spl_interp(wsky[zcont],osky[zcont],tmpval,wsky)
         osky=osky-cont

         ; Find all local maxima, most will be sky lines.
         zsky=lclxtrem(osky,5,/max,/point_order)

         yr=[-max(osky)/2.0,max(osky)]

         done=0

         if exists(ordfile) then begin
            readcol,ordfile,xloc,wloc,fwhm,fwhs,bad,format='f,f,f,f,i',/silent
            nmeas=n_elements(xloc)
         endif else begin
            nmeas=0
         endelse

         repeat begin

            ; do as much as possible given what is known from current
            ;   line assignments
            if nmeas eq 1 then begin
               xa = poly(xloc[0],calib.cof[i,*])
               calib.cof[i,0] = calib.cof[i,0]-xa+wloc[0]
            endif else if nmeas eq 2 then begin
               calib.cof[i,2:*] = 0.0
               calib.cof[i,1] = (wloc[1]-wloc[0])/(xloc[1]-xloc[0])
               xa = poly(xloc[0],calib.cof[i,*])
               calib.cof[i,0] = calib.cof[i,0]-xa+wloc[0]
            endif else if nmeas eq 3 then begin
               por=min([2,calib.por])
               calib.cof[i,0:por]=goodpoly(xloc,wloc,por,2.0,yfit,newx,newy)
            endif else if nmeas ge 4 then begin
               por=min([3,calib.por])
               calib.cof[i,0:por]=goodpoly(xloc,wloc,por,2.0,yfit,newx,newy)
            endif else begin
            endelse

            ; Update the wavelength segment in the calibration info for the
            ;   current provisional coefficients.
            wsky=poly(findgen(n),calib.cof[i,*])
            calib.w[calib.o[i,0]:calib.o[i,1]]=wsky
            wsky=wsky[idx]

            ; Create synthetic spectrum from OH line list.
            disp = abs( (calib.w[calib.o[i,1]]-calib.w[calib.o[i,0]]) / $
                        (calib.o[i,1]-calib.o[i,0]) )
            npts = calib.o[i,1]-calib.o[i,0]+1+20
            lam0 = min(calib.w[calib.o[i,0]:calib.o[i,1]])-abs(disp)*10
            ohl = findgen(npts)*disp + lam0
            ohs = fltarr(npts)
            ohp=fix((ohlam - lam0)/disp + 0.5)
            z=where(ohp ge 0 and ohp lt npts)
            ohs[ohp[z]]=flux[z]
            gauss2d,11,11,5,5,2.0,arr
            kern=arr[*,5]
            kern=kern/total(kern)
            sohs=convol(ohs,kern)
            sohs=sohs/max(sohs)*max(osky)
            zsyn=lclxtrem(sohs,5,/max,/point_order)
            sohss=sohs-max(osky)/2.0

            ; plot dispersion fit (if enough points).
            if nmeas ge 3 then begin
               setwin,5,xsize=500,ysize=250
               setusym,-1
               plot,wloc,(wloc-poly(xloc,calib.cof[i,*]))/calib.cof[i,1], $
                  xtitle='Wavelength (microns)',ytitle='Residuals (pixels)', $
                  title=title,psym=8
               setusym,1
               oplot,newy,(newy-poly(newx,calib.cof[i,*]))/calib.cof[i,1],psym=8
            endif

            ; plot spectrum with overlain synthetic spectrum
            setwin,4,xsize=1000,ysize=600
            plot,wsky,osky,yr=yr, $
               xtitle='Wavelength (microns)',ytitle='Signal',title=title
            oplot,ohl,sohss,color=80
            if nmeas ne 0 then begin
               xa = poly(xloc,calib.cof[i,*])
               if nmeas ge 3 then begin
                  oplot,xa,replicate(0.,nmeas),psym=5,color=55
                  oplot,wloc,replicate(yr[0],nmeas),psym=5,color=55
                  xa = poly(newx,calib.cof[i,*])
                  oplot,xa,replicate(0.,nmeas),psym=5
                  oplot,newy,replicate(yr[0],nmeas),psym=5,color=80
               endif else begin
                  oplot,xa,replicate(0.,nmeas),psym=5
                  oplot,wloc,replicate(yr[0],nmeas),psym=5,color=80
               endelse
            endif

            print,'Click on line in sky spectrum (white)'
            cursor,x,y,3,/data
            if !mouse.button eq 1 then begin
               dirtycal=1
               z=where(abs(x-wsky[zsky]) eq min(abs(x-wsky[zsky])))
               z=z[0]
               oplot,[wsky[zsky[z]]],[osky[zsky[z]]],psym=4
               xloc0 = total(xsky[zsky[z]+dd]*osky[zsky[z]+dd]) / $
                          total(osky[zsky[z]+dd])
               fwhm0 = total(osky[zsky[z]+dd])/max(osky[zsky[z]+dd])

               print,'Click on line in synthetic spectrum (orange)'
               cursor,x,y,3,/data
               if !mouse.button eq 1 then begin
                  ; based on centroiding synthetic spectrum
                  z=where(abs(x-ohl[zsyn]) eq min(abs(x-ohl[zsyn])))
                  z=z[0]
                  ;wloc0 = total(ohl[zsyn[z]+dd]*sohs[zsyn[z]+dd]) / $
                  ;           total(sohs[zsyn[z]+dd])
                  fwhs0 = total(sohs[zsyn[z]+dd])/max(sohs[zsyn[z]+dd])
                  ; based on nearest bright line.
                  z=where(abs(x-ohlam) lt 2.0*abs(calib.cof[i,1]))
                  zz=where(flux[z] eq max(flux[z]))
                  wloc0 = ohlam[z[zz]]
                  if nmeas eq 0 then begin
                     xloc  = [xloc0]
                     wloc  = [wloc0]
                     fwhm  = [fwhm0]
                     fwhs  = [fwhs0]
                     bad   = 0
                     nmeas = 1
                  endif else begin
                     xloc  = [xloc,xloc0]
                     wloc  = [wloc,wloc0]
                     fwhm  = [fwhm,fwhm0]
                     fwhs  = [fwhs,fwhs0]
                     bad   = [bad,0]
                     nmeas = nmeas+1
                  endelse
                  print,nmeas,xloc0,wloc0,fwhm0,fwhs0
               endif
            endif else if !mouse.button eq 2 then begin
               if nmeas eq 1 then begin
                  dirtycal=1
                  nmeas=0
               endif else begin
                  dirtycal=1
                  z=where(abs(x-wloc) ne min(abs(x-wloc)))
                  xloc  = xloc[z]
                  wloc  = wloc[z]
                  fwhm  = fwhm[z]
                  fwhs  = fwhs[z]
                  bad   = bad[z]
                  nmeas = nmeas - 1
               endelse
            endif else begin
               if nmeas ne 0 and dirtycal then begin
                  openw,lun,ordfile,/get_lun
                  for j=0,nmeas-1 do $
                     printf,lun,xloc[j],wloc[j],fwhm[j],fwhs[j],bad[j]
                  free_lun,lun
               endif
               done=1
            endelse

         endrep until done

      endfor

      if dirtycal then begin
         print,'   ---> Updating files.cal with new dispersion correction'
         wrcalir,calib,'files.cal'
         ldcalir,'files.cal',calib,valid,/silent
         spawn,lscmd+'-l files.cal',result
         info.caldate=result[0]
         dirtycal=0
      endif

   endif ; end wavelength calibration block

   ;=========================
   ; Begin section on spectral extraction

   if not nospec then begin

      tblparse,tbl,oblist,badset
      ng=n_elements(oblist)/3

      if badset then begin
         print,''
         print,'OSIRIS data cannot be properly reduced with object sets that are'
         print,'not homogeneous.  You must locate the oddball frames and marked them'
         print,'to be skipped.  The second column in file ',info.root,'.tbl should be'
         print,'changed from - to x to mark it as a frame to skip.  You must fix this'
         print,'problem before this program will proceed to extract spectra from this'
         print,'nights data.  Edit the file and try again.'
         print,''
         goto,exitproc
      endif

      ; Find images mates for those that need it.
      if max(oblist[2,*]) ne 0 then print,'   ---> Scanning for image mates.'
      for i=0,ng-1 do begin
         if oblist[2,i] eq 1 then begin
            i1=oblist[0,i]
            i2=oblist[1,i]
            zg=where(tbl.imflag[i1:i2] ne 'x',countzg)
            idx=indgen(i2-i1+1)+i1
            idx=idx[zg]
            if i1 eq i2 then begin
               tbl.imtype[i1]='t'
               print,'Warning, ',tbl.fn[i1],' is a solitary frame and'
               print,'cannot be reduced.  It has now been marked as such and will'
               print,'be skipped in future processing.'
               oblist[0,i]=-1
            endif else begin
               print,tbl.objnam[i1]+blanks,tbl.imnum[i1], $
                     tbl.imnum[i2],format='(a16,1x,"frames",2x,i3," to ",i3)'
               zx=where(tbl.imflag[i1:i2] eq 'x',countzx)
               zidx=indgen(i2-i1+1)+i1
               if countzx ne 0 then begin
                  imnum=tbl.imnum[i1:i2]
                  excl=imnum[zx]
                  tbl.mate[zidx[zx]]=-1
               endif else begin
                  excl=-1
               endelse

               clscan,calib,info.root,tbl.imnum[oblist[0,i]], $
                      tbl.imnum[oblist[1,i]],frno,mate,EXCLUDE=excl, $
                      xrange=[250,450],path=info.dirroot
               tbl.mate[idx]=mate
;               for j=oblist[0,i],oblist[1,i] do begin
;                  outprof='Spec/'+info.root+'p.'+string(tbl.imnum[j],format='(i3.3)')
;                  outspec='Spec/'+info.root+'s.'+string(tbl.imnum[j],format='(i3.3)')
;                  outbadp='Spec/'+info.root+'b.'+string(tbl.imnum[j],format='(i3.3)')
;                  outmask='mask.'+string(tbl.imnum[j],format='(i3.3)')
;                  kill=0
;                  if exists(outprof) then kill=1
;                  if not kill then if exists(outspec) then kill=1
;                  if not kill then if exists(outbadp) then kill=1
;                  if kill then spawn,'rm '+outprof+' '+outspec+' '+ $
;                                           outbadp+' '+outmask+' >& /dev/null'
;               endfor
            endelse
            tbl.dirty=1
         endif
      endfor
      if max(oblist[2,*]) ne 0 then print,''

      maskname = info.root+'.mask'

      ; If on a final pass, build the mask image if not already done.
      if not exists(maskname) and final then begin
         bildmask,maskname
         spawn,rmcmd+' mask.*'
         spawn,rmcmd+' Spec/'+info.root+'b.* >& /dev/null'
      endif

      ; Set conditions for bad pixel mask
      if exists(maskname) then begin
         fmask=readfits(maskname,/silent)
         mask = fix(fmask / maskthresh) < 1
         findbad=0
      endif else begin
         findbad=1
      endelse

      ; Extract spectra
      if not exists('Spec') then file_mkdir,'Spec'
      for i=0,tbl.nobs-1 do begin
         if tbl.imtype[i] eq 'o' and tbl.imflag[i] eq '-' then begin
            outspec='Spec/'+info.root+'s.'+string(tbl.imnum[i],format='(i3.3)')
            outprof='Spec/'+info.root+'p.'+string(tbl.imnum[i],format='(i3.3)')
            ; Check to see if existing frame is consistent with frame mates.
            if exists(outspec) then begin
               chkhdr=headfits(outspec)
               frammate=sxpar(chkhdr,'FRAMMATE')
               if frammate ne tbl.mate[i] then begin
                  print,'Mate changed for frame ',tbl.fn[i],', recomputing.'
                  outbadp='Spec/'+info.root+'b.'+string(tbl.imnum[i],format='(i3.3)')
                  outmask='mask.'+string(tbl.imnum[i],format='(i3.3)')
                  spawn,rmcmd+' '+outprof+' '+outspec+' '+ $
                              outbadp+' '+outmask+' >& /dev/null'

               endif
            endif
            if not exists(outspec) or not exists(outprof) or allspec or final then begin
               if findbad then begin
                  optspec,calib,info.root,tbl.imnum[i],tbl.mate[i],/save, $
                     path=info.dirroot,findbad=findbad, $
                     outpath='Spec',gausscor=gausscor,final=final,plots=plots
               endif else begin
                  optspec,calib,info.root,tbl.imnum[i],tbl.mate[i],/save, $
                     path=info.dirroot,mask=mask, $
                     outpath='Spec',gausscor=gausscor,/final,plots=plots
               endelse
            endif
         endif
      endfor

      ; Within groups, generate final average spectra
      if not exists('Final') then file_mkdir,'Final'
      for i=0,ng-1 do begin
         if oblist[0,i] ne -1 and oblist[0,i] ne oblist[1,i] then begin
            root = 'Final/' + info.root + '.' + string(i,format='(i2.2)')
            suff = strlowcase(tbl.objnam[oblist[0,i]])
            outname= root + '.' + suff
            i1=oblist[0,i]
            i2=oblist[1,i]
            zg=where(tbl.imflag[i1:i2] eq '-',countzg)
            idx=indgen(i2-i1+1)+i1
            idx=idx[zg]
            if not exists(outname) or allspec or final or avgspec or $
               min(tbl.relsig[idx]) lt 0.0 then begin

               print,tbl.objnam[i1]+blanks,tbl.imnum[i1],tbl.imnum[i2],outname, $
                     format='(a16,1x,"frames",2x,i3," to ",i3," saving avg to ",a)'

               zx=where(tbl.imflag[i1:i2] ne '-',countzx)
               if countzx ne 0 then begin
                  imnum=tbl.imnum[i1:i2]
                  excl=imnum[zx]
               endif else begin
                  excl=-1
               endelse
               nframes = countzg
               jd=total(tbl.jd[idx])/double(nframes)
               airmas=mean(tbl.airmas[idx])
               avgspec,'Spec/'+info.root+'s',suff,tbl.imnum[i1],EXCLUDE=excl, $
                  tbl.imnum[i2],OUTROOT=root,JD=jd,AIRMASS=airmas, $
                  OBJECT=tbl.objnam[i1],scfactor=sf,/WEIGHTED
               tbl.relsig[idx]=sf
               tbl.dirty=1
            endif
         endif
      endfor

   endif

   ;=========================
   ; End spectral extraction section

   if min(groupcheck) ge 0 then begin
      dname=!d.name
      set_plot,'PS'
      setpage,/portrait,xsize=18.0,ysize=24.0
      device,/Helvetica
      page=0
      plot=0
      gap=0.03
      nplots=8 ; 5
      x1=0.05
      x2=0.95
      cs=0.5
      dx=0.05
      dl=0.009
      blankt=replicate(' ',30)
      dy = (1.0-(nplots-1)*gap)/float(nplots)

      for i=groupcheck[0],groupcheck[1] do begin
         z=where(tbl.imnum eq i and tbl.imtype eq 'o' and tbl.imflag ne 'x')
         z=z[0]
         if z ne -1 then begin
            if plot eq 0 then noerase=0 else noerase=1
            y2 = 1.0 - (gap+dy)*plot
            y1 = y2 - dy
            specname='Spec/'+info.root+'s.'+string(tbl.imnum[z],format='(i3.3)')
            profname='Spec/'+info.root+'p.'+string(tbl.imnum[z],format='(i3.3)')
            print,'plot',page,plot,specname,format='(a,1x,i2,1x,i2,1x,a)'
            spec=readfits(specname,hdr,/silent)
            readcol,profname,colno,avgcol,tag,lowband,hiband, $
               format='i,f,a,f,f',/silent
            idx=reverse(sort(spec))

            yr=[0.,mean(spec[idx[20:30]])]
            plotspec,calib,spec,yr=yr,position=[x1,y1,x2-dx,y2],noerase=noerase, $
               charsize=cs

            xyouts,x2,y2-dl*0.5,tbl.objnam[z],align=0.5,charsize=cs,/normal
            jdstr,tbl.jd[z],0,jds
            xyouts,x2,y2-dl*2.0,strmid(jds,0,10),align=0.5,charsize=cs,/normal
            xyouts,x2,y2-dl*3.0,strmid(jds,11,20),align=0.5,charsize=cs,/normal
            if tbl.avego[z] eq 1 then begin
               str=strcompress(string(tbl.exptim[z],format='(f10.1)'),/remove_all)+" sec"
            endif else begin
               str=strcompress(string(tbl.avego[z],tbl.exptim[z], $
                      format='(i10,"x",f10.1)'),/remove_all)+" sec"
            endelse
            xyouts,x2,y2-dl*4.0,str,align=0.5,charsize=cs,/normal
            str=strcompress(string(tbl.airmas[z],format='("Airmass=",f5.2)'),/remove_all)
            xyouts,x2,y2-dl*5.0,str,align=0.5,charsize=cs,/normal
            str=tbl.fn[z]+'-'+string(tbl.mate[z],format='(i3.3)')
            xyouts,x2,y2-dl*6.5,str,align=0.5,charsize=cs,/normal
            str=strcompress(string(tbl.relsig[z],format='("RelSig=",f10.3)'),/remove_all)
            xyouts,x2,y2-dl*7.5,str,align=0.5,charsize=cs,/normal
            plot = plot + 1
            if plot eq nplots then begin
               plot = 0
               page = page + 1
            endif
         endif
      endfor
      device,/close
      if page gt 0 or plot gt 0 and queue ne '[none]' then begin
         spawn,'lp -c -d '+queue+' idl.ps'
         spawn,rmcmd+' idl.ps'
      endif
      set_plot,dname
      !p.font=-1
      !p.multi=0
   endif

   ; Optional summary plots
   if min(plotrange) ge 0 then begin
      dname=!d.name
      set_plot,'PS'
      setpage,/portrait,xsize=18.0,ysize=24.0
      device,/Helvetica
      page=0
      plot=0
      gap=0.03
      nplots=8 ; 5
      x1=0.05
      x2=0.725
      x3=1.0
      cs=0.5
      dx=0.05
      dl=0.009
      blankt=replicate(' ',30)
      dy = (1.0-(nplots-1)*gap)/float(nplots)
      for i=plotrange[0],plotrange[1] do begin
         z=where(tbl.imnum eq i and tbl.imtype eq 'o' and tbl.imflag eq '-')
         z=z[0]
         if z ne -1 then begin
            if plot eq 0 then noerase=0 else noerase=1
            y2 = 1.0 - (gap+dy)*plot
            y1 = y2 - dy
            specname='Spec/'+info.root+'s.'+string(tbl.imnum[z],format='(i3.3)')
            profname='Spec/'+info.root+'p.'+string(tbl.imnum[z],format='(i3.3)')
            print,'plot',page,plot,specname,format='(a,1x,i2,1x,i2,1x,a)'
            spec=readfits(specname,hdr,/silent)
            readcol,profname,colno,avgcol,tag,lowband,hiband, $
               format='i,f,a,f,f',/silent
            idx=reverse(sort(spec))

            yr=[0.,mean(spec[idx[20:30]])]
            plotspec,calib,spec,yr=yr,position=[x1,y1,x2-dx,y2],noerase=noerase, $
               charsize=cs

            yr=[min([-1.0,min(avgcol)]),max([1.0,max(avgcol)])]
            plot,colno,avgcol,xtitle='Row number',position=[x2+dx,y1,x3,y2], $
               ytitle='Relative signal strength',/noerase,yr=yr, $
               xstyle=7,ystyle=7
            oplot,colno,lowband,linestyle=1
            oplot,colno,hiband,linestyle=1
            zp=where(tag eq 'p')
            zn=where(tag eq 'n')
            zs=where(tag eq 's')
            oplot,colno[zp],avgcol[zp],psym=5,symsize=0.5
            oplot,colno[zn],avgcol[zn],psym=2,symsize=0.5
            oplot,colno[zs],avgcol[zs],psym=8,symsize=0.2
            axis,xaxis=0,charsize=cs,xtitle='Row number'
            axis,xaxis=1,xtickname=blankt
            axis,yaxis=0,ytickname=blankt
            axis,yaxis=1,charsize=cs,ytitle='Relative signal strength'
            xyouts,x2,y2-dl*0.5,tbl.objnam[z],align=0.5,charsize=cs,/normal
            jdstr,tbl.jd[z],0,jds
            xyouts,x2,y2-dl*2.0,strmid(jds,0,10),align=0.5,charsize=cs,/normal
            xyouts,x2,y2-dl*3.0,strmid(jds,11,20),align=0.5,charsize=cs,/normal
            if tbl.avego[z] eq 1 then begin
               str=strcompress(string(tbl.exptim[z],format='(f10.1)'),/remove_all)+" sec"
            endif else begin
               str=strcompress(string(tbl.avego[z],tbl.exptim[z], $
                      format='(i10,"x",f10.1)'),/remove_all)+" sec"
            endelse
            xyouts,x2,y2-dl*4.0,str,align=0.5,charsize=cs,/normal
            str=strcompress(string(tbl.airmas[z],format='("Airmass=",f5.2)'),/remove_all)
            xyouts,x2,y2-dl*5.0,str,align=0.5,charsize=cs,/normal
            str=tbl.fn[z]+'-'+string(tbl.mate[z],format='(i3.3)')
            xyouts,x2,y2-dl*6.5,str,align=0.5,charsize=cs,/normal
            str=strcompress(string(tbl.relsig[z],format='("RelSig=",f10.3)'),/remove_all)
            xyouts,x2,y2-dl*7.5,str,align=0.5,charsize=cs,/normal
            plot = plot + 1
            if plot eq nplots then begin
               plot = 0
               page = page + 1
            endif
         endif
      endfor
      device,/close
      if page gt 0 or plot gt 0 and queue ne '[none]' then begin
         spawn,'lp -c -d '+queue+' idl.ps'
         spawn,rmcmd+' idl.ps'
      endif
      set_plot,dname
      !p.font=-1
      !p.multi=0
   endif

   ;=========================
   ; Exit processing

exitproc:
   ; If information table has been changed, regenerate output file.
   if tbl.dirty then begin
      print,'   ---> updating information table.'
      wrtbl,tablname,tbl
      spawn,lscmd+'-l '+tablname,result
      info.tbldate = result[0]
      tbl.dirty = 0
   endif

end
