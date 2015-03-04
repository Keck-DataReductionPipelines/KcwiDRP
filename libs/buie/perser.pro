;+
; NAME:
;  perser
; PURPOSE:
;  Automated asteroid period search tool.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  perser
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;   Written by Marc W. Buie, 96/10/18, Lowell Observatory
;   99/07/23, MWB, added hp and hs options (hardcopies)
;  2000/11/08, MWB, removed use of obsolete code constructs
;  2002/09/03, MWB, changed Str_sep call to strsplit
;-
pro perser_shift,date,mag,sdate,mshift,smag
   smag=mag
   for i=0,n_elements(mshift)-1 do begin
      z=where(sdate[i] eq date,count)
      if count ne 0 then smag[z] = mag[z] + mshift[i]
   endfor
end

pro perser

   infofile='perser.inf'
   resfile ='results.inf'
   roidcode=''
   roidname=''
   xut = 'UT time in hours'
   xph = 'Rotational phase'
   goodscan = 0
   syms=[1,2,4,5,6,7,8]

; A3752               Standard code of asteroid
; 3752 Camillo        Name for asteroid
; 0.466               V-R color
; 950829.001          list of data files to process
; 950903.001
; 950904.001
; 950920.001
; 950921.001

   if not exists(infofile) then begin
      ; Get current directory, this is used for the asteroid
      cd,'.',current=cdir
      roid=strmid(cdir,rstrpos(cdir,'/')+1,99)
      if strmid(roid,0,1) eq 'a' then roid=strmid(roid,1,99)
      roidcode='A'+strupcase(roid)
      ; Start up Larry's name and info fetch program to get Asteroid name
      spawn,'getinfo',unit=pipe2
      printf,pipe2,roidcode
      roidname=''
      readf,pipe2,roidname,h,g,format='(a32,1x,f6.2,1x,f5.2)'
      if strmid(roidname,0,5) eq 'XXXXX' then begin
         roidcode='C'+strupcase(roid)
         printf,pipe2,roidcode
         roidname=''
         readf,pipe2,roidname,h,g,format='(a32,1x,f6.2,1x,f5.2)'
         if strmid(roidname,0,5) eq 'XXXXX' then begin
            print,'Illegal object name, cannot proceed.'
            free_lun,pipe2
            return
         endif
      endif
      free_lun,pipe2
      roidname=strtrim(roidname,2)

      read,prompt='What is the V-R color of '+roidname+'? ',vmr

      spawn,'ls ??????.001',result
      if result[0] eq '' then begin
         print,'There does not appear to be any data to reduce here.'
         return
      endif

      refdate=picker(result,title='Reference?')
      refdate=strmid(refdate,0,6)

      openw,lun,infofile,/get_lun
      printf,lun,'PERSER v1.0'
      printf,lun,roidcode
      printf,lun,roidname
      printf,lun,vmr
      for i=0,n_elements(result)-1 do begin
         if strmid(result[i],0,6) eq refdate then $
            printf,lun,result[i],' ref' $
         else $
            printf,lun,result[i]
      endfor
      free_lun,lun
   endif

   ; Okay, load the info from the information file.
   openr,lun,infofile,/get_lun
   version=''
   readf,lun,version
   if version ne 'PERSER v1.0' then begin
      print,'ERROR!  bad version id [',version,'] in information file ',infofile
      return
   endif
   readf,lun,roidcode
   readf,lun,roidname
   readf,lun,vmr
   newfn=''
   j=0
   refdate=''
   while not eof(lun) do begin
      readf,lun,newfn,format='(a50)'
      newfn=strtrim(newfn)
      if strpos(newfn,'ref') ne -1 then begin
         refdate=strmid(newfn,0,6)
         newfn=strmid(newfn,0,10)
      endif
      if j eq 0 then $
         fnlist=newfn $
      else $
         fnlist=[fnlist,newfn]
      j=j+1
   endwhile
   numfiles=j
   free_lun,lun

   ; load the data from the files.
   udate=strmid(fnlist,0,6)
   for i=0,numfiles-1 do begin
      newdate=strmid(fnlist[i],0,6)
      if not exists(fnlist[i]) then begin
         print,'Data file: ',fnlist[i],' was not found.  Aborting.'
         return
      endif
      readcol,fnlist[i],in_bad,in_jd,in_filter,in_mag,in_err,format='i,d,a,f,f',/silent
      print,newdate,n_elements(in_jd)
      if i eq 0 then begin
         date= replicate(newdate,n_elements(in_jd))
         bad = in_bad
         jd  = in_jd
         filter = in_filter
         mag = in_mag
         err = in_err
      endif else begin
         date= [date,replicate(newdate,n_elements(in_jd))]
         bad = [bad,in_bad]
         jd  = [jd,in_jd]
         filter = [filter,in_filter]
         mag = [mag,in_mag]
         err = [err,in_err]
      endelse
   endfor
   if refdate eq '' then refdate=udate[0]

   ; Load the derived information (if any)
   ; Asteroid code
   ; Lightcurve reference epoch (time of zero phase)
   ; period error
   ; h g
   ; date mag_shift (one for each)
   if exists(resfile) then begin
      openr,lun,resfile,/get_lun
      codenam=''
      readf,lun,codenam
      readf,lun,jdref,format='(f13.5)'
      readf,lun,period,pererr
      readf,lun,hv,gv
      j=0
      while not eof(lun) do begin
         readf,lun,newfn,in_shift,format='(a6,1x,f)'
         if j eq 0 then begin
            sdate = newfn
            mshift= in_shift
         endif else begin
            sdate = [sdate,newfn]
            mshift= [mshift,in_shift]
         endelse
         j=j+1
      endwhile
      nshift = j
      free_lun,lun
   endif else begin
      sdate = strmid(fnlist,0,6)
      mshift = replicate(0.,n_elements(sdate))
      period = -1.0
      pererr = -1.0
      hv     = -99.0
      gv     = -99.0
      jdref = double(long(min(jd)-0.5)) + 0.5
      openw,lun,resfile,/get_lun
      printf,lun,roidcode
      printf,lun,jdref,format='(f13.5)'
      printf,lun,period,pererr
      printf,lun,hv,gv
      for i=0,numfiles-1 do $
         printf,lun,sdate[i],mshift[i]
      free_lun,lun
   endelse

   ; check list of files to make sure there isn't a missing shift
   for i=0,numfiles-1 do begin
      z=where(udate[i] eq sdate,count)
      if count eq 0 then begin
         sdate = [sdate,udate[i]]
         mshift = [mshift,0.]
         nshift= nshift+1
      endif
   endfor

   ; Correct any V data to R
   z=where(filter eq 'V',count)
   rmag = mag
   if count ne 0 then rmag[z] = mag[z] - vmr

   ; Generate geometric information
   ephem,jd,688,22+50,roidcode,eph,/cache
   ssgeom,eph,sun,earth,phang

   ; Correct the photometry to 1AU from Earth and Sun
   magu = rmag - 5.0*alog10(sun*earth)

   ; apply shifts to the data.
   perser_shift,date,magu,sdate,mshift,magus

   ; Correct the time of observation to asteroid-centric time
   const=1.49598d8/2.997925d5/86400.
   jdc = jd - earth*const
   time = (jdc - jdref) * 24.0

   ; Okay, now we enter into actually working with this data.
   choice=''
   repeat begin
      read,prompt='PERSER> ',choice
      choice=strlowcase(choice)
      op = strsplit(strcompress(strtrim(choice,2)),' ',/extract)

      case op[0] of

         ; Choose new trial period with cursor from last pdm scan
         'c': begin
               if goodscan then begin
                  print,'Press left or center to select new period in window 2.'
                  print,'Press the right mouse button to exit.'
                  done = 0
                  while not done do begin
                     setwin,2,xsize=600,ysize=450
                     plot,per,theta,xtitle='Period in hours',ytitle='Chi-sq',title=roidname
                     cursor,x,y,3,/data
                     done = !mouse.button eq 4
                     if not done then begin
                        period = x
                        pererr = -1.0
                        print,'New trial period at ',period
                        setwin,1,xsize=600,ysize=400
                        title=roidname+'   Trial period = '+string(period,format='(f8.5)')
                        plot,[0],[1],/nodata,xr=[0,1],yr=maxmin(magus[where(bad eq 0)]), $
                           xtit=xph, ytit='R magnitude',title=title
                        for i=0,numfiles-1 do begin
                           z=where(udate[i] eq date and bad eq 0,count)
                           if count ne 0 then begin
                              oplot,(time[z] mod period)/period,magus[z], $
                                 psym=syms[i mod 7],symsize=0.6
                           endif
                        endfor
                     endif
                  endwhile
               endif else begin
                  print,'You must first run a PDM scan with the "sp" command.'
               endelse
            end

         ; Edit data, to mark as bad or good.
         'e': begin
               editdate=picker(udate,title='Edit?')
               if editdate ne '[[[CANCEL]]]' then begin
                  z=where(editdate eq date,count)
                  l_bad=bad[z]
                  markdata,time[z],magus[z],l_bad, $
                    xtitle='Time in hours',ytitle='Magnitude',/connect,/yflip
                  if long(total(l_bad eq bad[z])) ne count then begin
                     bad[z] = l_bad
                     updatefile = fnlist[where(editdate eq udate)]
                     updatefile = updatefile[0]
                     print,'Update flags in file ',updatefile
                     wrphot,jd[z],filter[z],mag[z],err[z],updatefile,bad=bad[z]
                  endif
               endif
            end

         ; fit a parabola to the last pdm scan to determine best period
         'f': begin
               if goodscan then begin
                  ; Fit a parabola to the chi-sq vs. period curve
;                  coeff=poly_fit(per-per[0],theta,3,yfit=yfit)
                  coeff=poly_fit(per-period,theta,2,yfit=yfit)
                  newper = per[where(yfit eq min(yfit))]
                  z=where(yfit le min(yfit)+1.0)
                  dper = (newper-per[max(z)] + per[min(z)]-newper) / 2.0
                  period=newper[0]
                  pererr=dper[0]
                  print,'     New period is ',period,' +/- ',pererr
                  setwin,2,xsize=600,ysize=450
                  plot,per,theta,xtitle='Period in hours',ytitle='Chi-sq',title=roidname
                  oplot,per,yfit
               endif else begin
                  print,'You must first run a PDM scan with the "sp" command.'
               endelse
            end

         ; Set trial period
         'g': begin
               read,prompt='Trial period in hours? ',period
               pererr = -1.0
            end

         ; Hardcopy of all mags versus time.
         'h1': begin
               d_name = !d.name
               landscap
               z = where(bad eq 0)
               plot,time[z],magu[z],psym=8,xtit=xut,ytit='R magnitude', $
                  title=roidname,yr=maxmin(magu[z])
               device,/close
               spawn,'lp -o nobanner -c idl.ps'
               set_plot,d_name
            end

         ; Hardcopy of all shifted mags versus time.
         'h2': begin
               d_name = !d.name
               landscap
               z = where(bad eq 0)
               plot,time[z],magus[z],psym=8,xtit=xut,ytit='R magnitude', $
                  title=roidname,yr=maxmin(magus[z])
               device,/close
               spawn,'lp -o nobanner -c idl.ps'
               set_plot,d_name
            end

         ; Hardcopy of phased lightcurve
         'hl': begin
               if period gt 0. then begin

                  if pererr gt 0. then begin
                     title=roidname+'   Trial period = '+string(period,format='(f8.5)') + $
                               '+/='+string(pererr,format='(f7.5)')
                  endif else begin
                     title=roidname+'   Trial period = '+string(period,format='(f8.5)')
                  endelse

                  d_name = !d.name
                  landscap

                  plot,[0],[1],/nodata,xr=[0,1],yr=maxmin(magus[where(bad eq 0)]), $
                     xtit=xph, ytit='R magnitude',title=title
                  for i=0,numfiles-1 do begin
                     z=where(udate[i] eq date and bad eq 0,count)
                     if count ne 0 then begin
                        oplot,(time[z] mod period)/period,magus[z], $
                           psym=syms[i mod 7],symsize=0.6
                     endif
                  endfor

                  device,/close
                  spawn,'lp -o nobanner -c idl.ps'
                  set_plot,d_name

               endif else begin
                  print,'ERROR!  Period is not yet defined for this object.'
                  print,'Use the command "g" to set a guess for a period.'
               endelse
            end

         ; Hardcopy of PDM scan
         'hs': begin
               if goodscan then begin
                  d_name = !d.name
                  landscap
                  plot,per,theta,xtitle='Period in hours',ytitle='Chi-sq', $
                     title=roidname
                  device,/close
                  spawn,'lp -o nobanner -c idl.ps'
                  set_plot,d_name
               endif else begin
                  print,'PDM scan not defined as yet.'
               endelse
            end

         ; Compute Lomb-Scargle periodogram
         'l': begin
               read,prompt='Oversampling factor (>=2)? ',ofac
               z = where(bad eq 0)
               result=lnp_test(time[z],magus[z],wk1=wk1,wk2=wk2,jmax=jmax,ofac=ofac)
               setwin,3,xsize=600,ysize=450
               xr=[ max([min(1.0/wk1),1.0/wk1[jmax]-2.0]), $
                    min([max(1.0/wk1),1.0/wk1[jmax]+5.0])     ]
               plot,1.0/wk1,wk2,xr=[-2.0,5.0]+1.0/wk1[jmax],xtitle='Period in hours', $
                  ytitle='Power',title=roidname
               print,'Peak ',1.0/wk1[jmax],', power ',result[0],', significance ',result[1]
            end

         ; Plot all mags versus time.
         'p1': begin
               setwin,0
               z = where(bad eq 0)
               plot,time[z],magu[z],psym=8,xtit=xut,ytit='R magnitude', $
                  title=roidname,yr=maxmin(magu[z])
            end

         ; Plot shifted mags versus time.
         'p2': begin
               setwin,0
               z = where(bad eq 0)
               plot,time[z],magus[z],psym=8,xtit=xut,ytit='R magnitude', $
                  title=roidname,yr=maxmin(magus[z])
            end

         ; Plot all datasets versus time in separate windows
         'pt': begin
               for i=0,numfiles-1 do begin
                  setwin,i+10,xsize=400,ysize=300
                  z=where(udate[i] eq date and bad eq 0,count)
                  if count ne 0 then begin
                     ploterror,time[z],magus[z],err[z],xtit=xut, $
                        ytit='R magnitude',title=roidname+' '+udate[i], $
                        yr=maxmin(magus[z]),psym=8,symsize=0.6
                  endif else begin
                     erase
                     print,udate[i],' has no good data left'
                  endelse
               endfor
            end

         ; Plot all data versus phase.
         'pp': begin
               if period gt 0. then begin
                  setwin,1,xsize=600,ysize=400
                  if pererr gt 0. then begin
                     title=roidname+'   Trial period = '+string(period,format='(f8.5)') + $
                               '+/='+string(pererr,format='(f7.5)')
                  endif else begin
                     title=roidname+'   Trial period = '+string(period,format='(f8.5)')
                  endelse
                  plot,[0],[1],/nodata,xr=[0,1],yr=maxmin(magus[where(bad eq 0)]), $
                     xtit=xph, ytit='R magnitude',title=title
                  for i=0,numfiles-1 do begin
                     z=where(udate[i] eq date and bad eq 0,count)
                     if count ne 0 then begin
                        oplot,(time[z] mod period)/period,magus[z], $
                           psym=syms[i mod 7],symsize=0.6
                     endif
                  endfor
                  for i=0,numfiles-1 do begin
                     setwin,i+10,xsize=400,ysize=300
                     z=where(udate[i] eq date and bad eq 0,count)
                     if count ne 0 then begin
                        ploterror,(time[z] mod period)/period,magus[z],err[z],xtit=xph, $
                           ytit='R magnitude',title=roidname+' '+udate[i], $
                           xr=[0,1],yr=maxmin(magus[z]),psym=8,symsize=0.6
                     endif else begin
                        erase
                        print,udate[i],' has no good data left'
                     endelse
                  endfor
               endif else begin
                  print,'ERROR!  Period is not yet defined for this object.'
                  print,'Use the command "g" to set a guess for a period.'
               endelse
            end

         ; change reference night
         'r': begin
               refdate=picker(udate,title='Reference?')
            end

         ; set relative shifts from nightly means.
         'sm': begin
               z=where(refdate eq date and bad eq 0)
               refmean=mean(magu[z])
               for i=0,numfiles-1 do begin
                  z1=where(udate[i] eq date and bad eq 0,count)
                  if count ne 0 then begin
                     means = refmean - mean(magu[z1])
                     z=where(udate[i] eq sdate)
                     mshift[z]=means
                     if udate[i] eq refdate then tag='ref' else tag=''
                     print,i,udate[i],mean(phang[z1]),mean(magu[z1]),mshift[z[0]],tag, $
                        format='(i3,1x,a,1x,f5.1,1x,f8.3,2x,f7.3,1x,a)'
                  endif else begin
                     print,udate[i],' has no data left'
                  endelse
               endfor
               perser_shift,date,magu,sdate,mshift,magus
            end

         ; Scan period
         'sp':begin
               if period le 0. then $
                  read,prompt='Trial period? ',tperiod $
               else $
                  tperiod=period
               read,prompt='Width of the scan in hours? ',scanwidth
               repeat begin
                  read,prompt='Number of points in scan?   ',scanpnts
                  scanpnts=fix(scanpnts)
                  if scanpnts lt 5 then begin
                     print,'Error!  number of points in scan must be greater than 4'
                  endif
               endrep until scanpnts ge 5
               df = abs( 1.0/tperiod - 1.0/(tperiod-scanwidth) )
               z = where(bad eq 0)
               pdm2,time[z],magus[z],err[z],1.0/tperiod-df,1.0/tperiod+df, $
                    df/scanpnts*2,freq,per,theta,silent=!version.os eq 'Win32'
               setwin,2,xsize=600,ysize=450
               plot,per,theta,xtitle='Period in hours',ytitle='Chi-sq',title=roidname
               data=[[per],[theta]]
               mkhdr,hdr,data
               sxaddpar,hdr,'TPERIOD',tperiod,'Central period for scan.'
               sxaddpar,hdr,'SCANWID',scanwidth,'Width of scan in hours.'
               sxaddpar,hdr,'SCANPNTS',scanpnts,'Number of points in scan.'
               writefits,'lastscan.fits',data,hdr
               goodscan=1
            end

         ; set relative shifts from best fit given current period.
         'v': begin
               zrf = where(date eq refdate and bad eq 0)
               if period gt 0. then begin
                  goodscan=0
                  for i=0,numfiles-1 do begin
                     if udate[i] ne refdate then begin
                        zz = where(date eq udate[i] and bad eq 0,count)
                        if count ne 0 then begin
                           pass=0
                           stepsize=0.02
                           repeat begin
                              done=1
                              pdm2shif,time[zrf],magu[zrf],err[zrf], $
                                       time[zz], magu[zz], err[zz],  period, $
                                       mshift[i]-stepsize,mshift[i]+stepsize, $
                                       stepsize/40,shiftv,theta
                              best=where(theta eq min(theta),countbest)
                              if countbest ne 1 then begin
                                 print,'WARNING! ',udate[i], $
                                    ' shift has more than one "best", not changed.'
                              endif else begin
                                 best=best[0]
                                 print,udate[i],mshift[i],shiftv[best]
                                 if abs(mshift[i]-shiftv[best])/stepsize gt 0.99 then begin
                                    done=0
                                    stepsize = stepsize * 2.0
                                    print,'Expanding search, stepsize now ',stepsize
                                 endif else if pass ne 0 and stepsize gt 0.021 then begin
                                    done=0
                                    stepsize = stepsize / 2.0
                                    print,'Contracting search, stepsize now ',stepsize
                                 endif
                                 mshift[i]=shiftv[best]
                              endelse
                              pass = pass + 1
                           endrep until pass eq 10 or done
                           if pass eq 10 then print,'Interation limit reached, no convergence.'
                        endif else begin
                           print,udate[i],' has no data left'
                        endelse
                     endif
                  endfor
                  perser_shift,date,magu,sdate,mshift,magus
               endif else begin
                  print,'ERROR!  Period is not yet defined for this object.'
                  print,'Use the command "g" to set a guess for a period.'
               endelse
            end

         else:
         endcase


   endrep until op[0] eq 'q'

   ; Save results on exit.
   openw,lun,infofile,/get_lun
   printf,lun,'PERSER v1.0'
   printf,lun,roidcode
   printf,lun,roidname
   printf,lun,vmr
   for i=0,n_elements(fnlist)-1 do begin
      if strmid(fnlist[i],0,6) eq refdate then $
         printf,lun,fnlist[i],' ref' $
      else $
         printf,lun,fnlist[i]
   endfor
   free_lun,lun

   openw,lun,resfile,/get_lun
   printf,lun,roidcode
   printf,lun,jdref,format='(f13.5)'
   printf,lun,period,pererr
   printf,lun,hv,gv
   for i=0,numfiles-1 do $
      printf,lun,sdate[i],mshift[i]
   free_lun,lun

end
