;+
; NAME:
;  mimiretc
; PURPOSE:   (one line only)
;  Exposure time and throughput calculator for {\it Mimir}
; DESCRIPTION:
;  This program is a non-blocking widget tool for grism and filter
;    combinations for Mimir, an infrared imaging spectrograph at Lowell
;    Observatory built in cooperation with Boston University.  This program
;    attempts to model the light passing through in the instrument and
;    possible being dispersed.  Along the way, the efficicncy of the
;    instrument is tallied and you get to see what will be collected at
;    the detector.
;
;  When this program starts, it expects to find a collection of files in the
;    current directory.  These files define the instrument and the throughput
;    of its individual components.  The first file that is read is 'mimir.info'.
;    This file has the following format:
; 1 Mimir_Info_v1.0
; 2 437.5       effective focal length of collimator in mm
; 3 132.195     effective focal length of camera in mm
; 4 27.0        pixel size of detector in microns
; 5 unity.dat
; 6 unity.dat
; 7 blank.dat
; 8 grism_jhk.dat
; ...
; a :grism
;   sp.dat
; ...
; b :filter
;
;  The line numbers do not appear in the file.  Trailing comments are ok only
;    on lines with numeric values (not with file names).  The first line is
;    the version ID tag.  Line 2 is the effective focal length of the collimator
;    in millimeters and is used only to determine pixel scale on the sky.
;    Line 3 is the effective focal length of the camera lens in mm.  This is
;    used to determine the translation from angular deviation after the grism
;    to pixels.  Line 4 is the size of the pixels on the detector in microns.
;
;  The balance of the file is a list of file names in two sections.  The first
;    section is a list of grisms.  The list is terminated with the line
;    ":grism".  This grism list is made up of file names that point to grism
;    definition files.  The second section is a list of filters.  Again, the
;    filter list is a list of file names.
;
;  Grism files are in the following format:
;
; 1 Mimir_Grism_v1.0
; 2 120.0      Grooves/mm
; 3 29.986     Groove angle, degrees
; 4 1.52       Index of refraction for resin
; 5 29.3       Prism apex angle, degrees
; 6 1.42       Index of refraction for prism
;
;  The first line is the version id string.  The next five lines specify the
;    grism properties as indicated by the option comment strings.  DO NOT
;    include the line numbers in the file.
;
;  Filter files are two-column tables of numbers.  The first column is a
;    wavelength in microns and the second column is the fraction of light
;    transmitted at that wavelength (between 0 and 1).  This file does not
;    need to be tabulated on a regular grid.  Internally the program will
;    use linear interpolation to get intermediate values.  To ensure that
;    the file doesn't lead to non-physical values, make sure the first pair
;    and last pair of numbers have identical throughputs so that extrapolation
;    beyond the tabulated range will return a constant value.
;
;  A complete set of files (including some test entries) are stored in
;    ftp://ftp.lowell.edu/pub/buie/mimir/etc.  Get all the .dat and .info
;    files.
;
;  The File menu option of "postscript" causes a color postscript file to be
;    generated with all three plots (wavelength, angle, and pixel) to the file
;    idl.ps.
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  mimiretc
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  mimir.info is read only upon startup.  If you edit this file while the
;  program is running you will need to exit and restart to pick up the changes.
;  All other files are read whenever a plot is generated meaning you can edit
;  a grism or filter file, save it, replot and see the new values.
; PROCEDURE:
; MODIFICATION HISTORY:
;  written by Marc W. Buie, Lowell Observatory, 2002/11/26
;  2002/12/02, MWB, fixed set_plot bug on Windows platforms
;-
pro mimiretc_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   ; Free up any dynamically allocated space with the state structure
   ptr_free,(*state).filterlist
   ptr_free,(*state).grismlist

   ; Free up the state structure itself.
   ptr_free, state

end

pro mimiretc_loadgrism, state, newfile

   (*state).groovefreq = -1.0

   if newfile eq 'none' then return

   if not exists(newfile) then begin
      print,(*(*state).grismlist)[grism],' file not found.'
      return
   endif

   openr,lun,newfile,/get_lun
   line=''
   fmt='(a)'
   readf,lun,line,format=fmt
   if line ne 'Mimir_Grism_v1.0' then begin
      print,newfile,' is not the right version/format of file.'
      print,'[',line,']'
      free_lun,lun
      return
   endif
   readf,lun,grooves
   readf,lun,grang
   readf,lun,grn
   readf,lun,apex
   readf,lun,prn
   free_lun,lun

   (*state).apexangle   = apex/!radeg
   (*state).grooveangle = grang/!radeg
   (*state).groovefreq  = grooves
   (*state).prismindex  = prn
   (*state).resinindex  = grn
   (*state).inangle     = asin(prn/grn*sin(apex/!radeg))
;   (*state).inangle     = apex/!radeg

end

pro mimiretc_loadinfo, state

   if not exists((*state).infofile) then begin
      text = [ $
         'The instrument definition file '+(*state).infofile+' does not', $
         'exist.  No file can be loaded at this time.  You may or may not', $
         'be able to do anything useful from this point on.']
      result=dialog_message(text,/error)
   endif else begin
      openr,lun,(*state).infofile,/get_lun
      line=''
      fmt = '(a)'
      readf,lun,line,format=fmt
      if line eq 'Mimir_Info_v1.0' then begin
         readf,lun,collefl
         (*state).collefl = collefl
         readf,lun,camefl
         (*state).camefl = camefl
         readf,lun,pixelscale
         (*state).pixelscale = pixelscale
         readf,lun,line,format=fmt
         (*state).telefile = line
         readf,lun,line,format=fmt
         (*state).opticfile = line
         readf,lun,line,format=fmt
         (*state).detfile = line
         grismlist = 'none'
         repeat begin
            if not eof(lun) then begin
               readf,lun,line,format=fmt
               if line ne ':grism' then begin
                  grismlist = [grismlist,line]
               endif
            endif
         endrep until line eq ':grism' or eof(lun)
         ptr_free,(*state).grismlist
         (*state).grismlist = ptr_new(grismlist)
         filterlist = 'none'
         repeat begin
            if not eof(lun) then begin
               readf,lun,line,format=fmt
               if line ne ':filter' then begin
                  filterlist = [filterlist,line]
               endif
            endif
         endrep until line eq ':filter' or eof(lun)
         ptr_free,(*state).filterlist
         (*state).filterlist = ptr_new(filterlist)

         widget_control,(*state).filterselid,set_value=(*(*state).filterlist)
         widget_control,(*state).grismselid,set_value=(*(*state).grismlist)

      endif else begin
         text= [ $
            'The file '+(*state).infofile+' does not appear to be in the proper', $
            'format.  The version information seen is ['+line+']']
         result=dialog_message(text,/error)
      endelse
      free_lun,lun
   endelse

end

pro mimiretc_refreshplot, state

   screen = !d.name ne 'PS'

   if screen then begin
      c1 = '00007f'xl
      c2 = '404040'xl
      c3 = '707070'xl
      c4 = '303070'xl
      c5 = '7070f0'xl
      c6 = '307030'xl
      c7 = '70f070'xl
      c8 = '703030'xl
      c9 = 'f07070'xl
      c10= '0000ff'xl
      pos=[0.07,0.09,0.95,0.95]
      thick=1.0
   endif else begin
      r=[0,127, 64,112,112,240, 48,112,112,112,255,255]
      g=[0,  0, 64,112, 48,112,112,240, 48,112,  0,255]
      b=[0,  0, 64,112, 48,112, 48,112, 48,240,  0,255]
      c1 = 1
      c2 = 2
      c3 = 3
      c4 = 4
      c5 = 5
      c6 = 6
      c7 = 7
      c8 = 8
      c9 = 9
      c10=10
      pos=[0.06,0.06,0.78,0.95]
      tvlct,r,g,b
      thick=5.0
   endelse

   if screen then widget_control, (*state).drawwin, get_value=winnum
   grism = widget_info( (*state).grismselid, /droplist_select )
   filters = widget_info( (*state).filterselid, /list_select )

   ; Load the data for the plot
   if not exists((*state).telefile) then begin
      print,'cannot find telescope file:',(*state).telefile
      return
   endif
   readcol,(*state).telefile,rawwave,rawtrans,format='f,f'
   interp,rawwave,rawtrans,(*state).wave,tel_trans

   if not exists((*state).opticfile) then begin
      print,'cannot find optics file:',(*state).opticfile
      return
   endif
   readcol,(*state).opticfile,rawwave,rawtrans,format='f,f'
   interp,rawwave,rawtrans,(*state).wave,opt_trans

   if not exists((*state).detfile) then begin
      print,'cannot find detector file:',(*state).detfile
      return
   endif
   readcol,(*state).detfile,rawwave,rawtrans,format='f,f'
   interp,rawwave,rawtrans,(*state).wave,det_trans

   fil_trans = replicate(1.0,(*state).npts)
   for i=0,n_elements(filters)-1 do begin
      if filters[i] eq -1 then begin
         ; NOP
      endif else if (*(*state).filterlist)[filters[i]] eq 'none' then begin
         ; NOP
      endif else begin
         if not exists((*(*state).filterlist)[filters[i]]) then begin
            print,'cannot find filter file:',(*(*state).filterlist)[filters[i]]
         endif else begin
            readcol,(*(*state).filterlist)[filters[i]], $
               rawwave,rawtrans,format='f,f'
            interp,rawwave,rawtrans,(*state).wave,trans
            fil_trans = fil_trans*trans
         endelse
      endelse
   endfor

   (*state).throughput = tel_trans * opt_trans * det_trans * fil_trans
   mimiretc_loadgrism, state, (*(*state).grismlist)[grism]
   if (*state).groovefreq gt 0.0 then begin
      (*state).w0 = (*state).d0 * ((*state).resinindex-1.0)/0.58 / $
           (*state).groovefreq*1000.0 * sin((*state).grooveangle)
      interp,(*state).w0,(*state).e0,(*state).wave,eff0
      (*state).eff0 = eff0
      (*state).w1 = (*state).d1 * ((*state).resinindex-1.0)/0.58 / $
           (*state).groovefreq*1000.0 * sin((*state).grooveangle)
      interp,(*state).w1,(*state).e1,(*state).wave,eff1
      (*state).eff1 = eff1
      (*state).w2 = (*state).d2 * ((*state).resinindex-1.0)/0.58 / $
           (*state).groovefreq*1000.0 * sin((*state).grooveangle)
      interp,(*state).w2,(*state).e2,(*state).wave,eff2
      (*state).eff2 = eff2
      (*state).w3 = (*state).d3 * ((*state).resinindex-1.0)/0.58 / $
           (*state).groovefreq*1000.0 * sin((*state).grooveangle)
      interp,(*state).w3,(*state).e3,(*state).wave,eff3
      (*state).eff3 = eff3

      sd0 = (*state).resinindex*sin((*state).inangle)
      sd1 = (*state).resinindex*sin((*state).inangle) - $
            (*state).w1*(*state).groovefreq/1000.0
      sd2 = (*state).resinindex*sin((*state).inangle) - $
            2.0*(*state).w2*(*state).groovefreq/1000.0
      sd3 = (*state).resinindex*sin((*state).inangle) - $
            3.0*(*state).w3*(*state).groovefreq/1000.0
      (*state).sd0 = sd0
      interp,(*state).w1,sd1,(*state).wave,sd1w
      (*state).sd1 = sd1w
      interp,(*state).w2,sd2,(*state).wave,sd2w
      (*state).sd2 = sd2w
      interp,(*state).w3,sd3,(*state).wave,sd3w
      (*state).sd3 = sd3w
   endif

   if screen then wset,winnum

   if (*state).plottype eq 0 or (*(*state).grismlist)[grism] eq 'none' then begin
      plot,[0],[1],/nodata,xr=[(*state).wave1,(*state).wave2],position=pos, $
         yr=[0.0,1.0],xtitle='Wavelength (microns)',ytitle='Throughput'

      if (*state).groovefreq gt 0.0 then begin

         oplot,(*state).wave,(*state).throughput,color=c1

         wrange = minmax((*state).w0)
         z=where((*state).wave ge wrange[0] and (*state).wave le wrange[1],count)
         if count gt 0 then begin
            oplot,(*state).wave[z],(*state).eff0[z],color=c2
            oplot,(*state).wave[z],(*state).eff0[z]*(*state).throughput[z], $
                     color=c3,thick=thick
            z=where((*state).eff0 eq max((*state).eff0))
            xyouts,(*state).wave[z[0]],(*state).eff0[z[0]]+0.02,'m=0',align=0.5
         endif

         wrange = minmax((*state).w1)
         z=where((*state).wave ge wrange[0] and (*state).wave le wrange[1],count)
         if count gt 0 then begin
            oplot,(*state).wave[z],(*state).eff1[z],color=c4
            oplot,(*state).wave[z],(*state).eff1[z]*(*state).throughput[z], $
                  color=c5,thick=thick
            z=where((*state).eff1 eq max((*state).eff1))
            xyouts,(*state).wave[z[0]],(*state).eff1[z[0]]+0.02,'m=1',align=0.5
         endif

         wrange = minmax((*state).w2)
         z=where((*state).wave ge wrange[0] and (*state).wave le wrange[1],count)
         if count gt 0 then begin
            oplot,(*state).wave[z],(*state).eff2[z],color=c6
            oplot,(*state).wave[z],(*state).eff2[z]*(*state).throughput[z], $
                  color=c7,thick=thick
            z=where((*state).eff2 eq max((*state).eff2))
            xyouts,(*state).wave[z[0]],(*state).eff2[z[0]]+0.02,'m=2',align=0.5
         endif

         wrange = minmax((*state).w3)
         z=where((*state).wave ge wrange[0] and (*state).wave le wrange[1],count)
         if count gt 0 then begin
            oplot,(*state).wave[z],(*state).eff3[z],color=c8
            oplot,(*state).wave[z],(*state).eff3[z]*(*state).throughput[z], $
                  color=c9,thick=thick
            z=where((*state).eff3 eq max((*state).eff3))
            xyouts,(*state).wave[z[0]],(*state).eff3[z[0]]+0.02,'m=3',align=0.5
         endif

      endif else begin

         oplot,(*state).wave,(*state).throughput,color=c10,thick=thick

      endelse
      
   endif else begin

      if (*state).groovefreq gt 0.0 then begin

         if (*state).plottype eq 1 then begin
            plot,[0],[1],/nodata,xr=[-60,60],position=pos, $
               yr=[0.0,1.0],xtitle='Deflection angle (degrees)', $
               ytitle='Throughput'
         endif else begin
            plot,[0],[1],/nodata,xr=[0,1024],position=pos, $
               yr=[0.0,1.0],xtitle='Pixel position', $
               ytitle='Throughput'
            oplot,(512+400)*[1,1],[0,1],color=c10
            oplot,(512-400)*[1,1],[0,1],color=c10
         endelse

         if (*state).sd0 ge -1.0 and (*state).sd0 le 1.0 then begin
            d0 = asin((*state).sd0)*!radeg-(*state).apexangle*!radeg
            if (*state).plottype eq 1 then begin
               oplot,[d0,d0],[0.,1.]
               xyouts,d0,0.,'Zeroth Order',orient=90.,/data
            endif else begin
               x0 = (*state).camefl*1000.0*tan(d0/!radeg)/(*state).pixelscale+512
               oplot,[x0,x0],[0.,1.]
               xyouts,x0,0.,'Zeroth Order',orient=90.,/data
            endelse
         endif

         wrange = minmax((*state).w1)
         z=where((*state).sd1 ge -1.0 and (*state).sd1 le 1.0 and $
                 (*state).wave ge wrange[0] and (*state).wave le wrange[1], count)
         if count gt 1 then begin
            d1 = asin((*state).sd1[z])*!radeg-(*state).apexangle*!radeg
            if (*state).plottype eq 1 then begin
               oplot,d1,(*state).eff1[z],color=c4
               oplot,d1,(*state).eff1[z]*(*state).throughput[z], $
                        color=c5,thick=thick
            endif else begin
               x1 = (*state).camefl*1000.0*tan(d1/!radeg)/(*state).pixelscale+512
               oplot,x1,(*state).eff1[z],color=c4
               oplot,x1,(*state).eff1[z]*(*state).throughput[z], $
                        color=c5,thick=thick
            endelse
         endif

         wrange = minmax((*state).w2)
         z=where((*state).sd2 ge -1.0 and (*state).sd2 le 1.0 and $
                 (*state).wave ge wrange[0] and $
                 (*state).wave le wrange[1], count)
         if count gt 1 then begin
            d2 = asin((*state).sd2[z])*!radeg-(*state).apexangle*!radeg
            if (*state).plottype eq 1 then begin
               oplot,d2,(*state).eff2[z],color=c6
               oplot,d2,(*state).eff2[z]*(*state).throughput[z], $
                        color=c7,thick=thick
            endif else begin
               x2 = (*state).camefl*1000.0*tan(d2/!radeg) / $
                    (*state).pixelscale + 512
               oplot,x2,(*state).eff2[z],color=c6
               oplot,x2,(*state).eff2[z]*(*state).throughput[z], $
                        color=c7,thick=thick
            endelse
         endif

         wrange = minmax((*state).w3)
         z=where((*state).sd3 ge -1.0 and (*state).sd3 le 1.0 and $
                 (*state).wave ge wrange[0] and $
                 (*state).wave le wrange[1], count)
         if count gt 1 then begin
            d3 = asin((*state).sd3[z])*!radeg-(*state).apexangle*!radeg
            if (*state).plottype eq 1 then begin
               oplot,d3,(*state).eff3[z],color=c8
               oplot,d3,(*state).eff3[z]*(*state).throughput[z], $
                        color=c9,thick=thick
            endif else begin
               x3 = (*state).camefl*1000.0*tan(d3/!radeg) / $
                    (*state).pixelscale + 512
               oplot,x3,(*state).eff3[z],color=c8
               oplot,x3,(*state).eff3[z]*(*state).throughput[z], $
                        color=c9,thick=thick
            endelse
         endif

      endif else begin
         erase
         xyouts,0.5,0.5,'Grism information not found.',/normal,align=0.5
      endelse
      
   endelse

   if not screen then begin
      left =  0.80
      top  =  0.94
      dy   = -0.024
      cs   = 0.8
      xyouts,left,top,'Mimir calculator output',/normal,charsize=cs
      xyouts,left,top+dy*2,/normal,charsize=cs, $
         string('Collimator EFL ',(*state).collefl,' mm', $
                format='(a,f5.1,a)')
      xyouts,left,top+dy*3,/normal,charsize=cs, $
         string('Camera   EFL ',(*state).camefl,' mm', $
                format='(a,f5.1,a)')
      xyouts,left,top+dy*4,/normal,charsize=cs, $
         string('Pixel scale      ',(*state).pixelscale,' microns', $
                format='(a,f4.1,a)')
      xyouts,left,top+dy*5,/normal,charsize=cs, $
         string('Telescope file   ',(*state).telefile, $
                format='(a,a)')
      xyouts,left,top+dy*6,/normal,charsize=cs, $
         string('Optics file      ',(*state).opticfile, $
                format='(a,a)')
      xyouts,left,top+dy*7,/normal,charsize=cs, $
         string('Detector file    ',(*state).detfile, $
                format='(a,a)')

      gtop = 9
      xyouts,left,top+dy*gtop,/normal,charsize=cs, $
         string('Grism file       ',(*(*state).grismlist)[grism], $
                format='(a,a)')
      gdel = 2
      if (*state).groovefreq gt 0.0 then begin
         xyouts,left,top+dy*(gtop+1),/normal,charsize=cs, $
            string('Spacing ',(*state).groovefreq,' grooves/mm', $
                   format='(4x,a,f5.1,a)')
         xyouts,left,top+dy*(gtop+2),/normal,charsize=cs, $
            string('Groove angle ',(*state).grooveangle*!radeg,' degrees', $
                   format='(4x,a,f5.1,a)')
         xyouts,left,top+dy*(gtop+3),/normal,charsize=cs, $
            string('Apex angle ',(*state).apexangle*!radeg,' degrees', $
                   format='(4x,a,f5.1,a)')
         xyouts,left,top+dy*(gtop+4),/normal,charsize=cs, $
            string('Index of resin ',(*state).resinindex, $
                   format='(4x,a,f5.3)')
         xyouts,left,top+dy*(gtop+5),/normal,charsize=cs, $
            string('Index of prism ',(*state).prismindex, $
                   format='(4x,a,f5.3)')
          gdel = 7
      endif

      ftop = gtop+gdel
      for i=0,n_elements(filters)-1 do begin
         if filters[i] eq -1 then begin
            xyouts,left,top+dy*(ftop+i),/normal,charsize=cs, $
               string('Filter file      ','none', $
                      format='(a,a)')
         endif else begin
            xyouts,left,top+dy*(ftop+i),/normal,charsize=cs, $
               string('Filter file      ',(*(*state).filterlist)[filters[i]], $
                      format='(a,a)')
         endelse
      endfor

      xyouts,1.00,0.0,/normal,align=1.0,charsize=cs*0.9,systime()
   endif

end

pro mimiretc_updateinfo, state, val
   ; val depends on current plot type

   ; wavelength plot, val=wavelength in microns, this may or may not map onto
   ;  each order
   if (*state).plottype eq 0 or (*state).groovefreq le 0.0 then begin
      ; take input wavelength, find nearest match in master wavelength vector
      wav = val[0]
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) gt (*state).wavestep then begin
         wav1 = wav
         wav2 = -1.0
         wav3 = -1.0
         z=-1
      endif else begin
         wav1 = (*state).wave[z]
         if (*state).groovefreq gt 0.0 then begin
            wav2 = (*state).wave[z]
            wav3 = (*state).wave[z]
         endif else begin
            wav2 = -1.0
            wav3 = -1.0
         endelse
      endelse

      ang1 = 999.0
      pos1  = 0
      eff1  = -1.0
      ang2 = 999.0
      pos2  = 0
      eff2  = -1.0
      ang3 = 999.0
      pos3  = 0
      eff3  = -1.0
      if (*state).groovefreq gt 0.0 then begin
         ; order 1
         if wav1 ge min((*state).w1) and wav1 le max((*state).w1) and $
               z ge 0 then begin
            sang1 = (*state).sd1[z]
            if sang1 ge -1.0 and sang1 le 1.0 then begin
               ang1 = asin(sang1)*!radeg-(*state).apexangle*!radeg
               pos1 = (*state).camefl*1000.0*tan(ang1/!radeg) / $
                             (*state).pixelscale + 512
               if pos1 gt 1024 then pos1=0
               eff1 = (*state).eff1[z]*(*state).throughput[z]
            endif
         endif else wav1=-1.0
         ; order 2
         if wav2 ge min((*state).w2) and wav2 le max((*state).w2) and $
               z ge 0 then begin
            sang2 = (*state).sd2[z]
            if sang2 ge -1.0 and sang2 le 1.0 then begin
               ang2 = asin(sang2)*!radeg-(*state).apexangle*!radeg
               pos2 = (*state).camefl*1000.0*tan(ang2/!radeg) / $
                             (*state).pixelscale + 512
               if pos2 gt 1024 then pos2=0
               eff2 = (*state).eff2[z]*(*state).throughput[z]
            endif
         endif else wav2=-1.0
         ; order 3
         if wav3 ge min((*state).w3) and wav3 le max((*state).w3) and $
               z ge 0 then begin
            sang3 = (*state).sd3[z]
            if sang3 ge -1.0 and sang3 le 1.0 then begin
               ang3 = asin(sang3)*!radeg-(*state).apexangle*!radeg
               pos3 = (*state).camefl*1000.0*tan(ang3/!radeg) / $
                             (*state).pixelscale + 512
               if pos3 gt 1024 then pos2=0
               eff3 = (*state).eff3[z]*(*state).throughput[z]
            endif
         endif else wav3=-1.0
      endif else begin
         if z ge 0 then eff1 = (*state).throughput[z]
      endelse

   ; dispersion angle plot, val=dispersion angle
   endif else if (*state).plottype eq 1 then begin

      ; Order 1
      ; value is the dispersion angle
      ang1 = val[0]
      ; compute position
      pos1 = (*state).camefl*1000.0*tan(ang1/!radeg) / (*state).pixelscale + 512
      if pos1 gt 1024 then pos1=0
      ; compute wavelength
      wav = ((*state).resinindex*sin((*state).inangle) - $
              sin(ang1/!radeg+(*state).apexangle)) * $
                     1000.0/(*state).groovefreq
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) le (*state).wavestep and $
         wav ge min((*state).w1) and wav le max((*state).w1) then begin
         wav1 = (*state).wave[z]
      endif else begin
         wav1 = 0.0
         z=-1
      endelse
      if z ge 0 then eff1 = (*state).eff1[z]*(*state).throughput[z] else eff1=-1.0

      ; Order 2
      ; value is the dispersion angle
      ang2 = val[0]
      ; compute position
      pos2 = (*state).camefl*1000.0*tan(ang2/!radeg) / (*state).pixelscale + 512
      if pos2 gt 1024 then pos2=0
      ; compute wavelength
      wav = ((*state).resinindex*sin((*state).inangle) - $
              sin(ang1/!radeg+(*state).apexangle)) * $
                     1000.0/(*state).groovefreq/2.0
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) le (*state).wavestep and $
         wav ge min((*state).w2) and wav le max((*state).w2) then begin
         wav2 = (*state).wave[z]
      endif else begin
         wav2 = 0.0
         z=-1
      endelse
      if z ge 0 then eff2 = (*state).eff2[z]*(*state).throughput[z] else eff2=-1.0

      ; Order 3
      ; value is the dispersion angle
      ang3 = val[0]
      ; compute position
      pos3 = (*state).camefl*1000.0*tan(ang3/!radeg) / (*state).pixelscale + 512
      if pos3 gt 1024 then pos3=0
      ; compute wavelength
      wav = ((*state).resinindex*sin((*state).inangle) - $
              sin(ang3/!radeg+(*state).apexangle)) * $
                     1000.0/(*state).groovefreq/3.0
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) le (*state).wavestep and $
         wav ge min((*state).w3) and wav le max((*state).w3) then begin
         wav3 = (*state).wave[z]
      endif else begin
         wav3 = 0.0
         z=-1
      endelse
      if z ge 0 then eff3 = (*state).eff3[z]*(*state).throughput[z] else eff3=-1.0

   ; pixel plot, val=column number on array
   endif else begin

      ; Order 1
      ; value is the position on the detector
      pos1 = val[0]
      ; compute angle
      ang1=atan((pos1-512)*(*state).pixelscale/1000.0/(*state).camefl)*!radeg
      ; compute wavelength
      wav = ((*state).resinindex*sin((*state).inangle) - $
              sin(ang1/!radeg+(*state).apexangle)) * $
                     1000.0/(*state).groovefreq
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) le (*state).wavestep and $
         wav ge min((*state).w1) and wav le max((*state).w1) then begin
         wav1 = (*state).wave[z]
      endif else begin
         wav1 = 0.0
         z=-1
      endelse
      if z ge 0 then eff1 = (*state).eff1[z]*(*state).throughput[z] else eff1=-1.0

      ; Order 2
      ; value is the position on the detector
      pos2 = val[0]
      ; compute angle
      ang2=atan((pos2-512)*(*state).pixelscale/1000.0/(*state).camefl)*!radeg
      ; compute wavelength
      wav = ((*state).resinindex*sin((*state).inangle) - $
              sin(ang2/!radeg+(*state).apexangle)) * $
                     1000.0/(*state).groovefreq/2.0
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) le (*state).wavestep and $
         wav ge min((*state).w2) and wav le max((*state).w2) then begin
         wav2 = (*state).wave[z]
      endif else begin
         wav2 = 0.0
         z=-1
      endelse
      if z ge 0 then eff2 = (*state).eff2[z]*(*state).throughput[z] else eff2=-1.0

      ; Order 3
      ; value is the position on the detector
      pos3 = val[0]
      ; compute angle
      ang3=atan((pos3-512)*(*state).pixelscale/1000.0/(*state).camefl)*!radeg
      ; compute wavelength
      wav = ((*state).resinindex*sin((*state).inangle) - $
              sin(ang3/!radeg+(*state).apexangle)) * $
                     1000.0/(*state).groovefreq/3.0
      z=where(abs(wav-(*state).wave) eq min(abs(wav-(*state).wave)))
      z=z[0]
      if abs((*state).wave[z]-wav) le (*state).wavestep and $
         wav ge min((*state).w3) and wav le max((*state).w3) then begin
         wav3 = (*state).wave[z]
      endif else begin
         wav3 = 0.0
         z=-1
      endelse
      if z ge 0 then eff3 = (*state).eff3[z]*(*state).throughput[z] else eff3=-1.0

   endelse

   ; all values defined now, update widgets with given information
   if ang1 lt 90.0 then str1=strn(ang1,format='(f6.1)') else str1=''
   if pos1 gt 0    then str2=strn(fix(pos1+0.5))        else str2=''
   if wav1 gt 0.0  then str3=strn(wav1,format='(f4.2)') else str3=''
   if eff1 ge 0.0  then str4=strn(eff1*100.0,format='(f5.1)')+'%' else str4=''
   widget_control,(*state).xang1id,set_value=str1
   widget_control,(*state).xpos1id,set_value=str2
   widget_control,(*state).xwav1id,set_value=str3
   widget_control,(*state).yvalue1id,set_value=str4

   if ang2 lt 90.0 then str1=strn(ang2,format='(f6.1)') else str1=''
   if pos2 gt 0    then str2=strn(fix(pos2+0.5))        else str2=''
   if wav2 gt 0.0  then str3=strn(wav2,format='(f4.2)') else str3=''
   if eff2 ge 0.0  then str4=strn(eff2*100.0,format='(f5.1)')+'%' else str4=''
   widget_control,(*state).xang2id,set_value=str1
   widget_control,(*state).xpos2id,set_value=str2
   widget_control,(*state).xwav2id,set_value=str3
   widget_control,(*state).yvalue2id,set_value=str4

   if ang3 lt 90.0 then str1=strn(ang3,format='(f6.1)') else str1=''
   if pos3 gt 0    then str2=strn(fix(pos3+0.5))        else str2=''
   if wav3 gt 0.0  then str3=strn(wav3,format='(f4.2)') else str3=''
   if eff3 ge 0.0  then str4=strn(eff3*100.0,format='(f5.1)')+'%' else str4=''
   widget_control,(*state).xang3id,set_value=str1
   widget_control,(*state).xpos3id,set_value=str2
   widget_control,(*state).xwav3id,set_value=str3
   widget_control,(*state).yvalue3id,set_value=str4

end

pro mimiretc_eve, event

   widget_control, event.top, GET_UVALUE=state

   if event.id eq (*state).mainbase then $
      event_name = 'Mainbase' $
   else $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   exit = event_name eq 'THE_MENU'
   if exit then exit = event.value eq 'Exit'

   case event_name of

      'THE_MENU': begin
         case event.value of

            'Hardcopy': begin
               d_name=!d.name
               set_plot,'PS'
               setpage,/landscape,xsize=25.0,ysize=19.0
               device,/color
               saveplottype = (*state).plottype
               (*state).plottype=0
               mimiretc_refreshplot,state
               (*state).plottype=1
               mimiretc_refreshplot,state
               (*state).plottype=2
               mimiretc_refreshplot,state
               (*state).plottype = saveplottype
               hardcopy
               set_plot,d_name
            end

            'Postscript': begin
               d_name=!d.name
               set_plot,'PS'
               setpage,/landscape,xsize=25.0,ysize=19.0
               device,/color
               saveplottype = (*state).plottype
               (*state).plottype=0
               mimiretc_refreshplot,state
               (*state).plottype=1
               mimiretc_refreshplot,state
               (*state).plottype=2
               mimiretc_refreshplot,state
               (*state).plottype = saveplottype
               device,/close
               set_plot,d_name
            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               RETURN
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      'Mainbase': begin

         ; Use if you have other widgets on screen, need to take off their
         ;   size from the event x,y size.
         info=widget_info((*state).rightinfoid,/geometry)
         widget_control,(*state).drawwin,xsize=event.x-info.xsize,ysize=event.y

         ; Use if draw window is only thing in the tool.
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y
;         widget_control,(*state).drawwin,xsize=event.x,ysize=event.y-info.ysize

;        refresh window here
         mimiretc_refreshplot, state
      end

      'Filter Selection': begin
         mimiretc_refreshplot, state
      end

      'Grism Selection': begin
         mimiretc_refreshplot, state
      end

      'Set Plot Type': begin
         if event.value eq 'angle' then begin
            (*state).plottype = 1
         endif else if event.value eq 'pixels' then begin
            (*state).plottype = 2
         endif else begin
            (*state).plottype = 0
         endelse
         mimiretc_refreshplot, state
      end

      'Window': begin
         
         case event.type of

            2: begin
               val=convert_coord([event.x],[event.y],/device,/to_data)
               mimiretc_updateinfo, state, val[0]
            end

            else: begin
               print,'EVENT NAME: ',event_name
               message, 'Unknown event:', /INFO
               help, event, /STRUCTURE
            end

         endcase
      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro mimiretc

   ; optional
   if xregistered('mimiretc') then return

   if (!d.flags and 256) eq 0 then begin
      print, 'Error. No windowing device. MIMIRETC cannot be started.'
      return
   endif

   ; Load the master efficiency curves.  If any not found just quit.
   if not exists('order0.dat') then begin
      print,'order0.dat not found, cannot continue'
      return
   endif
   readcol,'order0.dat',d0,e0
   w0=fltarr(n_elements(d0))

   if not exists('order1.dat') then begin
      print,'order1.dat not found, cannot continue'
      return
   endif
   readcol,'order1.dat',d1,e1
   w1=fltarr(n_elements(d1))

   if not exists('order2.dat') then begin
      print,'order2.dat not found, cannot continue'
      return
   endif
   readcol,'order2.dat',d2,e2
   w2=fltarr(n_elements(d2))

   if not exists('order3.dat') then begin
      print,'order3.dat not found, cannot continue'
      return
   endif
   readcol,'order3.dat',d3,e3
   w3=fltarr(n_elements(d3))

   ;Define the main base.
   mainbase = widget_base( TITLE='MIMIR Exposure Time Calculator', $
                           /COLUMN, UVALUE=0, MBAR=bar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(bar, /RETURN_NAME, $
                    ['1\File',$
                     '0\Hardcopy',$
                     '0\Postscript',$
                     '2\Exit',$
                     '1\Tools',$
                     '0\tool 1', $
                     '0\tool 2', $
                     '2\tool 3'], UVALUE='THE_MENU', /MBAR)

   base = widget_base(mainbase, /row)

   win1 = widget_draw( base, XSIZE=1000, YSIZE=700, RETAIN=2, $
                       /BUTTON_EVENTS, UVALUE='Window', /MOTION_events )

   rightinfo   = widget_base(base,/col)

   plottype = cw_bgroup(rightinfo,['wavelength','angle','pixels'],/no_release, $
                         /return_name,/exclusive,/col,uvalue='Set Plot Type')


   grismsel = widget_droplist(rightinfo,/dynamic_resize, $
                              uvalue='Grism Selection',value=['none'])

   filtersel = widget_list(rightinfo,/multiple,uvalue='Filter Selection', $
                           value=['none'],ysize=7)

   b3   = widget_base(rightinfo,/frame,/col)

   ; start Order 1 box
   b4   = widget_base(b3,/frame,/col)
   t1 = widget_label(b4,/align_center,value='Order 1')

   b5   = widget_base(b4,/row)
   xang1 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='degrees',/dynamic_resize)

   b5   = widget_base(b4,/row)
   xpos1 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='column',/dynamic_resize)

   b5   = widget_base(b4,/row)
   xwav1 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='microns',/dynamic_resize)

   b5   = widget_base(b4,/row)
   yvalue1 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='Throughput',/dynamic_resize)
   ; end Order 1 box

   ; start Order 2 box
   b4   = widget_base(b3,/frame,/col)
   t1 = widget_label(b4,/align_center,value='Order 2')

   b5   = widget_base(b4,/row)
   xang2 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='degrees',/dynamic_resize)

   b5   = widget_base(b4,/row)
   xpos2 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='column',/dynamic_resize)

   b5   = widget_base(b4,/row)
   xwav2 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='microns',/dynamic_resize)

   b5   = widget_base(b4,/row)
   yvalue2 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='Throughput',/dynamic_resize)
   ; end Order 2 box
   
   ; start Order 3 box
   b4   = widget_base(b3,/frame,/col)
   t1 = widget_label(b4,/align_center,value='Order 3')

   b5   = widget_base(b4,/row)
   xang3 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='degrees',/dynamic_resize)

   b5   = widget_base(b4,/row)
   xpos3 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='column',/dynamic_resize)

   b5   = widget_base(b4,/row)
   xwav3 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='microns',/dynamic_resize)

   b5   = widget_base(b4,/row)
   yvalue3 = widget_label(b5,/align_center,value='',/dynamic_resize)
   t2 = widget_label(b5,/align_center,value='Throughput',/dynamic_resize)
   ; end Order 3 box

   wave1 = 0.8
   wave2 = 5.8
   wavestep = 0.01

   wave = findgen((wave2-wave1)/wavestep)
   wave = wave*wavestep + wave1
   npts = n_elements(wave)

   state = ptr_new({ $

      ; Data and information in the widget
      apexangle: 0.0, $          ; Prism apex angle, radians
      camefl: 1.0, $             ; effective focal length of camera in mm
      collefl: 1.0, $            ; effective focal length of collimator in mm
      d0: d0, $                  ; dimensionless parameter, order 0
      d1: d1, $                  ; dimensionless parameter, order 1
      d2: d2, $                  ; dimensionless parameter, order 2
      d3: d3, $                  ; dimensionless parameter, order 3
      e0: e0, $                  ; efficiency, order 0
      e1: e1, $                  ; efficiency, order 1
      e2: e2, $                  ; efficiency, order 2
      e3: e3, $                  ; efficiency, order 3
      eff0: fltarr(npts), $      ; efficiency, order 0, matches wave array
      eff1: fltarr(npts), $      ; efficiency, order 1, matches wave array
      eff2: fltarr(npts), $      ; efficiency, order 2, matches wave array
      eff3: fltarr(npts), $      ; efficiency, order 3, matches wave array
      detfile: '', $             ; detectory quantum efficiency file
      filterlist: ptr_new(['none']), $ ; list of filters
      grismlist: ptr_new(['none']), $ ; list of grisms
      grooveangle: 0.0, $        ; groove angle, radians
      groovefreq: -1.0, $        ; grooves/mm
      inangle: 0.0, $            ; refracted input angle to grating, radians
      infofile: 'mimir.info', $  ; name of instrument information file
      npts: npts, $              ; number of points in master wave array
      opticfile: '', $           ; optics efficiency file
      pixelscale: 27.0, $        ; pixel size in microns
      plottype: 0, $             ; plottype 0-wavelength, 1-angle, 2-pixels
      prismindex: 1.42, $        ; Index of refraction for prism
      resinindex: 1.58, $        ; Index of refraction for resin
      sd0: 0.0, $                ; sin of angle of zero dispersion
      sd1: fltarr(npts), $       ; sin of 1st order dispersion angle
      sd2: fltarr(npts), $       ; sin of 2nd order dispersion angle
      sd3: fltarr(npts), $       ; sin of 3rd order dispersion angle
      telefile: '', $            ; telescope efficiency file
      throughput: fltarr(npts), $ ; system throughput without dispersive element
      w0: w0, $                  ; wavelengths for order 0
      w1: w1, $                  ; wavelengths for order 1
      w2: w2, $                  ; wavelengths for order 2
      w3: w3, $                  ; wavelengths for order 3
      wave: wave, $              ; master wavelength grid
      wave1: wave1, $            ; starting wavelength for plotting
      wave2: wave2, $            ; ending wavelength for plotting
      wavestep: wavestep, $      ; wavelength step size in microns

      ; Widget ids
      drawwin: win1, $           ; ID of main draw window
      filterselid: filtersel, $  ; ID of filter selction widget
      grismselid: grismsel, $    ; ID of grism selection widget
      plottypeid: plottype, $    ; plot type widget
      rightinfoid: rightinfo, $  ; ID of base containing all right side info
      xang1id: xang1, $          ; ID of text label for order 1 xaxis value
      xpos1id: xpos1, $          ; ID of text label for order 1 xaxis value
      xwav1id: xwav1, $          ; ID of text label for order 1 xaxis value
      yvalue1id: yvalue1, $      ; ID of text label for order 1 yaxis value
      xang2id: xang2, $          ; ID of text label for order 2 xaxis value
      xpos2id: xpos2, $          ; ID of text label for order 2 xaxis value
      xwav2id: xwav2, $          ; ID of text label for order 2 xaxis value
      yvalue2id: yvalue2, $      ; ID of text label for order 2 yaxis value
      xang3id: xang3, $          ; ID of text label for order 3 xaxis value
      xpos3id: xpos3, $          ; ID of text label for order 3 xaxis value
      xwav3id: xwav3, $          ; ID of text label for order 3 xaxis value
      yvalue3id: yvalue3, $      ; ID of text label for order 3 yaxis value

      mainbase: mainbase $       ; ID of top level base.

      })

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   widget_control, (*state).plottypeid, set_value=(*state).plottype

   ; Load the instrument information file
   mimiretc_loadinfo, state

   ; generate first plot
   mimiretc_refreshplot, state

   ; Give control to the XMANAGER.
   xmanager, 'mimiretc', mainbase, $
             EVENT_HANDLER='mimiretc_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='mimiretc_cleanup'

end
