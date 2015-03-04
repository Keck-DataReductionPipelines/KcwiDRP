;+
; NAME:
;   chknccal
; PURPOSE:  
;   Review and grade calibration images for nasacam.
; DESCRIPTION:
;   Widget searches the nasacam.calib and nasacam.image tables to build
;   lists of calibration entries and associated raw and [sub]super calibration
;   images. It allows an interactive search based on date range, filters,
;   calibration type, and calibration status to fill a scrollable list of
;   calibration entries. By default, the entries searched  are flats for all 
;   dates and filters  with pending status. When an entry is selected, another
;   list with individual images is populated, and a thumbnail view of the
;   calibration product (eg, sub super flat) is presented. Individual images
;   may be selected and displayed, and the status of the calibration entry
;   may be modified in the data base. Image display occurs in a separate
;   reused ITOOL window. The image display may be set to either raw or 
;   processed mode. In processed mode, each image is divided 
;   by its proper [sub]superflat after subtraction of overscan and
;   appropriate superbias image (which is chosen as the superbias with good
;   status nearest in time).
; CATEGORY:
;    CCD data processing
; CALLING SEQUENCE:
;   chknccal
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  CALROOT  - string giving the path to locate the calibration images referenced
;             in the data base entries, and created by addnccal. 
;             The current default is
;             '/kolvir/data1/buie/rawfits/nasacamcal' (on kolvir).
;  CALTABLENAME- Name of table in MYSQL database for calibration entries.
;             The default is 'calib'. 
;  CROP     - region of original image to save, [x1,x2,y1,y2]
;             default is 2006 NASACAM default of [21, 2064, 3, 2046].
;  DATABASE - Name of MYSQL database for calibration entries.
;             The default is 'nasacam'.
;  IMGROOT  - string giving the path to locate the image files referenced
;             in the data base entries. The default is 
;             '/kolvir/data1/buie/rawfits' (on kolvir).
;  IMGTABLENAME- Name of table in MYSQL database for image entries.
;             The default is 'image'. 
;  INSTTABLENAME- Name of table in MYSQL database for instrument entries.
;             The default is 'instrument'. 
;  KEYFILE-   Default keyfile for PARSEKEY. The default is to provide
;             a default key list suitable for nasacam data.
;  OVERSCAN - column overscan region to use for frame bias level,
;             default is 2006 NASACAM default of [2069, 2103].
;  
; OUTPUTS:
; No outputs for this procedure.
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
;    Data base entries are updated.
; RESTRICTIONS:
;   Assumes images are located in subdirectories named YYYYMMDD for each night.
;   They may be either of the form YYYYMMDD.nnn or YYYYMMDD.nnn.fits

;   Places calibration products in paths named YYYY/MM/DD for each night.
;   It currently processes data ONLY for the NASACAM CCD on the 31", and there
;   is no support for dark images.
;   Image frame numbers must be in the range 1-999 inclusive.
; 
; PROCEDURE:
; MODIFICATION HISTORY:
;   2006/08/07, Written by Peter L. Collins, Lowell Observatory
;   2006/08/10, PLC, fixes to support proper update of edit status
;                    buttons with calibration selection, add 'unknown'
;                    to editable status states, and proper update of
;                    thumbnail and image itool windows on requery & refresh
;   2006/08/12, PLC, fixed a bug in the main SQL query that prevented biases
;                    from displaying when a filter was selected, also a bug in
;                    chknccal_list that occasionally caused the subsuper
;                    thumbnail to be displayed in a separate window.
;   2006/08/19, PLC, cleanups, made sure dblun closed on exit and
;                    set /NO_RELEASE on the Raw/Processed and Set Status
;                    radio buttons to eliminate redundant operations.
;
;-


; cleanup on exit
pro chknccal_cleanup, tlb

   ; get pointer to state structure
   widget_control, tlb, get_uvalue=state

   if obj_valid((*state).oitool) then (*state).oitool->close
   ; remove img and itool objects.
   if obj_valid((*state).oitool) then  obj_destroy,(*state).oitool
   if obj_valid((*state).oimg) then  obj_destroy,(*state).oimg
   ;remove old query list from state.
   if ptr_valid((*state).starttime) then ptr_free,(*state).starttime
   if ptr_valid((*state).frame1) then ptr_free,(*state).frame1
   if ptr_valid((*state).frame2) then ptr_free,(*state).frame2
   if ptr_valid((*state).type) then ptr_free,(*state).type
   if ptr_valid((*state).status) then ptr_free,(*state).status
   if ptr_valid((*state).calname) then ptr_free,(*state).calname
   ;remove old img list from state.
   if ptr_valid((*state).midtimeimg) then ptr_free,(*state).midtimeimg
   if ptr_valid((*state).exptimeimg) then ptr_free,(*state).exptimeimg
   if ptr_valid((*state).siglvlimg) then ptr_free,(*state).siglvlimg
   if ptr_valid((*state).oscanoiseimg) then ptr_free,(*state).oscanoiseimg
   if ptr_valid((*state).calib) then ptr_free,(*state).calib

   free_lun, (*state).dblun

   ; Free up the state structure.
   ptr_free, state

end

; this could be done by quote itself, perhaps
; it wraps str in the quotestr, which is either 1 or 2(left/right) elements.
function chknccal_enquote, str, QUOTESTR=quotestr
   nq =  n_elements(quotestr) 
   if nq eq 0 then return, quote(str)

   ; this is over simplified
   if nq eq 1 then return, quotestr + str + quotestr
   return, quotestr[0] + str + quotestr[1]
end

; generate complete path to the calibration super
function chknccal_calpath, base, str, calname 
   calpath = addslash(base)
   date = strsplit(str, /extract)
   datecomponents = strsplit(date[0], '-',/extract)
   calpath += strjoin(datecomponents[0:2],'/')
   calpath = addslash(calpath)
   calpath += calname

   return, calpath
end


; create a query fragment for enumerated entries (type and status,
; although type is actually char(10) with 3 discrete values.)
;
; enum_names are the field values to select for- the enum buttons
; are 1 or 0 according to whether the corresponding value is desired.
; the query fragment is where ( type = x or type = y ...)
; All enum buttons 0 is an illegal case.
; IT IS ASSUMED that the enum or search values in the db equal the enum_names
; passed with case lowered.
; the query is encased in parentheses with 'or' as  needed
; the filter selector values are quoted. 
pro chknccal_enumquery, type,enum_names,enum_buttons, enumquery
   enumquery=''
   on = where( enum_buttons ne 0, oncount)
   if oncount eq 0 then begin
      print, 'Internal error, attempt to query ', type, '  with no values set'
      return
   endif

   for j=0, oncount-1 do begin
      if enumquery ne '' then enumquery += ' or '
      enumquery += (type + ' = ' + quote(strlowcase(enum_names[on[j]])))
   endfor

   enumquery = chknccal_enquote(enumquery, QUOTESTR=[' ( ', ' ) '])
end

   
; create the query fragment for the dates.
; Inputs consist of 2 date text windows, datewindow{1,2}
; and the InEquality pulldown menu value.
; If datewindow1 is blank then the query is returned as an empty string.
;
; If the menu specifies exact  the query is
; where   'ObsDate = 'xxxxx'            xxxx from datewindow1
; If the menu specifies like  the query is
; where   'ObsDate like 'xxxxx'            xxxx from datewindow1
; If the menu specifies >=  the query is
; where   'ObsDate >= 'xxxxx'            xxxx from datewindow1
; OR
; where   'ObsDate >= 'xxxxx' and ObsDate <= 'yyyyy'    xxxx from datewindow1
;                                                         yyyy from datewindow2
; If the menu specifies <=  the query is
; where   'ObsDate <= 'xxxxx'            xxxx from datewindow1
;
; the query is encased in parentheses with 'and' as  needed
; the date values are quoted.
pro chknccal_datequery,state,datequery
   widget_control, (*state).datewindow1id, GET_VALUE=datew1
   widget_control, (*state).datewindow2id, GET_VALUE=datew2

   date1 = datew1[0]
   date2 = datew2[0]

   datequery = ''
   if date1 eq '' then return

   case (*state).inequalnames[(*state).inequalindex] of

      'exact': begin
         datequery = ' ObsDate = ' + quote(date1)
      end

      'like': begin
         datequery = ' ObsDate like ' + quote(date1)
      end

      '<=': begin
         datequery = ' ObsDate <= ' + quote(date1)
      end

      '>=': begin
         datequery = ' ObsDate >= ' + quote(date1)
         if date2 ne '' then $
            datequery += (' and ObsDate <= ' + quote(date2))
      end
   endcase

   datequery = chknccal_enquote(datequery, QUOTESTR=[' ( ', ' ) '])
end

; Appropriately sensitize/desensitize datewindow2- when the window
; is desensitized the previous contents are cleared.
pro chknccal_date2alive, state
   if (*state).inequalnames[(*state).inequalindex] eq '>=' then $
      widget_control, (*state).datewindow2id,  SENSITIVE=1 $
   else $
      widget_control, (*state).datewindow2id, SET_VALUE='', SENSITIVE=0 
end
   
   
; create the query fragment for the filters.
; there are 2 filters, which may be specified (non blank) or not.
; if neither filter is specified the query fragment is returned empty.
; if 1 filter is specified it would be where ' filtx = 'n' '
; if 2 filters are specified it would be where ' filt1 = 'n1' and filt2 = 'n2''
; the query is encased in parentheses with 'and' as  needed
; the filter selector values are quoted (even though numeric)
; 
pro chknccal_filterquery, state, filterquery 
   widget_control, (*state).filterwindow1id, GET_VALUE=filtw1
   widget_control, (*state).filterwindow2id, GET_VALUE=filtw2

   filter1 = filtw1[0]
   filter2 = filtw2[0]

   filterquery = ''
   if filter1 eq '' and filter2 eq '' then return

   queryfrag = ''
   if filter1 ne '' then  queryfrag +=  ' filt1 = ' + quote(filter1)
   if filter2 ne '' then  begin
      if queryfrag ne '' then queryfrag += ' and '
      queryfrag +=  ' filt2 = ' + quote(filter2)
   endif
   
   filterquery = chknccal_enquote(queryfrag, QUOTESTR=[' ( ', ' ) '])
   filterquery += " or type != 'flat' "
   filterquery = chknccal_enquote(filterquery, QUOTESTR=[' ( ', ' ) '])
   print, 'filter query:', filterquery
end

; generates a query for the type with chknccal_enumquery
pro chknccal_typequery, state, typequery
   widget_control, (*state).typebutnid, GET_VALUE=typebtns
   chknccal_enumquery, 'type',(*state).typenames,typebtns,typequery
end

; generates a query for the status with chknccal_enumquery
pro chknccal_statusquery, state, statusquery
   widget_control, (*state).statusbutnid, GET_VALUE=statusbtns
   chknccal_enumquery, 'status',(*state).statusnames, $
                statusbtns,statusquery
end


; generate the matrix of statistics for available calibration sets.
; currently we want to know the counts of unknown and pending status
; entries for biases and flats. Routine generates an array of 3 strings
; depicting a labeled 2x2 matrix.
; a special counting sql query is used to generate the numbers.
pro chknccal_calstats, state, calstatstrings
   countquery = [ "select type, status, count(*) from ", $
                  (*state).caltablename, $
                  " where ", $
                  " ( type = 'flat' or type = 'bias' )", $
                  " and ", $
                  " ( status = 'unknown' or status = 'pending' )", $
                  " group by type, status ", $
                  "order by type, status", $
                  ";" $
                ]

   print,  'count query: ', countquery

   mysqlquery,(*state).dblun,countquery,typecol,statuscol,countcol, $
                                    format='a,a,i'

   calstatstrings = [ ("       Pending    Unknown" ) ] 

   types_for_stats = ['bias', 'flat']

   for i = 0, n_elements(types_for_stats)-1 do begin
      statval = [0,0]
      t = where( typecol eq types_for_stats[i], tcount)
      if tcount gt 0 then begin
         p = where ( statuscol[t] eq 'pending', pcount)
         if pcount eq  1 then statval[0] = countcol[t[p[0]]]
         u = where ( statuscol[t] eq 'unknown', ucount)
         if ucount eq  1 then statval[1] = countcol[t[u[0]]]
      endif

      calstatstrings = [ calstatstrings, $
          string( types_for_stats[i], statval[*], $
                  FORMAT="(a5, i8, i8)") ]
   endfor

   print, 'calstatstring: ', calstatstrings
end

   
; generate the overall query- it is set up as a string array
; as a convenience in printing. The various fragments for the where
; statement are seperated by 'and'
;
; the query prints entirety of records selected
pro chknccal_calib_query, state
   what =' ObsDate,type,num1,num2,calname,status,startime '
   calibquery = 'select ' + what +  ' from  ' + (*state).caltablename 

   chknccal_datequery,state, s
   selector = ['']
   selector[0] = s

   chknccal_filterquery,state, s
   if s ne '' then begin
      if selector[0] eq ''  then selector[0] = s  $
      else selector = [ selector, ' and ' + s]
   endif

  chknccal_typequery,state, s
   if s ne '' then begin
      if selector[0] eq ''  then selector[0] = s  $
      else selector = [ selector, ' and ' + s]
   endif

  chknccal_statusquery,state, s
   if s ne '' then begin
      if selector[0] eq ''  then selector[0] = s  $
      else selector = [ selector, ' and ' + s]
   endif

   if selector[0] ne '' then calibquery = $
      [calibquery, ' where ' , selector, 'order by startime', ';']

   print,  'calib query: ', calibquery

   mysqlquery,(*state).dblun,calibquery,ObsDate,type,frame1,frame2, calname, $
                                 status,starttime,format='a,a,i,i,a,a,a'
   if  ObsDate[0] ne '' then begin
      calibentries = strarr(n_elements(ObsDate))
      for i=0,n_elements(ObsDate)-1 do begin
         if starttime[i] eq 'NULL' then starttime[i]=ObsDate[i]
         if calname[i] eq 'NULL' then calname[i]=''

         calibentries[i]=string(starttime[i], type[i],frame2[i]-frame1[i]+1, $
                    status[i], calname[i], format= $
                  "(a16, ' [',a4, '] ',  i2,  ' [', a7, '] ', a)")

      endfor
   endif else begin
      calibentries=['']
   endelse

   ; update the cal stats window- this is also done in the ctrl base after
   ; changing an entry's status.
   chknccal_calstats, state, calstatstrgs
   widget_control,(*state).calstatid, SET_VALUE=calstatstrgs

   ;desensitize the edit status buttons
   widget_control, (*state).setstatusbutnid,SENSITIVE=0

   ; update the list of indiviually selectable entries
   widget_control, (*state).listid, SET_VALUE=calibentries

   ; save dual selection context in case the same entries exist and need refresh
   ; of thumbnail and itool window.
   oldlistindex = (*state).listindex
   oldlistimgindex = (*state).listimgindex
   if oldlistindex ge 0 then oldstarttime = (*(*state).starttime)[oldlistindex]

   ; remove index to selected calibration entry.
   (*state).listindex=-1

   ;remove old query list from state.
   if ptr_valid((*state).starttime) then ptr_free,(*state).starttime
   if ptr_valid((*state).frame1) then ptr_free,(*state).frame1
   if ptr_valid((*state).frame2) then ptr_free,(*state).frame2
   if ptr_valid((*state).type) then ptr_free,(*state).type
   if ptr_valid((*state).status) then ptr_free,(*state).status
   if ptr_valid((*state).calname) then ptr_free,(*state).calname
   ;remove old img list from state.
   if ptr_valid((*state).midtimeimg) then $
      ptr_free,(*state).midtimeimg
   if ptr_valid((*state).exptimeimg) then $
      ptr_free,(*state).exptimeimg
   if ptr_valid((*state).siglvlimg) then $
      ptr_free,(*state).siglvlimg
   if ptr_valid((*state).oscanoiseimg) then $
      ptr_free,(*state).oscanoiseimg
   widget_control, (*state).listimgid, SET_VALUE=''

   ;update state with the query list.
   if ObsDate[0] ne '' then begin
      (*state).starttime = ptr_new(starttime)
      (*state).frame1  = ptr_new(frame1)
      (*state).frame2  = ptr_new(frame2)
      (*state).type  = ptr_new(type)
      (*state).status  = ptr_new(status)
      (*state).calname  = ptr_new(calname)

      if oldlistindex ge 0 then begin
         z = where( strcmp(oldstarttime,(*(*state).starttime)),count)
         if count gt 0 then begin
            (*state).listindex = z[0]
            chknccal_list,state
            if oldlistimgindex ge 0 then begin
               (*state).listimgindex = oldlistimgindex
               chknccal_list,state
            endif
         endif
      endif
   endif

   if  (*state).listindex lt 0 then begin
      ; clear thumbnail and itool windows if they exist.
      ; clear existing thumbnail image and labels.
      widget_control, (*state).thumbwin, get_value=winnum
      wset,winnum
      thumbsize = (*state).thumbsize
      img = intarr(thumbsize-1,thumbsize-1)
      tv,img
      widget_control, (*state).thumbtext1id, SET_VALUE=''
      widget_control, (*state).thumbtext2id, SET_VALUE=''
      if obj_valid((*state).oitool) then (*state).oitool->close
   endif else begin
      ; can this actually happen?
      if (*state).listimgindex  lt 0 and obj_valid((*state).oitool) then $
         (*state).oitool->close
   endelse
end

pro chknccal_img_query, state,fn,i
   selector = ['select filename, midtime, sky, osnoise ']
   selector = [selector, 'from ' + (*state).imgtablename, $
                ' where filename = ' + quote(fn) ]
   imgquery = [ selector, ';' ]

   print,  imgquery
   mysqlquery,(*state).dblun, imgquery, filename,midtime,sky,osnoise, $
              FORMAT="(a,a,f,f)"
   if filename eq '' or n_elements(filename) ne 1 then begin
      print, ' img query failed with result size', n_elements(filename)
      print,  imgquery
      return
   endif

   stt = strsplit(midtime, /EXTRACT)
   (*(*state).midtimeimg)[i] = stt[1]
   (*(*state).siglvlimg)[i] = sky
   (*(*state).oscanoiseimg)[i] = osnoise
   return
end

pro chknccal_inst_query, state,fn,i
   selector = ['select filename, exptime']
   selector = [selector, 'from ' + (*state).insttablename, $
                ' where filename = ' + quote(fn) ]
   instquery = [ selector, ';' ]

   mysqlquery,(*state).dblun, instquery, filename,etime, $
              FORMAT="(a,a,f,f)"
   if filename eq '' or n_elements(filename) ne 1 then begin
      print, ' inst query failed with result size', n_elements(filename)
      print,  instquery
      return
   endif

   (*(*state).exptimeimg)[i] = etime
   return
end
pro chknccal_nearestbias_query, state,index,biasname

   selector = ['select calname, status, startime']
   selector = [selector, 'from ' + (*state).caltablename, $
                " where type  = 'bias' and status = 'good'" , $
                " order by ( ABS( unix_timestamp(startime ) -  ", $
                "unix_timestamp( "  + quote((*(*state).starttime)[index]) + $
                " ) ) ) ", "limit 1"]
   nearestquery = [ selector, ';' ]
   print,  'closest bias query: ', nearestquery
   biasname = ''
   mysqlquery,(*state).dblun,nearestquery,calname,status,starttime, $
               format='a,a,a'

   if calname[0] eq '' or n_elements(calname[0]) ne 1 then begin
      print, 'nearest query failed with result size', n_elements(calname[0])
      print,  nearestquery
      return
   endif
   biasname= calname[0]
   print, 'bias super ', biasname
   return
end

pro chknccal_status_update, state,index,status
   selector= [' update ' +  (*state).caltablename, $
              ' set status = ' + quote(status), $
              ' where   startime = ' + quote( (*(*state).starttime)[index]), $
              ' and calname = ' + quote( (*(*state).calname)[index]) + ';' $
              ]
   print, 'update status query: ', selector

   mysqlcmd, (*state).dblun, selector,updateresp,nlines
end



pro chknccal_listimg, state

   i= (*state).listindex
   j = (*state).listimgindex
   widget_control, (*state).rawprocbutnid, GET_VALUE=rawprocbtn
   if obj_valid((*state).oimg) then  obj_destroy,(*state).oimg

   stt = strsplit((*(*state).starttime)[i],/EXTRACT)
   base = strjoin(strsplit(stt[0],'-',/EXTRACT))
   rawfn = base + '.' + string( (j + (*(*state).frame1)[i]), FORMAT= $
                                             "(i3.3)")
   rawfn = addslash((*state).imgroot) + addslash(base) + rawfn
   realfn = rawfn
   if not file_test(realfn,/READ,/NOEXPAND_PATH) then realfn= rawfn+'.fits' 
   if not file_test(realfn, /READ,/NOEXPAND_PATH) then begin
      print, rawfn, ' cannot be found'
      if obj_valid((*state).oitool) then (*state).oitool->close
   endif else begin
      print, 'READFITS raw image from ', realfn
      rimg = float(readfits(realfn, hdr, /SILENT))

      ; Extract some info from the header, using list of header correspondence
      ; names. Adapted from ccdphot.pro
      parsekey, hdr, (*state).nckeylist, hdrinfo
      print, 'image file ', hdrinfo.imfile
      (*(*state).calib).filter = hdrinfo.filter

      ; Copy the header imformation to the image-parameters structure.
      if rawprocbtn ne 0 then begin
         ccdproc, rimg, hdrinfo, (*(*state).calib),pimg
         img = temporary(pimg)
      endif else begin
         img = temporary(rimg)
      endelse
      (*state).oimg = obj_new('itoolimage', img)

      ; set up header parameters, for a raw image.
      (*state).oimg->getproperty, PIM_PARMS=pim_parms

      (*pim_parms).airmass  = hdrinfo.airmass
      (*pim_parms).date     = hdrinfo.date
      (*pim_parms).expdelta = hdrinfo.expdelta
      (*pim_parms).exptime  = hdrinfo.exptime
      (*pim_parms).imfile   = hdrinfo.imfile
      (*pim_parms).filter   = hdrinfo.filter
      (*pim_parms).object   = hdrinfo.object
      (*pim_parms).ut       = hdrinfo.ut
      (*pim_parms).jd       = hdrinfo.jd

      if obj_valid((*state).oitool) and $
         (*state).oldrawprocbtn eq rawprocbtn then begin
         (*state).itoolrefresh++
         (*state).oitool->nextimage, (*state).oimg
      endif else begin
         if obj_valid((*state).oitool ) then (*state).oitool->close
         (*state).itoolreload++
         (*state).oitool = obj_new('itool',(*state).oimg,/NODISMISS, $
                        PHOTPARMFILE='', GROUP_LEADER=(*state).mainbase, $
                        TMPLFILE='', WZOOMFACT=1)
         (*state).oitool->realize, /NO_BLOCK
      endelse
      print, 'ITOOL: ', (*state).itoolreload, ' invocations, ', $
                        (*state).itoolrefresh, ' refreshes'
      (*state).oldrawprocbtn = rawprocbtn
   endelse
end


pro chknccal_list, state
   ;remove old img list from state.
   if ptr_valid((*state).midtimeimg) then $
      ptr_free,(*state).midtimeimg
   if ptr_valid((*state).exptimeimg) then $
      ptr_free,(*state).exptimeimg
   if ptr_valid((*state).siglvlimg) then $
      ptr_free,(*state).siglvlimg
   if ptr_valid((*state).oscanoiseimg) then $
      ptr_free,(*state).oscanoiseimg

    widget_control, (*state).thumbwin, get_value=winnum
    wset,winnum

   i = (*state).listindex
   widget_control, (*state).listid,SET_LIST_SELECT=i

   ; draw the thumbnail of the super if available.
   ; NOTE: this code should be folded in with search for biasname, etc.
   sstring = (*(*state).starttime)[i]
   calproduct = chknccal_calpath((*state).calroot,sstring, $
                                 (*(*state).calname)[i])
   if calproduct ne '' and file_test(calproduct,/READ,/NOEXPAND_PATH) then begin
      print, 'open calibration super thumb: ', calproduct
      thumb1str= (*(*state).calname)[i]
      widget_control, (*state).thumbtext1id, SET_VALUE=thumb1str

      ; thumb code adapted from m buie flattest2.pro and flattest.pro
      thumbsize = (*state).thumbsize
      print, 'READFITS thumb image from ', calproduct
      img = readfits(calproduct,/SILENT)
      sz = size(img,/DIMENSION)
      crop = (*state).crop
      if (sz[0] ne (crop[1] - crop[0] + 1) or $
         sz[1] ne (crop[3] - crop[2] + 1) ) then begin
         print, 'calibration file ', calproduct, $
                ' image has bad dimensions.'
         widget_control, (*state).listimgid, SET_VALUE=''
      endif else begin
         ; find smallest bin factor so the image will fit.
         bf=( (sz[0] > sz[1]) + (thumbsize - 1))/thumbsize
         i1=(sz[0]/bf)*bf - 1
         j1=(sz[1]/bf)*bf - 1
         print, 'thumb size: ', sz[0]/bf,sz[1]/bf
         widget_control, (*state).thumbwin, DRAW_XSIZE = sz[0]/bf, $
         DRAW_YSIZE=sz[1]/bf
         print, ' size: ', sz[0], sz[1]
         print, 'bin factor: ', bf
         skysclim,img,v1,v2,mval,sig,NPTS=30000
         print, 'thumb stretch: ', v1,v2,mval,sig
         img=rebin(img[0:i1,0:j1],sz[0]/bf,sz[1]/bf)
         tv,bytscl(img, min=v1, max=v2,top=!d.n_colors-1)

         ; populate image list.
         nimgs = (*(*state).frame2)[i] - (*(*state).frame1)[i] + 1
         midtimeimg  = strarr( nimgs)
         exptimeimg  = fltarr( nimgs)
         siglvlimg  = fltarr( nimgs)
         oscanoiseimg  = fltarr( nimgs)

         (*state).midtimeimg  = ptr_new(midtimeimg)
         (*state).exptimeimg  = ptr_new(exptimeimg)
         (*state).siglvlimg  = ptr_new(siglvlimg)
         (*state).oscanoiseimg  = ptr_new(oscanoiseimg)

         ; set dummy default values.
         imgentries = strarr(nimgs)
         for j=0, nimgs-1 do begin
            stt = strsplit((*(*state).starttime)[i],/EXTRACT)
            midtimeimg[j] = stt[1]
            exptimeimg[j] = 0.0
            siglvlimg[j]=0.0
            oscanoiseimg[j]=0.0
            rawfn = strjoin(strsplit(stt[0],'-',/EXTRACT)) + '.' + $
                    string((j + (*(*state).frame1)[i]),FORMAT="(i3.3)")
            ; using the rawfn string, query the image db table for
            ; the real values defaulted above.
            chknccal_img_query, state, rawfn,j
            chknccal_inst_query, state, rawfn,j

            imgentries[j] = string(rawfn, (*(*state).midtimeimg)[j], $
                                (*(*state).exptimeimg)[j], $
                                round((*(*state).siglvlimg)[j]), $
                                (*(*state).oscanoiseimg)[j], $
                                FORMAT="(a, '  ',a, ' ',f5.1,f7.0,f6.1)")
         endfor
         ; this should be done on the fly...
         ; set up the super flat and bias images that pertain
         ; to the selection.
         ; update the list of individually selectable images
         (*state).flatname=''
         if (*(*state).type)[i] eq 'bias' then begin
            (*state).biasname= chknccal_calpath((*state).calroot, $
                                   sstring, (*(*state).calname)[i])
         endif else begin
            chknccal_nearestbias_query,state,i,biasname
            biasdatestr = strmid(biasname,0,4) + '-' + $
                          strmid(biasname,4,2) + '-' + $
                          strmid(biasname,6,2)
            (*state).biasname= chknccal_calpath((*state).calroot, $
                                   biasdatestr, biasname)
         endelse
         if (*state).biasname ne '' and not $
               file_test((*state).biasname,/READ,/NOEXPAND_PATH) then $
            (*state).biasname = ''
         if (*(*state).type)[i] eq 'flat' then $
            (*state).flatname= chknccal_calpath((*state).calroot, $
                                   sstring, (*(*state).calname)[i])

         if (*state).flatname ne '' and not $
               file_test((*state).flatname,/READ,/NOEXPAND_PATH) then $
            (*state).flatname = ''

         print, 'supers: ', (*state).biasname, '     ', $
                            (*state).flatname

         if ptr_valid( (*state).calib) then ptr_free,(*state).calib 
         crop = (*state).crop
         overscan = (*state).overscan
         print, 'READFITS master bias  image from ', (*state).biasname
         bimg = readfits((*state).biasname,/SILENT)
         if (*state).flatname ne '' then begin
            bn = strsplit((*state).biasname, '/', /EXTRACT)
            thumb2str = 'bias: ' +  bn[n_elements(bn)-1]
            widget_control, (*state).thumbtext2id, SET_VALUE=thumb2str
            print, 'READFITS master flat  image from ',  $
                    (*state).flatname
            fimg = readfits((*state).flatname,/SILENT)
            flatcode =[0]
         endif else begin
            fimg = 0
            flatcode =[-1]
         endelse

         calib = { $
            xl:overscan[0], $    
            xr:overscan[1], $
            x1:crop[0], $
            x2:crop[1], $
            y1:crop[2], $
            y2:crop[3], $
            bias:bimg, $
            dark:0, $
            flat:fimg, $
            frngptr:[-1], $
            flatptr:flatcode, $
            filter:'' $
         }

         (*state).calib = ptr_new(calib)

         widget_control, (*state).listimgid, SET_VALUE=imgentries

         ; if an image had been selected in the previous calibration 
         ; selection, (ie list and listimg widgets both selected)
         ; attempt to select the same image number in the new calibration
         ; entry- or, if that image does not exist, go back to the first.
         ; However, if itool is not running at the moment we assume the 
         ; user killed it and does not want an image selection. So, we
         ; force listimgindex to -1 in that case.
         imgindex = (*state).listimgindex
         if imgindex ge 0 then begin
            if obj_valid((*state).oitool) then  begin
               if imgindex gt nimgs - 1 then imgindex = 0
               widget_control, (*state).listimgid,SET_LIST_SELECT=imgindex
               (*state).listimgindex = imgindex
               chknccal_listimg, state
            endif else begin
               (*state).listimgindex = -1
            endelse
         endif

         ;make the set status button reflect current status. 
         ;Convert current status to index.
         z = where( strcmp( (*(*state).status)[i], $
                            (*state).setstatusnames,/FOLD_CASE), count)
         statusindex = z[0]
         widget_control, (*state).setstatusbutnid,SENSITIVE=1, $
                           SET_VALUE=statusindex
      endelse
   endif else begin
      print, 'The calibration super file (', calproduct, $
             ') cannot be found'
      widget_control, (*state).listimgid, SET_VALUE=''
   endelse
end


pro chknccal_eve, event

   widget_control, event.top, GET_UVALUE=state

   event_name =''
   if event.id eq (*state).mainbase then event_name = 'Mainbase'
   if event.id eq (*state).listid then event_name = 'List'
   if event.id eq (*state).listimgid then event_name = 'ListImg'
   if event_name eq '' then $
      widget_control, event.id,  GET_UVALUE=event_name, /HOURGLASS

   case event_name of

      ; update query for ObsDate1 event and sensitize/desensitize
      ; ObsDate2 button as needed.
      'datewindow1': begin
         chknccal_date2alive,state
         chknccal_calib_query, state
      end

      ; ObsDate2 event
      'datewindow2': begin
         chknccal_calib_query, state
      end

      ; Filter1 event
      'filterwindow1': begin
         chknccal_calib_query, state
      end

      ; Filter2 event
      'filterwindow2': begin
         chknccal_calib_query, state
      end

      ;inequality menu event
      'InEqualityMenu': begin

         (*state).inequalindex = event.index
         chknccal_date2alive,state
         chknccal_calib_query, state
      end

      ; biases/darks/flats button group
      'TypesMenu': begin
         widget_control, (*state).typebutnid, GET_VALUE=typebtns
         on = where( typebtns ne 0, oncount)
         ; don't ever allow the state of all buttons off
         if oncount eq 0 then begin
            typebtns[event.value] = 1
            widget_control, (*state).typebutnid, SET_VALUE=typebtns
         endif
         chknccal_calib_query, state
      end

      ; Refresh button 
      'Refresh': begin
         chknccal_calib_query, state
      end

      ; unknown/good/bad/pending button group
      'StatusMenu': begin
         widget_control, (*state).statusbutnid, GET_VALUE=statusbtns

         on = where( statusbtns ne 0, oncount)
         ; don't ever allow the state of all buttons off
         if oncount eq 0 then begin
            statusbtns[event.value] = 1
            widget_control, (*state).statusbutnid, SET_VALUE=statusbtns
         endif

         chknccal_calib_query, state
      end
      
      ; force biases/darks/flats button to all set
      'TypesAll': begin
         alltypesset = (*state).typedefaults > 1
         widget_control, (*state).typebutnid,SET_VALUE=alltypesset
         chknccal_calib_query, state
      end
      
      ; force biases/darks/flats button to original default.
      'TypeDefaults': begin
         widget_control, (*state).typebutnid,SET_VALUE=(*state).typedefaults
         chknccal_calib_query, state
      end

      ; force unknown/good/bad/pending button to all set
      'StatusAll': begin
         allstatussset = (*state).statusdefaults > 1
         widget_control, (*state).statusbutnid,SET_VALUE=allstatussset
         chknccal_calib_query, state
      end
      
      ; force unknown/good/bad/pending button to original default
      'StatusDefaults': begin
         widget_control, (*state).statusbutnid,SET_VALUE=(*state).statusdefaults
         chknccal_calib_query, state
      end

      ; force the whole gui to original defaults.
      'DefaultAll': begin
         widget_control, (*state).statusbutnid,SET_VALUE=(*state).statusdefaults
         widget_control, (*state).typebutnid,SET_VALUE=(*state).typedefaults
         widget_control, (*state).filterwindow1id, SET_VALUE=''
         widget_control, (*state).filterwindow2id, SET_VALUE=''
         widget_control, (*state).datewindow1id, SET_VALUE=''
         widget_control, (*state).datewindow2id, SET_VALUE=''
         widget_control, (*state).datewindow2id, SET_VALUE=''
         (*state).inequalindex = 0
         widget_control, (*state).InEqualId, $
                           SET_DROPLIST_SELECT=(*state).inequalindex

         chknccal_date2alive, state

         chknccal_calib_query, state
      end
      ; Set Raw/Processed Status- processed needs checking to see if
      ; master bias exists.
      'RawProcMenu': begin
         print, 'Raw/Proc Menu Event'
         if ptr_valid( (*state).midtimeimg) and $
                       (*state).listimgindex ge 0 then $
            chknccal_listimg, state
      end

      ; change good/bad/pending status. The listid widget remains unchanged
      ; until the next refresh or other query. However the calstats box is
      ; updated immediately.
      'SetStatusMenu': begin
         i = (*state).listindex
         if i ge 0 then begin
            widget_control, (*state).setstatusbutnid, GET_VALUE=setstatusbtn
            chknccal_status_update, state, i, $
                                   (*state).setstatusnames[setstatusbtn]

            ; update the cal stats window-issue another query.
            chknccal_calstats, state, calstatstrgs
            widget_control,(*state).calstatid, SET_VALUE=calstatstrgs
         endif
      end


      'List': begin
         i = event.index

         ; clear existing thumbnail image and labels.
         widget_control, (*state).thumbwin, get_value=winnum
         wset,winnum
         thumbsize = (*state).thumbsize
         img = intarr(thumbsize-1,thumbsize-1)
         tv,img
         widget_control, (*state).thumbtext1id, SET_VALUE=''
         widget_control, (*state).thumbtext2id, SET_VALUE=''
         if not ptr_valid( (*state).starttime) then begin
            ; if you click on the garbage entry when window empty.
            print, 'Invalid, there are no entries available'
         endif else begin

            (*state).listindex = i        
            chknccal_list,state
         endelse
      end

      'ListImg': begin
         j = event.index;
         if not ptr_valid( (*state).midtimeimg) then begin
            ; if you click on the garbage entry when window empty.
            print, 'Invalid, there are no img entries available'
         endif else begin
            (*state).listimgindex = event.index
            chknccal_listimg, state
         endelse
      end
          
      'THE_MENU': begin
         case event.value of

;            : begin
;            end

            'Exit' : begin
               widget_control, event.top, /DESTROY
               return
            end

            else: begin
               message, 'Unknown menu event:', /INFO
               help, event, /STRUCTURE
            end

         endcase

      end ; THE_MENU

      

      'Mainbase': begin

      end

      else: begin
         print,'EVENT NAME: ',event_name
         message, 'Unknown event:', /INFO
         help, event, /STRUCTURE
      end

   endcase

end ; end of event handler

pro chknccal, DATABASE=database,CALTABLENAME=caltablename, $
              IMGTABLENAME=imgtablename, $
              INSTTABLENAME=insttablename,CALROOT=calroot, $
              IMGROOT=imgroot,CROP=crop,OVERSCAN=overscan, $
              KEYFILE=keyfile

   ; these were adapted for addncccal- please check provenance
   nasacam2006_crop=[21,2064,3,2046]
   nasacam2006_overscan=[2069,2103]
   ; adapted from nasacam.key on /net/frakir/raid/buie/Reduced 8 Aug 2006
   ; 'FILENAME' entry changed.
   nasacam2006_keylist= [['AIRMASS',   'K', 'AIRMASS'    ], $ 
                        [ 'DATE',      'K',  'DATE'      ], $
                        [ 'DATETMPL',  'T',  'YYYY/MM/DD'], $
                        [ 'EXPDELTA',  'V',  '0.'        ], $
                        [ 'EXPTIME',   'K',  'EXPTIME'   ], $
                        [ 'FILTER',    'K',  'FILT_*'    ], $
                        [ 'FILENAME',  'K',  'FILENAME'  ], $
                        [ 'OBJECT',    'K',  'OBJECT'    ], $
                        [ 'UT',        'K',  'UTCSTART'  ], $
                        [ 'RA',        'K',  'TELRA'     ], $
                        [ 'DEC',       'K',  'TELDEC'    ], $
                        [ 'EPOCH',     'K',  'EQUINOX'   ] ]

   self = 'chknccal: '
   if badpar(database,[0,7],0,caller=self+'(DATABASE) ',  $
             default='nasacam') then return
   if badpar(caltablename,[0,7],0,caller=self+'(CALTABLENAME) ',  $
             default='calib') then return
   if badpar(imgtablename,[0,7],0,caller=self+'(IMGTABLENAME) ',  $
             default='image') then return
   if badpar(insttablename,[0,7],0,caller=self+'(INSTTABLENAME) ',  $
             default='instrument') then return
   if badpar(calroot,[0,7],0,caller=self+'(CALROOT) ',  $
             default='/kolvir/data1/buie/rawfits/nasacamcal') then return
   if badpar(imgroot,[0,7],0,caller=self+'(IMGROOT) ',  $
            default='/kolvir/data1/buie/rawfits') then return
   if badpar(crop, [0,2,3],1,caller=self+'(CROP) ', $
             default=nasacam2006_crop) then return
   if badpar(overscan,[0,2,3],1,caller=self+'(OVERSCAN) ', $
             default=nasacam2006_overscan) then return
   if badpar(keyfile,[0,7],0,caller=self+'(KEYFILE) ', $
             default='[[DEFAULT]]') then return

   if xregistered('chknccal') then begin 
      print, self, 'widget already running'
      return
   endif

   if (!d.flags and 256) eq 0 then begin
      print, self + 'Error. No windowing device. Cannot be started.'
      return
   endif

   ; Get header correspondence list.
   if keyfile ne '[[DEFAULT]]' then begin
      loadkeys,keyfile, nckeylist
   endif else begin
      nckeylist = nasacam2006_keylist
   endelse

   ; open the data base.
   openmysql,dblun,database

   ; display the sql atabase and tbl.
   dblabel = string( database, caltablename, FORMAT="( a, '.',a)")
   dblabel = chknccal_enquote( dblabel, QUOTESTR=[ '    [', ']'])
   ;Define the main base.
   mainbase = widget_base( TITLE='NasaCam Calibration File Review ' + dblabel, $
                           /COLUMN, UVALUE=0, MBAR=calbar, /TLB_SIZE_EVENTS )

   menu = CW_PdMenu(calbar, /RETURN_NAME, $
                    ['1\File',$
                     '2\Exit'$
                    ], $
                      UVALUE='THE_MENU', /MBAR)

   selectbase = widget_base(mainbase,/COLUMN)


   ;set up the selector base
   datebase = widget_base(selectbase, /ROW)

   ; the default is always the first element.
   inequalnames  = ['exact','>=','<=','like']
   inequalindex = 0
   ineq  = widget_droplist(datebase,VALUE=inequalnames, UVALUE='InEqualityMenu')
   d1 = widget_label(datebase, value = 'First Date');

   datewindow1 = widget_text(datebase,/EDITABLE,XSIZE=10, UVALUE='datewindow1')
   d3 = widget_label(datebase, value = 'Last');
   datewindow2 = widget_text(datebase,/EDITABLE,XSIZE=10, $
                            UVALUE='datewindow2', SENSITIVE=0)

   filterbase = widget_base(selectbase, /ROW)
   f1 = widget_label(filterbase, value = 'Filter 1');

   filterwindow1=widget_text(filterbase,/EDITABLE,XSIZE=3,UVALUE='filterwindow1')
   f2 = widget_label(filterbase, value = 'Filter 2');
   filterwindow2=widget_text(filterbase,/EDITABLE,XSIZE=3,UVALUE='filterwindow2')

   enumbase = widget_base(selectbase, /COLUMN)

   ; names must match the enum names in the db in a case-insensitive way.
   typenames = [ 'Bias', 'Dark',  'Flat'] 
   typedefaults = [0,0,1] ; Flats Only
   typebase = widget_base(enumbase, /ROW,FRAME=5)
   t1 = widget_button(typebase, value = 'All', UVALUE='TypesAll')
   typebutnid = cw_bgroup( typebase, typenames, SET_VALUE=typedefaults, $
                          UVALUE='TypesMenu',/ROW, /NONEXCLUSIVE)
   t2 = widget_button(typebase, value = 'Defaults', UVALUE='TypeDefaults')

   ; names must match the enum names in the db in a case-insensitive way.
   statusnames = [ 'Unknown', 'Good', 'Bad', 'Pending']
   statusdefaults= [0,0,0,1] ; Pending Only
   statusbase = widget_base(enumbase, /ROW,FRAME=5)
   s1 = widget_button(statusbase, value = 'All', UVALUE='StatusAll')
   statusbutnid = cw_bgroup( statusbase, statusnames,UVALUE='StatusMenu', $
                                 /ROW,/NONEXCLUSIVE,SET_VALUE=statusdefaults)
   s2 = widget_button(statusbase, value = 'Defaults', UVALUE='StatusDefaults')
   infobase = widget_base(mainbase,/ROW)
   i1 = widget_button(infobase, value = 'Default ALL', UVALUE='DefaultAll')
   ; display cal table statistics.
   calstatid = widget_text(infobase, YSIZE=3, XSIZE=25, VALUE=calstatstrgs)
   i2 = widget_button(infobase, value = 'Refresh', UVALUE='Refresh')

   matrixbase = widget_base(mainbase,/COLUMN,FRAME=5)
   listbase = widget_base(matrixbase,/COLUMN)
   listid = widget_list(listbase, YSIZE=5, XSIZE=58, UVALUE='LIST')

   ; list of available original images.
   listimgbase = widget_base(matrixbase,/COLUMN)
   listimglbl= $
   '  fn (.fits)  midtime   etim   D/N   noise'
   m1 = widget_label(listimgbase,VALUE=listimglbl,/ALIGN_LEFT)
   listimgid = widget_list(listimgbase, YSIZE=5, XSIZE=55, UVALUE='LISTIMG')

   superbase=widget_base(mainbase,/COLUMN,FRAME=3)
   ; display small thumbnail (binned) image of super flat, dark, or bias.
   thumbsize=300
   thumbwin = widget_draw(superbase, XSIZE=thumbsize,YSIZE=thumbsize)
   thumbtext1id = widget_label(superbase, VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)
   thumbtext2id = widget_label(superbase,VALUE='',/DYNAMIC_RESIZE,/ALIGN_LEFT)

   ctrlbase = widget_base(mainbase,/ROW)
   imgprocbase = widget_base(ctrlbase,/COLUMN)
   rawprocnames=['Raw', 'Processed']
   rawprocdefault=1
   rawprocbutnid = cw_bgroup( imgprocbase, rawprocnames,UVALUE='RawProcMenu', $
                                 SET_VALUE=rawprocdefault, /ROW,/EXCLUSIVE, $
                                 FRAME=3,/NO_RELEASE,LABEL_TOP='IMAGE DISPLAY') 
   editbase = widget_base(ctrlbase,/COLUMN)
   setstatusnames=['Good', 'Bad', 'Pending', 'Unknown']
   setstatusdefault=2
   setstatusbutnid = cw_bgroup(editbase,setstatusnames,UVALUE='SetStatusMenu', $
                                 SET_VALUE=setstatusdefault, /ROW,/EXCLUSIVE, $
                                 FRAME=3,/NO_RELEASE,LABEL_TOP= 'EDIT STATUS')
   state = ptr_new({ $

      ; Data and information in the widget

      ; Widget ids

      mainbase: mainbase, $       ; ID of top level base.
      selectbase: selectbase, $       ; ID of select criteria base.
      mysqldb:database, $
      caltablename:caltablename, $
      imgtablename:imgtablename, $
      insttablename:insttablename, $
      calroot:calroot, $
      imgroot:imgroot, $
      crop:crop, $
      overscan:overscan, $
      nckeylist:nckeylist, $
      InEqualId:ineq, $
      inequalindex:inequalindex, $
      inequalnames:inequalnames, $
      typenames:typenames, $
      typedefaults:typedefaults, $
      statusnames:statusnames, $
      statusdefaults:statusdefaults, $
      datewindow1id:datewindow1, $
      datewindow2id:datewindow2, $
      dblun:dblun, $
      filterwindow1id:filterwindow1, $
      filterwindow2id:filterwindow2, $
      typebutnid:typebutnid, $
      statusbutnid:statusbutnid, $
      listid:listid, $
      listindex:-1, $
      calstatid:calstatid, $
      thumbwin:thumbwin, $
      thumbsize:thumbsize, $
      rawprocbutnid:rawprocbutnid, $
      listimgid:listimgid, $
      listimgindex:-1, $
      thumbtext1id:thumbtext1id, $
      thumbtext2id:thumbtext2id, $
      setstatusnames:setstatusnames, $
      setstatusbutnid:setstatusbutnid, $
      starttime:ptr_new(), $
      frame1:ptr_new(), $
      frame2:ptr_new(), $
      type:ptr_new(), $
      status:ptr_new(), $
      calname:ptr_new(), $
      midtimeimg:ptr_new(), $
      exptimeimg:ptr_new(), $
      siglvlimg:ptr_new(), $
      oscanoiseimg:ptr_new(), $
      oimg:obj_new(), $
      oitool:obj_new(), $
      ; for processed images
      biasname:'', $
      flatname:'', $
      calib:ptr_new(), $
      oldrawprocbtn:-1, $
      itoolreload:0, $
      itoolrefresh:0 $
      })

   ;Realize the main base.
   widget_control, mainbase, /REALIZE

   ;Stash the state structure pointer.
   widget_control, mainbase, SET_UVALUE=state

   chknccal_calib_query, state

   chknccal_calstats, state,calstatstrgs
   widget_control,calstatid, SET_VALUE=calstatstrgs

   ; Give control to the XMANAGER.
   xmanager, 'chknccal', mainbase, $
             EVENT_HANDLER='chknccal_eve',/NO_BLOCK, $
             GROUP_LEADER=mainbase, CLEANUP='chknccal_cleanup'

end
