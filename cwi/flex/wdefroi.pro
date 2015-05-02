	FUNCTION wdefroi, idata, xverts, yverts, zoom=zoom, $
			sub=sub, logscale=logscale, image=image, time=time, $
			lc=lc, wherefill=wherefill
;	--------------------------------------------------------------
;+							31-May-94
;	NAME: wdefroi
;
;	Purpose: Define a region of interest via the outline of a
;		rectangle, polygon or intensity contour and return 
;		the indices or SUB-Image that contains the region.
;
;	Calling Sequence:
;		list = wdefroi( data, xverts, yverts, [zoom=zoom, 
;				sub=sub, image=image
;				time=time, lc=lc, logscale=logscale,
;                               wherefill=wherefill])
;	Where,	
;	
;	INPUT:
;		data		is the image for which an ROI is wanted.
;		xverts		Optional output of x-coordin. outline region
;		yverts		Optional output of y-coordin. outline region
;		zoom		is the zoom factor (default is 1).
;		sub		is the image index number to use for ROI
;				selection if data is 3-dimen.
;		image		if set will return IMAGE(S) instead of indices
;		time		list of times which corresponds with data
;				a Yohkoh index record is also ok.
;		lc		lc activiate light curve option ...this uses
;				LOTS of MEMORY
;		logscale	if set will log scale the data for display.
;		wherefill	if set will use where instead of ployfillv
;				to define the indices of enclosed region.
;				(wherefill works better than ployfillv for
;				images with a very small number of pixels)
;
;	OUTPUT:
;		list 		is the returned list of selected indices of
;				the input image OR 
;				a regular sub-image(s) that contains the 
;				region if "/image" switch is used.
;
;	History:
;		written by GAL, July-93 
;		24-Jul-93, added xverts and yverts
;		25-Jul-93, added image keyword option to return sub-image.
;		24-Oct-93, added tv command to display images if stepper
;			is not involked... and changed scale to logscale
;			to be more descriptive of the action.
;		26-Oct-93, minor mod to ensure bytscl of displayed images.
;		31-May-94, add wherefill keyword
;	----------------------------------------------------------------
;-

	common verts, xerts, yerts		
	common sroi, idxb, idxp, idxc, boxout, idx
	common lcroi, edata, etime, nimg, xs, ys, ilist
	common kywrd, awherefill

	awherefill = keyword_set(wherefill)	;1 if set, else zero

	IF n_elements(zoom) eq 0 Then zoom = 1	;default zoom

	szdat = SIZE(idata)
	xs = szdat(1)
	ys = szdat(2)
	ndim = szdat(0)			;2 or 3 dim data
 	d_type = szdat(szdat(0)+1)	;get data-type of img


;	Define the image to "select and cut" from:	
	if ndim eq 3 then begin		;3-d data
	  nimg = szdat(3)		;number of images
	  if keyword_set(lc) then edata = idata 
	  if n_elements(time) ne 0 then etime = anytim2ints(time)
	  if n_elements(sub) ne 0 then begin
;	    if (!d.window ne -1) then wshow
	    data = idata(*,*,sub)
	    if Keyword_set(logscale) then data = Alog10(data>1)	;log scale
	    tvscl, rebin(data, xs*zoom, ys*zoom, /sample)
          endif else begin
	    if Keyword_set(logscale) then Begin
	      data = idata		;have to make copy
	      data = Alog10(data>1)	;log scale
	      stepper, data, xsize=xs*zoom, ysize=ys*zoom, lastss=sub
	      data = data(*,*,sub)
	    endif else begin
	      stepper, idata, xsize=xs*zoom, ysize=ys*zoom, lastss=sub
	      data = idata(*,*,sub)	
	    endelse
	  endelse
	endif else begin		;2-d data
	  data = idata
	  if Keyword_set(logscale) then data = Alog10(data>1)	;log scale
	  tvscl, rebin(data, xs*zoom, ys*zoom, /sample)
	endelse



;	Parent-Base widget:
	base_ptr = WIDGET_BASE(title='wdefroi', /column)


;	Some top buttons:
	txt1 = WIDGET_LABEL(base_ptr, $ 
		value= 'Region Of Interest Selection')
	XMENU, ['Box', 'Polygon', 'Contour', 'Xloadct'], base_ptr, /row, /frame
		
;	Message to User:
	txt2 = WIDGET_LABEL(base_ptr, value='Instructions')
	info2 = WIDGET_BASE(base_ptr,/row,/frame)

	info2p = WIDGET_TEXT(info2, xsize=35+17, ysize=10, $
		value = ['Watch for Messages here.', $
		'Message is updated for each type of ROI selected.', $
		' ', 'Use the Upper three buttons to define an ROI', $
		' ', 'Then use the lower buttons to:', $
		'Select  and isolate an ROI for another ROI', $
		'Reset   or Redraw the original image', $
		'L C     or Light Curve of SUMMED pixels in time', $
		'Exit    or Return to IDL'])

;	Contour Controls:
	txt3 = WIDGET_LABEL(base_ptr, value='Contour Controls')
	info3 = WIDGET_BASE(base_ptr, /column, /frame)

	if d_type ne 1 then begin
	  info3p_lev = WIDGET_SLIDER(info3, title='Contour level', $
		max = max(fix(bytscl(data))) )
	endif else begin
	  info3p_lev = WIDGET_SLIDER(info3, title='Contour level', $
		max = max(fix(data)) )
	endelse

	info4 = WIDGET_BASE(info3, /row, /frame)

	info4p_lab = WIDGET_LABEL(info4, value='Max value is:')
	if d_type ne 1 then begin
	  info4p_max = WIDGET_TEXT(info4, xsize=10, ysize=1, $
		value = string(fix(max(bytscl(data)))))	
	endif else begin
	  info4p_max = WIDGET_TEXT(info4, xsize=10, ysize=1, $
		value = string(fix(max(data))))
	  ;Note: if data is byte...must convert to int. before string	
	endelse

	info4p_lab2= WIDGET_LABEL(info4, value='Mouse level:')
	info4p_lvl2= WIDGET_TEXT(info4, xsize=10, ysize=1, $
		value = string(0) )

;	Control widgets
	cntrl = WIDGET_BASE(base_ptr,/row)
;	tstp = widget_button(cntrl, value='Test', uvalue='Test')
	
	selctp = widget_button(cntrl, value='Select', uvalue='Select')

	resetp = widget_button(cntrl, value='Reset', uvalue='Reset')

	lcp    = widget_button(cntrl, value='Light Curve', $
			uvalue='Light Curve')
	if not keyword_set(lc) then WIDGET_CONTROL, lcp, sensitive=0

	exitp = widget_button(cntrl, value='Exit', uvalue='Exit')

	info5 = WIDGET_BASE(base_ptr,/row, /frame)
	npixlp = widget_LABEL(info5, value='Number of Selected Pixels:')
	npixp  = widget_text(info5, xsize=10, ysize=1, value= '0')
	
;	---------------------
	WIDGET_CONTROL, base_ptr, /realize

;	create user value for 

	etop = MAKE_STR("{dummy,"	+ $
		"dat: bytarr("+string(szdat(1))+","+string(szdat(2))+"),"	+ $
		"tmp: bytarr("+string(szdat(1))+","+string(szdat(2))+"),"	+ $
		"nx: 0,"	+ $
		"ny: 0,"	+ $
		"zm: 0,"	+ $
		"tmp_flg: 0,"	+ $
		"idb_flg: 0,"	+ $
		"idp_flg: 0,"	+ $
		"idc_flg: 0,"	+ $
		"mess_ptr: 0L,"	+ $
		"lev_ptr: 0L,"	+ $
		"max_ptr: 0L,"	+ $
                "mlv_ptr: 0L,"  + $
		"npx_ptr: 0L,"  + $
		"selctp:  0L,"  + $
		"resetp:  0L,"  + $
		"exitp:   0L    }")

	if d_type ne 1 then begin
	  etop.dat = bytscl(data)	;just byte scale for now
	  etop.tmp = bytscl(data)
	endif else begin		
	  etop.dat = data
	  etop.tmp = data
	endelse
	etop.nx = xs
	etop.ny = ys
	etop.zm = zoom
	etop.tmp_flg = 0
	etop.idb_flg = 0
	etop.idp_flg = 0
	etop.idc_flg = 0
	etop.mess_ptr = info2p
	etop.lev_ptr = info3p_lev
	etop.max_ptr = info4p_max
	etop.mlv_ptr = info4p_lvl2
	etop.npx_ptr = npixp
	etop.selctp  = selctp
	etop.resetp  = resetp
	etop.exitp   = exitp

	idxb = 0 & idxp = 0 & idxc = 0

	widget_control, set_uvalue=etop, base_ptr

	Xmanager, 'sxt_wdefroi', base_ptr, event='wdefroi_event'

;	print, 'exit from event manager'
;	help, idxb, idxp, idxc
;	help, etop, /str

;	if etop.idb_flg then idx = idxb
;	if etop.idp_flg then idx = idxp
;	if etop.idc_flg then idx = idxc

;	stop, 'stop before return with idl'

;	idata, nimg, xs, ys, ilist
	if ndim eq 3 then begin		;3-d data        
	  ref_no = indgen(nimg)*1L* LONG(xs*ys)
	  indices = lonarr(n_elements(idx), nimg)
	  FOR i = 0, nimg-1 do begin
	    indices(0,i) = LONG(idx) + ref_no(i)
	  ENDFOR
	  idx = indices
	endif
;	help, xerts, yerts

	if n_elements(xerts) ne 0 then xverts = xerts	;update
	if n_elements(yerts) ne 0 then yverts = yerts

	If keyword_set(image) then begin
	  cub = idx2img(idata, idx, xverts, yverts)	;get sub-image
	  RETURN, cub
	endif
	
	RETURN, idx
     END
