;+
; NAME: 
;   gpsproc
; PURPOSE: 
;   Process and average a single day GPS record.
; DESCRIPTION:
; CATEGORY:
;   Miscellaneous
; CALLING SEQUENCE:
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
;   94/08/15 - Written by Marc W. Buie, Lowell Observatory
;-
pro gpsproc,file,title=title

on_ioerror,bad

if not exists(file) then begin
   print,'GPSPROC: File ',file,' not found.  Skipping.'
endif else begin

   openr,unit,file,/get_lun

   line=''

;Read through and count the number of lines.
   nobs=0
   while(not eof(unit)) do begin
      readf,unit,line,format='(a1)'
      nobs=nobs+1
   endwhile

   bl=strarr(30)
   for i=0,29 do bl[i]=' '

   ;Rewind file.
   point_lun,unit,0

   jd    = dblarr(nobs)
   tqual = intarr(nobs)
   nsats = intarr(nobs)
   lat   = dblarr(nobs)
   lon   = dblarr(nobs)
   alt   = fltarr(nobs)

   jd0  = 0.0d0
   lat0 = 0.0d0
   lon0 = 0.0d0

   for i=0,nobs-1 do begin
      readf,unit,jd0,tqual0,nsats0,lat0,lon0,alt0, $
         format='(f13.5,2(1x,i1),2(1x,f11.8),1x,f6.1)'
      jd[i]    = jd0
      tqual[i] = tqual0
      nsats[i] = nsats0
      lat[i]   = lat0
      lon[i]   = lon0
      alt[i]   = alt0
   endfor
   free_lun,unit

   jd0 = double(long(jd[0] - 0.5d0))+0.5d0
   caldatm,jd0,year,mon,day,hr,mn,sc

   time = float(jd - jd0)*24.0d0
   meanerr,lat,lat*0+1,meanlat,latsigm,latsig
   meanerr,lon,lon*0+1,meanlon,lonsigm,lonsig
   meanerr,alt,alt*0+1,meanalt,altsigm,altsig

   dlat = (lat - meanlat)*180.0d0/!dpi*3600.0d0
   dlon = (lon - meanlon)*180.0d0/!dpi*3600.0d0
   dalt =  alt - meanalt

   bins = 0.1
   hislat=float(histogram(dlat,binsize=bins))/n_elements(dlat)*100
   xlat=findgen(n_elements(hislat))*bins+min(dlat)
   hislon=float(histogram(dlon,binsize=bins))/n_elements(dlon)*100
   xlon=findgen(n_elements(hislon))*bins+min(dlon)
   bins = 10.0
   hisalt=float(histogram(dalt,binsize=bins))/n_elements(dalt)*100
   xalt=findgen(n_elements(hisalt))*bins+min(dalt)

;   p_multi=!p.multi

   dx1=0.08  ; left, left border
   dx2=0.00  ; left, right border
   dy1=0.00  ; left, bottom border
   dy2=0.00  ; left, top border
   top=0.04
   plot,time,dlat,ytit='Latitude offset (")',xr=[0,24], $
      position=[0.0+dx1,0.75+dy1-top,0.75-dx2,1.0-dy2-top],xticknam=bl
   plot,time,dlon,ytit='Longitude offset (")',xr=[0,24], $
      position=[0.0+dx1,0.5+dy1-top,0.75-dx2,0.75-dy2-top],/noerase,xticknam=bl
   plot,time,dalt,ytit='Altitude offset (m)',xr=[0,24], $
      position=[0.0+dx1,0.25+dy1-top,0.75-dx2,0.5-dy2-top],/noerase,xticknam=bl
   plot,time,nsats,yr=[0,6.5],xtit='UT hours',ytit='# sat & Q', $
      xr=[0,24],position=[0.0+dx1,0.0+dy1+0.12-top,0.75-dx2,0.25-dy2-top], $
      /noerase, yminor=1
   oplot,time,tqual,psym=3

   dx1=0.0   ; left, left border
   dx2=0.01  ; left, right border
   plot,hislat,xlat,psym=10,/noerase,yticknam=bl,yr=minmax(dlat), $
      position=[0.75+dx1,0.75+dy1-top,1.0-dx2,1.0-dy2-top],xticknam=bl
   plot,hislon,xlon,psym=10,/noerase,yticknam=bl,yr=minmax(dlon), $
      position=[0.75+dx1,0.5+dy1-top,1.0-dx2,0.75-dy2-top],xticknam=bl
   plot,hisalt,xalt,psym=10,/noerase,yticknam=bl,yr=minmax(dalt), $
      position=[0.75+dx1,0.25+dy1-top,1.0-dx2,0.5-dy2-top],xticknam=bl

   meanlat = meanlat * 180.0d0 / !dpi
   latsig  = latsig  * 180.0d0 / !dpi
   meanlon = meanlon * 180.0d0 / !dpi
   lonsig  = lonsig  * 180.0d0 / !dpi

   if Keyword_set(title) then xyouts,0.755,0.18,/normal,title

	if meanlat ge 0. then sign='+' else sign=' -'
	mlatd=fix(abs(meanlat))
	tmp=(abs(meanlat)-mlatd)*60.0
	mlatm=fix(tmp)
	mlats=(tmp-mlatm)*60.0
	xyouts,0.755,0.16,/normal,charsize=0.75,'Lat '+sign+ $
      string(mlatd,format='(i2.2)')+':'+ $
      string(mlatm,format='(i2.2)')+':'+ $
      string(mlats,format='(f6.3)')+' +/- '+ $
	   string(latsig*3600,format='(f3.1)')+'" '+ $
      string(latsig/360.0*2*!pi*6378.164*1000.0,format='(f4.1)')+' m'

	if meanlon ge 0. then sign='W' else sign='E'
	mlond=fix(abs(meanlon))
	tmp=(abs(meanlon)-mlond)*60.0
	mlonm=fix(tmp)
	mlons=(tmp-mlonm)*60.0
	xyouts,0.755,0.14,/normal,charsize=0.75,'Lon '+sign+ $
      string(mlond,format='(i3.3)')+':'+ $
      string(mlonm,format='(i2.2)')+':'+ $
      string(mlons,format='(f6.3)')+' +/- '+ $
	   string(lonsig*3600,format='(f3.1)')+'" '+ $
      string(lonsig/360.0*2*!pi*6378.164*1000.0,format='(f4.1)')+' m'

	xyouts,0.755,0.12,/normal,charsize=0.75,'Alt '+ $
      string(meanalt,format='(f7.1)')+' m'+ $
	   string(altsig,format='(f7.1)')+' m'

   xyouts,0.755,0.10,/normal,charsize=0.75,'# obs '+ $
      string(n_elements(dalt),format='(i3)')+ $
      string(fix(float(n_elements(dalt))/720.0*100),format='(i3)')+ $
      '% possible'

   jdstr,jd0,110,str
   xyouts,0.755,0.06,/normal,str

;   !p.multi=p_multi

endelse

return

bad:

print,'bad i/o'
free_lun,unit

end
