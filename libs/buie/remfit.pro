;+
; NAME:
;  remfit
; PURPOSE:
;  Remove old pointing model from new data, create new pointing map data.
; DESCRIPTION:
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  remfit,datafile,coeffs,newdata
; INPUTS:
;  datafile - Name of pointing data file (created by pntcol.pro)
;  coeffs   - Name of file that contains the current pointing data.
;  newdata  - Name of file to contain the new raw pointing data.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/12/20
;-
pro remfit,datafile,coeffs,newdata

   readcol,datafile,jd,hra,hdec,cra,cdec,format='d,a,a,a,a'
   hra_r  = raparse(hra)
   hdec_r = decparse(hdec)
   cra_r  = raparse(cra)
   cdec_r = decparse(cdec)

   readcol,coeffs,coef,format='d'

   lat =  35.0966833d0  ; Larry's values
   lon = 111.5359167d0

   ;lat=35.0967189d0    ; GPS value for 42"
   ;lon=111.5365053d0

   lat_r=lat/180.0d0*!dpi
   lon_r=lon/180.0d0*!dpi

   ; Convert all coordinates to ofdate.
   jd2year,jd,yr
   yr=mean(yr)
   precess,hra_r,hdec_r,2000.0,yr,/radian
   precess,cra_r,cdec_r,2000.0,yr,/radian

   hra    = hra_r*180.0d0/!dpi
   hdec   = hdec_r*180.0d0/!dpi
   cra    = cra_r*180.0d0/!dpi
   cdec   = cdec_r*180.0d0/!dpi

   ; Compute hour angle
   hangle,jd,hra_r,hdec_r,lat_r,lon_r,hha_r
   hangle,jd,cra_r,cdec_r,lat_r,lon_r,cha_r

   hha = hha_r*180.0d0/!dpi
   cha = cha_r*180.0d0/!dpi

   ; Remove old pointing model from header ha,dec
   pntfix3,coef,cha,cdec,hafix,decfix,dha,ddec
   oha  = hha  - dha/3600.0d0
   odec = hdec - ddec/3600.0d0

   ; output will be haobs, decobs, hatrue, dectrue, junk integer, flag (T/F)
   ; ha in decimal hours, dec in decimal degrees
   hha  = oha / 15.0
   cha  = cha / 15.0
   hdec = odec
   cdec = cdec

   openw,lun,newdata,/get_lun
   for i=0,n_elements(hha)-1 do begin
      printf,lun,hha[i],hdec[i],cha[i],cdec[i],1000,'T', $
         format='(4f15.8,2x,i8,2x,a1)'
   endfor
   free_lun,lun

end
