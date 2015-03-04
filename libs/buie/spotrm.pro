;+
; NAME:
;  spotrm
; PURPOSE:
;  Spot remover for images
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  spotrm,image
; INPUTS:
;  image - Image to manipulate
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2000/10/16 - Written by Marc W. Buie, Lowell Observatory
;-
pro spotrm,image

   sz = size(image,/dim)
   nx = sz[0]
   ny = sz[1]

   savec=5
   loadct,0,bottom=savec,/silent
   tvlct,red,gre,blu,/get
   ; 0 black
   ; 1 green
   ; 2 reddish
   ; 3 yellow
   ; 4 red
   red[0]=0
   gre[0]=0
   blu[0]=0
   red[1]=0
   gre[1]=255
   blu[1]=0
   red[2]=200
   gre[2]=50
   blu[2]=80
   red[3]=255
   gre[3]=255
   blu[3]=0
   red[4]=255
   gre[4]=0
   blu[4]=0
   tvlct,red,gre,blu

   rspot = 20
   rkill =  3
   a=(findgen(21)/20)* 2.0 * !pi
   xsp = cos(a)
   ysp = sin(a)

   setwin,0,xsize=nx,ysize=ny

   skysclim,image,lowval,hival,meanval,sigma
   lowval = meanval - 3.0*sigma
   hival  = meanval + 5.0*sigma

   bim = bytscl(image,min=lowval,max=hival,top=!d.n_colors-1-savec)+savec
   tv,bim
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[0]-1],yr=[0,sz[1]-1],xstyle=5,ystyle=5,/noerase

   repeat begin
      print,'     Click left to remove something (right=quit).'
      cursor,xs1,ys1,3
      if !mouse.button ne 4 then begin
         if !mouse.button eq 1 then begin
            getannul,image,xs1,ys1,0.0,rspot,data,idx
            bad=bytarr(n_elements(idx))
            robomean,data,3.0,0.5,mean,avgdev,stddev,bad=bad
            print,mean,stddev
            zg = where(bad eq 0,countzg)
            zb = where(bad eq 1,countzb)
            bidx=idx
         endif else begin
            getannul,image,xs1,ys1,rkill+2,rspot,data,idx
            bad=bytarr(n_elements(idx))
            robomean,data,3.0,0.5,mean,avgdev,stddev,bad=bad
            zg = where(bad eq 0,countzg)
            print,mean,stddev
            getannul,image,xs1,ys1,0.0,rkill,data,bidx
            countzb = n_elements(bidx)
            zb = lindgen(countzb)
         endelse
         if countzb gt 0 then begin
            print,'replacing ',strn(countzb),' pixels.'
            j=fix(randomu(seed,countzb)*(countzg-1)+0.5) < (countzg-1)
            image[bidx[zb]] = image[idx[zg[j]]]
            bim = bytscl(image,min=lowval,max=hival,top=!d.n_colors-1-savec)+savec
            tv,bim
         endif
         oplot,rspot*xsp+xs1,rspot*ysp+ys1,color=4
      endif

   endrep until !mouse.button eq 4

end
