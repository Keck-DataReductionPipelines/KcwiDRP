;+
; NAME:
;  findobj
; PURPOSE:
;  Locate image changes with 3-plane color overlays
; DESCRIPTION:
;
; CATEGORY:
;  Astronomy
; CALLING SEQUENCE:
;  findobj,root,image1,image2,image3,NOCLEAN=noclean
;    -or-
;  findobj,image1,image2[,image3]
;
; INPUTS:
;
;  First input format:
;     root - String containing the root of the file name (no path).
;     image1 - Image number (integer) to load into the red image plane.
;     image2 - Image number to load into the green image plane.
;     image3 - Image number to load into the blue image plane (default=image2)
;
;  Second input format:
;     image1 - File name of image to load into red image plane.
;     image2 - File name of image to load into green image plane.
;     image3 - File name of image to load into blue image plane (default=image2).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  BINFAC  - binning factor for image display, default=1
;  EXTEN   - Extension number of subimage to read (default=0, primary)
;  NOCLEAN - Flag, if set will suppress the automatic removal of cosmic rays.
;              Note: this step can take a very long time if you don't have
;              a fast machine.  On an Ultra 1/170 I find I will occasionally
;              supress cleaning.
;  NOSCALE - Flag, if set suppressing the scaling operation when reading the FITS file
;  PATH    - Path where data is to be found, default = current directory
;  QUEUE   - Printer queue for hardcopy, default is chani for color and
;              office for b/w
;  SAVECLEAN - Flag, if set will cause the cleaned image to be saved alongside
;              the original image.  The new file will have 'c' appended to the
;              root of the file name.
;
; OUTPUTS:
;  No explicit outputs but you are prompted for a final hardcopy in either
;    color or black and white.
;
; KEYWORD OUTPUT PARAMETERS:
;  CCUBE   - If provided, this keyword returns a three dimensional byte array
;              of the final stacked color cube displayed in window 3.
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  You must have a 24-bit color display to use this program.
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  97/04/17 - Written by Marc W. Buie, Lowell Observatory
;  97/07/11, MWB, added PATH keyword
;  98/01/05, MWB, added QUEUE keyword
;  98/01/06, MWB, added SAVECLEAN keyword
;  98/03/03, MWB, added second input option
;  98/03/25, MWB, added EXTEN keyword
;  2000/06/06, MWB, added CCUBE keyword
;  2007/11/07, MWB, added NOSCALE keyword, fixed a problem with the binfac keyword
;-
pro findobj,in_root,in_image1,in_image2,in_image3, $
     NOCLEAN=noclean,PATH=path,QUEUE=queue,SAVECLEAN=saveclean,EXTEN=exten, $
     BINFAC=binfac,CCUBE=ccube,NOSCALE=noscale

   sz=size(in_image1)
   IF sz[1] eq 7 THEN BEGIN
      root=''
      image1=in_root
      image2=in_image1
      sz=size(in_image2)
      IF sz[1] ne 0 THEN image3=in_image2 ELSE image3=image2
   ENDIF ELSE BEGIN
      root=in_root
      image1=in_image1
      image2=in_image2
      sz=size(in_image3)
      IF sz[1] ne 0 THEN image3=in_image3 ELSE image3=image2
   ENDELSE

   self='findobj: '
   if badpar(noclean,[0,1,2,3],0,CALLER=self+'(NOCLEAN) ',default=0) then return
   if badpar(path,[0,7],0,CALLER=self+'(PATH) ',default='.') then return
   if badpar(queue,[0,7],0,CALLER=self+'(QUEUE) ', $
                           default='[[default]]') then return
   if badpar(saveclean,[0,1,2,3],0,CALLER=self+'(SAVECLEAN) ',default=0) then return
   if badpar(exten,[0,1,2,3],0,CALLER=self+'(EXTEN) ',default=0) then return
   if badpar(binfac,[0,1,2,3],0,CALLER=self+'(BINFAC) ',default=1) then return
   if badpar(noscale,[0,1,2,3],0,CALLER=self+'(NOSCALE) ',default=0) then return

   path=addslash(path)

   fmt='(a,".",i3.3)'
   cfmt='(a,"c.",i3.3)'

   ; First, look to see if there is already a cleaned image available.  If
   ;   there is, forget the CLEAN keywords and just read that.
   IF root ne '' THEN BEGIN
      imfile1 = string(root,image1,format=cfmt)
      imfile2 = string(root,image2,format=cfmt)
      imfile3 = string(root,image3,format=cfmt)
   ENDIF ELSE BEGIN
      imfile1 = image1
      imfile2 = image2
      imfile3 = image3
   ENDELSE

   if exists(path+imfile1) then begin
      im1=readfits(path+imfile1,hdr1,exten=exten,noscale=noscale,/silent)
   endif else begin
      imfile1 = string(root,image1,format=fmt)
      if not exists(path+imfile1) then begin
         print,'File ',path+imfile1,' could not be found.  Aborting.'
         return
      endif
      raw=readfits(path+imfile1,hdr1,exten=exten,noscale=noscale,/silent)
      if noclean then begin
         im1=raw
      endif else begin
         print,'  ---> removing cosmic rays from ',imfile1
         acre,raw,im1,5,4
         if saveclean then begin
            savename=string(root,image1,format=cfmt)
            writefits,path+savename,im1,hdr1
         endif
      endelse
   endelse

   if exists(path+imfile2) then begin
      im2=readfits(path+imfile2,hdr2,exten=exten,noscale=noscale,/silent)
   endif else begin
      imfile2 = string(root,image2,format=fmt)
      if not exists(path+imfile2) then begin
         print,'File ',path+imfile2,' could not be found.  Aborting.'
         return
      endif
      raw=readfits(path+imfile2,hdr2,exten=exten,noscale=noscale,/silent)
      if noclean then begin
         im2=raw
      endif else begin
         print,'  ---> removing cosmic rays from ',imfile2
         acre,raw,im2,5,4
         if saveclean then begin
            savename=string(root,image2,format=cfmt)
            writefits,path+savename,im2,hdr2
         endif
      endelse
   endelse

   ; Read in image, and some header values
   IF image3 ne image2 THEN BEGIN
      if exists(path+imfile3) then begin
         im3=readfits(path+imfile3,hdr3,exten=exten,noscale=noscale,/silent)
      endif else begin
         imfile3 = string(root,image3,format=fmt)
         if not exists(path+imfile3) then begin
            print,'File ',path+imfile3,' could not be found.  Aborting.'
            return
         endif
         raw=readfits(path+imfile3,hdr3,exten=exten,noscale=noscale,/silent)
         if noclean then begin
            im3=raw
         endif else begin
            print,'  ---> removing cosmic rays from ',imfile3
            acre,raw,im3,5,4
            if saveclean then begin
               savename=string(root,image3,format=cfmt)
               writefits,path+savename,im3,hdr3
            endif
         endelse
      endelse
   ENDIF ELSE BEGIN
      im3=im2
   ENDELSE

   if size(im1,/type) eq 1 then im1=float(im1)
   if size(im2,/type) eq 1 then im2=float(im2)
   if size(im3,/type) eq 1 then im3=float(im3)

   if binfac ne 1 then begin
      arrsz=size(im1)
      nx=(arrsz[1]/binfac)*binfac
      ny=(arrsz[2]/binfac)*binfac
      im1=rebin(im1[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
      im2=rebin(im2[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
      im3=rebin(im3[0:nx-1,0:ny-1],nx/binfac,ny/binfac)
   endif

   imfile1 = string(root,image1,format=fmt)
   imfile2 = string(root,image2,format=fmt)
   imfile3 = string(root,image3,format=fmt)

 help,im1,im2
 
   sz=size(im1)
   nx = sz[1]
   ny = sz[2]

   ; Display image and setup coordinate system on display.
   dbinfac = 2
   dim1 = rebin(im1[0:nx/dbinfac*dbinfac-1,0:ny/dbinfac*dbinfac-1], $
                   nx/dbinfac,ny/dbinfac,sample=0)
   dim2 = rebin(im2[0:nx/dbinfac*dbinfac-1,0:ny/dbinfac*dbinfac-1], $
                   nx/dbinfac,ny/dbinfac,sample=0)
   dim3 = rebin(im3[0:nx/dbinfac*dbinfac-1,0:ny/dbinfac*dbinfac-1], $
                   nx/dbinfac,ny/dbinfac,sample=0)

   ls = -3
   hs = 5
   skysclim,dim1,low1,hi1,mean1,sigma1
   low1 = mean1+ls*sigma1  ; 7
   hi1  = mean1+hs*sigma1 ; 16
   skysclim,dim2,low2,hi2,mean2,sigma2
   low2 = mean2+ls*sigma2
   hi2  = mean2+hs*sigma2
   skysclim,dim3,low3,hi3,mean3,sigma3
   low3 = mean3+ls*sigma3
   hi3  = mean3+hs*sigma3

print,mean1,sigma1
print,mean2,sigma2
print,mean3,sigma3

   setwin,0,xsize=nx/dbinfac,ysize=ny/dbinfac,/show
   bim1 = bytscl(dim1,min=low1,max=hi1,top=255)
   tv,bim1
   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase

   print,'Click on a reference star'
   cursor,xloc1,yloc1,3
   if !mouse.button eq 4 then return
   basphote,5.5,im1,1.0,xloc1,yloc1,10,20,40,/nolog,/silent, $
      xcen=x1m,ycen=y1m,flux=fl1
   oplot,[x1m],[y1m],psym=4,color='ff0000'xl
   x1m = fix(x1m+0.5)
   y1m = fix(y1m+0.5)

   setwin,1,xsize=nx/dbinfac,ysize=ny/dbinfac,/show
   bim2 = bytscl(dim2,min=low2,max=hi2,top=255)
   tv,bim2

   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase
   oplot,[-20,20,20,-20,-20]+x1m,[-20,-20,20,20,-20]+y1m,color='ff2000'xl
   if image2 ne image1 then begin
      cursor,xloc2,yloc2,3
      if !mouse.button eq 4 then return
      basphote,5.5,im2,1.0,xloc2,yloc2,10,20,40,/nolog,/silent, $
         xcen=x2m,ycen=y2m,flux=fl2
      x2m = fix(x2m+0.5)
      y2m = fix(y2m+0.5)
   endif else begin
      xloc2=xloc1
      yloc2=yloc1
      x2m = x1m
      y2m = y1m
      fl2 = fl1
   endelse
   oplot,[x2m],[y2m],psym=4,color='ff0000'xl

   setwin,2,xsize=nx/dbinfac,ysize=ny/dbinfac,/show
   bim3 = bytscl(dim3,min=low3,max=hi3,top=255)
   tv,bim3

   plot,[0],[1],/nodata,xmargin=[0,0],ymargin=[0,0], $
      xr=[0,sz[1]-1],yr=[0,sz[2]-1],xstyle=5,ystyle=5,/noerase
   oplot,[-20,20,20,-20,-20]+x1m,[-20,-20,20,20,-20]+y1m,color='ff2000'xl
   if image3 ne image2 then begin
      cursor,xloc3,yloc3,3
      if !mouse.button eq 4 then return
      basphote,5.5,im3,1.0,xloc3,yloc3,10,20,40,/nolog,/silent, $
         xcen=x3m,ycen=y3m,flux=fl3
      x3m = fix(x3m+0.5)
      y3m = fix(y3m+0.5)
   endif else begin
      xloc3=xloc2
      yloc3=yloc2
      x3m = x2m
      y3m = y2m
      fl3 = fl2
   endelse
   oplot,[x3m],[y3m],psym=4,color='ff0000'xl

   dleft  = min([x1m,x2m,x3m])
   dright = min(nx-[x1m,x2m,x3m])-1
   dup    = min(ny-[y1m,y2m,y3m])-1
   ddown  = min([y1m,y2m,y3m])

   print,'Image 2-1 offset ',x2m-x1m,y2m-y1m
   print,'Image 3-1 offset ',x3m-x1m,y3m-y1m
   print,'Image 1 section ',x1m-dleft,x1m+dright,y1m-ddown,y1m+dup
   print,'Image 2 section ',x2m-dleft,x2m+dright,y2m-ddown,y2m+dup
   print,'Image 3 section ',x3m-dleft,x3m+dright,y3m-ddown,y3m+dup
   print,'fluxes ',fl1,fl2,fl3,fl2/fl1,fl3/fl1
   print,'means  ',mean1,mean2,mean3
   print,'sigmas ',sigma1,sigma2,sigma3
   sig=(sigma1+sigma2+sigma3)/3.0
   sig1 = sig
   sig2 = sig*fl2/fl1
   sig3 = sig*fl3/fl1

   xsz=dright+dleft+1
   ysz=dup+ddown+1

   setwin,3,xsize=xsz,ysize=ysz,/show
   fim1 = bytscl(im1,min=mean1+ls*sig1,max=mean1+hs*sig1,top=255)
   fim2 = bytscl(im2,min=mean2+ls*sig2,max=mean2+hs*sig2,top=255)
   fim3 = bytscl(im3,min=mean3+ls*sig3,max=mean3+hs*sig3,top=255)
   ccube=0
   ccube = $
      [[[fim1[x1m-dleft:x1m+dright,y1m-ddown:y1m+dup]]], $
       [[fim2[x2m-dleft:x2m+dright,y2m-ddown:y2m+dup]]], $
       [[fim3[x3m-dleft:x3m+dright,y3m-ddown:y3m+dup]]]]

   tv,ccube,true=3

   ans=''
   read,ans,prompt='Hardcopy? (c/b/n)'
   IF strmid(ans,0,1) eq 'c' THEN BEGIN
      if queue eq '[[default]]' then pqueue = 'chani' else pqueue = queue
      npix = max([xsz,ysz])
      IF npix lt 600 THEN $
         scale = 25.0 $
      ELSE IF npix lt 1064 THEN $
         scale = 56.0 $
      ELSE IF npix lt 2100 THEN $
         scale = 112.0 $
      ELSE $
         scale = float(npix)/15.0
      hardim,[[[fim1[x1m-dleft:x1m+dright,y1m-ddown:y1m+dup]]], $
              [[fim2[x2m-dleft:x2m+dright,y2m-ddown:y2m+dup]]], $
              [[fim3[x3m-dleft:x3m+dright,y3m-ddown:y3m+dup]]]],0,255, $
             autosize=1,queue=pqueue,true_color=3,width=npix/scale, $
             title='R:'+imfile1+' G:'+imfile2+' B:'+imfile3
   ENDIF ELSE IF strmid(ans,0,1) eq 'b' THEN BEGIN
      if queue eq '[[default]]' then pqueue = 'office' else pqueue = queue
      npix = max([xsz,ysz])
      IF npix lt 600 THEN $
         scale = 25.0 $
      ELSE IF npix lt 1064 THEN $
         scale = 56.0 $
      ELSE IF npix lt 2100 THEN $
         scale = 112.0 $
      ELSE $
         scale = float(npix)/15.0
      hardim,fim1,0,255,title=imfile1, $
         autosize=1,queue=pqueue,/negative,width=npix/scale
   ENDIF

end
