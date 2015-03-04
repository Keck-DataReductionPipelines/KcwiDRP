;+
; NAME:
;  mosastrom
; PURPOSE:   (one line only)
;  Astrometry solution for multi-detector image sets
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  mosastrom,name,xoffset,yoffset,rotang
; INPUTS:
;  name    - String that defines the name of the image to process
;  xoffset - Vector, one element per image in the mosaic, that describes
;               the offset of each relative to the optical axis.  The actual
;               definition is that this is the X position of the optical
;               axis in the native pixel coordinates of each image.
;  yoffset - Vector, one element per image in the mosaic, that describes
;               the offset of each relative to the optical axis.  The actual
;               definition is that this is the Y position of the optical
;               axis in the native pixel coordinates of each image.
;  rotang  - Rotation of each image relative to the anchor image. (radians)
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ANCHOR - Number of the image that is the anchor upon which the
;              monolithic coordinate system is to be based.  The default
;              is the one whose (0,0) pixel is closest to the optical axis.
;              Note that this value is a number that points into the
;              input xoffset,yoffset arrays and starts at 0.
;  KEYLIST  - Name of a file containing a correspondence list. This list
;                associates a set of standard names with the actual keyword
;                names found in a FITS file header. If this keyword is
;                omitted, a default list is used (see loadkeys.pro)
;  NEWTERMS - String array with the names of the terms to use in the fit.
;              Default=use the terms as found in the initial astrometric
;              solution.
;  NEWRENORM - This keyword allows you to change the renormalization
;                 factor for the astrometric solution.  The default is
;                 to use the value that is found in fitcoeff.dat.
;  PATH   - String, name of the directory where the images can be found.
;              Default=current directory
;  CATDIR - Sring, name of the directory where the catalog files can be found
;              default is the current directory
;  SRCDIR - String, name of the directory where the source files can be found
;              Default='Src' (relative to the current directory).
;  REFDIR - STring, name of the directory where the correlated star/catalog
;              lists are stored, Default='Refstars'
;  SUBARU - Flag, if set indicates that the file and naming scheme from
;              the Subaru Observatory is to be used (individual files for
;              each image in the mosaic).  If this not set the default
;              file behavior is to look for a multi-group FITS file that
;              contains all images in one file.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/01/14, Written by Marc W. Buie, Southwest Research Institute
;-
pro mosastrom,name,xoffset,yoffset,rotang, $
       ANCHOR=anchor,CATDIR=catdir,KEYLIST=keylist,NEWTERMS=newterms, $
       PATH=path,SRCDIR=srcdir,REFDIR=refdir,SUBARU=subaru, $
       NEWRENORM=newrenorm

   self='mosastrom: '
   if badpar(name,7,0,caller=self+'(name) ') then return
   if badpar(xoffset,[2,3,4,5],1,caller=self+'(xoffset) ', $
                                npts=nxoff) then return
   if badpar(yoffset,[2,3,4,5],1,caller=self+'(yoffset) ', $
                                npts=nyoff) then return
   if badpar(rotang,[2,3,4,5],1,caller=self+'(rotang) ', $
                                npts=nrot) then return

   if badpar(anchor,[0,1,2,3],0,caller=self+'(ANCHOR) ', $
                                default=-1) then return
   if badpar(catdir,[0,7],0,CALLER=self+'(CATDIR) ', $
                              default='') then return
   if badpar(keylist,[0,7],0,CALLER=self+'(KEYLIST) ', $
                              default='[[DEFAULT]]') then return
   if badpar(newterms,[0,7],[0,1],caller=self+'(NEWTERMS) ', $
                                default='') then return
   if badpar(path,[0,7],0,caller=self+'(PATH) ', $
                                default='') then return
   if badpar(srcdir,[0,7],0,caller=self+'(SRCDIR) ', $
                                default='Src/') then return
   if badpar(refdir,[0,7],0,caller=self+'(REFDIR) ', $
                                default='Refstars/') then return
   if badpar(subaru,[0,1,2,3],0,caller=self+'(SUBARU) ', $
                                default=1) then return

   if nxoff ne nyoff or nxoff ne nrot then begin
      print,self,'xoffset,yoffset,rotang must all have the same length'
      return
   endif

   nimages = nxoff

   if subaru then begin
      if nimages ne 10 then begin
         print,self,'Subaru mosaic has 10 images and offset,angle does not.'
         return
      endif
   endif

   if anchor lt -1 and anchor ge nimages then begin
      print,self,'anchor frame number is out of range.'
      return
   endif

   if path ne '' then begin
      path=addslash(path)
      if not exists(path) then begin
         print,self,'path to image [',path,'] does not exist.'
         return
      endif
   endif

   catdir=addslash(catdir)
   if not exists(catdir) then begin
      print,self,'catalog file directory [',catdir,'] does not exist.'
      return
   endif

   srcdir=addslash(srcdir)
   if not exists(srcdir) then begin
      print,self,'source file directory [',srcdir,'] does not exist.'
      return
   endif

   refdir=addslash(refdir)
   if not exists(refdir) then begin
      print,self,'reference directory [',refdir,'] does not exist.'
      return
   endif

   ; Get header correspondence list.
   loadkeys,keylist,hdrlist

   ; resolve the name
   if subaru then begin
      nexten=10
      idx=indgen(nexten)
      sidx=strcompress(string(idx),/remove_all)
      imname = name+sidx+'.fits'
      exten  = replicate(0,10)
      fitname = name+sidx
      fnsrc  = name+sidx+'.src'
      fnref  = name+sidx+'.ref'
      chipno = idx
   endif else begin
      nexten=n_elements(xoffset)
      idx=indgen(nexten)
      imname = name
      exten  = idx+1
      fitname = name+'x'+strb36(exten)
      words=strsplit(name,'.')
      num=fix(words[1])
      fnsrc  = string(num,format='(i3.3)')+'.srcx'+strb36(exten)
      fnref  = string(num,format='(i3.3)')+'x'+strb36(exten)+'.ref'
      chipno = exten
   endelse

   if not exists(path+imname[idx[0]]) then begin
      print,self,'File ',path+imname[idx[0]],' not found.'
      return
   endif
   hdr=headfits(path+imname[idx[0]],exten=exten[0])
   parsekey,hdr,hdrlist,hdrinfo
   nx=sxpar(hdr,'NAXIS1')
   ny=sxpar(hdr,'NAXIS2')

   nval=11
   xborder = findgen(nval)/10.0*(nx-1)
   yborder = replicate(0.,nval)
   xborder = [xborder,replicate(nx-1.0,nval)]
   yborder = [yborder,findgen(nval)/10.0*(ny-1)]
   xborder = [xborder,reverse(findgen(nval)/10.0*(nx-1))]
   yborder = [yborder,replicate(ny-1.0,nval)]
   xborder = [xborder,replicate(0.,nval)]
   yborder = [yborder,reverse(findgen(nval)/10.0*(ny-1))]

   ; resolve the anchor, when done, the idx array points to the images
   ;   in order of increasing distance from the anchor and the anchor is
   ;   the first image in the list.
   if anchor eq -1 then begin
      xmid = nx/2.0
      ymid = ny/2.0
      sep=sqrt((xoffset-xmid)^2+(yoffset-ymid)^2)
   endif else begin
      sep=sqrt((xoffset-xoffset[anchor])^2+(yoffset-yoffset[anchor])^2)
   endelse
   idx=sort(sep)

   ; twist the file names to match the sorted order
   imname  = imname[idx]
   exten   = exten[idx]
   fitname = fitname[idx]
   fnsrc   = fnsrc[idx]
   fnref   = fnref[idx]
   chipno  = chipno[idx]
   if subaru then begin
      starfile = strlowcase(hdrinfo.object)+'_'+strb36(idx)+'.cat'
   endif else begin
      starfile = strlowcase(hdrinfo.object)+'x'+strb36(exten)+'.cat'
   endelse

   ; Get the astrometric solution for the anchor, this comes from the posted
   ;   solution in the fitcoeff.dat file.  First load the coefficients,
   ;   and then resolve any missing renormfac information.
   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,prot,renormfac,cra,cdec,photzp, $
      terms,coeffarr,ncoeffs,nlines
   z=where(renormfac lt 0,count)
   if count ne 0 then renormfac[z] = sqrt(nx^2+ny^2)

   ; find the solution we need to get started
   z=where(fitname[0] eq ffn,count)
   if count ne 2 then begin
      print,self,'No astrometric solution found for ',imname[0]
      return
   endif

   if ftype[z[0]] eq 'eta' then begin
      floce = z[0]
      flocx = z[1]
   endif else begin
      floce = z[1]
      flocx = z[0]
   endelse

   if badpar(newrenorm,[0,2,3,4,5],0,caller=self+'(NEWRENORM) ', $
                                     default=renormfac[flocx]) then return

   ; Extract the coefficients
   cxi  = trimrank(coeffarr[flocx,*])
   ceta = trimrank(coeffarr[floce,*])

   ; setup the astrometric solution
   astinfo = { $
      renormfac: renormfac[flocx], $
      cxi: cxi, $
      ceta: ceta, $
      prot: prot[flocx], $
      terms: terms, $
      xcref: xc[flocx], $
      ycref: yc[flocx], $
      raref: cra[flocx], $
      decref: cdec[flocx] $
      }

   ; compute position of optical axis based on this solution.
   astxy2rd,xoffset[idx[0]],yoffset[idx[0]],astinfo,raopt,decopt,/full

   if newterms[0] eq '' then newterms=terms

   cd1_1= cos(rotang)
   cd1_2= sin(rotang)
   cd2_1= -1.0*sin(rotang)
   cd2_2= cos(rotang)

   for i=0,nimages-1 do begin
      if not exists(path+imname[i]) then begin
         print,self,'Image file ',path+imname[i],' not found.'
         return
      endif
      if exten[i] eq 0 then $
         print,'Processing   ',imname[i] $
      else $
         print,'Processing   ',imname[i],' extension ',strn(exten[i])

      if not exists(srcdir+fnsrc[i]) then begin
         print,self,'Source file ',srcdir+fnsrc[i],' not found.'
         return
      endif
      print,'Source file  ',fnsrc[i]
      if not exists(catdir+starfile[i]) then begin
         print,self,'Catalog file ',catdir+starfile[i],' not found.'
         return
      endif
      print,'Catalog file ',starfile[i]

      ; set the offset and rotation in the astinfo structure, don't do this
      ;    for the anchor frame.  That needs to use the original values from
      ;    the incoming solution.
      if i ne 0 then begin
         astinfo.xcref = xoffset[idx[i]]
         astinfo.ycref = yoffset[idx[i]]
         astinfo.prot  = rotang[idx[i]]
      endif

      ; show the image and the initial guess on how the catalog overlaps
      showsrc,path+imname[i],srcdir+fnsrc[i],binfac=4,windownum=7
      rdstarc,catdir+starfile[i],cra,cdec,dummy,smag,nfound
      astrd2xy,cra,cdec,astinfo,cx,cy,/full
      oplot,cx,cy,psym=4,color='00ffff'xl,symsize=2

      ; generate a new ref list
      refgen,path+imname[i],astinfo,srcdir+fnsrc[i],catdir+starfile[i], $
             ref,exten=exten[i]
      if ref.nstars eq 0 then begin
         print,self,'No overlapping stars found.  Aborting.'
         showsrc,path+imname[i],srcdir+fnsrc[i],binfac=4
         rdstarc,catdir+starfile[i],cra,cdec,dummy,smag,nfound
         astrd2xy,cra,cdec,astinfo,cx,cy,/full
         setusym,-1
         cx0 = mean(minmax(cx))
         cy0 = mean(minmax(cy))
         oplot,cx-cx0+1024,cy-cy0+2048,psym=8,color='00ffff'xl,symsize=3
         print,minmax(cx)
         print,minmax(cy)
         help,astinfo,/st
         print,astinfo.terms
         setusym,1
         return
      endif
      print,strn(ref.nstars),' source/catalog matches found.'

      ; show the image and the initial guess on how the catalog overlaps
help,ref,/st
      showsrc,path+imname[i],srcdir+fnsrc[i],binfac=4,windownum=8
      rdstarc,catdir+starfile[i],cra,cdec,dummy,smag,nfound
      astrd2xy,cra,cdec,astinfo,cx,cy,/full
      oplot,ref.xpos,ref.ypos,psym=5,color='00ffff'xl,symsize=2


      ; tweak rotation and offset
      if i ne 0 then begin
         xoff = xoffset[idx[i]]
         yoff = yoffset[idx[i]]
         rota = rotang[idx[i]]
         tastinfo=astinfo
         tastinfo.xcref = 0.0
         tastinfo.ycref = 0.0
         tastinfo.prot  = 0.0
         chiprot,tastinfo,ref,xoff,yoff,rota
         print,'old ',xoffset[idx[i]],yoffset[idx[i]],rotang[idx[i]]
         print,'new ',xoff,yoff,rota
         xoffset[idx[i]] = xoff
         yoffset[idx[i]] = yoff
         rotang[idx[i]]  = rota
         astinfo.xcref = xoffset[idx[i]]
         astinfo.ycref = yoffset[idx[i]]
         astinfo.prot  = rotang[idx[i]]
         cd1_1= cos(rotang)
         cd1_2= sin(rotang)
         cd2_1= -1.0*sin(rotang)
         cd2_2= cos(rotang)
      endif

      ; convert these x,y coordinates to the anchor monolith coordinates
      newx = ref.xpos
      newy = ref.ypos
      x0 = ref.xpos-xoffset[idx[i]]
      y0 = ref.ypos-yoffset[idx[i]]
      newxp = cd1_1[idx[i]]*x0 + cd1_2[idx[i]]*y0
      newyp = cd2_1[idx[i]]*x0 + cd2_2[idx[i]]*y0

      ; concatenate newref information into a master list for the whole thing
      if i eq 0 then begin
         x  = newx
         y  = newy
         xp = newxp
         yp = newyp
         ra = ref.ra
         dec = ref.dec
         cno = replicate(chipno[i],ref.nstars)
         nstars = ref.nstars
      endif else begin
         x = [x,newx]
         y = [y,newy]
         xp = [xp,newxp]
         yp = [yp,newyp]
         ra = [ra,ref.ra]
         dec = [dec,ref.dec]
         cno = [cno,replicate(chipno[i],ref.nstars)]
         nstars = nstars+ref.nstars
      endelse

      setwin,0
      plot,xp,yp,psym=3
      for j=0,i do begin
         x0 = xborder-xoffset[idx[j]]
         y0 = yborder-yoffset[idx[j]]
         newxp = cd1_1[idx[j]]*x0 + cd1_2[idx[j]]*y0
         newyp = cd2_1[idx[j]]*x0 + cd2_2[idx[j]]*y0
         oplot,newxp,newyp,color='0000ff'xl
         xyouts,mean(newxp),mean(newyp),strn(idx[j]), $
            align=0.5,color='0000ff'xl,charsize=1.5
      endfor

      ; fit for a new solution using the master list
      astrd2sn,ra,dec,raopt,decopt,xi,eta,/arcsec
      dx = xp/newrenorm
      dy = yp/newrenorm
      bad=bytarr(nstars)
      astsolve,dx,dy,xi,eta,newterms,newrenorm,bad,cxi,ceta, $
         xiscat=xiscat,etascat=etascat,cxisig=cxisig,cetasig=cetasig

      ; save the fit to a new astinfo structure, rotations and offset
      ;   are not needed here, just use dummy values to make the point clear.
      astinfo = { $
         renormfac: newrenorm, $
         cxi: cxi, $
         ceta: ceta, $
         prot: 0.0, $      ; the correct value is rotang[idx[i]]
         terms: newterms, $
         xcref: 0.0, $     ; the correct value is xoffset[idx[i]]
         ycref: 0.0, $     ; the correct value is yoffset[idx[i]]
         raref: raopt, $
         decref: decopt $
         }

;      if i eq 4 then break

      ans=''
      read,prompt='Chip '+strn(idx[i])+', '+strn(i)+' of '+strn(nimages)+'> ',ans
      if ans eq 'q' then return

   endfor

end
