;+
; NAME: 
;  imsalt
; PURPOSE:   (one line only)
;  Implants fake point-source images into an image of the sky.
; DESCRIPTION:
;  IMSALT will take four vectors of the same length (exten, x_pos, y_pos,
;    and scale) that describe the false objects to be put in FITS images
;    of real stars.  These vectors are applied in order to insert psf(s)
;    of the appropriate parameters in the appropriate image(s) or extension.
;    IMSALT then writes a FITS file of the same name, with the false data
;    inserted.  It is important to specify the paths of the input files
;    and/or the two output files, since SALTDES resists overwriting the
;    original data; it will not run if the name and path of the output is
;    the same as the input. 
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  imsalt,imname,x_pos,y_pos,scale,exten
; INPUTS:
;  imname     :String, the name of the fits file to be read
;  x_pos      :Vector, x locations on image
;  y_pos      :Vector, y locations on image 
;  mag        :Vector, magnitude of fake objects
;  exten      :Vector, chooses which extensions are to contain false
;              objects and how many in each. EX: exten=[1,1,7,5]
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  KEYFILE   - Name of a file containing a correspondence list. This list
;                 associates a set of standard names with the actual keyword
;                 names found in a FITS file header. If this keyword is
;                 omitted, a default list is used, as if a file with the
;                 following contents had been supplied:
;                  AIRMASS   K  AIRMASS
;                  DATE      K  DATE-OBS
;                  DATETMPL  T  DD-MM-YYYY
;                  EXPDELTA  V  0.0
;                  EXPTIME   K  EXPTIME
;                  FILTER    K  FILTERS
;                  FILENAME  K  CCDFNAME
;                  OBJECT    K  OBJECT
;                  UT        K  UT 
;                 The middle column is a flag. It may be K, for Keyword,
;                 T, for Template, or V, for Value. If it is V, the contents
;                 of the third field on that line should make sense for the
;                 name in the first field.
;
;  IMDIR      :String, the path to the original images- DEFAULT:current dir
;  PSFDIR     :String, the path to the psfs- DEFAULT:SRCDIR+'/psf'
;  SRCDIR     :String, the path to the src and srd files- DEFAULT:''
;  OUTDIR     :String, set the path for the output data file. IMSALT
;              will not run if imdir=outdir - DEFAULT:'salted/'
;  NOPLOTS    :Flag, if set suppresses plots of the data, the false
;              data, the salted data, and the data with 'colored salt.'
;  SALTFLAG   :Flag, if set this to make a note in the header that the
;              file has been salted  (add keyword SALTED=T)
;  QUIET      :Flag, if set will suppress chatty printed output.
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  The inputs x_pos, y_pos, scale, and exten must have the same
;  length.  Exten is used only with group FITS image files.
;
;  This program requires that PSFs be pre-generated from the images (see
;     PSFGEN.PRO).  The psf files must adheare to the following restrictions.
;     The psfs must have the same basic format as the data files, meaning
;     that they must be multi-extension if the data files are multi-extension.
;     The psf header must contain the following information: 
;       XCEN,YCEN - x and y location of the object on the psf
;       FWHM      - fwhm of the object
;       MAX       - peak pixel value of the psf
;       FLUX      - flux from the psf in OBJRAD (also in psf header)
;  The psf name must be the same as the filename, except with a .psf appended
;     (replaces .fits tag if file name includes that).
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  2004/07/14, Written by Maureen Teyssier, REU Lowell Observatory
;  2004/07/15, MWB, incorporated into library.
;-
pro imsalt,imname,x_pos,y_pos,rdmR,exten, $
       OUTDIR=outdir,SRCDIR=srcdir,IMDIR=imdir,PSFDIR=psfdir, $
       NOPLOTS=noplots,SALTFLAG=saltflag,RA=ra,DEC=dec,QUIET=quiet, $
       KEYFILE=keyfile

   self='IMSALT: '
   if badpar(imname,7,0,CALLER=self+'(imname) ') then return
   if badpar(x_pos,[1,2,3,4,5],1,CALLER=self+'(x_pos) ',npts=nx_pos) then return
   if badpar(y_pos,[1,2,3,4,5],1,CALLER=self+'(y_pos) ',npts=ny_pos) then return
   if badpar(rdmR,[1,2,3,4,5],1,CALLER=self+'(rdmR) ',npts=nrdmR) then return
   if badpar(exten,[0,1,2,3,4,5],1,CALLER=self+'(exten) ', $
                                   npts=nexten) then return

   if badpar(srcdir,[0,7],0,CALLER=self+'(SRCDIR) ',DEF='') then return
   if badpar(imdir,[0,7],0,CALLER=self+'(IMDIR) ',DEF='') then return
   if badpar(outdir,[0,7],0,CALLER=self+'(OUTDIR) ', $
                            DEF=addslash(imdir)+'salted') then return
   if badpar(psfdir,[0,7],0,CALLER=self+'(psfdir) ', $
                            DEF=addslash(srcdir)+'psf') then return

   if badpar(noplots,[0,1,2,3],0,CALLER=self+'(NOPLOTS) ',default=0) then return
   if badpar(saltflag,[0,1,2,3],0,CALLER=self+'(SALTFLAG) ',default=0) then return
   if badpar(quiet,[0,1,2,3],0,CALLER=self+'(QUIET) ',default=0) then return 
   if badpar(keyfile,[0,7],0,caller=self+'(KEYFILE) ', $
                default='nodefault') then return

   psfdir=addslash(psfdir)
   outdir=addslash(outdir)
   imdir=addslash(imdir)

   ;make sure the input vectors are the same length
   n=[nx_pos,ny_pos,nrdmR,nexten]
   if min(n) ne max(n) then begin
      print,self,'The four vector arrays (x_pos,y_pos,rdmR and exten)' 
      print,self,'must be the same length.' 
      return
   endif

   ;make sure the salted files don't overwrite the data
   if outdir eq imdir then begin
      print,self,'The specified path for the output file is the same as'
      print,self,'the path for the input file. Since the output file has the' 
      print,self,'same name as the input file, the result will be that the' 
      print,self,'unsalted data is overwritten.  Please change the output path.'
      return 
   endif

   if not exists(outdir) then begin
      print,self,'Output directory ',outdir,' not found.  Quitting.'
      return
   endif

   if not exists(imdir+imname) then begin
      print,self,'Image file ',imdir+imname,' not found.'
      return
   endif

   loadkeys,keyfile,hdrlist,foundit=foundkey
   if not foundkey then begin
      print,self,'Keyfile ',keyfile,' could not be loaded.  Aborting.'
      return
   endif
 
   imfile = imname
   tag = strmid(imfile,4,99,/reverse_offset)
   if tag eq '.fits' then begin
      imfile = strmid(imfile,0,strlen(imfile)-5)
      ft = '.fits'
   endif else begin
      ft = ''
   endelse

   psfname=imfile+'.psf'

   if not exists(psfdir+psfname) then begin
      print,self,'PSF file ',psfdir+psfname,' not found.'
      return
   endif

   srdfile = imfile+'.srd'
   srdlist = file_search(srcdir+srdfile+'*',count=nsrd)

   ;Check header of image to see if it is a multi-extension image, and build
   ;an extension list if it is.
   hdr=headfits(imdir+imname)
   numext=sxpar(hdr,'NEXTEND')
   extend=sxpar(hdr,'EXTEND')
   if numext eq 0 or extend ne 1 then extlist=0 else extlist=indgen(numext)+1 
   numext=n_elements(extlist)

   if nsrd ne numext then begin
      print,self,'Error!, image requires ',strn(numext), $
                 ' srd files.  Only found ',strn(nsrd)
      return
   endif

   ;open a .slt file
   sltname=imfile+'.slt'
   openw,bob,psfdir+sltname,/get_lun
   for ext=0,(numext-1) do begin;loop over all extensions

      ;no extension case
      if extlist[ext] eq 0 then begin
         ;read in the image and the psf
         outim=readfits(imdir+imname,hdr)
         parsekey,hdr,hdrlist,info
         ra=info.ra
         dec=info.dec
         fits_read,psfdir+psfname,psf,psfhdr
         exttag=''

      ;multi-extension case
      endif else begin

         if ext eq 0 then begin
            fits_read,imdir+imname,dummy,tmphdr,exten=extlist[ext],/header_only
            parsekey,tmphdr,hdrlist,info
            ra=info.ra
            dec=info.dec
         endif

         ;read in the image and the psf
         outim=readfits(imdir+imname,s_hdr,exten_no=extlist[ext])
         fits_read,psfdir+psfname,psf,psfhdr,exten_no=extlist[ext]
         exttag='x'+strb36(extlist[ext])

         if keyword_set(quiet) eq 0 then print,self,'IMAGE EXTENSION:',exttag

      endelse

      ;find the size of the image and the psf
      sz_i=size(outim,/dimen)
      sz=size(psf,/dimen)

      ;out of the psf hdr
      xcen = sxpar(psfhdr,'XCEN')
      ycen = sxpar(psfhdr,'YCEN')
      fwhm = sxpar(psfhdr,'FWHM')
      flux = sxpar(psfhdr,'FLUX')
      max  = sxpar(psfhdr,'MAX')

      ;out of the srd hdr
      srdhdr   = headfits(srcdir+srdfile+exttag)
      objrad   = sxpar(srdhdr,'OBJRAD')
      srdgain  = sxpar(srdhdr,'GAIN')
      exptime  = sxpar(srdhdr,'EXPTIME')
      skylevel = sxpar(srdhdr,'SKYLEVEL')
      skysigma = sxpar(srdhdr,'SKYSIGMA')
      photzp   = sxpar(srdhdr,'PHOTZP')

      ;--------adding the PSF
      z=where(exten eq extlist[ext], count)

      ;if there exist elements for this extension,add psf, add to .slt file
      if count gt 0 then begin

         ;choose the x,y,R values to put in this image
         x1 = x_pos[z]
         y1 = y_pos[z]
         R  = rdmR[z]

         for i=0,count-1 do begin;object loop

            ;creating a blank array for plot purposes
            blank=fltarr(sz_i[0],sz_i[1])

            ;obtaining the scale from R and finding sig.to noise
            sub_r=R[i]-photzp
            scale=10^((sub_r-24)/(-2.5))*exptime/(srdgain*flux)
            s_noise=(max*scale)/skysigma

            ;indexing to make sure that the correct part of the array is 
            ;added to the image-if the psf is hanging off the edge, only 
            ;the overlapping part is added on, and if the psf falls off
            ;the image, the xy values returned are -1.
            if x1[i] lt 0 or x1[i] ge sz_i[0] or $
               y1[i] lt 0 or y1[i] ge sz_i[1] then begin
               x_corr=-1
               y_corr=-1
            endif else begin

               ii0=x1[i]-sz[0]/2 > 0
               ij0=y1[i]-sz[1]/2 > 0
               pi0=sz[0]/2-x1[i] > 0
               pj0=sz[1]/2-y1[i] > 0

               if x1[i]+sz[0]/2 ge sz_i[0] then $
                  pi1=sz[0]-((sz[0]/2-(sz_i[0]-x1[i]))+2) $
               else $
                  pi1=sz[0]-1

               if y1[i]+sz[1]/2 ge sz_i[1] then $ 
                  pj1=sz[1]-((sz[1]/2-(sz_i[1]-y1[i]))+2) $
               else $
                  pj1=sz[1]-1

               if x1[i]+sz[0]/2 ge sz_i[0] then $
                  ii1=sz_i[0]-1 else $ 
                  ii1=x1[i]+sz[0]/2
               if y1[i]+sz[1]/2 ge sz_i[1] then ij1=sz_i[1]-1 else $ 
                  ij1=y1[i]+sz[1]/2

               x_corr=x1[i]+xcen-(sz[0]/2)
               y_corr=y1[i]+ycen-(sz[1]/2)

               if not quiet then $
                  print,x1[i],y1[i],xcen,ycen,ii0,ii1,ij0,ij1,pi0,pi1,pj0,pj1, $
                        x_corr,y_corr,sz,sz_i, $
                      format='(2(1x,i4),2(1x,f6.3),1x,4(1x,i4),1x,4(1x,i2),' + $
                               '1x,2(1x,f6.1),1x,2(1x,i2),1x,2(1x,i4))'

               ;insert psf in desired locations in image and in blank array
               outim[(ii0):ii1,(ij0):ij1] += psf[(pi0):pi1,(pj0):pj1]*scale
               blank[(ii0):ii1,(ij0):ij1] += psf[(pi0):pi1,(pj0):pj1]*scale
        

            endelse 

            if not quiet then print,self,'POSITION:',x_corr,y_corr

            x_pos[z[i]]=x_corr
            y_pos[z[i]]=y_corr

            ;putting a table of values in a .slt file
            printf,bob,imfile,exttag,x_corr,y_corr,R[i],sub_r,s_noise, $
               format='(a,1x,a2,1x,f7.2,1x,f7.2,1x,f6.3,1x,f6.3,1x,f5.1)'

         endfor; object loop

      endif

      ;---writing the salted file---
      ;location of the salted file
      salted_file=outdir+imfile+ft
      if saltflag then sxaddpar,hdr,'SALTED','T'

      ;if multigroup, write out the primary header
      if extlist[ext] eq 1 then writefits,salted_file,0,hdr

      ;convert the image back to integer to save space
      if extlist[ext] eq 0 then $
         writefits,salted_file,outim,hdr $
      else $ 
         writefits,salted_file,outim,s_hdr,/append

      ;PLOT- win0:data, win1:just salt, win2:salted data, win3:show salt
      if not noplots then begin
         setwin,0,xsize=sz_i[0],ysize=sz_i[1]
         skysclim,outim,lowval,hival,meanval,sigma
         pixel1=bytscl(outim,min=lowval,max=hival,top=255)
         tv,pixel1[0:sz_i[0]-1,0:sz_i[1]-1]

         setwin,1,xsize=sz_i[0],ysize=sz_i[1]
         blank1=bytscl(blank,min=0,max=100*sigma,top=255)
         tv,blank1[0:sz_i[0]-1,0:sz_i[1]-1]

         setwin,2,xsize=sz_i[0],ysize=sz_i[1]
         pixel2=bytscl(outim,min=lowval,max=hival,top=255)
         tv,pixel2[0:sz_i[0]-1,0:sz_i[1]-1]
         plot,[0],[1],xr=[0,sz_i[0]-1],yr=[0,sz_i[1]-1],/noerase, $
            xstyle=5,ystyle=5,xmargin=[0,0],ymargin=[0,0]          
         ;oplot,srcxnew,srcynew,psym=5,color=100

         setwin,3,xsize=sz_i[0],ysize=sz_i[1]
         tv,[[[pixel2[0:sz_i[0]-1,0:sz_i[1]-1]]], $
            [[pixel1[0:sz_i[0]-1,0:sz_i[1]-1]]], $ 
            [[pixel1[0:sz_i[0]-1,0:sz_i[1]-1]]]],true=3
      endif
   endfor ;the exten loop

   free_lun,bob

end
