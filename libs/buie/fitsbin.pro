;+
; NAME:
;  fitsbin
; PURPOSE:
;  Software binning of an image in FITS file format.
; DESCRIPTION:
;
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  fitsbin,file,INPATH=inpath,OUTPATH=outpath,BINFAC=binfac
; INPUTS:
;  file - String (or array) of name of file to be binned.  If the file name
;            includes a trailing .fits, the .fits is option on the file name.
;            The output file name will not include the .fits tag.
; OPTIONAL INPUT PARAMETERS:
;  BINFAC  - Binning factor (default=2).
;
;  INPATH  - String, path to directory where data are to be read from.
;               Default=current directory
;
;  OUTPATH - String, path to directory where data are to be written.
;               Default=current directory
;
;  Note: if INPATH and OUTPATH end up being the same, then outpath is
;            changed by adding the directory name, binned, the end of the
;            path.  WARNING: No attempt is made to resolved different means
;            of expressing the same directory.  The test is made just by
;            a simple string comparison.
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
;  Written by Marc W. Buie, Lowell Observatory, 2000/06/22
;  2003/10/01, MWB, converted to IDL file_mkdir calls
;-
pro fitsbin,file,INPATH=inpath,OUTPATH=outpath,BINFAC=binfac

   if badpar(inpath,[0,7],0,CALLER='FITSBIN: (INPATH) ', $
                             default='') then return
   if inpath ne '' then inpath=addslash(inpath)

   if badpar(outpath,[0,7],0,CALLER='FITSBIN: (OUTPATH) ', $
                             default='') then return
   if outpath ne '' then outpath=addslash(outpath)

   if outpath eq inpath then outpath=outpath+'binned'
   outpath=addslash(outpath)

   if badpar(file,7,[0,1],CALLER='FITSBIN: (file) ',npts=nfiles) then return

   if badpar(binfac,[0,1,2,3],0,CALLER='FITSBIN: (binfac) ',default=2) then return

   if not exists(outpath) then file_mkdir,outpath

   for i=0,nfiles-1 do begin
      infile = inpath+file[i]
      outfile = outpath+file[i]

      if exists(infile+'.fits') then infile=infile+'.fits'

      if exists(infile) then begin

         print,'Reading primary header from ',infile
         primehdr=headfits(infile)
         numext=sxpar(primehdr,'NEXTEND')
         if numext eq 0 then begin
            image=0
            image=readfits(infile,hdr,/silent,/noscale)
            bitpix=sxpar(hdr,'BITPIX')
            sz=size(image,/dim)
            print,'Binning data by a factor of ',strn(binfac)
            oxw = (sz[0]/binfac)*binfac
            oyw = (sz[1]/binfac)*binfac
            nxw = (sz[0]/binfac)
            nyw = (sz[1]/binfac)
            if bitpix eq 16 then begin
               image = fix(rebin(image[0:oxw-1,0:oyw-1],nxw,nyw)+0.5 < 32767.0)
            endif else if bitpix eq -32 then begin
               image = rebin(image[0:oxw-1,0:oyw-1],nxw,nyw)
            endif else begin
               print,'FITSBIN: Error, unsupported BITPIX value of ',strn(bitpix)
            endelse
            sxaddpar,hdr,'NAXIS1',nxw
            sxaddpar,hdr,'NAXIS2',nyw
            sxaddpar,hdr,'BINFAC',binfac,' Software binning factor applied'
            print,'Writing image to ',outfile
            writefits,outfile,image,hdr
         endif else begin
            print,'File has',strcompress(numext),' extensions.'

            print,'Writing primary header to ',outfile

            writefits,outfile,0,primehdr

            for exten=1,numext do begin
               print,'Reading extension ',exten
               image=0
               image=readfits(infile,hdr,exten_no=exten,/silent,/noscale)
               bitpix=sxpar(hdr,'BITPIX')
               sz=size(image,/dim)
               print,'Binning data by a factor of ',strn(binfac)
               oxw = (sz[0]/binfac)*binfac
               oyw = (sz[1]/binfac)*binfac
               nxw = (sz[0]/binfac)
               nyw = (sz[1]/binfac)
               if bitpix eq 16 then begin
                  image = fix(rebin(image[0:oxw-1,0:oyw-1],nxw,nyw)+0.5 < 32767.0)
               endif else if bitpix eq -32 then begin
                  image = rebin(image[0:oxw-1,0:oyw-1],nxw,nyw)
               endif else begin
                  print,'FITSBIN: Error, unsupported BITPIX value of ',strn(bitpix)
               endelse
               sxaddpar,hdr,'NAXIS1',nxw
               sxaddpar,hdr,'NAXIS2',nyw
;               image = fix(rebin(temporary(image),sz[0]/binfac,sz[1]/binfac)+0.5 < 32767.0)
               sxaddpar,hdr,'BINFAC',binfac,' Software binning factor applied'
               print,'Writing extension ',exten
               writefits,outfile,image,hdr,/append
            endfor
         endelse

      endif else begin
         print,'File ',infile,' was not found.'
      endelse

   endfor

end
