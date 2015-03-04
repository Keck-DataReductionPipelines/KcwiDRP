;+
; NAME:
;  rdastfc
; PURPOSE:
;  Read an astrometry fit coefficient file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
; rdastfc,file,ffn,ftype,xc,yc,prot,renormfac,cra,cdec,photzp, $
;            terms,coeffarr,ncoeffs,nlines
; INPUTS:
;  file     - Input file name to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  FNCENTERS - File with image centers.  (Default=centers.dat)
;
; OUTPUTS:
;  Each of these ouputs are vectors or arrays.  The length of the vectors or
;    the number of rows is equal to the number of fit coefficient sets in
;    the file.  For arrays, the dimensions are [nlines,10]
;  ffn      - File name for this fit.
;  ftype    - Type of fit (eta or xi)
;  xc       - X center of array
;  yc       - Y center of array
;  prot     - Pre-rotation of raw coordinates (degrees)
;  renormfac- Renormalization factor used for solution.  If not known it
;               will be set to -1
;  cra      - Center right acension (radians)
;  cdec     - Center declination (radians)
;  photzp   - Photometric zero-point for this image.  If the value is
;               99.0 the zero point has not been determined.  This will
;               allow you to later compute a real magnitude from instrumental
;               magnitudes on the frame.
;  terms    - Term names that are used (see astterms.pro for more information).
;               string vector size is [ncoeffs]
;  coeffarr - Array of astrometric fit coefficients (see astterms.pro)
;                array size is [nlines,ncoeffs]
;  ncoeffs  - Number of fitted coefficients (scalar)
;  nlines   - Number of coefficients read from file (scalar).
;
; KEYWORD OUTPUT PARAMETERS:
;  VERSION - Version tag of file that was read.
;  ERROR   - Flag, set if error found during read.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;  All of the solutions in the file must have the same number of terms.
;
; PROCEDURE:
;  The solution is a two-step conversion.  You start with raw coordinates
;    that typically relate to the original position in the native coordinate
;    system for the device.  The first transformation step is to convert
;    to another system related to the first by a translation and rotation.
;     e.g.   xp = (  (x-xc)*cos(prot) + (y-yc)*sin(prot) ) / renormfac
;            yp = ( -(x-xc)*sin(prot) + (y-yc)*cos(prot) ) / renormfac
;
;    The astrometric transformation then maps xp,yp to ra,dec.
;
; MODIFICATION HISTORY:
;  1999/04/15, Written by Marc W. Buie, Lowell Observatory
;  2000/01/19, MWB, added version 1.1 support (now includes photzp)
;  2002/04/09, MWB, changed from Str_Sep to strsplit
;  2005/06/28, MWB, fixed bug when reading an empty file.
;  2009/08/04, MWB, added prot and renormfac variables thus changing the
;                      calling sequence (sorry)
;  2009/12/01, MWB, moved up to version 1.3, calling sequence slightly
;                      different again. (terms was flagarr, some returned
;                      values have a smaller rank).  Added ERROR output.
;-
pro rdastfc,file,ffn,ftype,xc,yc,prot,renormfac,cra,cdec,photzp, $
            terms,coeffarr,ncoeffs,nlines, $
            FNCENTERS=fncenters,VERSION=version,ERROR=error

   self='RDASTFC: '
   if badpar(file,7,0,caller=self+'(file) ') then return
   if badpar(fncenters,[0,7],0,caller=self+'(FNCENTERS) ', $
                default='centers.dat') then return

   error=1

   ; Pre-declare strings for later use.
   version=''
   line=''

   ; Next load the fit coefficients file, watch for different versions.
   openr,lun,file,/get_lun

   ; Count the number of lines in file.
   nlines=0L
   while not eof(lun) do begin
      readf,lun,line,format='(a1)'
      nlines=nlines+1
   endwhile
   point_lun,lun,0

   ; First line gives hint to version
   readf,lun,version,format='(a)'
   if version eq 'ASTFIT v1.0' or version eq 'ASTFIT v1.1' or $
      version eq 'ASTFIT v1.2' or version eq 'ASTFIT v1.3' then begin
      nlines=nlines-1
   endif else begin
      ; This is no recognized version tag, that means it's either an incorrect
      ;   file type, or, it's the original version that didn't have a tag.
      ;   If the latter, there must be a certain number of blank delimited
      ;   fields in the line.
      version=strtrim(strcompress(version),2)
      words=strsplit(version,' ',/extract)
      nwords=n_elements(words)
      if nwords le 15 then begin
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         version=''
         free_lun,lun
         return
      endif
      nterms = fix(total(fix(words[4:13])))
      if nwords ne nterms+14 then begin
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         version=''
         free_lun,lun
         return
      endif
      version='ASTFIT v0.0'
      point_lun,lun,0

   endelse

   if nlines eq 0 then begin
      free_lun,lun
      return
   endif

   ; Almost common init
   if version lt 'ASTFIT v1.3' then begin
      flagarr  = intarr(nlines,10)
      coeffarr = dblarr(nlines,10)
      ncoeffs  = intarr(nlines)
      photzp   = replicate(99.0,nlines)
      convert  = 1
   endif else begin
      readf,lun,line,format='(a)'
      terms=strsplit(strtrim(strcompress(line),2),' ',/extract)
      ncoeffs=n_elements(terms)
      nlines=nlines-1
      coeffarr = dblarr(nlines,ncoeffs)
      photzp   = replicate(99.0,nlines)
      convert  = 0
   endelse

   ffn      = strarr(nlines)
   ftype    = strarr(nlines)
   xc       = dblarr(nlines)
   yc       = dblarr(nlines)
   prot     = dblarr(nlines)
   renormfac= replicate(-1.0d0,nlines)
   cra      = dblarr(nlines)
   cdec     = dblarr(nlines)

   if version eq 'ASTFIT v0.0' then begin
      for i=0,nlines-1 do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         flagarr[i,*]=fix(words[4:13])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[14:14+ncoeffs[i]-1])
      endfor

      if not exists(fncenters) then begin
         nlines=0
         free_lun,lun
         print,self,'Error! centers file [',fncenters,'] not found.'
         print,'This file is required to assist converting a version 0 file.'
         return
      endif

      ; Load the centers.dat file
      readcol,fncenters,cfn,cras,cdecs,format='a,a,a',/silent
      ncen=n_elements(cfn)
      cra0=raparse(cras)
      cdec0=decparse(cdecs)

      ; Copy the centers to cra,cdec
      for i=0,nlines-1 do begin
         ; Locate index for center information
         z=where(ffn[i] eq cfn,count)
         if count eq 0 then begin
            print,'Error: No center found for file: ',ffn[i]
            ffn[i] = ''
         endif else if count eq 1 then begin
            cra[i]  = cra0[z[0]]
            cdec[i] = cdec0[z[0]]
         endif else begin
            print,'Error: Multiple centers found for file: ',ffn[i]
            ffn[i] = ''
         endelse
      endfor

   endif else if version eq 'ASTFIT v1.0' then begin

      for i=0,nlines-1 do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         cra[i]   = raparse(words[4])
         cdec[i]  = decparse(words[5])
         flagarr[i,*]=fix(words[6:15])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[16:16+ncoeffs[i]-1])
      endfor

   endif else if version eq 'ASTFIT v1.1' then begin

      for i=0,nlines-1 do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         cra[i]   = raparse(words[4])
         cdec[i]  = decparse(words[5])
         photzp[i]= float(words[6])
         flagarr[i,*]=fix(words[7:16])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[17:17+ncoeffs[i]-1])
      endfor

   endif else if version eq 'ASTFIT v1.2' then begin

      for i=0,nlines-1 do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         prot[i]  = double(words[4])
         renormfac[i]= double(words[5])
         cra[i]   = raparse(words[6])
         cdec[i]  = decparse(words[7])
         photzp[i]= float(words[8])
         flagarr[i,*]=fix(words[9:18])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[19:19+ncoeffs[i]-1])
      endfor

   endif else if version eq 'ASTFIT v1.3' then begin

      for i=0,nlines-1 do begin
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         prot[i]  = double(words[4])
         renormfac[i]= double(words[5])
         cra[i]   = raparse(words[6])
         cdec[i]  = decparse(words[7])
         photzp[i]= float(words[8])
         coeffarr[i,*] = double(words[9:9+ncoeffs-1])
      endfor

   endif

   free_lun,lun

   ; Not all possiblities that were allowed before are now allowed.  This
   ;   shouldn't be a problem since those modes were used.  The big restriction
   ;   is that all sets of terms in this file must have the same length.
   if convert then begin

      stdterms = ['CONST','X','Y','R','XX','YY','XXX','YYY','XYY','XXY']

      if max(ncoeffs) ne min(ncoeffs) then begin
         print,self,'Invalid coefficients file.'
         print,self,'All solutions must have the same number of terms.'
         return
      endif
      ncoeffs=max(ncoeffs)
      ; Only the first line of terms is used for the conversion
      z=where(flagarr[0,*] eq 1,count)
      if ncoeffs ne count then begin
         print,self,'Error!  mismatch in coefficient count.  Impossible.'
         return
      endif
      terms = stdterms[z]
      coeffarr=coeffarr[*,0:ncoeffs-1]

   endif

   error=0

end

