;+
; NAME:
;  astinfo
; PURPOSE:   (one line only)
;  Decode (or add) astrometric information from a FITS header
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astinfo,hdr,info,error
; INPUTS:
;  hdr - FITS header to be read (or modified)
;  info - Astrometric calibration information used by routines like
;            astxy2rd.pro.
; OPTIONAL INPUT PARAMETERS:
;  TOHEADER - Flag, if set, the information structure must exist and its
;                information is appended to the input FITS header.
;             If not set, the header is parsed for the astrometric information
;                from which the info structure is built.  If the header does
;                not have the astrometric information a scalar 0 will be
;                returned.
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  info - Normally an output, but is an input if TOHEADER is set.
;  error - 1 if there is an error, 0 if everything is ok.
;          When reading a header this usually means there was no astrometry
;          information in the header.  When modifying a header it means
;          there was something wrong and nothing was done.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2009/11/18
;  2009/12/02, MWB, dropped support for different order of fit for xi and eta.
;                     Now they are required to be the same.  Also dropped
;                     support for old style of term selection.
;  2010/03/21, MWB, added error output variable and make a fix that properly
;                     handles writing a new solution to a header that already
;                     has a solution with more terms than the new one.
;-
pro astinfo,hdr,info,error,TOHEADER=toheader

   error=1
   self='astinfo: '
   if badpar(hdr,7,1,caller=self+'(hdr) ') then return
   if badpar(toheader,[0,1,2,3],0,caller=self+'(TOHEADER) ', $
                                  default=0) then return

   if toheader then begin

      ; Remove any astrometric solution keywords pertaining to the terms
      ;   that may already be in the header.  The other keywords are not
      ;   a problem since they are always present.
      terms=strtrim(sxpar(hdr,'ASTTN*'),2)
      nterms=n_elements(terms)
      if nterms gt 1 then begin
         sxdelpar,hdr,'ASTTN'+strtrim(string(indgen(nterms)+1),2)
         sxdelpar,hdr,'ASTCX'+strtrim(string(indgen(nterms)+1),2)
         sxdelpar,hdr,'ASTCE'+strtrim(string(indgen(nterms)+1),2)
      endif

      sxaddpar,hdr,'ASTINFO', 'T',' Supplemental astrometric solution appended'
      sxaddpar,hdr,'ASTXCEN', info.xcref,' X center of optical geometry'
      sxaddpar,hdr,'ASTYCEN', info.ycref,' Y center of optical geometry'
      sxaddpar,hdr,'ASTPROT', info.prot,' Chip pre-rotation (degrees)'
      sxaddpar,hdr,'ASTRNORM',info.renormfac,' renormalization factor'
      rastr,info.raref,4,ras
      decstr,info.decref,3,decs
      sxaddpar,hdr,'ASTTRA',  ras,' Right Ascension of tangent point'
      sxaddpar,hdr,'ASTTDEC', decs,' Declination of tangent point'
      for i=0,n_elements(info.terms)-1 do begin
         key1 = 'ASTTN'+strn(i+1)
         key2 = 'ASTCX'+strn(i+1)
         key3 = 'ASTCE'+strn(i+1)
         sxaddpar,hdr,key1,strupcase(info.terms[i])
         sxaddpar,hdr,key2,info.cxi[i],' Xi(x,y) transformation'
         sxaddpar,hdr,key3,info.ceta[i],' Eta(x,y) transformation'
      endfor
      error=0

   endif else begin

      astinfo = sxpar(hdr,'ASTINFO',count=found)
      if found eq 1 and astinfo then begin
         xcen=sxpar(hdr,'ASTXCEN')
         ycen=sxpar(hdr,'ASTYCEN')
         prot=sxpar(hdr,'ASTPROT')
         renormfac=sxpar(hdr,'ASTRNORM')
         racen=raparse(sxpar(hdr,'ASTTRA'))
         deccen=decparse(sxpar(hdr,'ASTTDEC'))
         terms=strtrim(sxpar(hdr,'ASTTN*'),2)
         cxi=sxpar(hdr,'ASTCX*')
         ceta=sxpar(hdr,'ASTCE*')
         info = { $
            renormfac: renormfac, $
            cxi: cxi, $
            ceta: ceta, $
            prot: prot, $
            terms: terms, $
            xcref: xcen, $
            ycref: ycen, $
            raref: racen, $
            decref: deccen $
            }
         error=0
      endif else begin
         print,self,' Warning!  header does not contain astrometric info'
         info=0
         error=1
      endelse

   endelse

end
