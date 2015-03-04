;+
; NAME:
;  tmplinfo
; PURPOSE:   (one line only)
;  Generate information about a template image based on an image header
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  tmplfn,input,info
; INPUTS:
;  input - There are two options for input.
;           1 - a scalar string that is the name of the image file.  This
;                  should include the path if the file cannot be found in
;                  the current directory.
;           2 - a string array with the FITS header from an image file.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  KEYLIST  - Name of a file containing a correspondence list. This list
;                associates a set of standard names with the actual keyword
;                names found in a FITS file header. If this keyword is
;                omitted, a default list is used, as if a file with the
;                following contents had been supplied:
;                   AIRMASS   K  AIRMASS
;                   DATE      K  DATE-OBS
;                   DATETMPL  T  DD-MM-YYYY
;                   EXPDELTA  V  0.0
;                   EXPTIME   K  EXPTIME
;                   FILTER    K  FILTERS
;                   FILENAME  K  CCDFNAME
;                   OBJECT    K  OBJECT
;                   UT        K  UT 
;                   RA        K  RA
;                   DEC       K  DEC
;                The middle column is a flag. It may be K, for Keyword,
;                T, for Template, or V, for Value. If it is V, the contents
;                of the third field on that line should make sense for the
;                name in the first field.
;
; OUTPUTS:
;   info - Anonymous structure with information about a template image
;             if constructed from this data image.  This information is
;             intended to help with populating the refim and template
;             database tables.
;   error - Flag, if set indicates there was an error building the desired
;             information structure.  In this case the info structure
;             is ill defined.  If error is not set then everything is fine.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  The image header must contain the astrometric solution in a form
;    compatible with astinfo.pro
; PROCEDURE:
; MODIFICATION HISTORY:
;  2010/08/06, Written by Marc W. Buie, Southwest Research Institute
;-
pro tmplinfo,input,info,error,KEYLIST=keylist,PATH=path

   error=1

   self='TMPLINFO: '
   if badpar(input,7,[0,1],caller=self+'(image) ') then return
   if badpar(keylist,[0,7],0,caller=self+'(KEYLIST) ', $
                             default='[[DEFAULT]]') then return

   if n_elements(input) eq 1 then begin ; file name was input
      if not exists(input) then begin
         print,self,'File ',input,' not found.'
         return
      endif
      hdr=headfits(input)
   endif else begin ; FITS header provided
      hdr = input
   endelse

   loadkeys,keylist,hdrlist
   parsekey,hdr,hdrlist,hdrinfo
   astinfo,hdr,astinfo,herror
   if herror then begin
      print,self,'Image header does not contain astrometric information'
      return
   endif

   ; get size of image in pixels
   nx = sxpar(hdr,'NAXIS1')
   ny = sxpar(hdr,'NAXIS2')

   ; get pointer to X and Y terms in astrometric solution
   zx=trimrank(where(astinfo.terms eq 'X',count))
   if count ne 1 then begin
      print,self,'Cannot find X term in astrometric solution.'
      return
   endif
   zy=trimrank(where(astinfo.terms eq 'Y',count))
   if count ne 1 then begin
      print,self,'Cannot find Y term in astrometric solution.'
      return
   endif

   ; xi and eta scales (arcsec/pixel)
   xiscale  = sqrt( astinfo.cxi[zx]^2  + astinfo.cxi[zy]^2  )
   etascale = sqrt( astinfo.ceta[zx]^2 + astinfo.ceta[zy]^2 )
   scale = (xiscale+etascale)/2.0/astinfo.renormfac

   ; Compute FOV of image, measured as the angular distance across diagonal
   fov = sqrt(float(nx)^2 + float(ny)^2) * scale

   jdstr,hdrinfo.jd,100,date,timesep='-'

   object=strcompress(hdrinfo.object,/remove_all)

   outfn = date+'_'+object+'_'+hdrinfo.filter

   info = { $
            name: outfn, $
            date: date,  $
            filter: hdrinfo.filter, $
            racen: hdrinfo.ra, $
            deccen: hdrinfo.dec, $
            fov: fov, $
            scale: scale $
            }
   error=0

end

