;+
; NAME:
;  astprmt
; PURPOSE:
;  Promote version of an astrometry fit coefficient file to highest version.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astprmt,fitfile,centers
; INPUTS:
;
;  fitfile - File with astrometry fit coefficients (Default=fitcoeff.dat)
;
;  centers - File with image centers.  (Default=centers.dat)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  RENORMFAC - This input is needed when promoting across the v1.1 and v1.2
;                 version boundary.
;
; OUTPUTS:
;
;  The fitfile is updated to the most recent version.  Not changed if already
;    current or if there is an error reading the file.
;
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
;  Normally, this routine will always promote file that is not at the latest
;    version level.  However, if the version is v1.1 and RENORMFAC is not
;    provided, the file will not get promoted.  If you can provide RENORMFAC
;    then the file will always get promoted if needed.
;
; PROCEDURE:
;
; MODifICATION HISTORY:
;  1997/10/09, Written by Marc W. Buie, Lowell Observatory
;  2000/01/19, MWB, added version 1.1 support
;  2009/08/05, MWB, added version 1.2 support, total rewrite to eliminate
;                 duplicated code between here and RDASTFC.PRO
;  2009/12/01, MWB, upgraded to version 1.3 support
;
;-
pro astprmt,fitfile,fncenters,RENORMFAC=renormfac

   self='ASTPRMT: '
   if badpar(fitfile,[0,7],0,caller=self+'(fitfile) ', $
                default='fitcoeff.dat') then return
   if badpar(fncenters,[0,7],0,caller=self+'(fncenters) ', $
                default='centers.dat') then return
   if badpar(renormfac,[0,4,5],0,caller=self+'(RENORMFAC) ', $
                default=-1.0) then return

   ; If not present, don't do anything.
   if not exists(fitfile) then return

   ; do a quick check of the fit coeff file version.
   version=''
   openr,lun,fitfile,/get_lun
   readf,lun,version,format='(a)'
   free_lun,lun

   ; It's current, do nothing.
   latest='ASTFIT v1.3'
   if version eq latest then return

   ; Check a special case, if v1.1 and renormfac not set, do nothing.
   if version eq 'ASTFIT v1.1' and renormfac le 0.0 then return

   ; At this point it seems like the file needs to be promoted.  Go ahead
   ;   and read it in.
   rdastfc,fitfile,ffn,ftype,xc,yc,prot,renorm,cra,cdec,photzp, $
            terms,coeffarr,ncoeffs,nlines, $
            version=oldversion,FNCENTERS=fncenters,error=error

   if error then begin
      print,self,'Error reading file, unable to promote.'
      return
   endif

   ; if version is blank it was not recognized, do nothing
   if oldversion eq '' then begin
      print,self,'fitcoeff.dat file is of an unrecognized format, aborting.'
      return
   endif

   ; apply renormfac value, if provided.
   if renormfac gt 0.0 then begin
      z=where(renorm le 0.0, count)
      if count ne 0 then renorm[z] = renormfac
   endif

   ; write data back out, thus promoting the file.
   print,'ASTPRMT: Upgrading file from ',version,' to ',latest
   print,nlines,' total coefficient sets found.'
   wrastfc,fitfile,ffn,ftype,xc,yc,prot,renorm,cra,cdec,photzp,terms,coeffarr

end

