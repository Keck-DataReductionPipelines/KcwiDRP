;+
; NAME: 
;  backsub
; PURPOSE: 
;  Background subtraction from an image.
; DESCRIPTION:
;  The background is determined from a robust fit of a line to each row
;     or column in the image depending on the keywords.  If neither keyword
;     is set, then the robust mean is used.  These lines or means are then
;     subtracted from the input image.
;
;  For each row (or column) that is fit, the 6 most extreme points (from
;     the mean) are removed from the first pass fit.  See the docs for
;     GOODPOLY and the NEX keyword.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  backsub,image, [ROW,COLUMN]
; INPUTS:
;     image - Image (2-d or 3-d) to subtract background from, image is
;              modified in place.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;     ROW    - Flag, if set, program assumes that image is flat along a row.
;     COLUMN - Flag, if set, program assumes that image is flat along a column.
;
;        Default is to assume that the image is flat along rows and columns.
;
;     EXCLUDE- Scalar item or vector list of rows to exclude from background
;                 fitting process.
;     KEEPMEAN - Flag, if set this program will NOT subtract the constant
;                 term from the image if using a polynomical of order >= 1.
;  MAX_VALUE - The maximum value to be fitted.  If this keyword is provided,
;                data values greater than MAX_VALUE are treated as missing
;                and are not used in the fit for the background.
;  MIN_VALUE - The minimum value to be fitted.  If this keyword is provided,
;                data values greater than MIN_VALUE are treated as missing
;                and are not used in the fit for the background.
;     ORDER  - Order of polynominal to fit to background (ROW and COLUMN only),
;              the default is 0 (mean of direction), 1 is linear, etc.
;     SILENT - If set, disables chatty output to screen.
; OUTPUTS:
;     image - Background subtracted image.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
;  Only one of ROW and COLUMN can be set.
;
; PROCEDURE:
; MODIFICATION HISTORY:
;  93/11/12, Written by Marc W. Buie, Lowell Observatory
;  95/03/23, MWB, added ORDER keyword.
;  98/06/08, MWB, Added MIN/MAX_VALUE keywords.
;  2002/12/12, MWB, Added KEEPMEAN keyword.
;  2004/06/13, MWB, added support for UINT, ULONG, LONG64, ULONG64
;-
pro backsub,image, ROW=row_wise, COLUMN=col_wise, ORDER=order, $
       EXCLUDE=exclude, MIN_VALUE=min_value, MAX_VALUE=max_value, $
       MASK=mask,SILENT=silent,KEEPMEAN=keepmean

nex = 6
ret = string("12b)  ;"

if badpar(image,[1,2,3,4,5,12,13,14,15],[2,3],CALLER='BACKSUB (array): ', $
                rank=rank,dimen=dimen) then return
if badpar(order,[0,1,2,3],0,CALLER='BACKSUB (order): ',DEFAULT=0) then return
if badpar(min_value,[0,1,2,3,4,5],0,CALLER='BACKSUB (MIN_VALUE) ') then return
if badpar(max_value,[0,1,2,3,4,5],0,CALLER='BACKSUB (MAX_VALUE) ') then return
if badpar(mask,[0,1,2,3],2,CALLER='BACKSUB (MASK) ',DEFAULT=-1) then return
if badpar(silent,[0,1,2,3],0,CALLER='BACKSUB (SILENT) ',DEFAULT=0) then return
if badpar(keepmean,[0,1,2,3],0,CALLER='BACKSUB (KEEPMEAN) ',DEFAULT=0) then return

if keyword_set(row_wise) and keyword_set(col_wise) then begin
   print,'BACKSUB: Error!  Only one of ROW and COLUMN can be set.'
   return
endif

ncols = dimen[0]
nrows = dimen[1]
if rank eq 3 then nframes=dimen[2] else nframes=1
icol = indgen(ncols)
irow = indgen(nrows)

; Create an empty mask if none provided.
if mask[0] eq -1 then begin
   mask=bytarr(ncols,nrows)
   inmask=0
endif else begin
   inmask=1
endelse

; This takes care of fitting a line to each row and then subtracting.
if keyword_set(row_wise) then begin
   include = indgen(ncols)
   if keyword_set(exclude) then begin
      intrsect,include,exclude,use,nfound,/nnot
      if nfound gt 0 and nfound le ncols then include=use
   endif
   if rank eq 2 then begin
      for i=0L,nrows-1 do begin
         l_row = image[*,i]
         l_bad = mask[*,i]
         if order eq 0 then begin
            robomean,l_row[include],3,.5,back,bad=l_bad[include]
            image[*,i] = l_row - back
         endif else begin
            coeff=goodpoly(icol[include],l_row[include],order,3,f_row, $
                           EX=nex,MIN_VALUE=min_value,MAX_VALUE=max_value, $
                           bad=l_bad[include])
            if keepmean then coeff[0] = 0.0
            image[*,i] = l_row - poly(icol,coeff)
         endelse
      endfor
   endif else begin
      for k=0,nframes-1 do begin
         if not silent then $
            if k mod 10 eq 0 then print,ret,nframes-k,format="($,a,i4)"
         for i=0,nrows-1 do begin
            l_row = image[*,i,k]
            l_bad = mask[*,i]
            if order eq 0 then begin
               robomean,l_row[include],3,.5,back,bad=l_bad[include]
               image[*,i,k] = l_row - back
            endif else begin
               coeff=goodpoly(icol[include],l_row[include],order,3,f_row, $
                              EX=nex,MIN_VALUE=min_value,MAX_VALUE=max_value, $
                              bad=l_bad[include])
               if keepmean then coeff[0] = 0.0
               image[*,i,k] = l_row - poly(icol,coeff)
            endelse
         endfor
      endfor
      if not silent then print,' done'
   endelse
endif

; This takes care of fitting a line to each column and then subtracting.
if keyword_set(col_wise) then begin
   include = indgen(nrows)
   if keyword_set(exclude) then begin
      intrsect,include,exclude,use,nfound,/nnot
      if nfound gt 0 and nfound le nrows then include=use
   endif
   if rank eq 2 then begin
      for i=0,ncols-1 do begin
         l_col = image[i,*]
         l_bad = mask[i,*]
         if order eq 0 or n_elements(include) le order then begin
            robomean,l_col[include],3,.5,back,bad=l_bad[include]
            image[i,*] = l_col - back
         endif else begin
            coeff=goodpoly(irow[include],l_col[include],order,3,f_col, $
                           EX=nex,MIN_VALUE=min_value,MAX_VALUE=max_value, $
                           bad=l_bad[include])
            if keepmean then coeff[0] = 0.0
            image[i,*] = l_col - poly(irow,coeff)
         endelse
      endfor
   endif else begin
      for k=0,nframes-1 do begin
         if not silent then $
            if k mod 10 eq 0 then print,ret,nframes-k,format="($,a,i4)"
         for i=0,ncols-1 do begin
            l_col = image[i,*,k]
            l_bad = mask[i,*]
            if order eq 0 then begin
               robomean,l_col,3,.5,back,bad=l_bad[include]
               image[i,*,k] = l_col - back
            endif else begin
               coeff=goodpoly(irow[include],l_col[include],order,3,f_col, $
                              EX=nex,MIN_VALUE=min_value,MAX_VALUE=max_value, $
                              bad=l_bad[include])
               if keepmean then $
                  image[i,*,k] = l_col - f_col + coeff[0] $
               else $
                  image[i,*,k] = l_col - f_col
            endelse
         endfor
         if not silent then print,' done'
      endfor
   endelse
endif

; This takes care of finding the mean of each image and then subtracting.
if not keyword_set(col_wise) and not keyword_set(row_wise) then begin
   if rank eq 2 then begin
      if inmask then $
         robomean,image,3,.5,back,bad=mask[*] $
      else $
         robomean,image,3,.5,back
      image = image - back
   endif else begin
      for k=0,nframes-1 do begin
         if not silent then $
            if k mod 10 eq 0 then print,nframes-k,format='($,i4)'
         if inmask then $
            robomean,image[*,*,k],3,.5,back,bad=mask[*] $
         else $
            robomean,image[*,*,k],3,.5,back
         image[*,*,k] = image[*,*,k] - back
      endfor
      if not silent then print,' done'
   endelse
endif

end
