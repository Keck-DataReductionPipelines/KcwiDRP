function make_str, ustr_string, str_name=str_name, noexe=noexe, nodel=nodel
;
;+
; NAME:
;	MAKE_STR
;
; PURPOSE:
;	Control dynamic structure building - avoids collision of
;	names which might result if different modules used the same
;	structure names and had different numbber/types of tag fields
;
; CALLING SEQUENCE:
;	user_value= MAKE_STR(ustr_string [,str_name=str_name, /NOEXEC]
;
; INPUTS:
;	USTR_STRING - string of form '{dummy,T1:intarr(10), T2:oL...}'
;
; OUTPUTS:
;	return value  = structure defined by USTR_STRING
;
; OPTIONAL KEYWORD PARAMETERS:
;	STR_NAME - (Output) Structure name created
;	NOEXEC - If set, a unique name is allocated but no structure
;		 is created - in this case, the return value is the
;		 allocated name (string variable), not the structure
;
; COMMON BLOCKS;
;       MAKE_STR_PRIVATE - used to generate unique names (via counter)
;       MAKE_STR_BLK  - track previously created structures
;       MAKE_STR_BLK1 - flag to enable/disable structure exist checks
;
; FILE I/O:
;	NONE
;
; METHOD
; 	uses a common block to store counter which is incremented on
;	each call and incorporated into the structrure name.  Uses the
;	IDL EXECUTE statement to dynamically create the structure
;
; MODIFICATION HISTORY:
;	Version 1 -   SLF, 3/5/91     Samuel Freeland
;	Version 1.1 - SLF, 10/23/91 - added call to strstat to check for
;				      structure existance
;       Version 2.0 - slf, 15-jan-93  - added 2nd common block to track
;				        defined structures
;		      slf, 18-jan-93  - add string compress
;		      slf,  1-feb-93  - kludge for strings large than 512
;                     slf,  3-feb-93  add make_str_blk for
;		      slf,  7-feb-93  improved feb 1 upgrades
;		      slf, 26-mar-93  use systime + counter for str names
;                     slf, 15-jan-97  Version 5 (derivation of HOME)
;                     richard.schwartz@gsfc.nasa.gov, 9-feb-1999.
;                       Fix path to work on Windows and Mac, too.
;                     Zarro (SM&A/GSFC) 25-May-99, added check for
;                     undefined HOME and/or write access
;                     Csillaghy (SSL/UCB) 8-Dec-1999, /QUIET keyword
;                     to str_checks.
;                     kim, 2-sep-2003.  Instead of modifying path, cd to temp dir to
;                     compile the temporary routine.  Needed for IDL 6.0
;                     kim, 4-sep-2003.  Changed /either to /is_function for
;                     older IDL
;                     Modified, 17-June-2009, Zarro (ADNET)
;                      - added call to file_delete instead of openr,/delete
;
;
;-
;
;---------------------------------------------------------------------------
common  make_str_private,call_count             ; private common
common make_str_blk, str_names, str_strings     ; private common
common make_str_blk1, check_on                  ; str_check.pro may update
;---------------------------------------------------------------------------


if n_elements(call_count) eq 0 then begin
   call_count=0L 				; initialize commons
   str_names=''
   str_strings=''
ENDIF

;
; 3-feb-1993 - slf, default is to disable checking
if n_elements(check_on) eq 0 then str_checks,/off, /QUIET

str_string=strtrim(ustr_string(0),2)		; make scaler slf, 10-Apr-92
str_string=strcompress(str_string,/remove)
;						; slf 15-jan-1993
;						; compress it to save common
;
chk_previous=where(str_strings eq str_string,excnt)
sec10yr=315360000
if excnt eq 0 then begin 		; make new structure
   repeat begin
      call_count=call_count+1
;     slf - change structure naming to :ms_xxxxxxxxxyyy
      str_name = 'ms_' + $
	string(long(systime(2)) mod sec10yr, format='(i9.9)') + $
	string(call_count mod 1000, format = '(i3.3)')
      exist=0
      if check_on then $
         exist=strstat(str_name) ; ,/quiet)     ; use quiet to suppress
   endrep until (1-exist) or (1-check_on)

;  form string for execute function
   comma = strpos(str_string,",")
   str_exe='arg={' +  str_name  + strmid(str_string,comma,strlen(str_string))

endif else begin
   str_name=str_names(chk_previous(0))
   str_exe='arg={' + str_name + '}'
endelse
;
;
arg=str_name		;
if not keyword_set(noexe)  then begin		; skip struct create if noexethen b
;
;  -------------------------------------------------------------------------
;  slf 1/7-feb-1993 - execute statement workaround - write a new function
;  definition file using the uniq name, compile and run it.
   if strlen(str_exe) ge 128 and excnt eq 0 then begin
;     ------------ define file name and contents -----------------------
      home=get_logenv('HOME')  & delim=path_delimiter()		; all os parameters
      if strlowcase(!version.os) eq 'vms' then 	home='sys$login:'; vms parameters

      if not test_dir(home,/quiet) then home=get_temp_dir()
      temppro = concat_dir(home, str_name + '.pro')	; file/routine name

      outarr=str2arr(str_exe)			; split at tag delimiter
      noutarr=n_elements(outarr)
      outarr=[outarr(0:noutarr-2) + ', $ ', outarr(noutarr-1)]
;
;     ------ write the function which will define the structure ------------
      openw,lun,/get_lun,temppro
      printf,lun,'function ' + str_name + ', exe_str'
      printf,lun,outarr, format='(a)'
      printf,lun,'return, arg'
      printf,lun,'end'
      free_lun,lun				; close the function file
;     ----------------------------------------------------------------------
;
;     ------ set up the path and execute the function ---------------------
      ;path_temp=!path				; save current path
      ;!path= home + delim + !path		; set path to see new file
      ;arg=call_function(str_name,strexe)	; call str definition func
      ;!path=path_temp				; restore original path

      ;kim - 2-sep-2003 changed the above to the below to handle new IDL path
      ; caching feature in IDL 6.0

      cd, home, current=current_dir
      qsave = !quiet
      !quiet = 1
      resolve_routine, str_name, /is_function 
      arg=call_function(str_name,strexe)	; call str definition func
      !quiet = qsave
      cd, current_dir

      error=0
      catch,error
      if error ne 0 then begin
        catch,/cancel
        if exist(current_dir) then cd,current_dir
      endif

;     ----------------------------------------------------------------------
;     now delete the file if appropriate (open/close with delete)
;      openr,lun,/get_lun,temppro, delete=1-keyword_set(nodel)
;      free_lun,lun
      if (1-keyword_set(nodel)) then file_delete,temppro,/quiet
;     ---------------------------------------------------------------------
      exe_status=1				; signal success
    endif else begin				; else, do it the old way
      exe_status = execute(str_exe)		; (execute string)
   endelse

   if excnt eq 0 and exe_status then begin	; succesful creation, so
      str_names=[str_names,str_name]		; update common block
      str_strings=[str_strings,str_string]
   endif
endif
;
return, arg	; structure name if noexe, structure otherwise
end

