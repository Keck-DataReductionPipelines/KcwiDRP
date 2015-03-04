pro fdecomp, filename, disk, dir, name, qual, version, OSfamily = osfamily
;+
; NAME:
;     FDECOMP
; PURPOSE:
;     Routine to decompose file name(s) for any operating system.
;
; CALLING SEQUENCE:
;     FDECOMP, filename, disk, dir, name, qual, [OSFamily = ]
;
; INPUT:
;     filename - string file name(s), scalar or vector
;
; OUTPUTS:
;     All the output parameters will have the same number of elements as 
;       input filename 
;
;       disk - disk name, always '' on a Unix machine, scalar or vector string
;       dir - directory name, scalar or vector string
;       name - file name, scalar or vector string 
;       qual - qualifier, set equal to the characters beyond the last "."
;       version - obsolete (was for VMS) always ''
;
; OPTIONAL INPUT KEYWORD:
;     OSFamily - one of the four scalar strings specifying the operating 
;             system:  'Windows','MacOS' or 'unix'.    If not supplied,
;             then !VERSION.OS_FAMILY is used to determine the OS.
; EXAMPLES:
;     Consider the following file names 
;
;     unix:    file = '/rsi/idl63/avg.pro' 
;     MacOS:   file = 'Macintosh HD:Programs:avg.pro'
;     Windows: file =  'd:\rsi\idl63\avg.pro'
;       
;     then IDL> FDECOMP,  file, disk, dir, name, qual, version
;       will return the following
;
;                 Disk             Dir          Name        Qual    
;       Unix:      ''            '/rsi/idl63/'  'avg'       'pro'   
;;       Mac:     'Macintosh HD'  ':Programs:'   'avg'      'pro'  
;       Windows:    'd:'         \rsi\idl63\    'avg'       'pro'   
;
; NOTES:
;     All tokens are removed between the name and qualifier
;          (i.e period is removed)
;
; ROUTINES CALLED:
;     None.
; HISTORY
;     version 1  D. Lindler  Oct 1986
;     Include VMS DECNET machine name in disk    W. Landsman  HSTX  Feb. 94
;     Converted to Mac IDL, I. Freedman HSTX March 1994          
;     Converted to IDL V5.0   W. Landsman   September 1997
;     Major rewrite to accept vector filenames V5.3   W. Landsman June 2000
;     Fix cases where disk name not always present  W. Landsman  Sep. 2000
;     Make sure version defined for Windows  W. Landsman April 2004
;     Include final delimiter in directory under Windows as advertised
;                W. Landsman   May 2006
;     Remove VMS support, W. Landsman    September 2006
;-
;--------------------------------------------------------
;
  On_error,2                            ;Return to caller

  if N_params() LT 2 then begin
     print, 'Syntax - FDECOMP, filename, disk, [dir, name, qual, ver ] '
     return
  endif

; Find out what machine you're on, and take appropriate action.
 if not keyword_set(OSFAMILY) then osfamily = !VERSION.OS_FAMILY
 sz = size(filename)
 scalar = sz[0] EQ 0
 if scalar then filename = strarr(1) + filename

 case OSFAMILY of

  "MacOS": begin

; disk name is all characters up to the first colon
; directory is string of folders         
; file name+qualifier is all characters after the last colon
; version   is null string
   
  st = filename
  N= N_elements(st)
  disk = st
  replicate_inplace,disk,''
  version = disk
  qual = disk
  lpos = strpos(st,':')
  good = where( lpos GE 0, Ngood)
  if Ngood GT 0 then begin 
       stg = st[good]
       lpos = reform( lpos[good], 1, Ngood) 
       disk[good] = strmid( stg, 0, lpos)
       st[good] = strmid(stg,lpos+1 ) 
   endif

   dir = disk
   lpos = strpos(st,':',/reverse_search)
   good = where(lpos GT 0, Ngood)
   if Ngood GT 0 then begin
             stg = st[good]
             lpos = reform( lpos[good], 1, Ngood) 
             dir[good] = ':' + strmid( stg, 0, lpos ) + ':'
             st[good] = strmid(stg, lpos+1 )
    endif

    name = st
    lpos = strpos(st,'.',/reverse_search)
    good = where(lpos GE 0, Ngood)
    
    if Ngood GT 0 then begin
             stg = st[good]
             lpos = reform( lpos[good], 1, Ngood) 
 
             name[good] = strmid(stg,0,lpos )
             qual[good] = strmid(stg,lpos+1 )
     endif

        
    end
 
 "Windows": begin
     st = filename
     disk = st
     replicate_inplace,disk,''
     dir = disk
     qual = disk
     version = disk
     lpos = strpos( st, ':')                 ; DOS diskdrive (i.e. c:)
     good = where(lpos GT 0, Ngood) 
     if Ngood GT 0 then begin
         stg = st[good]
         lpos = reform( lpos[good], 1, Ngood)
         disk[good] = strmid( stg, 0, lpos+1) 
         st[good] = strmid(stg,lpos+1 )
     endif

;  Search the path name (i.e. \dos\idl\) and locate all backslashes

     lpos = strpos(st,'\',/reverse_search)
     good = where(lpos Gt 0, Ngood)
     ;  Parse off the directory path 
    if Ngood GT 0 then begin
             stg = st[good]
             lpos = reform( lpos[good],1, Ngood)
             dir[good] = strmid( stg, 0, lpos +1 )
             st[good] = strmid(stg, lpos+1 )
    endif

 
; get Windows name and qualifier (extension)...qual is optional

    lpos = strpos(st,'.',/reverse_search)
    good = where(lpos GE 0, Ngood)
    name = st

    if Ngood GT 0 then begin
             stg = st[good]
             lpos = reform(lpos[good], 1, Ngood)
 
             name[good] = strmid(stg,0,lpos )
             qual[good] = strmid(stg,lpos+1 )
     endif

    end

 
 ELSE: begin                 ;Unix

    st = filename
    n = N_elements(st)
 
; get disk

    disk = st 
    replicate_inplace,disk, ''
    version =disk
    qual = disk
    dir = disk

; get dir

    lpos = strpos(st,'/',/reverse_search)
    good = where(lpos GE 0, Ngood)
    if Ngood GT 0 then begin 
          stg = st[good] 
          lpos = reform( lpos[good],1, Ngood )
 
             dir[good] = strmid(stg,0, lpos+1) 
             st[good] = strmid(stg,lpos+1 )
    endif
; get name and qual

   name = st
   lpos = strpos(st,'.',/reverse_search)
    good = where(lpos GE 0, Ngood)
 
    if Ngood GT 0 then begin
             stg = st[good]
             lpos = reform( lpos[good], 1, Ngood )
             name[good] = strmid(stg,0,lpos )
             qual[good] = strmid(stg,lpos+1 )
     endif
 

 end 

ENDCASE                         ; end OTHER version

  if scalar then begin
       name = name[0]
       qual = qual[0]
       disk = disk[0]
       dir = dir[0]
       version = version[0]
       filename = filename[0]
  endif
  return
  end
