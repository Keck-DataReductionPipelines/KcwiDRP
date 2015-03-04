pro getpro,proc_name            ;Obtain a copy of a procedure
;+
; NAME:
;     GETPRO
; PURPOSE:
;     Search !PATH for a procedure, and copy into user's working directory
; EXPLANATION:
;     Extract a procedure from an IDL Library or directory given in the 
;     !PATH  system variable and place it in the current default directory
;     (presumably to be edited by the user). 
;
; CALLING SEQUENCE:
;     GETPRO, [ proc_name ]          ;Find PROC_NAME in !PATH and copy
;
; OPTIONAL INPUT:
;     proc_name - Character string giving the name of the IDL procedure or 
;               function.  Do not give an extension.   If omitted, 
;               the program will prompt for PROC_NAME.   
;
; OUTPUTS:
;     None.
;
; SIDE EFFECTS:
;      A file with the extension .pro and a name given by PROC_NAME will
;      be created on the user's directory.
;
; PROCEDURE:
;      The FILE_WHICH() function is used to locate the procedure in the IDL
;      !PATH.     When found, FILE_COPY is used to 
;      copy the procedure into the user's current default directory.    If not 
;      found in !PATH, then the ROUTINE_INFO() function is used to determine 
;      if it is an intrinsic IDL procedure.  
;
; EXAMPLE:
;       Put a copy of the USER library procedure CURVEFIT on the current
;       directory
;
;       IDL> getpro, 'CURVEFIT'
;
; RESTRICTIONS:
;       User will be unable to obain source code for a native IDL function
;       or procedure, or for a FORTRAN or C routine added with CALL_EXTERNAL.
;       User must have write privilege to the current directory
;
; PROCEDURE CALLS:
;       FDECOMP, ZPARCHECK
; REVISION HISTORY:
;      Written W. Landsman, STX Corp.   June 1990
;      Now use intrinsic EXPAND_PATH() command  W. Landsman November 1994
;      Use ROUTINE_NAMES() to check for intrinsic procs  W. Landsman July 95
;      Update for Windows/IDL     W. Landsman      September 95
;      Check if procedure is in current directory  W. Landsman  June 1997
;      Use ROUTINE_INFO instead of undocumented ROUTINE_NAMES W.L. October 1998
;      Use FILE_WHICH() to locate procedure W. Landsman May 2006 
;      Assume since V5.5, remove VMS support  W. Landsman Sep 2006
;-
  On_error,2                                     ;Return to caller on error
  compile_opt idl2


  if N_params() EQ 0 then begin ;Prompt for procedure name?
        proc_name = ' ' 
        read,'Enter name of procedure you want a copy of: ',proc_name     
  
  endif else zparcheck, 'getpro', proc_name, 1, 7, 0, 'Procedure name'

  fdecomp, proc_name, disk, dir, name      ;Don't want file extensions
  name = strtrim( name, 2 )  

;First check if procedure is already on current directory (no overwriting)

  if file_test(name + '.pro') then begin
       message,name + '.pro already exists in the current directory',/INF
        return
  endif 

; Now make sure user has write privileges

  openw, lun, 'temp.tmp', /DELETE, /GET_LUN, ERROR=ERR
  if err NE 0 then begin               ;Problem writing a temporary file?
     cd,current=curdir   
     message,curdir + ' has insufficient privilege or file protection violation'
  endif
  free_lun,lun

;Locate file in the user's !PATH  

  fname = file_which(name + '.pro')
  if fname NE '' then begin           ;File found? 
         file_copy,fname, name + '.pro' 
         message,'Procedure '+ NAME + '.pro copied from '+ fname,/INF
         return
   endif else begin

; Now check if it is an intrinsic IDL procedure or function.  

  funcnames = routine_info(/system,/func)
  name = strupcase(name)
  test = where ( funcnames EQ name, fcount)

  funcnames = routine_info(/system)
  test = where ( funcnames EQ name, pcount)

  if (fcount EQ 0) and (pcount EQ 0) then begin   

     message,'Procedure '+NAME+' not found in the !PATH search string',/CONT
     message,'Check your spelling or search the individual directories',/INF

  endif else begin     

  if fcount GT 0 then $
       message,NAME + ' is an intrinsic IDL function',/CONT  $
  else message,NAME + ' is an intrinsic IDL procedure',/CONT
       message,'No source code is available',/INF

  endelse
  endelse
  return
  
  end 
