;+
; NAME:
;    cputime
; PURPOSE: (one line)
;    Return the accumulated user and system times since an arbitrary time.
; DESCRIPTION:
;    The overhead in using this procedure is about 3 milliseconds per call.
; CATEGORY:
;    Utility
; CALLING SEQUENCE:
;    cputime, utime, stime
; INPUTS:
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;    utime : CPU user time in seconds.
;    stime : CPU system time in seconds.
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The CALL_EXTERNAL function of IDL is used to communicate with a function
;  written in C.  This function is cputime.c which, compiled and linked, has
;  entry point 'cputime' in the Shareable Object Library file 'cputime.so' in
;  /gryll/data1/dwl/idl.
;    See /gryll/data1/dwl/idl/cputime.c for further information.
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, April, 1993.
;-
PRO cputime, utime, stime
utime = 0.0
stime = 0.0
; This is a dummy routine just in case the DLM does not exist
END
