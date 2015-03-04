/database                                                 December 2006

This subdirectory contains the IDL procedures, mostly written by Don Lindler,
(ACC/Goddard) to create and access an IDL database.    Although the core
software is quite old (written in 1987?), it still allows the use of a simple
database well suited to the IDL environment. The LaTex file database.tex in the
/text directory describes  the use of the database procedures with an emphasis
on astronomical  applications.    

The database procedures require the non-standard system variables
!PRIV, !TEXTOUT, and !TEXTUNIT.   These can be added to the IDL session 
by using the procedure ASTROLIB.

Sample astronomical catalogs, formatted as IDL databases, are available in the
/zdbase directory.   These include the Yale Bright Star Catalog, the 
RC3 Galaxy catalog, and a catalog of HST observations.


Dec-2006:    DBINDEX - Automatically enlarge the index (.dbx) file if necessary.  
	     Avoid use of EXECUTE() if possible.

Nov-2006:    A version of DBBUILD in the /V61 directory now allows up to 50
              input items, and does not use the EXECUTE() routine.

26-Sep-2006: Removed DBCOMPRESS since DBDELETE now calls TRUNCATE_LUN to 
             compress the file after removing entries.
	     
14-Jul-2006: DBCREATE - New MAXENTRY keyword to override hardcoded #maxentries	     

24-Sep-2002:  DBTARGET() - Find database entries within specified radius of 
                     supplied astronomical target name.
                   
20-Sep-2002: DBPRINT - New /AdjustFormat will adjust the format length for 
                string items to the maximum length for the entries to be 
                printed.    


23-Jul-2001: DBCREATE, DBFIND, DBFIND_SORT, DBXVAL, DBINDEX_BLK, DBBUILD -
             Now allow user to specify items with 64 bit or unsigned integer
             datatypes.

2-Dec-2000:  DBINDEX, DBEXT_IND - Now allow multiple-valued index items
             Minor updates to database.tex file

17-Nov-2000: DBFIND, DBFIND_ENTRY, DBFIND_SORT, DBSEARCH, IMDBASE - Now
                have a Count keyword; deprecate use of !ERR

11-Oct-2000: DBXVAL - Work for multiple-valued strings

08-Aug-2000: DBPRINT - Change a loop index to type LONG (sigh...)

5-Feb-2000:  DBPRINT - Only byteswap when necessary, for much improved speed.

1-Nov-1999:  DBINDEX, DBEXT_DBF - Now allow a maximum of 18 instead of 15 items



 
