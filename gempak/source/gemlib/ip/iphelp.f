	SUBROUTINE IP_HELP  ( pname, pagflg, iret )
C************************************************************************
C* IP_HELP								*
C*									*
C* This subroutine writes a help file for a variable, unless PNAME	*
C* is blank.  In that case, help for the program will be written.	*
C*									*
C* IP_HELP  ( PNAME, PAGFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		Variable			*
C*	PAGFLG		LOGICAL		Flag to indicate paging		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					 -6 = no help available		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* M. desJardins/GSFC	 4/90	Added call to FL_INQR			*
C* M. desJardins/GSFC	 7/90	Added help on program			*
C* M. desJardins/GSFC	10/90	Fix help for GFUNC, GVECT, TRACEx	*
C* M. desJardins/NMC	 1/92	Change so that HELP alone gives NOTAE 	*
C*				help; also combine NT_HELP & NT_PHLP	*
C* M. desJardins/NMC	 2/92	Fix for ambiguous parameters		*
C* K. Brill/NMC		 5/93	Remove unneeded ST_LCUC & ST_LSTR	*
C* S. Jacobs/NMC         2/94   Added file name return to FL_INQR       *
C* K. Tyle/GSC		 7/96	Renamed from NT_HELP			*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* K. Tyle/GSC		 2/97	Search current directory for .hl2, .hlp	*
C* T. Piper/SAIC	02/04	Changed the location of help files	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pname
	LOGICAL		pagflg
C*
	CHARACTER	hlpfil*72, record*80, ppp*12, answer*12,
     +			newfil*132
	LOGICAL		exist
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get variable for help.
C
	CALL ST_LDSP  ( pname, ppp, lenin, ier )
C
C*	If general help was requested, get help on program.
C
	IF  ( lenin .eq. 0 )  THEN
	    ppp = 'NOTAE'
	    hlpfil = '$GEMHLP/hlp/NOTAE.HLP'
	    CALL FL_INQR  ( hlpfil, exist, newfil, ier )
	  ELSE
C
C*	    Check for abbreviation.
C
	    CALL IP_FVAR  ( ppp, ifound, ier )
C
C*	    If this is not a parameter, maybe it it a program.
C
	    IF  ( ier .ne. 0 )  THEN
		iret = ier
		RETURN
	      ELSE IF  ( ifound .ne. 0 )  THEN
		ppp = cparmn ( ifound )
		CALL ST_LSTR  ( ppp, lenin, ier )
	    END IF
C
C*	    Check for the special case of GFUNC or GVECT or TRACE.
C
	    IF  ( ( ppp .eq. 'GFUNC' ) .or. ( ppp .eq. 'GVECT' ) ) 
     +		       THEN
		ppp = 'GPARM'
		lenin = 5
	      ELSE IF  (  ppp (1:5) .eq. 'TRACE' )  THEN
		ppp = 'TRACE'
		lenin = 5
	    END IF
C
C*	    Check for .HL2 file in current directory. 
C
	    hlpfil = ppp ( : lenin ) // '.HL2'
C
C*	    Inquire whether the file is available.
C
	    CALL FL_INQR  ( hlpfil, exist, newfil, ier )
C
C*	    If the file is not found, check $GEMHLP directory.
C
	    IF  ( .not. exist )  THEN
	    	hlpfil = '$GEMHLP/hlx/' // ppp ( : lenin ) // '.HL2'
	    	CALL FL_INQR  ( hlpfil, exist, newfil, ier )
	    END IF
C
C*	    If the file is not found, maybe it is a program.
C
	    IF  ( .not. exist )  THEN
		hlpfil = ppp ( : lenin ) // '.HLP'
		CALL FL_INQR  ( hlpfil, exist, newfil, ier )
	    END IF
C
C*	    If the file is not found, check $GEMHLP directory.
C
	    IF  ( .not. exist )  THEN
		hlpfil = '$GEMHLP/hlp/' // ppp ( : lenin ) // '.HLP'
		CALL FL_INQR  ( hlpfil, exist, newfil, ier )
	    END IF
	END IF
C
C*	If the file does not exist, write an error message and return.
C
	IF  ( .not. exist )  THEN
	    ier = -6
	    CALL ER_WMSG  ( 'IP', ier, ppp, ierr )
	    RETURN
	END IF
C
C*	Open the help file and check for error.
C
	CALL FL_SOPN  ( hlpfil, lun, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Read a record and write it to the terminal.
C
	iostat = 0
	knt    = 0
	IF  ( pagflg )  THEN
	    maxknt = 30
	  ELSE
	    maxknt = 100000
	END IF
	DO WHILE  ( iostat .eq. 0 )
C
C*	    Page if too many lines have been written.
C
	    IF  ( knt .ge. maxknt )  THEN
		CALL TM_PROM ( 'Enter <CR> to page',
     +				.false., .false., ier )
		CALL TM_RCHR ( answer, ier )
		IF  ( ier .eq. 2 )  THEN
		    iostat = -1
		END IF
		knt = 0
	    END IF
	    knt = knt + 1
	    IF  ( iostat .eq. 0 )  THEN
		READ ( lun, 2000, IOSTAT = iostat ) record
2000		FORMAT ( A )
	    END IF
	    IF  ( iostat .eq. 0 )  THEN
		CALL ST_LSTR  ( record, lenr, ier )
		IF  ( lenr .eq. 0 )  lenr = 1
		WRITE  (  6,  2001 ) record ( : lenr )
2001		FORMAT ( 1X, A )
	    END IF
	END DO
C
C*	Close the file.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
