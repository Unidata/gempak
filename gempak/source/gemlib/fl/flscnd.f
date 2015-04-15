	SUBROUTINE FL_SCND  ( path, tmplt, isort, nexp, files, nfile, 
     +                        iret )
C************************************************************************
C* FL_SCND								*
C* 									*
C* This subroutine scans a directory for the files matching the given	*
C* file name template.							*
C* 									*
C* FL_SCND  ( PATH, TMPLT, ISORT, NEXP, FILES, NFILE, IRET )		*
C* 									*
C* Input parameters:							*
C*	PATH		CHAR*		Directory path			*
C*	TMPLT		CHAR*		Filename template		*
C*	ISORT		INTEGER		Sorting order			*
C*					   1 = Alphabetical		*
C*					  -1 = Reverse alphabetical	*
C*	NEXP		INTEGER		Expected returned files number  *  
C*									*
C* Output parameters:							*
C*	FILES (NFILE)	CHAR*		Array of file names		*
C*	NFILE		INTEGER		Number of file names		*
C*	IRET		INTEGER		Return code			*
C*					    0 = normal return		*
C*					  -12 = cannot scan directory	*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 8/98						*
C* S. Jacobs/NCEP	 8/99	Added sorting order to call sequence	*
C* M. Li/GSC		 5/00	Set a maximum to nfile			*
C* A. Hardy/SAIC         2/02   Added variable nexp to call sequence	*
C* S. Jacobs/NCEP	 8/14	Added support for FFFFF template 	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	path, tmplt, files(*)
C
	CHARACTER	filnam*(MXNMFL*MXFLSZ), sep*1, templ*160
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert the GEMPAK template to use metacharacters for the
C*	directory scan in the C funtion.
C
	templ = tmplt
	CALL ST_RPST  ( templ, 'YYYY', '[0-9][0-9][0-9][0-9]',
     +			ipos, templ, ier )
	CALL ST_RPST  ( templ, 'YY', '[0-9][0-9]', ipos, templ, ier )
	CALL ST_RPST  ( templ, 'MMM', '[A-Za-z][A-Za-z][A-Za-z]',
     +			ipos, templ, ier )
	CALL ST_RPST  ( templ, 'MM', '[0-9][0-9]', ipos, templ, ier )
	CALL ST_RPST  ( templ, 'DD', '[0-9][0-9]', ipos, templ, ier )
	CALL ST_RPST  ( templ, 'HH', '[0-9][0-9]', ipos, templ, ier )
	CALL ST_RPST  ( templ, 'NN', '[0-9][0-9]', ipos, templ, ier )
	CALL ST_RPST  ( templ, 'DWK', '[A-Za-z][A-Za-z][A-Za-z]',
     +			ipos, templ, ier )
	CALL ST_RPST  ( templ, 'FFFFF', '[0-9][0-9][0-9][0-9][0-9]',
     +			ipos, templ, ier )
	CALL ST_RPST  ( templ, 'FFF', '[0-9][0-9][0-9]',
     +			ipos, templ, ier )
	CALL ST_RPST  ( templ, 'FF', '[0-9][0-9]', ipos, templ, ier )
C
C*	Call the C function to perform the directory scan.
C
	CALL ST_LSTR  ( path, lenp, ier )
	CALL ST_LSTR  ( templ, lent, ier )
	lenf   = 0
C
	sep    = ';'
	maxlen = MXNMFL * MXFLSZ 
	CALL CFL_SCND ( path, lenp, templ, lent, sep, maxlen, isort,
     +			filnam, lenf, nf, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    iret  = -12
	    nfile = 0
	    RETURN
	END IF
C
C*	Separate the string of file names into individual names.
C
	IF ( nf .gt. nexp ) THEN
	    nf = nexp
	    IF ( nf .gt. MXNMFL ) THEN
	        nf = MXNMFL
	    END IF
	END IF
	CALL ST_CLSL ( filnam(:lenf), sep, ' ', nf, files, nfile, ier )
C*
	RETURN
	END
