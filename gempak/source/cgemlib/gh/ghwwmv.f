	SUBROUTINE GH_WWMV ( part, npart, czlist, len, iret )
C************************************************************************
C* GH_WWMV                                                              *
C*									*
C* This subroutine moves a list of strings to a single output string,   *
C* using ';' as a separator.  Blank substrings and leading and trailing *
C* ';' are removed.                                                     *
C*                                                                      *
C* GH_WWMV ( PART, NPART, CZLIST, LEN, IRET )                           *
C*                                                                      *
C* Input parameters:                                                    *
C*	PART(*)		CHAR* 		List of county or zone UGCs     *
C*	NPART		INTEGER		Number of items in list         *
C*									*
C* Output parameters:                                                   *
C*	CZLIST		CHAR*		String of county or zone UGCs   *
C*	LEN		INTEGER		Length of UGC string            *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C* D. Kidwell/NCEP	 3/04	Replaced ST_RMBL with in-line code	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   part (*), czlist
C*
        CHARACTER    	ttt*1000, cc*1
C-----------------------------------------------------------------------
	iret = 0
C
C*	Move the list to the output string, eliminating blank
C*	substrings.  Do not use ST_RMBL because of its limit of 160 
C*	characters.
C 
	CALL ST_LSTC ( part, npart, ';', czlist, ier )
	CALL ST_LSTR ( czlist, lencz, ier )
	len   = 0
	ttt   = ' '
	DO ii = 1, lencz
	    cc = czlist ( ii:ii )
	    IF ( cc .ne. CHSPAC ) THEN
		len = len + 1
		ttt ( len:len ) = cc
	    END IF
	END DO
	czlist = ttt
C
	ipos = 1
	DO WHILE ( ipos .ne. 0 )
	    CALL ST_RPST ( czlist, ';;', ';', ipos, czlist, ier )
	END DO
C
	CALL ST_LSTR ( czlist, len, ier )
	IF ( czlist ( 1:1 ) .eq. ';' ) THEN
	    DO ii = 1, len - 1
	        czlist ( ii:ii ) = czlist ( ii + 1:ii + 1 )
	    END DO
	    czlist ( len:len ) = ' '
	    len = len - 1
	END IF
	IF ( czlist ( len:len ) .eq. ';' ) THEN
	    czlist ( len:len ) = ' '
	    len = len - 1
	END IF
C*
	RETURN
	END
