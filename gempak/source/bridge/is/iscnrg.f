	SUBROUTINE IS_CNRG ( report, lenr, maxs, trep, nlen, iret )
C************************************************************************
C* IS_CNRG 								*
C*									*
C* This subroutine removes geographical reference points and replaces	*
C* slashes with spaces in a Canadian SIGMET area string.		*
C*                                                                      *
C* IS_CNRG ( REPORT, LENR, MAXS, TREP, NLEN, IRET )	      		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Canadian SIGMET area string     *
C*	LENR		INTEGER		Length of string report         *
C*	MAXS		INTEGER		Maximum length of string        *
C*									*
C* Output parameters:							*
C*	TREP		CHAR*		New Canadian SIGMET area string *
C*	NLEN		INTEGER		New length of string trep	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					  1 = no slashes		*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	10/03	Created					*
C* F. J. Yen/NCEP	12/03	Return 1 when no slashes in area string.* 
C************************************************************************
    	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, trep
C*
C------------------------------------------------------------------------
	iret  = 0
C
	nslsh = INDEX ( report(:lenr), '/' )
	IF ( nslsh .eq. 0 ) THEN
	    nlen = lenr
	    trep(:nlen) = report(:lenr)
	    iret = 1
	    RETURN
	END IF
	io = nslsh + 1
	nlen = 0
	DO WHILE ( io .lt. lenr )
	    nlen = nlen + 1
	    trep ( nlen:nlen ) = ' '
C
C*	    Copy the lat/lon field up to the next slash
C
	    DO WHILE ( (report (io:io) .ne. '/') .and. (io .le. lenr) )
		nlen = nlen + 1
		trep ( nlen:nlen ) = report (io:io )
		io = io + 1
	    END DO
C
C*	    Skip over the geographical reference and slashes
C
	    nlen = nlen + 1
	    trep ( nlen:nlen ) = ' '
	    io = io + 1
	    DO WHILE ( (report (io:io) .ne. '/') .and. (io .le. lenr) )
		io = io + 1
	    END DO
	    io = io + 1
	END DO
C*
	RETURN
	END
