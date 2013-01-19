	SUBROUTINE RD_GITM ( segmnt, lens, ispnt, isstar, dttmsn, iret )
C************************************************************************
C* RD_GITM								*
C*									*
C* This subroutine finds and decodes the issue time into an integer	*
C* time array.  Then it converts it to the closest 3 hour synoptic 	*
C* GEMPAK time.								*
C*									*
C* RD_GITM  ( SEGMNT, LENS, ISPNT, ISSTAR, DTTMSN, IRET )		*
C*									*
C* Input parameters:							*
C*      SEGMNT		CHAR*		Bulletin segment		*
C*      LENS		INTEGER		Length of segment		*
C*									*
C* Input and output parameters:						*
C*	ISPNT		INTEGER		Segment pointer			*
C*									*
C* Output parameters:							*
C*      ISSTAR (5)	INTEGER		Integer issue time array	*
C*	DTTMSN		CHAR*		Closest 3HR synoptic GEMPAK time*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = issue time not found	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	segmnt, dttmsn
	INTEGER		isstar (*) 
C*
        CHARACTER       stlist(2)*4, lctmst*150, lctend*150
        INTEGER         ilens(2), idtsyn (5)
        DATA            stlist / ' AM ', ' PM ' /
        DATA            ilens / 4, 4 /
C------------------------------------------------------------------------
	iret = 0
	dttmsn = ' '
	isstar (1) = 9999
	DO ij = 1,4
	    isstar (ij) = 99
	END DO
C
C*	Find 'AM' or 'PM'
C
	ilpnt = MAX ( ispnt + 200, lens - 150 ) 
    	CALL ST_NXTS ( segmnt, ispnt, ilpnt, stlist, ilens, 2, ipos,
     +		istrg, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Find beginning of local time string
C
	lenhr = 4
	ibghr = ipos - lenhr
	CALL SV_ALNM ( segmnt ( ibghr:ipos - 1 ), lenhr, ityp, ier )
	IF ( ityp .ne. 1 ) THEN
	    lenhr = 3
	    ibghr = ipos - lenhr
	    CALL SV_ALNM ( segmnt ( ibghr:ipos - 1 ), lenhr, ityp, ier )
	    IF ( ityp .ne. 1 ) THEN
		iret = -1	
		RETURN
	    END IF
	END IF
C
C*	Find end of string
C
	ispnt = INDEX ( segmnt ( ibghr:lens ), CHCR ) + ibghr - 2
C
C*	Remove strings inside parenthesis.
C
	lenlc = ispnt - ibghr + 1
	IF ( lenlc .gt. 150 ) THEN
	    iret = -1
	    RETURN
	END IF
	lctmst = segmnt ( ibghr:ispnt )
	ix = 1
	in = 1
	iparl = INDEX ( lctmst ( ix:lenlc ), '(' )
	DO WHILE ( iparl .ne. 0 .and. ix .lt. lenlc ) 
	    iparr = INDEX ( lctmst ( ix:lenlc ), ')' )
	    IF ( iparr .eq. 0 ) THEN
		iret = -1
		RETURN
	    END IF
	    jend = iparr - iparl + 1
	    lctend = lctmst ( ix + iparr:lenlc )
	    jnd = lenlc - iparr + 1
	    ix = ix + iparl - 2
    	    kend = ix + iparr - 1
	    lctmst ( ix:lenlc ) = lctend ( 1:jnd )
	    iparl = INDEX ( lctmst ( ix:lenlc ), '(' )
	END DO 
C
C*	Convert plain language local date/time string to a GEMPAK
C*	time string.
C
	CALL TI_LOCL ( lctmst, dttmsn, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Convert GEMPAK issue time to integer array
C
	CALL TI_CTOI ( dttmsn, isstar, ier )
C
C*	Round time to the nearest hour.
C*      But the integer array isstar will not be rounded
C
	DO mn = 1,5
	    idtsyn (mn) = isstar (mn)
	END DO
	idtsyn (5) = 0
	IF ( isstar (5) .ge. 30 ) THEN
	    CALL TI_ADDM ( idtsyn, 60, idtsyn, ier ) 
	END IF
C
C*	Find the closest 3-hour synoptic time 
C
	irem = MOD ( idtsyn (4), 3 )
	IF ( irem .eq. 1 ) THEN
	    CALL TI_SUBM ( idtsyn, 60, idtsyn, ier ) 
	  ELSE IF ( irem .eq. 2 ) THEN
	    CALL TI_ADDM ( idtsyn, 60, idtsyn, ier ) 
	END IF
C
C*	Convert the synoptic integer time array to GEMPAK time
C
	CALL TI_ITOC ( idtsyn, dttmsn, ier )
C*
	RETURN
	END
