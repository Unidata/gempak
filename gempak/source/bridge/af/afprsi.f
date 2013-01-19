	SUBROUTINE AF_PRSI  ( string, iret )
C************************************************************************
C* AF_PRSI								*
C*									*
C* This subroutine gets the reporting station id for a PIREP.  The      *
C* reporting station id is the station id which immediately precedes    *
C* the characters UA or UUA.                                            *
C*									*
C* AF_PRSI  ( STRING, IRET )						*
C*									*
C* Input parameters:                                                    *
C*	STRING		CHAR*		String preceding UA or UUA      *
C*									*
C* Output parameters:							*
C*	CIVALS (ICRSID) CHAR*		Reporting station id            *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 1/00	                                        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*	
	CHARACTER*(*)	string
C*
	CHARACTER	carr (5)*8, rsid*8
C-----------------------------------------------------------------------
	iret = 0
C
	CALL ST_CLST ( string, ' ', ' ', 5, carr, num, ier )
	IF ( ( ier .eq. 1 ) .or. ( num .eq. 0 ) )  RETURN
	rsid = carr ( num )
	CALL ST_LSTR ( rsid, lens, ier )
	IF ( ( lens .eq. 4 ) .and. ( rsid ( :1 ) .eq. 'K' ) ) 
     +	       rsid = rsid ( 2:lens )
	civals ( icrsid ) = rsid
C*
	RETURN
	END
