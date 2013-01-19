	CHARACTER*(*) FUNCTION PT_ACTP ( atpn )
C************************************************************************
C* PT_ACTP								*
C*									*
C* This function converts a GEMPAK numeric aircraft type, ATPN, into a 	*
C* character aircraft type, ACTP:					*
C*									*
C*                    ACTP = PT_ACTP ( ATPN )				*
C*									*
C* ACTP can be converted to ATPN using PT_PATN.				*
C*									*
C* CHAR* PT_ACTP ( ATPN )						*
C*									*
C* Input parameters:							*
C*	ATPN		REAL		GEMPAK numeric aircraft type    *
C*									*
C* Output parameters:							*
C*	PT_ACTP		CHAR*		Character aircraft type string  *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 1/00                                           *
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	acftyp*8, acchar (4)*1
	INTEGER		inum (4)
C*
C*	The following table is used to assign numbers to the aircraft
C*	type characters.  If any changes are made to this table,
C*	corresponding changes must be made to function PT_PATN.
C*
	CHARACTER	apart (39)*1
	INCLUDE		'ERMISS.FNC'
	DATA		apart / '-', '.', '/', '0', '1', '2', '3', 
     +				'4', '5', '6', '7', '8', '9',
     +				'A', 'B', 'C', 'D', 'E', 'F', 'G',
     +				'H', 'I', 'J', 'K', 'L', 'M', 'N',
     +				'O', 'P', 'Q', 'R', 'S', 'T', 'U',
     +				'V', 'W', 'X', 'Y', 'Z' /
C*
C------------------------------------------------------------------------
	IF ( ERMISS ( atpn ) ) THEN
	    PT_ACTP = ' '
	    RETURN
	END IF
C
C*	Break input into four numbers.
C
	num = NINT ( atpn )
	inum (1) = MOD ( num, 40 )
	num = ( num - inum (1) ) / 40
	inum (2) = MOD ( num, 40 )
	num = ( num - inum (2) ) / 40
	inum (3) = MOD ( num, 40 )
	num = ( num - inum (3) ) / 40
	inum (4) = num
C
C*	Get each part of the number.
C
	DO  ii = 1, 4
C
C*	    Check that this is in the proper range.
C
	    IF  ( ( inum ( ii ) .le. 0 ) .or. 
     +		  ( inum ( ii ) .gt. 39 ) )  THEN
		acchar ( ii ) = ' '
	      ELSE
		acchar ( ii ) = apart ( inum ( ii ) )
	    END IF
	END DO
C
C*	Combine strings and remove blanks.
C
	acftyp = acchar (1) // acchar (2) // acchar (3) // acchar (4)
	CALL ST_RMBL  ( acftyp, acftyp, length, ier )
	PT_ACTP = acftyp
C*
	RETURN
	END
