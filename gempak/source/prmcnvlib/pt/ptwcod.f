	CHARACTER*(*) FUNCTION PT_WCOD  ( wnum )
C************************************************************************
C* PT_WCOD								*
C*									*
C* This function converts a GEMPAK numeric weather code, WNUM, 		*
C* into a character code, WCOD:						*
C*									*
C*                    WCOD = PT_WCOD ( WNUM )				*
C*									*
C* WCOD can be converted to WNUM using PT_WNUM.				*
C*									*
C* PT_WCOD ( WNUM )							*
C*									*
C* Input parameters:							*
C*	WNUM		REAL		Weather number 			*
C*									*
C* Output parameters:							*
C*	PT_WCOD		CHAR*		Character weather string	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/87						*
C* M. desJardins/GSFC	11/89	Added SPW				*
C* S. Jacobs/NCEP	11/96	Added Q, V, PO, UP, BD+, BN+, BS+	*
C************************************************************************
	CHARACTER	wthr*80, part (3)*4
	INTEGER		inum (3)
C
C*	The following table of parts is used to assign number to the
C*	weather parts.  If new names are added to the table, the
C*	longer codes must follow shorter ones.  Also, the parameters
C*	which locate parts of various lengths must be updated.  The
C*	list must also be changed in PT_WCOD.
C
	CHARACTER	wpart (79)*4
	DATA		wpart / 'R', 'L', 'S', 'A', 'T', 'H', 'K', 
     +				'D', 'F', 'Q', 'V', ' ',
     +			        'R-', 'R+', 'ZR', 'RW', 'L-', 'L+',
     +				'ZL', 'S-', 'S+', 'SW', 'IP', 'SG', 
     +				'SP', 'A-', 'A+', 'T-', 'T+', 'IF', 
     +				'GF', 'BS', 'BD', 'BY', 'BN', 'IC',
     +				'IN', 'AP', 'KH', 'PO', 'UP', 7 * ' ',
     +				'ZR-', 'ZR+', 'RW-', 'RW+', 'ZL-',
     +				'ZL+', 'SW-', 'SW+', 'IP-', 'IP+',
     +				'SG-', 'SG+', 'SP-', 'SP+', 'IPW',
     +				'IC-', 'IC+', 'TRW', 'SPW', 'BD+',
     +				'BN+', 'BS+', 4 * ' ',
     +				'IPW-', 'IPW+', 'TRW-', 'TRW+', ' ' /
C------------------------------------------------------------------------
C*	Check for special codes.
C
	IF  ( wnum .eq. 0. )  THEN
	    PT_WCOD = ' '
	    RETURN
	  ELSE IF  ( wnum .eq. -1. )  THEN
	    PT_WCOD = 'TORNA'
	    RETURN
	  ELSE IF  ( wnum .eq. -2. )  THEN
	    PT_WCOD = 'FUNNE'
	    RETURN
	  ELSE IF  ( wnum .eq. -3. )  THEN
	    PT_WCOD = 'WATER'
	    RETURN
	END IF
C
C*	Break input into three numbers.
C
	num = wnum
	inum (1) = MOD ( num, 80 )
	num = ( num - inum (1) ) / 80
	inum (2) = MOD ( num, 80 )
	num = ( num - inum (2) ) / 80
	inum (3) = num
C
C*	Get each part of the number.
C
	DO  iprt = 1, 3
C
C*	    Check that this is in the proper range.
C
	    IF  ( ( inum ( iprt ) .le. 0 ) .or. 
     +		  ( inum ( iprt ) .gt. 79 ) )  THEN
		part ( iprt ) = ' '
	      ELSE
		part ( iprt ) = wpart ( inum ( iprt ) )
	    END IF
	END DO
C
C*	Combine strings and remove blanks.
C
	wthr = part (1) // part (2) // part (3)
	CALL ST_RMBL  ( wthr, wthr, length, ier )
	PT_WCOD = wthr
C*
	RETURN
	END
