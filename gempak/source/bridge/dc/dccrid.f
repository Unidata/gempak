	SUBROUTINE DC_CRID  ( buln2, crid, lcrid, iret )
C************************************************************************
C* DC_CRID								*
C*									*
C* This subroutine locates and decodes the reconnaissance identifier	*
C* from the second line of a dropwinsonde or RECCO bulletin.		*
C*									*
C* DC_CRID  ( BULN2, CRID, LCRID, IRET )				*
C*									*
C* Input parameters:							*
C*	BULN2		CHAR*		Second line of bulletin 	*
C*									*
C* Output parameters:							*
C*	CRID		CHAR*		Reconnaissance identifier 	*
C*	LCRID		INTEGER		Length of CRID		 	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = could not locate and/or	*
C*					      decode reconnaissance ID	*
C**									*
C* Log:									*
C* J. Ator/NCEP		11/96						*
C* J. Ator/NCEP		08/97	UT_CRID -> AF_CRID, style changes	*
C* D. Kidwell/NCEP	12/97	Renamed from AF_CRID                    *
C************************************************************************
	CHARACTER*(*)	buln2, crid
C*
	CHARACTER	cwkb2*200, cwkid*30
C------------------------------------------------------------------------
C
C*	Initialize output variables.
C
	iret = -1
	crid = ' '
	lcrid = 0
C
C*	From the input string, create a work copy which has all
C*	leading blanks and/or tabs removed.
C
	CALL ST_LDSP  ( buln2, cwkb2, lcwkb2, ierlsp )
	IF  ( lcwkb2 .le. 0 )  THEN
	    RETURN
	END IF
C
C*	The first field at the beginning of the work copy should
C*	contain the aircraft flight ID.  Use the last 3 characters
C*	of this ID to begin building the reconnaissance identifier.
C 
	CALL ST_CLST  ( cwkb2 ( 1 : lcwkb2 ), ' ', ' ', 1,
     +			cwkid, nst, ierlst )
	IF  ( nst .ne. 1 )  THEN
	    RETURN
	END IF
C
	CALL ST_LSTR  ( cwkid, lcwkid, ierstr )
	IF  ( ( lcwkid .lt. 3 ) .or. ( lcwkid .gt. 7 ) )  THEN
	    RETURN
	END IF
C
	DO ii = ( lcwkid - 2 ), lcwkid
	    lcrid = lcrid + 1
	    crid ( lcrid : lcrid ) = cwkid ( ii : ii )
	END DO
C
C*	Locate the "OB" marker within the remainder of the work copy.
C
	IF  ( lcwkid .ge. lcwkb2 )  THEN
	    RETURN
	END IF
	ipt1 = INDEX ( cwkb2 ( ( lcwkid + 1 ) : lcwkb2 ), ' OB' )
	IF  ( ipt1 .eq. 0 )  THEN
	    RETURN
	END IF
C
C*	The next field after the "OB" marker should be the observation
C*	number.  Append this number to the reconnaissance identifier.
C
	ipt2 = lcwkid + ipt1 + 3
	IF  ( ipt2 .gt. lcwkb2 )  THEN
	    RETURN
	END IF
C
	CALL ST_CLST  ( cwkb2 ( ipt2 : lcwkb2 ), ' ', ' ', 1,
     +			cwkid, nst, ierlst )
	IF  ( nst .ne. 1 )  THEN
	    RETURN
	END IF
C
	CALL ST_LSTR  ( cwkid, lcwkid, ierstr )
	IF  ( ( lcwkid .lt. 1 ) .or. ( lcwkid .gt. 3 ) )  THEN
	    RETURN
	END IF
C
	DO ii = 1, lcwkid
	    IF  ( ( cwkid ( ii : ii ) .ge. '0' ) .and.
     +		  ( cwkid ( ii : ii ) .le. '9' ) )  THEN
		lcrid = lcrid + 1
		crid ( lcrid : lcrid ) = cwkid ( ii : ii )
	    END IF
	END DO
C
C*	Append an "A" to the reconnaissance identifier.
C
	lcrid = lcrid + 1
	crid ( lcrid : lcrid ) = 'A'
C
	iret = 0
C*
	RETURN
	END
