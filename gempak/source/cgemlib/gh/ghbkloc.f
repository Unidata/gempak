       SUBROUTINE GH_BKLOC ( stid, state, country, iret )
C************************************************************************
C* GH_BKLOC								*
C*									*
C* This subroutine looks up the state and country of a station id.      *
C*									*
C* GH_BKLOC ( STID, STATE, COUNTRY, IRET )                              *
C*									*
C* Input parameters:							*
C*	STID 		CHAR*		Station ID               	*
C*									*
C* Output parameters:							*
C*	STATE 		CHAR*		State abbreviation		*
C*	COUNTRY 	CHAR*		Country abbreviation		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					  1 = no sequence number found  *
C*									*
C**									*
C* Log:									*
C* S. Gilbert/NCEP    	10/07		Created				*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	CHARACTER	stid*8, stid_uc*8
        CHARACTER*(*)   state, country
	INTEGER		icounter, ier
	LOGICAL		found
C*
C-----------------------------------------------------------------------
	iret     = 0
	found    = .false.
	icounter = 1
	iseq     = 0
	iarea    = 0
C
	DO WHILE ( ( .not. found ) .and. ( icounter .le. MAXBK ) )
	    CALL ST_LCUC( bkstid( icounter ), stid_uc, ier )
	    IF ( stid_uc .eq. stid ) THEN
	       found = .true. 
	       state = bkstate( icounter ) 
	       country = bkcntry( icounter )
	    END IF
	    icounter = icounter + 1
	END DO 
C
	IF ( .not. found ) THEN
	   iret = 1
	   RETURN
	END IF
C*
	RETURN
	END
