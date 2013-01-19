       SUBROUTINE GH_BKSQ ( stid, iseq, iarea, iret )
C************************************************************************
C* GH_BKSQ								*
C*									*
C* This subroutine looks up the sequence number of a station id.        *
C*									*
C* GH_BKSQ ( STID, ISEQ, IAREA, IRET )                                  *
C*									*
C* Input parameters:							*
C*	STID 		CHAR*		Station ID               	*
C*									*
C* Output parameters:							*
C*	ISEQ 		INTEGER		Sequence number                 *
C*	IAREA 		INTEGER		Area number	                *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					  1 = no sequence number found  *
C*									*
C**									*
C* Log:									*
C* B. Yin/SAIC    	02/04		Created				*
C* B. Yin/SAIC    	03/04		Added iarea as a parameter	*
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	CHARACTER	stid*8, stid_uc*8
	INTEGER		iseq, icounter, ier
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
	       iseq = ibkseq( icounter ) 
	       iarea = ibkpri( icounter ) / 10
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
