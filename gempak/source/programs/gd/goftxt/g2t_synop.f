	SUBROUTINE G2T_SYNOP ( nt, iret )
C************************************************************************
C* G2T_SYNOP								*
C*									*
C* This subroutine sets the flag for synoptic (00Z, 12Z) time.		*
C*									*
C* G2T_SYNOP ( NT, IRET )						*
C*									*
C* Input parameters:							*
C*	NT		INTEGER		Nth time step			*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		04/07						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	synop = .true.
	IF ( nt .eq. 1 )  THEN
	    IF ( INDEX ( grdtm ( nt ), '/0600' ) .ne. 0 .or.
     +		 INDEX ( grdtm ( nt ), '/1800' ) .ne. 0 )  THEN
		 synop = .false.
	    END IF
	END IF
C*
	RETURN
	END
