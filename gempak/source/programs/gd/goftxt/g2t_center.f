	SUBROUTINE G2T_CENTER ( alias, iret )
C************************************************************************
C* G2T_CENTER  								*
C*									*
C* This subroutine sets the national center flags based on the alias	*
C* of the bound files.							*
C*									*
C* G2T_CENTER ( ALIAS, IRET						*
C*									*
C* Input parameters:							*
C*	ALIAS		CHAR*		Alias for bound type		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-1 = cannot CENTER file		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	alias
C-----------------------------------------------------------------------
	iret = 0
C
	IF ( alias .eq. ' ' )  THEN
	    iret = +3
	    RETURN
	END IF
C
	CALL ST_LCUC ( alias, alias, ier )
C
	IF  ( INDEX ( alias, 'TPC' ) .ne. 0 .or.
     +	      INDEX ( alias, 'NWC' ) .ne. 0 )  THEN
	    ncnter = 2
	  ELSE IF ( INDEX ( alias, 'PAC' ) .ne. 0 .or.
     +		    INDEX ( alias, 'ATL' ) .ne. 0 )  THEN
	    ncnter = 1
	  ELSE
	    iret = +3
	    RETURN
	END IF
C*
	RETURN
	END
