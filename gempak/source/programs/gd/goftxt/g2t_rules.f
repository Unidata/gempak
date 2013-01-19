	SUBROUTINE G2T_RULES ( instr, iret )
C************************************************************************
C* G2T_RULES								*
C*									*
C* This subroutine handles the rule number for the offshore text.	*
C*									*
C* G2T_RULES ( INSTR, IRET )						*
C*									*
C* Input parameters:							*
C*	INSTR		CHAR*		Input string for rules		*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	instr
C-----------------------------------------------------------------------
	iret = 0
C
	CALL ST_LSTR ( instr, ins, ier )
	IF  ( f_rule ) THEN
	    iw = INDEX ( crules, instr )
	    IF ( iw .eq. 0 )  THEN
		CALL ST_LSTR ( crules, lcr, ier )
		CALL ST_LSTR ( crules, lcr, ier )
		crules = crules ( : lcr ) //';'// instr ( : ins )
	    END IF
	  ELSE
            crules = '(' // instr ( : ins )
	    f_rule = .true.
	END IF
C	
	RETURN
	END
