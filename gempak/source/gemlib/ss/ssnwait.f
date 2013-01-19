	SUBROUTINE SS_NWAIT  ( iret )
C************************************************************************
C* SS_NWAIT								*
C*									*
C* This subroutine halts the execution of a program for short periods	*
C* using the nanosleep builtin function.  This subroutine is		*
C* specifically designed for looping FORTRAN event handlers.		*
C*									*
C* SS_NWAIT  ( IRET )							*
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log: 								*
C* R. McTaggart-Cowan/SUNY	01/05	Modified from ss_wait		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'

	iret = 0
C
C*	Call C subroutine.  Note that calling the UNIX nanosleep routine
C*	directly can be dangerous.
C
	nsecval = nanosec
	CALL CSSLEEP  ( nsecval, ier )
C*
	RETURN
	END
