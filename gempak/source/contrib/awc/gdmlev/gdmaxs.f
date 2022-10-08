	SUBROUTINE GDMAXS  (  axis, start, stop, iret )
C************************************************************************
C* GDMAXS								*
C*									*
C* This subroutine processes an axis variable.  The start and stop	*
C* values are returned.							*
C*									*
C* AXIS is expected to be of the form:					*
C*									*
C*	    start/stop							*
C*    or								*
C*	    start							*
C*									*
C* In the latter case, stop is set to start and only one level is done	*
C*									*
C* GDMAXS  ( AXIS, START, STOP, IRET )					*
C*									*
C* Input parameters:							*
C*	AXIS		CHAR*		Input for axis			*
C*									*
C* Output parameters:							*
C*	START		REAL		Starting value for axis		*
C*	STOP		REAL		Stopping value for axis		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = incorrect specification	*
C**									*
C* Log:									*
C* J. Whistler/NSSFC	12/94     					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	axis
C*
	CHARACTER*72    substr (4)
C*
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C
C*      Break AXIS into two substrings.
C
	CALL ST_CLST ( axis, '/', ' ', 2, substr, nst, ier )
	IF ( ier .ne. 0 ) THEN
	  iret = -2
	  CALL ER_WMSG ( 'IN', iret, axis, ier )
	  RETURN
	END IF
C
C*      Decode START and STOP from first and second substrings.
C
	CALL ST_CRNM ( substr (1), start, ier )
	CALL ST_CRNM ( substr (2), stop,  ier )
C
C*	IF START or STOP is missing, set a flag to use PARM and
C*      determine index pointer for start, stop, and increment.
C
	IF ( ERMISS ( start ) ) THEN
	  iret = -2
	  CALL ER_WMSG ( 'IN', iret, axis, ier )
	  RETURN
	END IF
C*
	IF ( ERMISS ( stop ) ) THEN
	    stop = start
	END IF
C*
	RETURN
	END
