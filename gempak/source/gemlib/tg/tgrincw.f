	SUBROUTINE TG_RINCW ( tbegin, tend, tinc, itftyp, sep, maxchr,
     +	                      timstr, iret )
C************************************************************************
C* TG_RNOIW								*
C*									*
C* This subroutine returns the times in a time range with an		*
C* increment.                                          			*
C*									*
C* TG_RINCW ( TBEGIN, TEND, TINC, ITFTYP, SEP, MAXCHR, TIMSTR, IRET )	*
C*									*
C* Input parameters:							*
C*      TBEGIN          CHAR*           Start time                      *
C*      TEND            CHAR*           Stop time                       *
C*	TINC		CHAR*1		Time increment			*
C*      ITFTYP          INTEGER         Range type                      *
C*                                        1 = forecast range            *
C*                                        2 = date/time range           *
C*      SEP             CHAR*1          Separator                       *
C*	MAXCHR		INTEGER		Max number of char in string	*
C*									*
C* Output parameters:							*
C*      TIMSTR          CHAR*           Time string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* R. Tian/SAIC          1/06						*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	tbegin, tend, tinc, sep, timstr
C*
	CHARACTER	times(LLMXGT)*20
C-----------------------------------------------------------------------
        iret = 0
C
C*	Call TG_RINC
C
	CALL TG_RINC ( tbegin, tend, tinc, itftyp, ntimes, times, iret )
C
C*	Build a single string from the returned array of times.
C
	CALL ST_LSTC ( times, ntimes, sep, timstr, iret )
C*
	RETURN
	END
