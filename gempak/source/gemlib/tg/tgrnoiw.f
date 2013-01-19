	SUBROUTINE TG_RNOIW ( tbegin, tend, itftyp, sep, timstr, iret )
C************************************************************************
C* TG_RNOIW								*
C*									*
C* This subroutine checks which times in a file are within a time       *
C* range without an increment.                                          *
C*									*
C* TG_RNOIW ( TBEGIN, TEND, ITFTYP, SEP, TIMSTR, IRET )			*
C*									*
C* Input parameters:							*
C*      TBEGIN          CHAR*           Start time                      *
C*      TEND            CHAR*           Stop time                       *
C*      ITFTYP          INTEGER         Range type                      *
C*                                        1 = forecast range            *
C*                                        2 = date/time range           *
C*      SEP             CHAR*1          Separator                       *
C*									*
C* Input and Output parameters:						*
C*      TIMSTR          CHAR*           Time string			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* R. Tian/SAIC          1/06						*
C* S. Jacobs/NCEP	 8/06	Removed nulls from input times		*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	tbegin, tend, sep, timstr
C*
	CHARACTER	timarr(LLMXGT)*20, times(LLMXGT)*20
	CHARACTER	stime*20, etime*20
C-----------------------------------------------------------------------
        iret = 0
C
C*	Break the time string into an array of times
C
	CALL ST_CLSL ( timstr, sep, ' ', LLMXGT, timarr, nt, iret )
C
C*	Remove NULLs at the end of the strings.
C
	CALL ST_RNUL ( tbegin, stime, lens, ier )
	CALL ST_RNUL ( tend, etime, lens, ier )
C
C*	Call TG_RNOI
C
	CALL TG_RNOI ( stime, etime, itftyp, nt, timarr,
     +		       ntimes, times, iret )
C
C*	Build a single string from the returned array of times.
C
	CALL ST_LSTC ( times, ntimes, sep, timstr, iret )
C*
	RETURN
	END
