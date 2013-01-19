	SUBROUTINE GD_GTIMW ( iacss, maxchr, sep, timstr, iret )
C************************************************************************
C* GD_GTMS								*
C*									*
C* This subroutine returns all the times present in a grid file.	*
C* Only the first times are returned.  They are sorted from earliest	*
C* to latest.								*
C*									*
C* The times are returned as a single string separated by 'sep'.	*
C*									*
C* GD_GTMS  ( IACSS, MAXCHR, SEP, TIMSTR, IRET )			*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	MAXCHR		INTEGER		Maximum number of char		*
C*      SEP             CHAR*1          Separator                       *
C*									*
C* Output parameters:							*
C*	TIMSTR		CHAR*		GEMPAK times string		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* R. Tian/SAIC          1/06						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	sep, timstr
C*
	CHARACTER	timarr(LLMXGT)*20
C-----------------------------------------------------------------------
        iret = 0
C
C*	Get times as an array.
C
	CALL GD_GTIM ( iacss, LLMXGT, timarr, ntimes, iret )
	IF ( ntimes * 21 .gt. maxchr ) THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Build a single string from the array of times.
C
	CALL ST_LSTC ( timarr, ntimes, sep, timstr, iret )
C*
	RETURN
	END
