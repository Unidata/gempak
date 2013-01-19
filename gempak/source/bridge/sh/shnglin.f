	SUBROUTINE SHN_GLIN ( line, iergln )
C************************************************************************
C* SHN_GLIN								*
C*									*
C* This routine reads and returns the next line (i.e. all of the	*
C* characters up to but not including the next linefeed character)	*
C* from the bulletin within COMMON /PASSBULL/.				*
C*									*
C* SHN_GLIN ( LINE, IERGLN )						*
C*									*
C* Output parameters:							*
C*	LINE		CHAR*		Next line of bulletin		*
C*	IERGLN		INTEGER		Return code:			*
C*					 0 = normal return		*
C*					-1 = end-of-bulletin reached	*
C*					 1 = error (line too long)	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C*
	CHARACTER*(*)	line
C-----------------------------------------------------------------------
	lineln = LEN ( line )
	line = ' '
	ii = 1
C
	DO WHILE  ( .true. )
	    IF  ( ibptr .gt. lenb )  THEN
C
C*		We've reached the end of the bulletin.
C
		iergln = -1
		RETURN
	    ELSE IF  ( ii .gt. lineln ) THEN
C
C*		The line is too long to fit within the output array.
C
		iergln = 1
		RETURN
	    ELSE IF  ( MOVA2I ( bull(ibptr:ibptr) ) .eq. 10 ) THEN
C
C*		We've found the next linefeed character.
C
		ibptr = ibptr + 1
		iergln = 0
		RETURN
	    ELSE
	        line(ii:ii) = bull(ibptr:ibptr)
		ibptr = ibptr + 1
		ii = ii + 1
	    END IF
	END DO
C*
	RETURN
	END
