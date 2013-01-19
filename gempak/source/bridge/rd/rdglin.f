	SUBROUTINE RD_GLIN ( segmnt, lens, ispnt, clin, lenl, iret )
C************************************************************************
C* RD_GLIN								*
C*									*
C* This subroutine gets a line.						*
C*									*
C* RD_GLIN  ( SEGMNT, LENS, ISPNT, CLIN, LENL, IRET )			*
C*									*
C* Input parameters:							*
C*	SEGMNT		CHAR*		Segment				*
C*	LENS		INTEGER		Length of segment		*
C*									*
C* Input and output parameters:						*
C*	ISPNT		INTEGER		Segment pointer			*
C*									*
C* Output parameters:							*
C*      CLIN		CHAR*		Line				*
C*      LENL		INTEGER		Length of line			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = no more lines		*
C*					  -2 = null or short line	*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	clin, segmnt
C------------------------------------------------------------------------
	iret = 0
	lenl = 0
C*
	IF ( ispnt .eq. lens ) THEN
	    iret = -1
	    RETURN
	END IF
	lenl = INDEX ( segmnt ( ispnt:lens ), CHLF )
	ip = ispnt
	ispnt = ispnt + lenl
	IF (lenl .lt. 1 ) THEN
	    iret = -2
	    RETURN
	END IF
	clin = segmnt ( ip:ispnt - 1 )
C*
	RETURN
	END
