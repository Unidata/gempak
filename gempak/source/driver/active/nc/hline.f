	SUBROUTINE HLINE ( np, ix, iy, iret )
C************************************************************************
C* HLINE - NC								*
C*									*
C* This subroutine draws lines on a graphics device (NC).  The line is	*
C* split into segments so as not to overflow the CGM LINE command (see	*
C* MLINE).								*
C*									*
C* HLINE ( NP, IX, IY, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	IX (NP)		INTEGER		X coordinates			*
C*	IY (NP)		INTEGER		Y coordinates			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/88						*
C* S. Schotz/GSC	 9/90	Remove device size hardcodes		*
C* S. Jacobs/NMC	 6/94	General clean up			*
C* M. Linda/GSC		 6/96	Added looping for very large NP		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Jacobs/NCEP	12/98	Changed MAX_BUFSZ to use 32767 not 7FFF	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		ix (*), iy (*)
C
C*	32767 is the largest 2-byte integer and 32767/4 is longest
C*	line segement.
C
	PARAMETER	( MAX_BUFSZ = ( 32767 / 4 ) )
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	For large NP, split line into segments to fit CGM LINE command.
C
	npass = ( ( np - 1 ) / MAX_BUFSZ ) + 1
	ib = 1
	DO m = 1, npass
	    ie = ib + MAX_BUFSZ - 1
	    IF ( ie .gt. np ) ie = np
	    num = ie - ib + 1
C
	    CALL MLINE ( num, ix ( ib ), iy ( ib ), iret )
C
C*	    Overlap one point between segments.
C
	    ib = ie
	END DO
C*
	RETURN
	END
