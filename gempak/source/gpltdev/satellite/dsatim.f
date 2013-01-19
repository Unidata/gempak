	SUBROUTINE DSATIM ( filnam, ixout0, iyout0, ixout1, iyout1,
     +			    ixspc0, iyspc0, ixspc1, iyspc1,
     +			    iret )
C************************************************************************
C* DSATIM								*
C*									*
C* This subroutine displays a satellite image from a satellite image	*
C* file.								*
C*									*
C* DSATIM ( FILNAM,  IXOUT0, IYOUT0, IXOUT1, IYOUT1,                    *
C*          IXSPC0, IYSPC0, IXSPC1, IYSPC1,                             *
C*          IRET )                                                      *
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Restore file name 		*
C*      IXOUT0          INTEGER         Image space                     *
C*      IYOUT0          INTEGER         Image space                     *
C*      IXOUT1          INTEGER         Image space                     *
C*      IYOUT1          INTEGER         Image space                     *
C*      IXSPC0          INTEGER         View space                      *
C*      IYSPC0          INTEGER         View space                      *
C*      IXSPC1          INTEGER         View space                      *
C*      IYSPC1          INTEGER         View space                      *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Krueger/EAI	 2/94						*
C* S. Jacobs/NMC	 3/94	Clean up				*
C* J. Cowie/COMET	 1/95	Added image coords for subsetting	*
C* J. Cowie/COMET	 5/95	Removed image bounds from calling seq.	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'FUNCCODE.PRM'
C*
	CHARACTER* (*) 	filnam
C*
	CHARACTER	satfil*132
	INTEGER 	isend (43)
C-----------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 43
	isend (2) = CSATIM
C
	satfil = filnam
	CALL ST_STOI  ( satfil, 132, nv, isend (3), iret )
	IF (iret .ne. NORMAL) RETURN
C
	isend (36) = ixout0
	isend (37) = iyout0
	isend (38) = ixout1
	isend (39) = iyout1
	isend (40) = ixspc0
	isend (41) = iyspc0
	isend (42) = ixspc1
	isend (43) = iyspc1
C
	CALL GPUT ( isend, 43, iret )
C
	IF ( iret .ne. NORMAL ) RETURN
C
C*	Get the return code.
C
	CALL GGET ( iret, 1, ier )
	IF ( ier .ne. NORMAL ) iret = ier
C
	RETURN
	END
