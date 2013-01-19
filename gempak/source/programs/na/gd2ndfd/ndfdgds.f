	SUBROUTINE NDFDGDS  ( navchg, rnvblk, nnv, nbytes, igds, iret )
C************************************************************************
C* NDFDGDS								*
C*									*
C* This subroutine uses the GEMPAK grid navigation block to decide	*
C* which GDS maker to call.						*
C*									*
C* NDFDGDS  ( NAVCHG, RNVBLK, NNV, NBYTES, IGDS, IRET )			*
C*									*
C* Input parameters:							*
C*	NAVCHG		LOGICAL		Flag for navigation change	*
C*	RNVBLK(NNV)	REAL		GEMPAK grid navigation block	*
C*	NNV		INTEGER		Size of the navigation block	*
C*									*
C* Input and output parameter:						*
C*	NBYTES		INTEGER		Input: # of bytes available in	*
C*					       IGDS			*
C*					Output: # of bytes filled in	*
C*					       IGDS			*
C*									*
C* Output parameters:							*
C*	IGDS(NBYTES)	INTEGER		GRIB GDS section		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-75 = map proj not supported	*
C**									*
C* Log:									*
C* T. Piper/SAIC	 3/03	Created from GDS_MAK			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rnvblk (nnv)
	INTEGER		igds(*)
	LOGICAL		navchg
C*
	CHARACTER*4	proj
C------------------------------------------------------------------------
	iret = 0 
	CALL ST_ITOC  ( rnvblk (2), 1, proj, ier )
C
C*	Select appropriate GDS maker routine.
C
	IF ( proj .eq. 'CED' ) THEN
	    CALL GDS_CED ( rnvblk, nnv, nbytes, igds, iret )
	ELSE IF ( proj .eq. 'MER' ) THEN
	    CALL GDS_MER ( rnvblk, nnv, nbytes, igds, iret )
	ELSE IF ( proj .eq. 'STR' ) THEN
	    CALL GDS_STR ( navchg, rnvblk, nnv, nbytes, igds, iret )
	ELSE IF ( proj .eq. 'SCC' ) THEN
	    CALL NDFDLCC ( navchg, rnvblk, nnv, igds, iret )
	ELSE IF ( proj .eq. 'LCC' ) THEN
	    CALL NDFDLCC ( navchg, rnvblk, nnv, igds, iret )
	ELSE
	    iret = -75
	END IF
C*
	RETURN
	END
