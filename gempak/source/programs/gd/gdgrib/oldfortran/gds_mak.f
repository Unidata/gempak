	SUBROUTINE GDS_MAK  ( navchg, rnvblk, nnv, nbytes, cgds, iret )
C************************************************************************
C* GDS_MAK								*
C*									*
C* This subroutine uses the GEMPAK grid navigation block to decide	*
C* which GDS maker to call.						*
C*									*
C* GDS_MAK  ( NAVCHG, RNVBLK, NNV, NBYTES, CGDS, IRET )			*
C*									*
C* Input parameters:							*
C*	NAVCHG		LOGICAL		Flag for navigation change	*
C*	RNVBLK (NNV)	REAL		GEMPAK grid navigation block	*
C*	NNV		INTEGER		Size of the navigation block	*
C*									*
C* Input and output parameter:						*
C*	NBYTES		INTEGER		Input: # of bytes available in	*
C*					       CGDS			*
C*					Output: # of bytes filled in	*
C*					       CGDS			*
C*									*
C* Output parameters:							*
C*	CGDS (NBYTES)	CHAR*1		GRIB GDS section		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-75 = map proj not supported	*
C**									*
C* Log:									*
C* K. Brill/HPC		 8/99						*
C* K. Brill/HPC		 3/00	Added NAVCHG flag			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		rnvblk (nnv)
	CHARACTER*1	cgds (*)
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
	    CALL GDS_CED ( rnvblk, nnv, nbytes, cgds, iret )
	ELSE IF ( proj .eq. 'MER' ) THEN
	    CALL GDS_MER ( rnvblk, nnv, nbytes, cgds, iret )
	ELSE IF ( proj .eq. 'STR' ) THEN
	    CALL GDS_STR ( navchg, rnvblk, nnv, nbytes, cgds, iret )
	ELSE IF ( proj .eq. 'SCC' ) THEN
	    CALL GDS_LCC ( navchg, rnvblk, nnv, nbytes, cgds, iret )
	ELSE IF ( proj .eq. 'LCC' ) THEN
	    CALL GDS_LCC ( navchg, rnvblk, nnv, nbytes, cgds, iret )
	ELSE
	    iret = -75
	END IF
C*
	RETURN
	END
