	SUBROUTINE HENDD  ( ieop, iret )
C************************************************************************
C* HENDD - PS								*
C*									*
C* This subroutine must be the last subroutine called by any program 	*
C* that uses GEMPLT.  It will flush internal buffers if necessary in the*
C* device driver.  The DEVICE subprocess may be retained so that the 	*
C* current definitions are available in later programs.			*
C*									*
C* HENDD  ( IEOP, IRET )						*
C*									*
C* Input parameters:							*
C*	IEOP		INTEGER		End plotting flag		*
C*					  0 = retain subprocess		*
C*					  1 = stop subprocess		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:                                                                 *
C* M. desJardins/GSFC	12/90						*
C* K. Brill/NMC		 8/91	Check ieop before calling HCLOSP	*
C* A. Chang/EAI          2/94   Modified to call C routine              *
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
	CALL PENDD ( ieop, iret )
C*
	RETURN
	END
