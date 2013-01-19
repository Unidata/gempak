	SUBROUTINE HENDD  ( ieop, iret )
C************************************************************************
C* HENDD - RBK								*
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
C* A. Hardy/GSC		9/98		Modified from utf's HENDD       *
C************************************************************************
C------------------------------------------------------------------------
	CALL AENDD ( ieop, iret )
C*
	RETURN
	END
