	SUBROUTINE HENDD  ( ieop, iret )
C************************************************************************
C* HENDD - GIF 								*
C*									*
C* This subroutine must be the last subroutine called by any program 	*
C* that uses GEMPLT.  It will flush internal buffers if necessary in	*
C* the device driver.  The DEVICE subprocess may be retained so that	*
C* the current definitions are available in later programs.		*
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
C* T. Lee/GSC		 7/00	Added ncurwn to hclosp call seq.	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Close file at end of plot.
C
	IF ( ieop .eq. 1 ) CALL HCLOSP  ( ncurwn, iret )
C*
	RETURN
	END
