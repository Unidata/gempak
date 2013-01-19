	SUBROUTINE IM_QCHN  ( chntyp, iret )
C************************************************************************
C* IM_QCHN								*
C*									*
C* This subroutine returns the product type from the image common	*
C* block.								*
C*									*
C* IM_QCHN  ( CHNTYP, IRET )						*
C*									*
C* Output parameters:							*
C*	CHNTYP		CHAR*		Image type			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C**									*
C* Log:									*
C* T. Lee/GSC		 2/00	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	chntyp
C------------------------------------------------------------------------
	iret = 0
C
C*	Check if the image file is valid.
C
	IF  ( imftyp .eq. IFINVD ) THEN
	    iret = -4
	    RETURN
	END IF
C
	chntyp = cmtype
C*	
	RETURN
	END
