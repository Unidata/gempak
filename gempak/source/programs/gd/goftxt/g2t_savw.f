	SUBROUTINE G2T_SAVW ( ktype, nb, nt, iret )
C************************************************************************
C* G2T_SAVW								*
C*									*
C* This subroutine saves max/min wave/wind data.			*
C*									*
C* G2T_SAVW ( KTYPE, NB, NT, IRET )					*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Type				*
C*					 1 = wave			*
C*					 2 = wind			*
C*	NB		INTEGER		Nth bound area			*
C*	NT		INTEGER		Nth time step			*
C*									*
C* Output parameters:							*
C*	IRET		Return code					*
C*			 0 = normal return				*
C**									*
C* Log:									*
C* T. Lee/SAIC		11/06						*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C-----------------------------------------------------------------------
	iret = 0
C
	mxval_s ( ktype, nb, nt ) = mxval ( ktype, nb, nt )
	mnval_s ( ktype, nb, nt ) = mnval ( ktype, nb, nt )
C*		
	RETURN
	END
