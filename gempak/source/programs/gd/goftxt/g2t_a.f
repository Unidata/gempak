	SUBROUTINE G2T_A ( ktype, lunb, nt, iret )
C************************************************************************
C* G2T_A								*
C*									*
C* This subroutine creates G2T text from period A.			*
C*									*
C* G2T_A ( KTYPE, LUNB, NT, IRET )					*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		05/07	Created					*
C* T. Lee/SAIC		08/07	Added wind trending simplification	*
C* T. Lee/SAIC		11/07	Added NT for combining period		*
C* T. Lee/SAIC		11/07	Eliminated RANGE/EXCEPT gap		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	end*1
C------------------------------------------------------------------------
	iret = 0
	end = ' '
C
C*	Stove RaEa in block 1.
C
	itrnd = 1
	CALL G2T_ABC ( ktype, nt, itrnd, ier )
C
        CALL G2T_TRIM ( ktype, itrnd, ier )
	CALL G2T_GAP  ( ktype, ier )
C
	CALL G2T_RANGX ( ktype, lunb, nt, itrnd, ier )
	CALL G2T_APPEX ( ktype, lunb, nt, itrnd, end, ier )
C*
	RETURN
	END 
