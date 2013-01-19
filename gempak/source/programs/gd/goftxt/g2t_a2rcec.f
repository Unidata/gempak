	SUBROUTINE G2T_A2RCEC ( ktype, lunb, nt, end, iret )
C************************************************************************
C* G2T_A2RCEC								*
C*									*
C* This subroutine creates G2T trending, period A becoming C.		*
C*									*
C* G2T_A2RCEC ( KTYPE, LUNB, NT, END, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	LUNB		INTEGER		LUN for G2T_TXT.TBL		*
C*	NT		INTEGER		Nth time step			*
C*	END		INTEGER		Appending text, 'EARLY'/'LATE'	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		08/07	Added wind trending simplification	*
C* T. Lee/SAIC		11/07	Added NT for combining period		*
C************************************************************************
	CHARACTER*(*)	end
	INCLUDE		'goftxt.cmn'
C------------------------------------------------------------------------
	iret = 0
C
	nt2 = nt + 2
C
C*	Store RaEa in trending block 1, RcEc in 2.
C
	itrnd = 1
	CALL G2T_ABC ( ktype, nt, itrnd, ier )
C
	itrnd = 2
	CALL G2T_ABC ( ktype, nt2, itrnd, ier )
C
C*	Simplify and trim text.
C
	CALL G2T_TRIM2 ( ktype, iret )
C
C*	Create the text.
C
	CALL G2T_AWSTXT ( ktype, lunb, nt, end, ier )
C*
	RETURN
	END 
