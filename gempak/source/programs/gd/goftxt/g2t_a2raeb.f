	SUBROUTINE G2T_A2RAEB ( ktype, lunb, nt, end, iret )
C************************************************************************
C* G2T_A2RAEB								*
C*									*
C* This subroutine creates G2T trending text, period A becoming RaEb.	*
C*									*
C* G2T_A2RAEB ( KTYPE, LUNB, NT, END, IRET )				*
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
C* T. Lee/SAIC		11/07	Added NT for combining periods		*
C* T. Lee/SAIC		03/08	Reset "PORTION" flag			*
C************************************************************************
	CHARACTER*(*)	end
	INCLUDE		'goftxt.cmn'
C------------------------------------------------------------------------
	iret = 0
C
	nt1 = nt + 1
	nt2 = nt + 2
C
C*	Store RbEb in trending block 1, RaEa in 2.
C
	itrnd = 2
	CALL G2T_ABC ( ktype, nt, itrnd, ier )
C
	itrnd = 1
	CALL G2T_ABC ( ktype, nt1, itrnd, ier )
C
C*	Replace Ea with Eb so block 2 becomes RaEb. Reset the EXCEPTION
C*	and PORTION flags.
C
	mxn_de  ( ktype, 2, 1 ) = mxn_de ( ktype, itrnd, 1 )
	mxn_de  ( ktype, 2, 2 ) = mxn_de ( ktype, itrnd, 2 )
	eflag_d ( ktype, 2 ) = eflag_d ( ktype, itrnd )
	id_d ( ktype, 2 ) = id_d ( ktype, itrnd )
	pflag_d ( ktype, 2 ) = pflag_d ( ktype, itrnd )
	IF  ( ktype .eq. 2 )  THEN
	    DO ik = 1, 2
		wdir_de ( ik, 2 ) = wdir_de ( ik, itrnd )
	    END DO
	END IF
C
C*	Store RaEa in trending block 1 and simplify/trim the text.
C
	itrnd = 1
	CALL G2T_ABC ( ktype, nt, itrnd,ier )
	CALL G2T_TRIM2 ( ktype, iret )
C
C*	Create the text.
C
	CALL G2T_AWSTXT ( ktype, lunb, nt, end, ier )
C*
	RETURN
	END 
