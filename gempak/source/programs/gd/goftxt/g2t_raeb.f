	SUBROUTINE G2T_RAEB ( ktype, lunb, nt, end, iret )
C************************************************************************
C* G2T_RAEB								*
C*									*
C* This subroutine creates G2T wave trend text, RaEb.			*
C*									*
C* G2T_RAEB ( KTYPE, LUNB, NT, END, IRET )				*
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
C* T. Lee/SAIC		03/07	Created					*
C* T. Lee/SAIC		11/07	Added NT for combining periods		*
C* T. Lee/SAIC		11/07	Called G2T_GAP				*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)	end
C------------------------------------------------------------------------
	iret = 0
C
	nt1 = nt + 1
C
	itrnd = 1
	CALL G2T_ABC ( ktype, nt, itrnd, ier )
C
	itrnd = 2
	CALL G2T_ABC ( ktype, nt1, itrnd, ier )
C
C*	Move Eb to Ea.  Write out RaEb.
C
	IF ( eflag_d ( ktype, 2 ) )  THEN
	    mxn_de ( ktype, 1, 1 ) = mxn_de ( ktype, itrnd, 1 )
	    mxn_de ( ktype, 1, 2 ) = mxn_de ( ktype, itrnd, 2 )
	    eflag_d ( ktype, 1 ) = .true.
	    eflag_d ( ktype, 2 ) = .false.
	    id_d ( ktype, 1 ) = id_d ( ktype, itrnd )
	    IF  ( ktype .eq. 2 )  THEN
		DO ik = 1, 2
		    wdir_de ( ik, 1 ) = wdir_de ( ik, itrnd )
		END DO
	    END IF
	END IF
C
C*	Simplify and create the text. 
C
	itrnd = 1
	CALL G2T_TRIM ( ktype, itrnd, iret )
	IF ( eflag_d ( ktype, itrnd ) )  CALL G2T_GAP ( ktype, ier )
C
	CALL G2T_RANGX ( ktype, lunb, nt, itrnd, ier )
	CALL G2T_APPEX ( ktype, lunb, nt, itrnd, end, ier )
C*
	RETURN
	END 
