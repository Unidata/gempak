	SUBROUTINE G2T_ABC ( ktype, nt, itrnd, iret )
C************************************************************************
C* G2T_ABC								*
C*									*
C* This subroutine retrieves range/exception information at time step	*
C* A/B/C and then store into ITRND position in the trending block, 	*
C* MXN_D and MXN_DE.  Then EFLAG_D is reset.				*
C*									*
C* G2T_ABC ( KTYPE, NT, ITRND, IRET )					*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	NT		INTEGER		Nth time step			*
C*	ITRND		INTEGER		Index for trending block	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07	Created					*
C* T. Lee/SAIC		12/07	Set "PORTION" flag (pflag_d)		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
C------------------------------------------------------------------------
	iret = 0
C
C*	Retrieve wind/wave info and send to trending block.
C
	mxn_d ( ktype, itrnd, 1 ) = mnval ( ktype, 1, nt )
	mxn_d ( ktype, itrnd, 2 ) = mxval ( ktype, 1, nt )
	wdir_d ( 1, itrnd ) = wdrmod ( 1, 1, nt )
	wdir_d ( 2, itrnd ) = wdrmod ( 2, 1, nt )
	eflag_d ( ktype, itrnd ) = fsuba ( ktype, nt )
	IF ( eflag_d ( ktype, itrnd ) )  THEN
	    mxn_de ( ktype,itrnd,1 ) = mnval ( ktype,nbnd(ktype,nt),nt )
	    mxn_de ( ktype,itrnd,2 ) = mxval ( ktype,nbnd(ktype,nt),nt )
	    id_d ( ktype, itrnd ) = subid ( ktype, nt )
	    pflag_d ( ktype, itrnd ) = porflg ( nthzon, nbnd(ktype,nt) )
	    wdir_de ( 1, itrnd ) = wdrmod ( 1, nbnd ( ktype, nt ), nt )
	    wdir_de ( 2, itrnd ) = wdrmod ( 2, nbnd ( ktype, nt ), nt )
	  ELSE
	    mxn_de ( ktype, itrnd, 1 ) = IMISSD
	    mxn_de ( ktype, itrnd, 2 ) = IMISSD
	    id_d ( ktype, itrnd ) = ' '
	    wdir_de ( 1, itrnd ) = ' '
	    wdir_de ( 2, itrnd ) = ' '
	END IF
C*
	RETURN
	END 
