	SUBROUTINE GR_RBAN  ( anlblk, deltan, deltax, deltay, gbnds,
     +			      ebnds, dbnds, iextnd, iret )
C************************************************************************
C* GR_RBAN								*
C*									*
C* This subroutine reads a Barnes analysis block.  All the bounds	*
C* are returned in the order:  lower left latitude; lower left		*
C* longitude; upper right latitude; upper right longitude.		*
C*									*
C* GR_RBAN  ( ANLBLK, DELTAN, DELTAX, DELTAY, GBNDS, EBNDS, DBNDS,	*
C*            IEXTND, IRET )						*
C*									*
C* Input parameters:							*
C*	ANLBLK (LLNANL)	REAL		Analysis block			*
C*									*
C* Output parameters:							*
C*	DELTAN		REAL		Station spacing			*
C*	DELTAX		REAL		Grid spacing in x dir		*
C*	DELTAY		REAL		Grid spacing in y dir		*
C*	GBNDS (4)	REAL		Grid area bounds		*
C*	EBNDS (4)	REAL		Extend area bounds		*
C*	DBNDS (4)	REAL		Data area bounds		*
C*	IEXTND (4)	INTEGER		Extend grid points		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = invalid analysis block	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88						*
C* K. Brill/GSC          4/90	Read for ANLBLK (1) = 2			*
C* M. desJardins/NMC	 4/91	Set DELTAX, DELTAY			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		gbnds (*), ebnds (*), dbnds (*), anlblk (*)
	INTEGER		iextnd (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check that this is a Barnes analysis block.
C
	IF  ( anlblk (1) .eq. 1.0 )  THEN
	    deltan = anlblk (2)
	    deltax = anlblk (3)
	    deltay = anlblk (4)
	    gamma  = anlblk (5)
	    DO  i = 1, 4
	      gbnds (i) = anlblk ( 5+i)
	      ebnds (i) = anlblk ( 9+i)
	      dbnds (i) = anlblk (13+i)
	    END DO
C
C*	    Compute IEXTND.
C
	    iextnd (1) = ( gbnds (2) - ebnds (2) ) / deltax + 0.1
	    iextnd (2) = ( gbnds (1) - ebnds (1) ) / deltay + 0.1
	    iextnd (3) = ( ebnds (4) - gbnds (4) ) / deltax + 0.1
	    iextnd (4) = ( ebnds (3) - gbnds (3) ) / deltay + 0.1
C*
	  ELSE IF  ( anlblk (1) .eq. 2.0 )  THEN
	    deltan = anlblk (2)
	    deltax = RMISSD
	    deltay = RMISSD
	    iextnd (1) = NINT ( anlblk (3) )
	    iextnd (2) = NINT ( anlblk (4) )
	    iextnd (3) = NINT ( anlblk (5) )
	    iextnd (4) = NINT ( anlblk (6) )
            DO i = 1, 4
	      gbnds (i) = anlblk (  6 + i )
	      ebnds (i) = anlblk ( 10 + i )
	      dbnds (i) = anlblk ( 14 + i )
	    END DO
	  ELSE
	    iret = -10
	END IF
C*
	RETURN
	END
