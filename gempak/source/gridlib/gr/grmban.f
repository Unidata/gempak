	SUBROUTINE GR_MBAN  ( deltan, deltax, deltay, gbnds, ebnds, 
     +			      dbnds, anlblk, iret )
C************************************************************************
C* GR_MBAN								*
C*									*
C* This subroutine makes a Barnes analysis block.  The analysis block	*
C* generated is LLNANL words long.  All the bounds must be entered in 	*
C* the order:  lower left latitude; lower left longitude; upper		*
C* right latitude; upper right longitude.				*
C*									*
C* GR_MBAN  ( DELTAN, DELTAX, DELTAY, GBNDS, EBNDS, DBNDS, ANLBLK,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DELTAN		REAL		Station spacing			*
C*	DELTAX		REAL		Grid spacing in x dir		*
C*	DELTAY		REAL		Grid spacing in y dir		*
C*	GBNDS (4)	REAL		Grid area bounds		*
C*	EBNDS (4)	REAL		Extended area bounds		*
C*	DBNDS (4)	REAL		Data area bounds		*
C*									*
C* Output parameters:							*
C*	ANLBLK (LLNANL)	REAL		Analysis block			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* G. Huffman/GSC	 4/89	Document removing GAMMA			*
C* K. Brill/NMC		02/92	Use LLNANL				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		gbnds (*), ebnds (*), dbnds (*), anlblk (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	The first word in the analysis block is a code for the type of
C*	analysis to perform.  ANLBLK (5) used to hold GAMMA.
C
	anlblk (1) = 1.0		
	anlblk (2) = deltan
	anlblk (3) = deltax
	anlblk (4) = deltay
	anlblk (5) = 0.
	DO  i = 1, 4
	    anlblk ( 5+i) = gbnds (i)
	    anlblk ( 9+i) = ebnds (i)
	    anlblk (13+i) = dbnds (i)
	END DO
C
C*	Words 18-LLNANL are spares.
C
	DO  i = 18, LLNANL
	    anlblk (i) = 0.0
	END DO
C*
	RETURN
	END
