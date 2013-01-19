	SUBROUTINE DC_IFMP  ( nmseq, nlyr, iloc, iret )
C************************************************************************
C* DC_IFMP								*
C*									*
C* Given the pointer to level 1, this subroutine sets the pointers for	*
C* levels 2 through NLYR for a multi-level interface mnemonic.		*
C*									*
C* DC_IFMP  ( NMSEQ, NLYR, ILOC, IRET )					*
C*									*
C* Input parameters:							*
C*	NMSEQ		INTEGER		Number of mnemonics per level	*
C*	NLYR		INTEGER		Total number of levels 		*
C*									*
C* Input and output parameters:						*
C*	ILOC (*)	INTEGER         Pointer array for mnemonic   	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NCEP  	 8/97	                                        *
C* D. Kidwell/NCEP	10/97	Adapted and generalized from AF_IFMP    *
C************************************************************************
	INTEGER		iloc (*)
C-----------------------------------------------------------------------
	iret = 0
C
	IF  (  ( iloc (1) .ne. 0 ) .and.
     +		  ( nlyr .gt. 1 )  )  THEN
	    DO jj = 2, nlyr
		iloc ( jj ) = iloc (1) + ( ( jj - 1 ) * nmseq )
	    END DO
	END IF
C*
	RETURN
	END
