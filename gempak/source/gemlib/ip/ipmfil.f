	SUBROUTINE IP_MFIL  ( mapfil, iret )
C************************************************************************
C* IP_MFIL								*
C*									*
C* This subroutine extracts the current map file name from $MAPFIL.	*
C*									*
C* IP_MFIL  ( MAPFIL, IRET )						*
C*									*
C* Output parameters:							*
C*	MAPFIL		CHAR*		Map file name			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = parameter not received	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 1/88						*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* G. Huffman/GSC	10/88	Change to $MAPFIIL			*
C* M. desJardins/GSFC	 2/90	Temporarily comment out TAE for UNIX	*
C* M. desJardins/NMC	 6/94	Eliminate TAE				*
C* K. Tyle/GSC		 7/96	NT_STR --> IP_STR			*
C************************************************************************
	CHARACTER*(*)	mapfil
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( '$MAPFIL', mapfil, ier )
C*
	RETURN
	END
