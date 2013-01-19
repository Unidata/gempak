	SUBROUTINE  PT_CNVR  ( outdat, nparm, cmpflg, chrflg, chrfnc,
     +			       chrdat, iret )
C************************************************************************
C* PT_CNVR								*
C*									*
C* This subroutine converts arrays of real data to character data	*
C* for the PC library.							*
C*									*
C* PT_CNVR  ( OUTDAT, NPARM, CMPFLG, CHRFLG, CHRFNC, CHRDAT, IRET )	*
C*									*
C* Input parameters:							*
C*	OUTDAT (NPARM)		REAL		Real-valued data 	*
C*	NPARM			INTEGER		Number of values	*
C*	CMPFLG (NPARM)		LOGICAL		Computable flag		*
C*	CHRFLG (NPARM)		LOGICAL 	Character flag 		*
C*	CHRFNC (NPARM)		CHAR*8		Conversion functions 	*
C*                                                                      *
C* Output parameters:							*
C*	CHRDAT (NPARM)		CHAR*8		Character data		*
C*	IRET			INTEGER		Return code		*
C*					  	   0 = normal return	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	9/84						*
C************************************************************************
	REAL		outdat (*)
	LOGICAL		cmpflg (*), chrflg (*)
	CHARACTER*(*)	chrdat (*), chrfnc (*)
C---------------------------------------------------------------------------
	iret = 0
C
C*	If parameter is computable, put blanks in output array.
C*	If parameter is character type, convert real to character.
C
	DO  i = 1, nparm
C
	  IF  ( cmpflg (i) ) THEN
		chrdat (i) = ' '
		IF  ( chrflg (i) ) CALL PT_FUNC (outdat (i), chrfnc (i),
     +					chrdat (i), ier )
	  END IF
C
	END DO
C*
	RETURN
	END
