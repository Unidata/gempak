	SUBROUTINE PC_INID  ( outdat, chrdat, num, iret )
C************************************************************************
C* PC_INID								*
C*									*
C* This subroutine initializes the output data arrays for the PC	*
C* package.  Real data is initialized to the missing data number.	*
C* Character data is initialized to blanks.				*
C*									*
C* PC_INID  ( OUTDAT, CHRDAT, NUM, IRET )				*
C*									*
C* Output parameters:							*
C*	OUTDAT (NUM)	REAL		Real output data		*
C*	CHRDAT (NUM)	CHAR*		Character output data		*
C*	NUM		INTEGER		Array size			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	9/84						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		outdat (*)
	CHARACTER*(*)	chrdat (*)
C-------------------------------------------------------------------------
	iret = 0
	DO  i = 1, num
	    chrdat (i) = ' '
	    outdat (i) = RMISSD
	END DO
C*
	RETURN
	END
