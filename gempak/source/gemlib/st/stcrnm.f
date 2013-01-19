	SUBROUTINE  ST_CRNM  ( string, value, iret )
C************************************************************************
C* ST_CRNM								*
C*									*
C* This subroutine converts a character string to a real number.  If	*
C* the conversion fails, RMISSD is returned.				*
C*									*
C* ST_CRNM  ( STRING, VALUE, IRET ) 					*
C*									*
C* Input parameters: 							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	VALUE		REAL		Real number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = conversion error 		*
C**									*
C* Log:									*
C* I. Graffman/CSC	12/82	STR_CHRL				*
C* M. desJardins/GSFC	 4/84	Modified to use READ to get number	*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Whistler/SSAI	 5/91	Modified to use internal string		*
C* J. Whistler/SSAI	 6/91	Modified internal read format using BN	*
C* K. Brill/NMC		 9/91	Check for number on IBM			*
C* M. desJardins/NMC	 3/92	Recoded to eliminate exceptions		*
C* S. Jacobs/NMC	 6/94	Increased sss*12 to sss*24		*
C* K. Tyle/GSC		12/95	Added check for single character 'E'	*
C* S. Jacobs/NCEP	 2/97	Added check for exponent > 20 or < -20	*
C* S. Maxwell/GSC        4/97   Modified to call ST_CRND                *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
C------------------------------------------------------------------------
	CALL ST_CRND ( string, value, ndec, iret )
C*
	RETURN
	END
