 	FUNCTION PR_PKDD  ( pspd )
C************************************************************************
C* PR_PKDD								*
C*									*
C* This function computes DRCT from PSPD.  PSPD is in the form DDFFF	*
C* where DD is the wind direction in tens of degrees, and FFF is	*
C* either the wind speed or wind speed plus 500, depending on the	*
C* units digit of direction rounded to the nearest 5 degrees.  The	*
C* following equation is used:						*
C*									*
C*                DRCT = 5. *  INT ( PSPD / 500. )			*
C*									*
C* REAL PR_PKDD  ( PSPD )						*
C*									*
C* Input parameters:							*
C*	PSPD		REAL		Packed speed and direction	*
C*									*
C* Output parameters:							*
C*	PR_PKDD		REAL		Wind direction in degrees	*
C**									*
C* Log:									*
C* I. Graffman/CSC	 8/83	Original source code			*
C* I. Graffman/RDS	12/87	Correction				*
C* G. Huffman/GSC	 8/88	Documentation and rounding fix		*
C* M. desJardins/GSFC	 9/88	Changed NINT to INT			*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for missing data.
C
	IF  ( ERMISS ( pspd ) )  THEN
	    PR_PKDD = RMISSD 
C
C*      The factor of 5 accounts for encoding the units digit of 
C*	direction (to the nearest 5 degrees) in the hundreds digit of 
C*	speed.
C
	  ELSE
	    PR_PKDD = 5. * INT ( pspd / 500. )
	    IF  ( PR_PKDD .ge. 360. )  PR_PKDD = PR_PKDD - 360.
	END IF
C*
	RETURN
	END
