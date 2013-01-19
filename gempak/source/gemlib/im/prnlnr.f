	SUBROUTINE PRNLNR  ( np, dlon, iret )
C************************************************************************
C* PRNLNR                                                               *
C*									*
C* This function converts a longitude in radians into radians which	*
C* fall within the range PI to -PI.					*
C*									*
C* PRNLNR ( NP, DLON, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of longitudes		*
C*									*
C* Input and output parameters:						*
C*	DLON (*)	REAL		Longitude in radians		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal			*
C**									*
C* Log:									*
C* G. Chatters/RDS	11/80 						*
C* C. Millburn		 6/84	Rewritten for GEMPAK3			*
C* M. desJardins/GSFC	 8/87	Corrected sign of pi in check for <	*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* K. Brill/EMC		 3/96	Add loop				*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	REAL		dlon (*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C*
	DO i = 1, np
C
C*	Check for missing data.
C
	    IF  ( .not. ERMISS ( dlon (i) ) )  THEN
                dln   = dlon (i) - IFIX ( dlon (i) / TWOPI ) * TWOPI
		IF ( dln .lt. -PI ) dln = dln + TWOPI
		IF ( dln .gt.  PI ) dln = dln - TWOPI
		dlon (i) = dln
	    END IF
	END DO
C*
	RETURN
	END
