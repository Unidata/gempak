	SUBROUTINE PRNLON ( np, dlon, iret )
C************************************************************************
C* PRNLON                                                               *
C*									*
C* This function converts a longitude in degrees into degrees which	*
C* fall within the range 180 to -180.					*
C*									*
C* PRNLON ( NP, DLON, IRET )						*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of longitudes		*
C*									*
C* Input and output parameters:						*
C*	DLON (NP)	REAL		Longitudes in degrees		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	11/80 						*
C* M. Goodman/RDS	 7/85	Rewritten for GEMPAK3			*
C* M. desJardins/GSFC	 1/88	Added check for missing data		*
C* K. Brill/EMC		 3/96	Add loop				*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
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
        	dln   = dlon (i) - IFIX ( dlon (i) / 360. ) * 360.
		IF ( dln  .lt. -180. ) dln = dln + 360.
		IF ( dln  .gt.  180. ) dln = dln - 360.
		dlon (i) = dln
	    END IF
	END DO
C*
	RETURN
	END
