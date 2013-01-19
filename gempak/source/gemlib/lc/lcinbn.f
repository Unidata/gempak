	SUBROUTINE LC_INBN  ( rlat, rlon, bound, iret )
C************************************************************************
C* LC_INBN								*
C*									*
C* This subroutine checks a latitude / longitude pair to see if it is 	*
C* within the range specified by LC_SBND.				*
C*									*
C* LC_INBN  ( RLAT, RLON, BOUND, IRET )					*
C*									*
C* Input parameters:							*
C*	RLAT		REAL		Latitude 			*
C*	RLON		REAL		Longitude 			*
C*									*
C* Output parameters:							*
C*	BOUND		LOGICAL 	Flag set if in bounds		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/84						*
C************************************************************************
	LOGICAL		bound
	INCLUDE		'lcbnd.cmn'
C-----------------------------------------------------------------
	bound  =  ( rlat .ge. rllbnd (1) ) .and.
     +		  ( rlat .le. rllbnd (3) ) .and.
     +		  ( rlon .ge. rllbnd (2) ) .and.
     +		  ( rlon .le. rllbnd (4) )
C
	iret = 0
C*
	RETURN
	END
