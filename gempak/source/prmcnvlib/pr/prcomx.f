	FUNCTION PR_COMX  ( clhx, clcx )
C************************************************************************
C* PR_COMX								*
C*									*
C* This function computes COMX, the combined cloud height and coverage,	*
C* from CLHX and CLCX.  The following equation is used:			*
C*									*
C*               COMX = ( CLHX * 10 ) + CLCX				*
C*									*
C* REAL PR_COMX  ( CLHX, CLCX )						*
C*									*
C* Input parameters:							*
C*	CLHX		REAL		Cloud height			*
C*	CLCX 		REAL		Cloud coverage			*
C*									*
C* Output parameters:							*
C*	PR_COMX		REAL		Combined height & coverage	*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	10/87						*
C* G. Huffman/GSC	7/88	Documentation; allow -X, no height	*
C* S. Schotz/GSC	10/88   Multiply height by 10 instead of 100	*
C*                              also check for clear case		*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
        INCLUDE   'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Check for clear case
C
	IF ( clcx .eq. 1 ) THEN
            PR_COMX = clcx
C
C*	  Check for obscured clouds with missing height; return cloud
C*	  coverage only.
C
	  ELSE IF  ( ( ( clcx .eq. 5 ) .or. ( clcx .eq. 9 ) )
     *        .and. ( ERMISS ( clhx ) ) )  THEN
	    PR_COMX = clcx
C
C*	  Check for missing or invalid data.
C
	  ELSE IF  ( ( clhx .lt. 0. ) .or. ( ERMISS ( clhx ) ) )  THEN
	    PR_COMX = RMISSD
 	  ELSE
C
C*	    Combine data by multiplying height by 10 and adding clouds.
C*          Missing cloud amounts are given the value 0 (clear).
C
	    iclhx = clhx
	    iclcx = clcx
	    IF  ( ( iclcx .lt. 0 ) .or. ( iclcx .gt. 9 ) )  iclcx = 0
	    PR_COMX  = ( iclhx * 10 ) + iclcx
	END IF
C*
	RETURN
	END
