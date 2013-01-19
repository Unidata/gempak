	FUNCTION PR_CMBC  ( clcl, clcm, clch )
C************************************************************************
C* PR_CMBC								*
C*									*
C* This function computes CMBC, the combined low, mid and high cloud	*
C* coverages, from CLCL, CLCM, and CLCH.  The following equation is 	*
C* used:								*
C*									*
C*           CMBC = (CLCL * 100) + (CLCM * 10) + CLCH			*
C*									*
C* REAL PR_CMBC  ( CLCL, CLCM, CLCH )					*
C*									*
C* Input parameters:							*
C*	CLCL		REAL		Low cloud coverage		*
C*	CLCM		REAL		Medium cloud coverage		*
C*	CLCH		REAL		High cloud coverage		*
C*									*
C* Output parameters:							*
C*	PR_CMBC		REAL		Combined cloud coverage		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* M. desJardins/GSFC	10/87	Set missing data to 0.			*
C* G. Huffman/GSC       7/88   	Documentation				*
C************************************************************************
        INCLUDE   'GEMPRM.PRM'
C------------------------------------------------------------------------
C*	Set missing data values to 0.
C
	IF  ( ( clcl .ge. 0 ) .and. ( clcl .le. 9 ) )  THEN
	    iclcl = clcl
	  ELSE
	    iclcl = 0
	END IF
C
	IF  ( ( clcm .ge. 0 ) .and. ( clcm .le. 9 ) )  THEN
	    iclcm = clcm
	  ELSE
	    iclcm = 0
	END IF
C
	IF  ( ( clch .ge. 0 ) .and. ( clch .le. 9 ) )  THEN
	    iclch = clch
	  ELSE
	    iclch = 0
	END IF
C
C*	Combine values into one number.
C
	PR_CMBC = ( iclcl * 100 ) + ( iclcm * 10 ) + iclch
*
	RETURN
	END
