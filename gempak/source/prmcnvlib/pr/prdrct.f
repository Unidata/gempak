	FUNCTION PR_DRCT  ( ux, vx )
C************************************************************************
C* PR_DRCT								*
C*									*
C* This function computes DRCT from Ux and Vx, both of which must	*
C* be either meters/sec or knots.  The following equation is used:	*
C*									*
C*              DRCT = ATAN2 ( -UX, -VX ) * RTD				*
C*									*
C* REAL PR_DRCT  ( UX, VX )						*
C*									*
C* Input parameters:							*
C*	UX		REAL		U component of velocity		*
C*	VX		REAL    	V component of velocity		*
C*									*
C* Output parameters:							*
C*	PR_DRCT		REAL		Wind direction in degrees	*
C**									*
C* Log:									*
C* I. Graffman/CSC	8/83	Original source code			*
C* M. Goodman/RDS  	8/84	Renamed PR_DRCT 			*
C* M. desJardins/GSFC	1/86	Corrected uwnd=0 problem		*
C*				Added parameter statements		*
C* I. Graffman/RDS	12/87	GEMPAK4 documentation			*
C* G. Huffman/GSC       7/88    Documentation; U, V names; north=360	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C*      Find bad data.
C
	IF  ( ERMISS (ux) .or. ERMISS (vx) )  THEN
	    PR_DRCT = RMISSD 
	  ELSE IF  ( ( vx .EQ. 0. ) .and. ( ux .eq. 0. ) )  THEN
	    PR_DRCT = 0.
	  ELSE
	    PR_DRCT = ATAN2 ( -ux, -vx ) * RTD
	    IF  ( PR_DRCT .le. 0. )  PR_DRCT = PR_DRCT + 360.
	END IF
C*
	RETURN
	END
