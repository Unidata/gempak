	SUBROUTINE GQMODE  ( mode, iret )
C************************************************************************
C* GQMODE								*
C* 									*
C* This subroutine returns the current mode for map/graph coordinate 	*
C* plotting.								*
C*									*
C* GQMODE  ( MODE, IRET )						*
C*									*
C* Output parameters:							*
C*	MODE		INTEGER		Plotting mode 			*
C*					   0 = no change		*
C*					   1 = map coordinates		*
C*					   2 = graph coordinates	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 8/84						*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 5/88	Documentation				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Retrieve mode from common.
C
	mode = igmode
C*
	RETURN
	END
