	SUBROUTINE GSMODE  ( mode, iret )
C************************************************************************
C* GSMODE								*
C* 									*
C* This subroutine sets the ploting mode for map/graph coordinate	*
C* plotting.  It may be used to change mode within programs.		*
C*									*
C* GSMODE  ( MODE, IRET )						*
C*									*
C* Input parameters:							*
C*	MODE		INTEGER		Plotting mode 			*
C*					   0 = no change		*
C* 					   1 = map coordinates		*
C* 					   2 = graph coordinates	*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 8/85						*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/NMC	07/91	Check for existence of device		*
C* K. Brill/NMC		08/91	Added DEVCHR.CMN to check ddev		*
C* S. Jacobs/NMC	 1/95	Added multi-window common block		*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* M. Li/GSC		 6/00	Removed erroneous setting of nmode	*
C************************************************************************
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set mode flag in common.  Update common areas if there is a
C*	change.
C
	IF  ( ( mode .eq. 1 ) .or. ( mode .eq. 2 ) )  THEN
	    IF  ( igmode .ne. mode )  THEN
		igmode = mode
		CALL UPDPXY 
	    END IF
C
C*	    Check for invalid mode.
C
	  ELSE IF  ( mode .ne. 0 )  THEN
	    iret = NIMODE
	END IF
C*
	RETURN
	END
