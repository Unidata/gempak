	SUBROUTINE GINITP  ( mode, istat, iret )
C************************************************************************
C* GINITP								*
C* 									*
C* This subroutine initializes GEMPLT for an application program.  The	*
C* GPLT subprocess is started if it has not been started by a 		*
C* previously executed program.  This subroutine must be the first	*
C* subroutine called by any program that uses GEMPLT.  The MODE 	*
C* parameter determines the plotting mode for map/graph plots.		*
C*									*
C* GINITP  ( MODE, ISTAT, IRET )					*
C*									*
C* Input parameters:							*
C*	MODE		INTEGER		Plotting mode 			*
C*					   0 = no change		*
C*					   1 = map coordinates		*
C*					   2 = graph coordinates	*
C* Output parameters:							*
C*	ISTAT		INTEGER		Status code			*
C*					   0 = GEMPLT started 		*
C*					   1 = GEMPLT previously 	*
C*					       started			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* G. Chatters/RDS	 8/81						*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/GSFC	 8/87	Added check for ddev 			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/NMC	 7/91	UNIX version				*
C* M. desJardins/NMC	 1/92	Combine VMS and UNIX versions		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'ADBUFF.CMN'
C*
	CHARACTER	device*12
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check to see if there is a device driver.
C
	IF  ( MTMACH .ne. MTVAX )  THEN
	    mproc = 1
	    CALL CCHECK  ( mproc, mbchan, iwact, iret )
C
C*	    If there is a device driver, find out which one it is.
C
	    IF  ( iwact .eq. 1 )  THEN
		CALL DINITP  ( iret )
		CALL DQDEV   ( device, iunit, iatyp, ier )
		IF ( ddev .ne. device ) THEN
		    CALL DENDD ( 1, ier )
		    ddev = ' '
		END IF
	      ELSE
		ddev = ' '
	    END IF
	END IF
C
C*	Set the mode.
C
	CALL GSMODE  ( mode, iret )
C
C*	ISTAT is really set in the applications level subroutine.
C*	It is included here for consistency.
C
	istat = 0
C*
	RETURN
	END
