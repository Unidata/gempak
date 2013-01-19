	SUBROUTINE GCLOSP  ( iret )
C************************************************************************
C* GCLOSP								*
C* 									*
C* This subroutine closes any intermediate plot file used by a		*
C* sequential access type device.  When plotting is continued, a 	*
C* new file will be opened.  No action is taken if the current device	*
C* does not use an intermediate plot file. 				*
C*									*
C* GCLOSP  ( IRET )							*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* S. Jacobs/NCEP	 4/96	Added NCURWN to DCLOSP			*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVWIN.CMN'
C------------------------------------------------------------------------
	IF  ( ddev .ne. ' ' )  THEN
C
C*	    Close the plot file.
C
	    CALL DCLOSP  ( ncurwn, iret )
	    ncurwn = ncurwn + 1
C
C*	    Get the information from /DEVCHR/.
C
	    CALL DQDCHR ( nncolr, ier )
C
C*	    Set the drawing attributes and map/graph projections.
C
	    CALL GSATTR ( ierr )
	END IF
C
	CALL GEPLOT ( ier )
C*
	RETURN
	END
