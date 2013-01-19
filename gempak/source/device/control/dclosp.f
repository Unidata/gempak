	SUBROUTINE DCLOSP  ( ncurwn, iret )
C************************************************************************
C* DCLOSP								*
C* 									*
C* This subroutine closes the plot file.				*
C*									*
C* DCLOSP  ( NCURWN, IRET )						*
C*									*
C* Output parameters:							*
C* 	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log									*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* S. Jacobs/NCEP	 4/96	Added NCURWN				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver to close files.
C
	CALL HCLOSP  ( ncurwn, iret )
C*
	RETURN
	END
