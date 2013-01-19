	SUBROUTINE SF_CLOS  ( isffln, iret )
C************************************************************************
C* SF_CLOS								*
C*									*
C* This subroutine closes a surface data file.  This subroutine must	*
C* be called to flush buffered data if anything has been written to	*
C* the file.								*
C*									*
C* SF_CLOS  ( ISFFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER	 	Surface file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				  	 -12 = DM error			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Close DM file.
C
	CALL DM_CLOS  ( isffln, ier )
	IF  ( ier .ne. 0 )  iret = -12
C
C*	Reset file open flag in common.
C
	isfcfn (isffln) = -1
C*
	RETURN
	END
