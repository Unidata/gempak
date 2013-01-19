	SUBROUTINE SF_DDAT  ( isffln, iret )
C************************************************************************
C* SF_DDAT								*
C*									*
C* This subroutine deletes data for a particular station and time	*
C* from a surface data file.  The time and station must be set		*
C* before calling this subroutine. 					*
C*									*
C* SF_DDAT  ( ISFFLN, IRET )						*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*					  -3 = file not open		*
C*					  -7 = location not set		*
C*					 -12 = DM error			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that station is set.
C
	IF ( (krow (isffln) .le. 0) .or. (kcol (isffln) .le. 0) ) THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Delete the data.
C
	CALL DM_DDAT  ( isffln, krow (isffln), kcol (isffln), 'SFDT',
     +			ier )
	IF  ( ier .ne. 0 )  iret = -12
C*
	RETURN
	END
