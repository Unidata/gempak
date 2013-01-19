	SUBROUTINE DM_WLBL ( iflno, iret )
C************************************************************************
C* DM_WLBL								*
C*									*
C* This subroutine writes a label to a DM file.				*
C*									*
C* DM_WLBL  ( IFLNO, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86						*
C* M. desJardins/GSFC	 3/87	Changed label format			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	INTEGER		label (30)
	CHARACTER	dmlbl*28
	DATA		dmlbl / 'GEMPAK DATA MANAGEMENT FILE ' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Write 'GEMPAK DATA MANAGEMENT FILE' in the first 7 words of the 
C*	label.
C
	iwrite = 1
	nchar  = 28
	CALL DM_WSTR  ( iflno, iwrite, nchar, dmlbl, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Put the label values in an array.
C
	label  (8) = kversn ( iflno )
	label  (9) = kfhdrs ( iflno )
	label (10) = kpfile ( iflno )
	label (11) = krow   ( iflno )
	label (12) = krkeys ( iflno )
	label (13) = kprkey ( iflno )
	label (14) = kprowh ( iflno )
	label (15) = kcol   ( iflno )
	label (16) = kckeys ( iflno )
	label (17) = kpckey ( iflno )
	label (18) = kpcolh ( iflno )
	label (19) = kprt   ( iflno )
	label (20) = kppart ( iflno )
	label (21) = kpdmgt ( iflno )
	label (22) = kldmgt ( iflno )
	label (23) = kpdata ( iflno )
	label (24) = kftype ( iflno )
	label (25) = kfsrce ( iflno )
	label (26) = kmachn ( iflno )
	label (27) = kmissd ( iflno )
	DO  i = 28, 30
	    label (i) = 0
	END DO
C
C*	Write the label to the file.
C
	iwrite = 8
	nword  = 23
	CALL DM_WINT  ( iflno, iwrite, nword, label (8), iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Write the real value to the label.
C
	iwrite = 31
	CALL DM_WFLT  ( iflno, iwrite, 1, smissd (iflno), iret )
C*
	RETURN
	END
