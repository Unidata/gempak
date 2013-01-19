	SUBROUTINE SFC_OPNF  ( filnam, wrtflg, isffln, iflsrc, iret )
C************************************************************************
C* SFC_OPNF								*
C*									*
C* This subroutine opens an existing surface data file.			*
C*									*
C* SFC_OPNF  ( FILNAM, WRTFLG, ISFFLN, IFLSRC, IRET )			*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	WRTFLG		LOGICAL		Write access flag		*
C*									*
C* Output parameters:							*
C*	ISFFLN		INTEGER		File number			*
C*	IFLSRC		INTEGER		Data source			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = file could not be opened	*
C*					 -6 = file not surface file	*
C*					-22 = file name is blank	*
C**									*
C* Log: 								*
C* D.W.Plummer/NCEP	12/97	From SF_OPNF				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	filnam
	LOGICAL		wrtflg
	CHARACTER	parms(MMPARM)*4
C*
C-------------------------------------------------------------------------
C
C*	Open the file.
C
	CALL SF_OPNF  ( filnam, wrtflg, isffln, iflsrc, nparms, 
     +			parms, iret )
C*
	RETURN
	END
