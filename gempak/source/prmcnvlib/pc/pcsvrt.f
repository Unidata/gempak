	SUBROUTINE PC_SVRT  ( iret )
C************************************************************************
C* PC_SVRT								*
C*									*
C* This subroutine sets up the tables to derive all possible vertical	*
C* coordinates.  The parameters to be computed are PRES, THTA, HGHT,	*
C* SGMA, TEMP, and DWPT.  SGMA will always be returned as not 		*
C* computable.								*
C*									*
C* PC_SVRT  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Cleaned up				*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C-------------------------------------------------------------------------
	iret  = 0
C
C*	Use index = 4 to save the table.
C
	index = 4
	CALL PC_METH  ( index, jdsprm, dsparm, MAXVRT, vparms, vcomp, 
     +			n, ier )
C*
	RETURN
	END
