	SUBROUTINE SF_OPNF  ( filnam, wrtflg, isffln, iflsrc, nparm,
     +			      parms,  iret )
C************************************************************************
C* SF_OPNF								*
C*									*
C* This subroutine opens an existing surface data file.			*
C*									*
C* SF_OPNF  ( FILNAM, WRTFLG, ISFFLN, IFLSRC, NPARM, PARMS, IRET )	*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	WRTFLG		LOGICAL		Write access flag		*
C*									*
C* Output parameters:							*
C*	ISFFLN		INTEGER		File number			*
C*	IFLSRC		INTEGER		Data source			*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS (NPARM)	CHAR*4		Parameter names			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = file could not be opened	*
C*					 -6 = file not surface file	*
C*					-22 = file name is blank	*
C**									*
C* Log: 								*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 4/90	Error for blank file name		*
C* S. Schotz/GSC	 8/90	Write error message for blank file name	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	filnam, parms (*)
	LOGICAL		wrtflg
C*
	LOGICAL		shrflg
C-------------------------------------------------------------------------
C*	Check for blank name.
C
	CALL ST_LSTR  ( filnam, lenf, ier )
	IF  ( lenf .eq. 0 )  THEN
	    iret = -22
	    CALL ER_WMSG ( 'SF', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Open the file.
C
	shrflg = .false.
	CALL SF_OFIL  ( filnam, wrtflg, shrflg, isffln, iflsrc, nparm, 
     +			parms,  iret )
C*
	RETURN
	END
