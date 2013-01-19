	SUBROUTINE SF_OPNR  ( filnam, isffln, iflsrc, nparm, parms, 
     +			      iret )
C************************************************************************
C* SF_OPNR								*
C*									*
C* This subroutine opens an existing surface data file for real-time	*
C* data ingest.  The file is opened for shared write access.  This	*
C* subroutine should not be used for non-real-time applications.		*
C*									*
C* SF_OPNR  ( FILNAM, ISFFLN, IFLSRC, NPARM, PARMS, IRET )		*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
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
C* I. Graffman/RDS	 7/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 4/90	Error for blank file name		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	filnam, parms (*)
C*
	LOGICAL		wrtflg, shrflg
C-------------------------------------------------------------------------
C*	Check for blank name.
C
	CALL ST_LSTR  ( filnam, lenf, ier )
	IF  ( lenf .eq. 0 )  THEN
	    iret = -22
	    RETURN
	END IF
C*
	shrflg = .true.
	wrtflg = .true.
C
C*	Open the file.
C
	CALL SF_OFIL  ( filnam, wrtflg, shrflg, isffln, iflsrc, nparm, 
     +			parms,  iret )
C*
	RETURN
	END
