	SUBROUTINE SN_OPNR ( filnam, isnfln, iflsrc, nparm, parms, 
     +			     ivert,  mrgdat, iret )
C************************************************************************
C* SN_OPNR								*
C*									*
C* This subroutine opens an existing sounding data file for real-time	*
C* data ingest.  The file is opened for shared write access.  This	*
C* subroutine should not be used for non-real-time applications.		*
C*									*
C* SN_OPNR  ( FILNAM, ISNFLN, IFLSRC, NPARM, PARMS, IVERT, MRGDAT,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Sounding file name		*
C*									*
C* Output parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	IFLSRC		INTEGER		Data source			*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PARMS (NPARM)	CHAR*4		Parameter names			*
C*	IVERT		INTEGER		Vertical coordinate		*
C*	MRGDAT		LOGICAL		Merged data flag		*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return		*
C*				  	 -2 = file could not be opened	*
C*				  	 -7 = file not sounding file	*
C*					-24 = file name is blank	*
C**									*
C* Log: 								*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 4/90	Added error for blank file name		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	filnam, parms (*)
	LOGICAL		mrgdat
C*
	LOGICAL		wrtflg, shrflg
C-------------------------------------------------------------------------
C*	Check for blank name.
C
	CALL ST_LSTR  ( filnam, lenf, ier )
	IF  ( lenf .eq. 0 )  THEN
	    iret = -24
	    RETURN
	END IF
C
C*	Open the file for shared, write access.
C
	wrtflg = .true.
	shrflg = .true.
	CALL SN_OFIL  ( filnam, wrtflg, shrflg, isnfln, iflsrc, nparm,
     +			parms,  ivert,  mrgdat, iret )
C*
	RETURN
	END
