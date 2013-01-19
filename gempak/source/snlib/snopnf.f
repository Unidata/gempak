	SUBROUTINE SN_OPNF  ( filnam, wrtflg, isnfln, iflsrc, nparm,
     +                        parms,  ivert,  mrgdat, iret )
C************************************************************************
C* SN_OPNF								*
C*									*
C* This subroutine opens an existing sounding data file.		*
C*									*
C* SN_OPNF  ( FILNAM, WRTFLG, ISNFLN, IFLSRC, NPARM, PARMS, IVERT,	*
C*            MRGDAT, IRET )						*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Sounding file name		*
C*	WRTFLG		LOGICAL		Write access flag		*
C*									*
C* Output parameters:							*
C*	ISNFLN		INTEGER		File number			*
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
C* M. desJardins/GSFC	 4/90	Add error for blank file name		*
C* S. Schotz/GSFC	 8/90	Write error message for blank file name	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	filnam, parms (*)
	LOGICAL		wrtflg, mrgdat
C*
	LOGICAL		shrflg
C-------------------------------------------------------------------------
C*	Check for blank name.
C
	CALL ST_LSTR  ( filnam, lenf, ier )
	IF  ( lenf .eq. 0 )  THEN
	    iret = -24
	    CALL ER_WMSG ( 'SN', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Call general subroutine to open file without shared access.
C
	shrflg = .false.
	CALL SN_OFIL  ( filnam, wrtflg, shrflg, isnfln, iflsrc,
     +			nparm,  parms,  ivert,  mrgdat, iret )
C*
	RETURN
	END
