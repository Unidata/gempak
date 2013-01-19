	SUBROUTINE SN_OFIL ( filnam, wrtflg, shrflg, isnfln, iflsrc, 
     +                       nparm,  parms,  ivert,  mrgdat, iret )
C************************************************************************
C* SN_OFIL								*
C*									*
C* This subroutine opens an existing sounding data file. It can be	*
C* called by other subroutines so that the parameter SHRFLG is not	*
C* set by the user.  SHRFLG will have an effect only if the file	*
C* is opened for write access.						*
C*									*
C* SN_OFIL ( FILNAM, WRTFLG, SHRFLG, ISNFLN, IFLSRC, NPARM, PARMS, 	*
C*           IVERT,  MRGDAT, IRET )					*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Sounding file name		*
C*	WRTFLG		LOGICAL		Write access flag		*
C*	SHRFLG		LOGICAL		Shared access flag		*
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
C*					-25 = invalid vert coord	*
C**									*
C* Log: 								*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		02/92	Write error for invalid vert coord	*
C* K. Brill/NMC		 8/93	Changes for 8-char stn ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	filnam, parms (*)
	LOGICAL		wrtflg, shrflg, mrgdat
C*
	INTEGER		iscale (MMPARM), iofset (MMPARM), ibit (MMPARM)
C-------------------------------------------------------------------------
C*	Open the file.
C
	CALL DM_OPEN ( filnam, wrtflg, shrflg, isnfln, iftyp,  iflsrc, 
     +	               nrow,   ncol,   nprt,   nfhdrs, iret )
C
C*	Check error messages.
C
	IF ( iret .ne. 0 ) THEN
	    iret = -2
	    CALL ER_WMSG ( 'SN', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Set file open flag.
C
	isndfn ( isnfln ) = isnfln
	mrgtyp ( isnfln ) = .true.
C
C*	Check that this is a sounding file
C
	IF ( iftyp .ne. MFSN ) THEN
	    iret = -7
	    CALL ER_WMSG ( 'SN', iret, filnam, ier )
	    CALL SN_CLOS ( isnfln, ier )
	    RETURN
	END IF
C
C*	Get parameter information.
C
	CALL DM_PART ( isnfln, 'SNDT', lenh, it, nparm, parms, iscale,
     +                 iofset, ibit,   iret )
C
C*	Check vertical coordinate.
C
	IF  ( iret .eq. 0 )  THEN
	    IF  ( parms (1) .eq. 'PRES' )  THEN
		ivert = 1
	      ELSE IF  ( parms (1) .eq. 'THTA' )  THEN
		ivert = 2
	      ELSE IF  ( ( parms (1) .eq. 'HGHT' )  .or.
     +			 ( parms (1) .eq. 'MHGT' )  .or.
     +			 ( parms (1) .eq. 'DHGT' ) )  THEN
		ivert = 3
	      ELSE
		iret = -25
	        CALL ER_WMSG ( 'SN', iret, parms(1), ier )
	        CALL SN_CLOS ( isnfln, ier )
	        RETURN
	    END IF
C*
	  ELSE
C
C*	    If SNDT is not a valid part, check to see if this is an
C*	    unmerged data file.
C
	    CALL SN_CKUA  ( isnfln, nparm, parms, iret )
	    ivert = 1
	END IF
C
C*	If this is neither a merged or unmerged dataset, close file
C*	and exit.
C
	IF ( iret .ne. 0 ) THEN
	    iret = -7
	    CALL ER_WMSG ( 'SN', iret, filnam, ier )
	    CALL SN_CLOS ( isnfln, ier )
	    RETURN
	  ELSE
	    mrgdat = mrgtyp ( isnfln )
	END IF
C
C*	Retrieve date/time keys. 
C
	CALL DM_LTIM ( isnfln, dttype (isnfln), kdate (isnfln), 
     +                 ktime (isnfln), iret )
	IF ( iret .ne. 0 ) THEN
	    CALL ER_WMSG ( 'DM', iret, filnam, ier )
	    iret = -7
	    CALL ER_WMSG ( 'SN', iret, filnam, ier )
	    CALL SN_CLOS ( isnfln, ier )
	    RETURN
	END IF
C
C*	Retrieve station information.
C
	CALL DM_LSTN ( isnfln, sttype (isnfln), kstid (isnfln),
     +                 kstnm (isnfln), kslat (isnfln), 
     +                 kslon (isnfln), kselv (isnfln),
     +                 kstat (isnfln), kcoun (isnfln),
     +		       kstd2 (isnfln), iret )
	IF ( iret .ne. 0 ) THEN
	    CALL ER_WMSG ( 'DM', iret, filnam, ier )
	    iret = -7
	    CALL SN_CLOS ( isnfln, ier )
	    RETURN
	END IF
C
C*	Save number of parameters, reset row and column.
C
	kparm  (isnfln) = nparm
	krow   (isnfln) = 0
	kcol   (isnfln) = 0
	timset (isnfln) = .false.
	stnset (isnfln) = .false.
	ftmset (isnfln) = .false.
	curtim (isnfln) = ' '
	curstn (isnfln) = ' '
	icrstn (isnfln,1) = 0
	icrstn (isnfln,2) = 0
C*
	RETURN
	END
