	SUBROUTINE SF_OFIL  ( filnam, wrtflg, shrflg, isffln, iflsrc,
     +			      nparm,  parms,  iret )
C************************************************************************
C* SF_OFIL								*
C*									*
C* This subroutine opens an existing surface data file.  It is		*
C* called internally by the user-callable open routines.		*
C*									*
C* SF_OFIL  ( FILNAM, WRTFLG, SHRFLG, ISFFLN, IFLSRC, NPARM, PARMS,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	WRTFLG		LOGICAL		Write access flag		*
C*	SHRFLG		LOGICAL		Share flag			*
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
C**									*
C* Log: 								*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		 8/93	Changes for STD2 and SPRI		*
C* D. Keiser/GSC	 3/96	Added choice between SFTX and SFDT	*
C* T. Lee/GSC		10/97	Added KSWFO and KWFO2 in DM_LSSF	*
C*				calling sequences			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*)	filnam, parms (*)
	LOGICAL		wrtflg, shrflg
C*
	INTEGER		iscale (MMPARM), iofset (MMPARM), ibit (MMPARM)
C------------------------------------------------------------------------
C*	Open the file.
C
	CALL DM_OPEN  ( filnam, wrtflg, shrflg, isffln, iftyp, iflsrc, 
     +			nrow,   ncol,   nprt,   nfhdrs, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -2
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Set file open flag.
C
	isfcfn (isffln) = isffln
C
C*	Check that this is a surface file.
C
	IF  ( iftyp .ne. MFSF )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    CALL SF_CLOS  ( isffln, ier )
	    RETURN
	END IF
C
C*	Get parameter information.
C
	IF ( ( iflsrc .eq. 100 ) .and. ( nprt .eq. 1 ) ) THEN
	    CALL DM_PART  ( isffln, 'SFTX', lenh, it, nparm, parms,
     +			    iscale, iofset, ibit, iret )
	ELSE
	    CALL DM_PART  ( isffln, 'SFDT', lenh, it, nparm, parms,
     +			    iscale, iofset, ibit, iret )
	END IF
	IF  ( iret .ne. 0 )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    CALL SF_CLOS  ( isffln, ier )
	    RETURN
	END IF
C
C*	Retrieve date/time keys. 
C
	CALL DM_LTIM  ( isffln, dttype (isffln), kdate (isffln), 
     +			ktime (isffln), iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, filnam, ier )
	    iret = -6
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    CALL SF_CLOS  ( isffln, ier )
	    RETURN
	END IF
C
C*	Retrieve station information.
C
	CALL DM_LSSF  ( isffln, sttype (isffln), kstid (isffln),
     +			kstnm (isffln), kslat (isffln), 
     +			kslon (isffln), kselv (isffln),
     +			kstat (isffln), kcoun (isffln),
     +			kstd2 (isffln), kspri (isffln),
     +			kswfo (isffln), kwfo2 (isffln),
     +			iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, filnam, ier )
	    iret = -6
	    CALL SF_CLOS  ( isffln, ier )
	    RETURN
	END IF
C
C*	Save number of parameters, reset row and column.
C
	kparm  (isffln) = nparm
	krow   (isffln) = 0
	kcol   (isffln) = 0
	timset (isffln) = .false.
	stnset (isffln) = .false.
	ftmset (isffln) = .false.
	curtim (isffln) = ' '
	curstn (isffln) = ' '
C*
	RETURN
	END
