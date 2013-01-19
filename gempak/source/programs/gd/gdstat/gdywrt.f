	SUBROUTINE GDYWRT  ( gmax, gmin, ave, sdev, cnt, kx, ky,
     +			     time, level, ivcord, parm, grdnam, ntime,
     +			     igrid, iret )
C************************************************************************
C* GDYWRT								*
C*									*
C* This subrutine writes grids to a file for GDSTAT.			*
C*									*
C* GDYWRT  ( GMAX, GMIN, AVE, SDEV, CNT, KX, KY, TIME, LEVEL, IVCORD,	*
C*           PARM, GRDNAM, NTIME, IGRID, IRET )				*
C*									*
C* Input parameters:							*
C*	GMAX (KX,KY)	REAL		Maximum				*
C*	GMIN (KX,KY)	REAL		Minimum				*
C*	AVE  (KX,KY)	REAL		Average				*
C*	SDEV (KX,KY)	REAL		Standard deviation		*
C*	CNT  (KX,KY)	REAL		Count				*
C*	KX		INTEGER		Grid X dimension		*
C*	KY		INTEGER		Grid Y dimension		*
C*	TIME  (2)	CHAR*		Grid date/time			*
C*	LEVEL (2)	INTEGER		Grid levels			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*		Grid parameter name		*
C*	GRDNAM		CHAR*		Output grid name		*
C*	NTIME		INTEGER		Number of times selected	*
C*	IGRID		INTEGER		Number of grids found		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  2 = user entered EXIT		*
C*					  0 = normal return		*
C*					 -7 = error writing grid	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* M. desJardins/GSFC	 8/90	GEMPAK 5.0				*
C* T. Lee/GSC		 7/01	Changed output				*
C* T. Piper/SAIC	 1/02	Set ighdr to proper size		*
C* K. Brill/HPC		11/02	Added MAX and MIN grids			*
C* R. Tian/SAIC		 3/05	Removed iflno, GD_WDAT -> DG_NWDT	*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C
	CHARACTER*(*)	time (2), parm, grdnam
	REAL 		gmax (*), gmin (*), ave (*), sdev (*), cnt (*)
	INTEGER		level (2)
	LOGICAL		respnd
C*
	INTEGER		ighdr (LLGDHD)
	CHARACTER	gnave*12, gnsdv*12, gncnt*12, gname*12
	CHARACTER	gnmax*12, gnmin*12, gpack*12
C*
	DATA		gpack / ' ' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Get grid name.
C
	IF  ( grdnam .eq. ' ' )  THEN
	    gname = parm
	  ELSE
	    gname = grdnam
	END IF
	CALL ST_LCUC  ( gname, gname, ier )
C
C*	Build five grid names.
C
	gnmax = 'MAX' // gname
	gnmin = 'MIN' // gname
	gnave = 'AVE' // gname
	gnsdv = 'SDV' // gname
	gncnt = 'CNT' // gname
C
C*	Write information to terminal.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( ' GDSTAT PARAMETERS' / )
	WRITE  ( 6, 1002 ) igrid, ntime, time
1002	FORMAT ( ' Found', I3, ' grids from', I3, ' selected times.'/
     +           ' Times: ', A, '-', A )
	WRITE  ( 6, 1005 )  gnmax, gnmin, gnave, gnsdv, gncnt
1005	FORMAT ( ' Maximum grid name:             ', A /
     +           ' Minimum grid name:             ', A /
     +           ' Average grid name:             ', A /
     +           ' Standard deviation grid name:  ', A /
     +           ' Count grid name:               ', A / )
C
C*	Allow user to enter a new parameter name.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  RETURN
	END IF
C
C*	Write out the grids.
C
	DO ii = 1, LLGDHD
	    ighdr ( ii ) = 0
	END DO
C
	CALL DG_NWDT  ( gmax, time, level, ivcord, gnmax, ighdr,
     +                  gpack, .true., iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', iret, ' ', ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GDSTAT', iret, gnmax, ier )
	END IF
C*
	CALL DG_NWDT  ( gmin, time, level, ivcord, gnmin, ighdr,
     +                  gpack, .true., iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', iret, ' ', ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GDSTAT', iret, gnmin, ier )
	END IF
C*
	CALL DG_NWDT  ( ave, time, level, ivcord, gnave, ighdr,
     +                  gpack, .true., iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', iret, ' ', ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GDSTAT', iret, gnave, ier )
	END IF
C*
	CALL DG_NWDT  ( sdev, time, level, ivcord, gnsdv, ighdr,
     +                  gpack, .true., iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', iret, ' ', ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GDSTAT', iret, gnsdv, ier )
	END IF
C*
	CALL DG_NWDT  ( cnt, time, level, ivcord, gncnt, ighdr,
     +                  gpack, .true., iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', iret, ' ', ier )
	    iret = -7
	    CALL ER_WMSG  ( 'GDSTAT', iret, gncnt, ier )
	END IF
	iret = 0
C*
	RETURN
	END
