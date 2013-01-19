	SUBROUTINE GDOOPN  ( gdfile, gdoutf, iflinp, iflout, ngrdin,
     +			     iret )
C************************************************************************
C* GDOOPN								*
C*									*
C* This subroutine opens an input and output grid file for GDMOD.	*
C* The navigations for the two files must be the same.			*
C*									*
C* GDOOPN  ( GDFILE, GDOUTF, IFLINP, IFLOUT, NGRDIN, IRET )		*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Input grid file			*
C*	GDOUTF		CHAR*		Output grid file		*
C*									*
C* Output parameters:							*
C*	IFLINP		INTEGER		Input file number		*
C*	IFLOUT		INTEGER		Output file number		*
C*	NGRDIN		INTEGER		Number of grids in input file	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = no grids in input file	*
C*					 -9 = file not opened		*
C*					-10 = different navigation	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88						*
C* K. Brill/NMC		02/92		Use LLNNAV, LLNANL		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	gdfile, gdoutf
C*
	CHARACTER	lasttm*20, firstm*20
	REAL		rnav1 (LLNNAV), rnav2 (LLNNAV), anl (LLNANL)
C------------------------------------------------------------------------
	iret   = 0
	iflinp = 0
	iflout = 0
C
C*	Open the input file.
C
	CALL GD_OPNF  ( gdfile, .false., iflinp, navsz, rnav1, iasz,
     +			anl, ihdsz1, maxgrd, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Get number of grids.
C
	CALL GD_NGRD  ( iflinp, ngrdin, firstm, lasttm, ier )
	IF  ( ngrdin .eq. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'GDMOD', iret, gdfile, ier )
	    CALL GD_CLOS  ( iflinp,  ier )
	    RETURN
	END IF
C
C*	Open the output file.
C
	CALL GD_OPNF  ( gdoutf, .true., iflout, navsz, rnav2, iasz,
     +			anl, ihdsz2, maxgrd, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -9
	    CALL GD_CLOS  ( iflinp, ier )
	    iflinp = 0
	    RETURN
	END IF
C
C*	Compare the navigations.
C
	invlen = 10
	IF  ( rnav1 (1) .eq. 2. )  invlen = 13
	DO  i = 1, invlen
	    IF  ( rnav1 (i) .ne. rnav2 (i) )  iret = -10
	END DO
C
C*	If the navigations are not the same, close the files.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GDMOD', iret, ' ', ier )
	    CALL GD_CLOS  ( iflinp, ier )
	    CALL GD_CLOS  ( iflout, ier )
	    iflinp = 0
	    iflout = 0
	END IF
C*
	RETURN
	END
