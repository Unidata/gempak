	SUBROUTINE PDS_BYT7  ( rnvblk, nnv, byte7, ibyt7, iret )
C************************************************************************
C* PDS_BYT7								*
C*									*
C* This subroutine uses the GEMPAK grid navigation table file to find	*
C* the grid number that matches the navigation in rnvblk.  If none is	*
C* found, byte7=255.							*
C*									*
C* PDS_BYT7  ( RNVBLK, NNV, BYTE7, IBYT7, IRET )			*
C*									*
C* Input parameters:							*
C*	RNVBLK (NNV)	REAL		GEMPAK grid navigation block	*
C*	NNV		INTEGER		Length of navigation block	*
C*									*
C* Output parameters:							*
C*	BYTE7		CHAR*1		Byte with grid # value stored	*
C*	IBYT7		INTEGER		Integer value of byte 7		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 +1 = cannot open grdnav.tbl	*
C*					 +2 = error reading grdnav.tbl	*
C*					 +3 = error decoding grdnav.tbl *
C*					 +4 = grid not in grdnav.tbl	*
C**									*
C* Log:									*
C* K. Brill/HPC		 7/99						*
C* K. Brill/HPC		11/99	Return +1-4 warning messages		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL		rnvblk (*)
	CHARACTER*1	byte7
C*
	LOGICAL		found, angflg
	CHARACTER*4	namgd, prjgd
	REAL		anggd (3), gargd (4)
	REAL		chknav (LLNNAV)
C------------------------------------------------------------------------
	iret = 0
	byte7 = CHAR ( 255 )
	ibyt7 = 255
	CALL FL_TBOP ( 'grdnav.tbl', 'grid', lug, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = +1
	    RETURN
	END IF
	angflg = ( rnvblk (1) .eq. 2.0 )
	found = .false.
	iretg = 0
	DO WHILE ( .not. found .and. iretg .eq. 0 )
	    CALL TB_GRNV  ( lug,  namgd, numgd, prjgd, anggd, gargd,
     +			    nxgd, nygd,  deln,  extnd, iretg )
	    IF ( iretg .eq. 0 ) THEN
		rlat1 = gargd (1)
		rlon1 = gargd (2)
		rlat2 = gargd (3)
		rlon2 = gargd (4)
		ang1 = anggd (1)
		ang2 = anggd (2)
		ang3 = anggd (3)
		CALL GR_MNAV ( prjgd, nxgd, nygd, rlat1, rlon1, rlat2,
     +			       rlon2, ang1, ang2, ang3, angflg, chknav,
     +			       ier )
		CALL GR_CNAV ( rnvblk, chknav, LLNNAV, found, ier )
	    END IF
	END DO
	IF ( iretg .eq. 0 ) THEN
	    IF ( numgd .lt. 256 .and. numgd .gt. 0 ) THEN
		byte7 = CHAR ( numgd )
		ibyt7 = numgd
	    END IF
	ELSE IF ( iretg .eq. -2 ) THEN
	    iret = +2
	ELSE IF ( iretg .eq. -9 ) THEN
	    iret = +3
	ELSE IF ( iretg .eq. -1 ) THEN
	    iret = +4
	END IF
	CALL FL_CLOS ( lug, ier )
C*
	RETURN
	END
