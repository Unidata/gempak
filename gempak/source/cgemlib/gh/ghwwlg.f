	SUBROUTINE GH_WWLG ( ibknew, nnew, ibkcon, ncon, ibkcan, ncan,
     +			     nseg, ivtec, ibkseg, iret )
C************************************************************************
C* GH_WWLG                                                              *
C*									*
C* This subroutine reorganizes the breakpoints by VTEC type (action and *
C* event code), and regroups the VTEC segments if necessary to eliminate*
C* overlapping segments.  It works on lists of points, not pairs.       *
C*                                                                      *
C* GH_WWLG ( IBKNEW, NNEW, IBKCON, NCON, IBKCAN, NCAN, NSEG, IVTEC,     *
C*	     IBKSEG, IRET )                                             *
C*                                                                      *
C* Input parameters:                                                    *
C*	IBKNEW(4,*)	INTEGER		Bkpt lists for type 'NEW'       *
C*	NNEW(*)		INTEGER		No. of 'NEW' bkpts for each w/w *
C*	IBKCON(4,*)	INTEGER		Bkpt lists for type 'CON'       *
C*	NCON(*)		INTEGER		No. of 'CON' bkpts for each w/w *
C*	IBKCAN(4,*)	INTEGER		Bkpt lists for type 'CAN'       *
C*	NCAN(*)		INTEGER		No. of 'CAN' bkpts for each w/w *
C*                                                                      *
C* Output parameters:                                                   *
C*	NSEG		INTEGER		Number of VTEC segments         *
C*	IVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	IBKSEG(*)	INTEGER		Bkpt numbers for VTEC segments  *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	 2/05	From GH_WWSG                            *
C************************************************************************
	INCLUDE		'ghcmn.cmn'
C*
	INTEGER		ibknew (4,*), nnew (*), ibkcon (4,*), 
     +			ncon (*), ibkcan (4,*), ncan (*),
     +			ivtec (3,*), ibkseg (*)
C*
	INTEGER		jvtec (3,50), jbkseg (50), mvtec (3,50), 
     +			mbkseg (50)
C------------------------------------------------------------------------
	iret = 0
C
C*	Store the VTEC action and watch/warning codes and the breakpoint
C*	numbers by VTEC segment.  Store one point per segment initially.
C*      Segments may be combined later.
C
	jseg = 0
	DO ii = 1, 4
	    DO jj = 1, nnew ( ii )
		jseg = jseg + 1
		jvtec ( 1, jseg )  = ii
		jvtec ( 2, jseg )  = 0
		jvtec ( 3, jseg )  = 0
		jbkseg ( jseg ) = ibknew ( ii, jj )
	    END DO
	    DO jj = 1, ncon ( ii )
		jseg = jseg + 1
		jvtec ( 1, jseg )  = ii + 4
		jvtec ( 2, jseg )  = 0
		jvtec ( 3, jseg )  = 0
		jbkseg ( jseg ) = ibkcon ( ii, jj )
	    END DO
	    DO jj = 1, ncan ( ii )
		jseg = jseg + 1
		jvtec ( 1, jseg )  = ii + 8
		jvtec ( 2, jseg )  = 0
		jvtec ( 3, jseg )  = 0
		jbkseg ( jseg ) = ibkcan ( ii, jj )
	    END DO
	 END DO
C
C*	Regroup the VTEC segments if there are any overlapping NEW and
C*	CAN segments, or if there are overlapping segments for hurricane
C*	watches and tropical storm warnings.
C*	VTEC combined action and watch/warning codes are
C*		 1 = new hurricane warning		NEW HU.W
C*		 2 = new hurricane watch		NEW HU.A
C*		 3 = new tropical storm warning		NEW TR.W
C*		 4 = new tropical storm watch		NEW TR.A
C*		 5 = continue hurricane warning		CON HU.W
C*		 6 = continue hurricane watch		CON HU.A
C*		 7 = continue tropical storm warning	CON TR.W
C*		 8 = continue tropical storm watch	CON TR.A
C*		 9 = cancel hurricane warning		CAN HU.W
C*		10 = cancel hurricane watch		CAN HU.A
C*		11 = cancel tropical storm warning	CAN TR.W
C*		12 = cancel tropical storm watch	CAN TR.A
C
C*	Look for overlapping hurricane watches and tropical storm 
C*	warnings (code 2 with 3 or 7, code 6 with 3, 7 or 11, code 10
C*	with 7 or 11).  Other combinations are handled below.
C
	CALL GH_WWLV ( jseg, jvtec, jbkseg, 1, mseg, mvtec, mbkseg, ier)
C
C*	Look for overlapping NEW and CAN actions for different watch/
C*	warning types (code 1 with 10, 11 or 12, code 2 with 9, 11 or 
C*	12, code 3 with 9, 10 or 12, code 4 with 9, 10 or 11).
C
	CALL GH_WWLV ( mseg, mvtec, mbkseg, 2, nseg, ivtec, ibkseg, ier)
C*
	RETURN
	END
