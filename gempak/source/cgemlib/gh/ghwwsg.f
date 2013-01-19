	SUBROUTINE GH_WWSG ( ibknew, nnew, istnew, ibkcon, ncon, 
     +			     istcon, ibkcan, ncan, istcan, nseg, 
     +			     ivtec, ibkseg, iret )
C************************************************************************
C* GH_WWSG                                                              *
C*									*
C* This subroutine reorganizes the breakpoints by VTEC type (action and *
C* event code), and regroups the VTEC segments if necessary to eliminate*
C* overlapping segments.                                                *
C*                                                                      *
C* GH_WWSG ( IBKNEW, NNEW, ISTNEW, IBKCON, NCON, ISTCON, IBKCAN, NCAN,  *
C*	     ISTCAN, NSEG, IVTEC, IBKSEG, IRET )                        *
C*                                                                      *
C* Input parameters:                                                    *
C*	IBKNEW(4,*)	INTEGER		Bkpt pairs for type 'NEW'       *
C*	NNEW(*)		INTEGER		No. of 'NEW' bkpts for each w/w *
C*	ISTNEW(*)	INTEGER         Index of 1st 'NEW' bkpt for w/w *
C*	IBKCON(4,*)	INTEGER		Bkpt pairs for type 'CON'       *
C*	NCON(*)		INTEGER		No. of 'CON' bkpts for each w/w *
C*	ISTCON(*)	INTEGER		Index of 1st 'CON' bkpt for w/w *
C*	IBKCAN(4,*)	INTEGER		Bkpt pairs for type 'CAN'       *
C*	NCAN(*)		INTEGER		No. of 'CAN' bkpts for each w/w *
C*	ISTCAN(*)	INTEGER		Index of 1st 'CAN' bkpt for w/w *
C*                                                                      *
C* Output parameters:                                                   *
C*	NSEG		INTEGER		Number of VTEC segments         *
C*	IVTEC(3,*)	INTEGER		VTEC action & event code values *
C*	IBKSEG(2,*)	INTEGER		Bkpt numbers for VTEC segments  *
C*	IRET		INTEGER		Return code                     *
C*									*
C* Log:									*
C* D. Kidwell/NCEP	11/03						*
C************************************************************************
	INTEGER		ibknew (4,*), nnew (*), ibkcon (4,*), 
     +			ncon (*), ibkcan (4,*), ncan (*),
     +			istnew (*), istcon (*), istcan (*),
     +			ivtec (3,*), ibkseg (2,*)
C*
	INTEGER		jvtec (3,50), jbkseg (2,50), mvtec (3,50), 
     +			mbkseg (2,50)
C------------------------------------------------------------------------
	iret = 0
C
C*	Store the VTEC action and watch/warning codes and the breakpoint
C*	numbers by VTEC segment.
C
	jseg = 0
	DO ii = 1, 4
	    DO jj = istnew ( ii ), nnew ( ii ), 2
		jseg = jseg + 1
		jvtec ( 1, jseg )  = ii
		jvtec ( 2, jseg )  = 0
		jvtec ( 3, jseg )  = 0
		jbkseg ( 1, jseg ) = ibknew ( ii, jj )
		jbkseg ( 2, jseg ) = ibknew ( ii, jj + 1 )
	    END DO
	    DO jj = istcon ( ii ), ncon ( ii ), 2 
		jseg = jseg + 1
		jvtec ( 1, jseg )  = ii + 4
		jvtec ( 2, jseg )  = 0
		jvtec ( 3, jseg )  = 0
		jbkseg ( 1, jseg ) = ibkcon ( ii, jj )
		jbkseg ( 2, jseg ) = ibkcon ( ii, jj + 1 )
	    END DO
	    DO jj = istcan ( ii ), ncan ( ii ), 2 
		jseg = jseg + 1
		jvtec ( 1, jseg )  = ii + 8
		jvtec ( 2, jseg )  = 0
		jvtec ( 3, jseg )  = 0
		jbkseg ( 1, jseg ) = ibkcan ( ii, jj )
		jbkseg ( 2, jseg ) = ibkcan ( ii, jj + 1 )
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
	CALL GH_WWOV ( jseg, jvtec, jbkseg, 1, mseg, mvtec, mbkseg, ier)
C
C*	Look for overlapping NEW and CAN actions for different watch/
C*	warning types (code 1 with 10, 11 or 12, code 2 with 9, 11 or 
C*	12, code 3 with 9, 10 or 12, code 4 with 9, 10 or 11).
C
	CALL GH_WWOV ( mseg, mvtec, mbkseg, 2, nseg, ivtec, ibkseg, ier)
C*
	RETURN
	END
