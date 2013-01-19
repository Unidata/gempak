	SUBROUTINE DCMBO ( iwndw, np, wtcod, x, y, ixoff, iyoff, iret )
C************************************************************************
C* DCMBO								*
C* 									*
C* This subroutine draws combination weather symbols on the current     *
C* graphics device.				                        *
C*									*
C* DCMBO ( IWNDW, NP, WTCOD, X, Y, IXOFF, IYOFF, IRET )			*
C* 									*
C* Input parameters:							*
C*	IWNDW		INTEGER		Clipping window			*
C*	NP		INTEGER 	Number of combo symbol groups   *
C*	WTCOD	(NP)	REAL		Weather symbol codes		*
C* 	X	(NP)	REAL		X coordinates in device units	*
C* 	Y	(NP)	REAL		Y coordinates in device units	*
C*	IXOFF	(NP)	INTEGER		X offsets in half characters	*
C*	IYOFF	(NP)	INTEGER		Y offsets in half characters	*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC          9/98	Copied from DWTHR			*
C* S. Jacobs/NCEP	 2/99	Changed the y-offsets for the symbols	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	REAL		wtcod (*), x (*), y (*)
	INTEGER		ixoff (*), iyoff (*) 
C*
        INTEGER         ixoff2(2), iyoff2(2)
        LOGICAL         found
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If the driver is VG, send the points directly to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSYMB ( 9, np, wtcod, x, y, ixoff, iyoff, iret )
	    RETURN
	END IF
C
C*	Save line type and line width.
C
 	IF  ( ( mltyp .ne. 1 ) .or. ( mlwid .ne. mcsywd) ) THEN
 	    jltyp = mltyp
 	    jlwid = mlwid
 	    CALL DSLINE ( 1, 0, mcsywd, 0, i1, i2, i3, i4, ier )
 	  ELSE
 	    jltyp = 0
 	    jlwid = 0
 	END IF
C
C*	Set clipping window.
C
	CALL DSCLIP ( iwndw, ier )
C
C*	Saving attributes of symbols.
C
        svtwtr = twtrsz        
        svmwtw = mwtwid
        svtspr = tsprsz
        svmspw = mspwid
        svtpwt = tpwtsz
        svmpww = mpwwid
        svtxt = txsize
        txsize = tcsysz
C
        IF  ( np .lt. 1 ) RETURN
	DO  i = 1, np
C
C*	Loop through symbols and setting x and y offsets.
C
            ixoff2 (1) = ixoff(i) - 2
            iyoff2 (1) = iyoff(i) + 1
            ixoff2 (2) = ixoff(i) + 2
            iyoff2 (2) = iyoff(i) - 1
C
	    icode = NINT ( wtcod (i) )
C
C*          Find the requested combination symbol from the common area.
C*          If the symbol number does not exist, return.
C
            found = .false.
            ksym  = 0
            DO WHILE  ( ( .not. found ) .and. ( ksym .lt. NSYNUM ) )
                ksym = ksym + 1
                IF  ( icode .eq. isycod (ksym) )  THEN
                    found = .true.
                    DO j = 1, 2
                        IF ( ccsym (j,ksym) .eq. 'WTHR' ) THEN
                            CALL DSWTHR ( tcsysz, mcsywd, scsysz, 
     +                                    lcsywd, ier )
                            CALL DWTHR ( iwndw, 1, rcmbsy (j,ksym), 
     +                            	 x(i), y(i), ixoff2(j), 
     +                                   iyoff2(j), ier )
                          ELSE
                            CALL DSPWTH ( tcsysz, mcsywd,  scsysz, 
     +                                    lcsywd, ier )
                            CALL DPWTH ( iwndw, 1, rcmbsy (j,ksym), 
     +                                   x(i), y(i), ixoff2(j), 
     +                                   iyoff2(j), ier )
                        END IF
                    END DO
C
C*                  Plotting the slanted line placed between the symbols.
C
                    CALL DSSPCL ( tcsysz, mcsywd, scsysz, lcsywd, ier )
                    CALL DSPCL ( iwndw, 1, 31.0, x(i), y(i), ixoff(i), 
     +                           iyoff(i), iret )
                END IF
            END DO
            IF  ( .not. found )  RETURN
	END DO
C
C*	Restore line type and line width.
C
	IF  ( ( jltyp .ne. 0 ) .or. ( jlwid .ne. 0 ) )
     +	    CALL DSLINE ( jltyp, 0, jlwid, 0, i1, i2, i3, i4, ier )
C
C*	Restoring attributes of symbols.
C
        twtrsz = svtwtr 
        mwtwid = svmwtw 
        tsprsz = svtspr 
        mspwid = svmspw 
        tpwtsz = svtpwt 
        mpwwid = svmpww 
        txsize = svtxt
C*
	RETURN
	END
