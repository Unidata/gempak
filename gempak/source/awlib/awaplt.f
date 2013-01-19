	SUBROUTINE AW_APLT ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_APLT                                                              *
C*                                                                      *
C* This subroutine is the plot data block.  This block transmits        *
C* alphanumeric characters to be displayed at a specific location on a  *
C* product in a specified format.  This subroutine represents mode 5,   *
C* submode 2.                                                           *
C* 								        *
C* AW_APLT ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER         Beginning byte position for the block   *
C*      LENBYT  INTEGER         Length of the block in bytes            *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added do loop; fixed logical statement  *
C* A. Hardy/GSC		 3/99	Updated prolog				*
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC          2/00   Added check for ispch = 0               *
C* A. Hardy/GSC          4/00   Added check for i > 0                   *
C* A. Hardy/GSC          5/00   Allow for large and small text sizes    *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
	CHARACTER*120   stchar 
        INTEGER         icnt, mcord, ncord, pltcod, blmdid, rvblmd,
     +                  chsiz, itotln, ixoff, iyoff, fntnum, txtwid,
     +                  txtflg
        INTEGER         bckspc, fwdspc, dwnspc, upspc, hlflin, newlin
	REAL            xlat, ylon, txtsiz
	LOGICAL         spcflg, textfl, second
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In APLT'
        END IF
 	iret = 0
	icnt = irept + 4
	itotln = (irept + lenbyt) -1 
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the block mode indicator and the reverse block mode.
C
	blmdid = ibits( ibte(icnt), 7, 1)
	rvblmd = ibits( ibte(icnt), 6, 1)
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,61)'BLOCK MODE INDICATOR','REVERSE BLOCK MODE'
	    write(flun,62) blmdid, rvblmd
 61         format(a20,5x,a20)
 62         format(i2,25x,i2)
        END IF
C
C*      Get the character size.
C
	chsiz = ibits( ibte(icnt), 0, 6)
	pltcod = ibits( ibte(icnt+1), 0, 8)
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,*)'CHARACTER SIZE'
	    write(flun,67) chsiz
 67         format(i10)
        END IF
        
C
C*      Get the M and N coordinates
C
        CALL AW_ADBT (bdata, icnt, mcord, iret )
        xlat = mcord
	icnt = icnt + 2
C
        CALL AW_ADBT (bdata, icnt, ncord, iret )
        ylon = ncord
        icnt = icnt + 2
C
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,59)'M COORD. (xlat)','N COORD. (ylon)'
	    write(flun,60)mcord, ncord
 59         format(a16,5x,a15)
 60         format(i10,5x,i10)
        END IF
C
C*      Initializing all of the special cursor movement counters.
C
        i = 1
	textfl = .false.
	spcflg = .false.
	second = .false.
	bckspc = 0
	fwdspc = 0
	dwnspc = 0
	upspc  = 0
	hlflin = 0
	newlin = 0
	ixoff  = 0
	iyoff  = 0
	idum = 0
	txtsiz = 0.857
	fntnum = 22
	txtwid = 1
	txtflg = 2
	ibeg  = icnt
        ispch = 1
C
C*      Allow for text sizes LARGE and SMALL.
C
        IF ( chsiz .eq. 3 ) THEN
            txtsiz = 1.286
          ELSE
            txtsiz = 0.857
        ENDIF
C
C*      If ispch is 0 it is a null character and the DO loop
C*      is not executed.
C
        DO WHILE ( ( ibeg .le. itotln ) .and. (ispch .ne. 0 ) ) 
	    ispch = ibits( ibte(ibeg), 0, 8)
C
C*          If a cursor moving character is encountered and
C*          textfl is TRUE, build string from buffer and pass
C*          into GTEXT to be plotted.
C
	    IF ( ( ispch .eq. 18 ) .or. ( ( ispch .ge. 8 ) 
     +             .and. ( ispch .le. 13 ) ) ) THEN
		IF ( textfl ) THEN
	            CALL GSTEXT ( fntnum, txtflg, txtsiz, txtwid, 
     +                            idum, idum, idum, ier )
                    CALL GTEXT ( 'G', xlat, ylon, stchar(1:i), 0.0, 
     +                            ixoff, iyoff, iret )
                    textfl = .false.
		    i = 1
                END IF
C
C*              Back space.
C
	        IF ( ispch .eq. 8 ) THEN
                    bckspc = bckspc + 1
		    ixoff = ixoff - 2
C
C*                Forward space.
C
                  ELSE IF ( ispch .eq. 9 ) THEN
		    fwdspc = fwdspc + 1
		    ixoff = ixoff + 2
C
C*                Down space.
C
                  ELSE IF ( ispch .eq. 10 ) THEN
		    dwnspc = dwnspc + 1
		    iyoff = iyoff - 2
C
C*                Up space.
C
                  ELSE IF ( ispch .eq. 11 ) THEN
		    upspc = upspc + 1
		    iyoff = iyoff + 2
C
C*                Half line (1.5).
C
                  ELSE IF ( ispch .eq. 12 ) THEN
		    hlflin = hlflin + 1
		    ixoff = 0
		    iyoff = iyoff - 3
C
C*                New line (1.0).
C
                  ELSE IF ( ispch .eq. 13 ) THEN
		    newlin = newlin + 1
		    ixoff = 0
		    iyoff = iyoff - 2
C
C*                Turning on special character flag.
C
                  ELSE IF ( ispch .eq. 18 ) THEN
		    spcflg = .true.
                END IF
C
C*            Reset special character flag to regular mode.
C
	      ELSE IF ( ispch .eq. 17 ) THEN
                spcflg = .false.
C
C*            Plotting special character.
C
              ELSE IF  ( spcflg ) THEN
	        IF ( second ) ixoff = ixoff + 2
		CALL AW_WXSM (bdata, ibeg, ispch, chsiz, xlat, ylon,
     +                        ixoff, iyoff, iret)
		second = .true. 
C
C*            Storing text in string.
C
              ELSE  IF ( .not. textfl ) THEN
                IF ( ( ispch .ne. 0 ) .and. ( i .gt. 0 ) ) THEN
	            DO WHILE ( ( ( ispch .ge. 32) .and. 
     +                             ( ispch .le. 127 ) ) .and. 
     +                             ( ibeg .le. itotln ) )
	                stchar( i:i ) = CHAR(INT(bdata(ibeg)))
	                i = i + 1
		        ibeg = ibeg + 1
	                ispch = ibits( ibte (ibeg), 0, 8)
		        textfl = .true.
                    END DO
C
		    ibeg = ibeg - 1
		    i = i - 1
	            IF ( dbug .eq. 'y' ) THEN
	                write(flun,*)'CHARACTER STRING:'
	                write(flun,68) stchar(1:i)
 68                     format(2x,a)
	            END IF
                 END IF
              END IF 
	      ibeg = ibeg + 1
        END DO
C
C*      Printing out last character string.
C
	IF ( textfl ) THEN
	    CALL GSTEXT ( fntnum, txtflg, txtsiz, txtwid, idum, idum,
     +                    idum, ier )
            CALL GTEXT ( 'G', xlat, ylon, stchar(1:i), 0.0, ixoff, 
     +                iyoff, iret )
        END IF
C
C*      Print out regular characters.
C
	IF ( dbug .eq. 'y' ) THEN
	    write(flun,*)' ===========> SPECIAL CURSOR MOVEMENTS ',
     +                 '<=========='
	    write(flun,70)'BACK','FORWARD','DOWN','UP','LINE HALF',
     +                  'NEW LINE'
            write(flun,71)bckspc,fwdspc,dwnspc,upspc,hlflin,newlin
 70         format(a4,5x,a7,5x,a4,5x,a4,5x,a9,5x,a8)
 71         format(i4,5x,i7,5x,i4,5x,i4,5x,i9,5x,i8)
	END IF
C
C*      Check for checksum. 
C
        IF ( dbug .eq. 'y' ) THEN
            IF ( iff .eq. 0) write(flun,*)'Checksum found.'
        END IF
C*
	RETURN
	END
