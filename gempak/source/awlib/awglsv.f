	SUBROUTINE AW_GLSV ( bdata, irept, lenbyt, iff, iret ) 
C************************************************************************
C* AW_GLSV                                                              *
C*                                                                      *
C* This subroutine is the long/short relative vectors block.  This      *
C* block is used to transmit lines consisting of both vectors that      *
C* can be put into one byte and vectors that require 16 bits.  This     *
C* subroutine represents mode 4, submode 5.	                        *
C* 								        *
C* AW_GLSV ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
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
C* T. Piper/GSC		11/98	Updated	prolog				*
C* A. Hardy/GSC         11/98   Added assignment do loop                *
C* A. Hardy/GSC		 3/99	Updated	prolog				*
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        INTEGER         icnt, mcord, ncord, lmbit, itotln 
	INTEGER         mdlta, ndlta, bflag, numpt, kptrk 
	INTEGER*2       moff, noff
	REAL            xlat(5000), ylon(5000), xpos(5000), ypos(5000)
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In GLSV'
        END IF
 	iret = 0
	icnt = irept + 4
	itotln = (irept + lenbyt) - 1
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the M and N coordinates.
C        
        numpt = 1
	kptrk = 1
        CALL AW_ADBT (bdata, icnt, mcord, iret )
	xlat(numpt) = mcord 
        xpos(kptrk) = xlat(numpt)
	icnt = icnt + 2
C
        CALL AW_ADBT (bdata, icnt, ncord, iret )
	ylon(numpt) = ncord 
        ypos(kptrk) = ylon(numpt)
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
            write(flun,80)'M COORD (xlat)','N COORD (ylon)'
	    write(flun,81)xlat(numpt), ylon(numpt)
 80         format(a15,5x,a15)
 81         format(f10.2,15x,f10.2)
        END IF
C
C*      Determine if delta M and N are short or long vector values.
C
	DO WHILE (icnt .lt. itotln )
            lmbit = ibits ( ibte(icnt), 7, 1)
	    IF (lmbit .eq. 1) THEN
C
C*              Short vector values being used.  Also,
C*              Using the beam flag setting for drawing lines.
C
        	bflag = ibits ( ibte(icnt+1), 7, 1 )
		IF ( ( bflag .eq. 1 ) .and. ( numpt .gt. 1 ) ) THEN 
                    CALL GLINE ( 'G', kptrk, xpos, ypos, iret )
		    kptrk = 0
                END IF
	        numpt = numpt + 1
		kptrk = kptrk + 1
		CALL AW_2CST ( bdata(icnt), mdlta, iret )
	        xlat(numpt) = xlat(numpt - 1) + mdlta
		xpos(kptrk) = xlat(numpt)
C
		CALL AW_2CST ( bdata(icnt+1), ndlta, iret )
	        ylon(numpt) = ylon(numpt - 1) + ndlta
		ypos(kptrk) = ylon(numpt)
C
                IF ( dbug .eq. 'y' ) THEN
                    write(flun,85)'LEFT MOST BIT ','BEAM FLAG'
                    write(flun,86)lmbit,bflag
 85                 format(a14,5x,a10)
 86                 format(i1,20x,i1)
                    write(flun,94)'BYTE','M DELTA(dec)','M + DELTA',
     +                          'BYTE','N DELTA(dec)','N + DELTA'
 94                 format(a4,7x,a12,5x,a10,5x,a4,7x,a12,5x,a10)
                    write(flun,90)bdata(icnt),mdlta,xlat( numpt ),
     +                          bdata(icnt+1),ndlta,ylon( numpt )
 90                 format(Z2.2,8x,i5,12x,f10.2,9x,
     +                     Z2.2,8x,i5,8x,f10.2)
                END IF
		icnt = icnt + 2
            ELSE
C
C*              Long vector values are being used. Also,
C*              Using the beam flag setting for drawing lines.
C
        	bflag = ibits ( ibte(icnt+2), 5, 1 )
		IF ( ( bflag .eq. 1 ) .and. ( numpt .gt. 1 ) ) THEN 
                    CALL GLINE ( 'G', kptrk, xpos, ypos, iret )
		    kptrk = 0
                END IF
	        numpt = numpt + 1
		kptrk = kptrk + 1
                ilen1 =  (ibits ( ibte(icnt), 0, 5 ))
		ilen2 =  (ibits ( ibte(icnt+1), 0, 8 ))
                IF ( ilen1 .gt. 127 ) ilen1 = ilen1 - 256
                IF ( ilen2 .lt. 0 ) ilen2 = ilen2 + 256
                moff = ( 256 * ilen1 ) + ilen2
		CALL AW_2CLG ( moff, mdlta, iret )
	        xlat ( numpt ) = xlat(numpt -1) + mdlta
		xpos(kptrk) = xlat(numpt)
C
                ilen1 =  (ibits ( ibte(icnt+2), 0, 5 ))
		ilen2 =  (ibits ( ibte(icnt+3), 0, 8 ))
                IF ( ilen1 .gt. 127 ) ilen1 = ilen1 - 256
                IF ( ilen2 .lt. 0 ) ilen2 = ilen2 + 256
                noff = ( 256 * ilen1 ) + ilen2
		CALL AW_2CLG ( noff, ndlta, iret )
	        ylon ( numpt ) = ylon(numpt - 1) + ndlta
		ypos(kptrk) = ylon(numpt)
C
                IF ( dbug .eq. 'y' ) THEN
                    write(flun,85)'LEFT MOST BIT ','BEAM FLAG'
                    write(flun,86)lmbit,bflag
                    write(flun,94)'BYTE', 'M DELTA(dec)','M + DELTA',
     +                          'BYTE', 'N DELTA(dec)','N + DELTA'
            write(flun,91)bdata(icnt),bdata(icnt+1),mdlta,xlat(numpt ),
     +        bdata(icnt+2),bdata(icnt+3),ndlta,ylon(numpt )
 91         format(Z2.2,2x,z2.2,5x,i5,12x,f10.2,7x,Z2.2,2x,z2.2,
     +        5x,i5,5x,f10.2)
                END IF 
		icnt = icnt + 4
	    END IF
         END DO
C
C*      Printing any remaining vectors in the array.
C
	IF ( bflag .eq. 0 ) THEN 
            CALL GLINE ( 'G', kptrk, xpos, ypos, iret )
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
