	SUBROUTINE AW_GABV ( bdata, irept, lenbyt, iff, iret ) 
C************************************************************************
C* AW_GABV                                                              *
C*                                                                      *
C* This subroutine is the absolute vectors block.  This block contains	*
C* coordinates of the vector end points that define one line on the     *
C* product.  This subroutine represents mode 4, submode 10.	        *
C* 								        *
C* AW_GABV ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
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
C* A. Hardy/GSC          10/00  Created					* 
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        INTEGER         icnt, mcord, ncord, lmbit, itotln 
	INTEGER         kptrk 
	REAL            xpos(5000), ypos(5000)
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In GABV'
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
C*      Get the 1st set of M and N coordinates.
C        
	kptrk = 1
        CALL AW_ADBT (bdata, icnt, mcord, iret )
	xpos ( kptrk ) = mcord 
	icnt = icnt + 2
C
        CALL AW_ADBT (bdata, icnt, ncord, iret )
	ypos ( kptrk ) = ncord 
	icnt = icnt + 2
        IF ( dbug .eq. 'y' ) THEN
            write(flun,80)'M COORD (xlat)','N COORD (ylon)'
	    write(flun,81)xpos(kptrk), ypos(kptrk)
 80         format(a15,5x,a15)
 81         format(f10.2,11x,f10.2)
        END IF
C
C*      Determine if the lmb of the N coordinate is left blank ( = 1)
C*      or is displayed normally ( = 0 ).
C
        CALL GSSMTH ( 2, 2.0, iret )
	DO WHILE (icnt .lt. itotln )
C
C*          Calculate the M coordinate.
C
 	    kptrk = kptrk + 1
            CALL AW_ADBT (bdata, icnt, mcord, iret )
	    xpos ( kptrk ) = mcord 
    	    icnt = icnt + 2
C
C*          Calculate the N coordinate.  
C
            lmbit =  (ibits ( ibte (icnt), 7, 1 ) )
            ilen1 =  (ibits ( ibte (icnt), 0, 7 ) )
	    ilen2 =  (ibits ( ibte (icnt+1), 0, 8 ) )
            IF ( ilen1 .gt. 127 ) ilen1 = ilen1 - 256
            IF ( ilen2 .lt. 0 ) ilen2 = ilen2 + 256
	    ypos ( kptrk ) = ( 256 * ilen1 ) + ilen2
	    icnt  = icnt + 2
C
            IF ( dbug .eq. 'y' ) THEN
                write(flun,85)'POINT NUMBER','LEFT MOST BIT ',
     +                      'M COORD (xlat)','N COORD (ylon)'
                write(flun,86)kptrk, lmbit, xpos(kptrk), ypos(kptrk)
 85             format(a12,5x,a14,5x,a15,5x,a15 )
 86             format(5x,i3,15x,i1,10x,f10.2,11x,f10.2)
            END IF 
            IF ( ( lmbit .eq. 1 ) .and. ( kptrk .ge. 2 ) ) THEN 
                CALL GLINE ( 'G', kptrk, xpos, ypos, iret )
	        kptrk = 0
            END IF
C
        END DO
C
C*      Printing any remaining vectors in the array.
C
 	IF ( ( lmbit .eq. 0 ) .and. ( kptrk .ge. 2 ) ) THEN 
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
