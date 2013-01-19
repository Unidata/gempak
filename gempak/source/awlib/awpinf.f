	SUBROUTINE AW_PINF ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_PINF                                                              *
C*                                                                      *
C* This subroutine is the product information block.  It identifies     *
C* the product base date and time and also the identifier program       *
C* of the model which was used to generate the product.  This           *
C* subroutine represents mode 1, submode 6.                             *
C* 								        *
C* AW_PINF ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
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
C* A. Hardy/GSC		 3/99	Updated prolog				*
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
	CHARACTER*40    dattim, orgpgm
        INTEGER         icnt, itotln
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In PINF'
	END IF
 	iret = 0
	icnt = irept + 3
	itotln = irept + lenbyt
	IF ( iff .eq. 0 ) itotln = itotln - 1
C
C*      Get the 8 character base date and time (HHDDMMYY).
C
	DO i = 1, 8
 	    dattim( i:i ) = CHAR(INT(bdata(icnt+i)))
	END DO
	icnt = icnt + 8
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,*)'BASE DATE AND TIME'
            write(flun,35) dattim
 35         format(5x,a8)
	END IF
C
C*      Get the originating model or program name if it exists.
C
	IF  ( (icnt+2) .lt. itotln) THEN 
            j = 1
            DO i = icnt, itotln
                orgpgm( j:j ) = CHAR(INT(bdata( icnt+j )))
                j = j + 1
            END DO
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'ORIGINATING NAME'
                write(flun,40) orgpgm
 40             format(a10)
	    END IF
        ENDIF
        IF ( dbug .eq. 'y' ) THEN
            IF ( iff .eq. 0 ) write(flun,*)'Checksum found.'
        END IF
C*
	RETURN
	END
