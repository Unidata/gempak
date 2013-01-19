	SUBROUTINE AW_PLIN ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_PLIN                                                              *
C*                                                                      *
C* This subroutine is the line information block.  It assigns a 	*
C* labeling value to the displayable line.  This subroutine represents 	*
C* mode 1, submode 7.                                                   *
C* 								        *
C* AW_PLIN ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
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
C* A. Hardy/GSC          8/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC		 3/99	Updated prolog				*
C* A. Hardy/GSC         10/00   Added array initialization;Put file     *
C*				number and dbug in awcmn.cmn		*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        CHARACTER*120   stchar
        INTEGER         icnt, ibeg, itotln, lenbyt
C------------------------------------------------------------------------
	IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In PLIN'
	END IF 
 	iret = 0
	icnt = irept + 4
	itotln = ( irept + lenbyt ) -1
C
C*      Getting the line value for labeling.      
C
        i = 1
        DO ibeg  = icnt, itotln
            stchar( i:i ) = CHAR(INT(bdata(ibeg)))
            i = i + 1
        END DO
        i = i - 1
	IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'CHARACTER STRING: '
	    write(flun,68) stchar(1:i)
 68         format(2x,a)
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
