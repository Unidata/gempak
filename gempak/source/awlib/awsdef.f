	SUBROUTINE AW_SDEF ( bdata, irept, lenbyt, iff, iret )
C************************************************************************
C* AW_SDEF                                                              *
C*                                                                      *
C* This subroutine is the system product ID number block.  This         *
C* subroutine represents mode 2, submode 2.                             *
C* 								        *
C* AW_SDEF ( BDATA, IREPT, LENBYT, IFF, IRET )                        	*
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
C* A. Hardy/GSC          3/99	Updated prolog				*
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        CHARACTER*15    tenchar
        INTEGER         icnt
C------------------------------------------------------------------------
 	iret = 0
	IF ( dbug .eq. 'y' )
     +      write(flun,*)'In AW_SDEF' 
        icnt = irept + 3
        itotln = irept + lenbyt
        IF ( iff .eq. 0 ) itotln = itotln - 1
C
        DO i = 1, 10
             tenchar( i:i ) = CHAR(INT(bdata(icnt+i)))
        END DO
C
        icnt = icnt + 12
	IF ( dbug .eq. 'y' ) THEN
            write(flun,15)'AWIPS ID PRODUCT NUMBER'
 15         format(a24)
	    write(flun,36)tenchar
 36         format(8x,a10)
        END IF
C
	IF ( dbug .eq. 'y' ) THEN
            IF ( iff .eq. 0 ) write(flun,*)'Checksum found.'
	END IF
C*
	RETURN
	END
