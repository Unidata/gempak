	SUBROUTINE AW_SVER ( bdata, irept, iff, iret )
C************************************************************************
C* AW_SVER                                                              *
C*                                                                      *
C* This subroutine is the systems version block.  It identifies the     *
C* version that the graphics file was encoded.  This subroutine         *
C* represents mode 2, submode 5.              	                        *
C* 								        *
C* AW_SVER ( BDATA, IREPT, IFF, IRET )                        	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER         Beginning byte position for the block   *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC		 3/99   Updated prolog				*
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata and added INT	*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
	CHARACTER*40    verid
        INTEGER         icnt
C------------------------------------------------------------------------
 	iret = 0
	icnt = irept + 3
C
C*      Get the 4 character version number.
C
        DO i = 1, 4
	    verid(i:i) = CHAR(INT(bdata(icnt+i)))
        END DO
	icnt = icnt + 5
	IF ( dbug .eq. 'y' ) THEN
	    write(flun,*)'VERSION NUMBER'
	    write(flun,36)verid
 36         format(a4)
        END IF
C
	IF ( dbug .eq. 'y' ) THEN
           IF ( iff .eq. 0 ) write (flun,*)'Checksum found.'
        END IF
C*
	RETURN
	END
