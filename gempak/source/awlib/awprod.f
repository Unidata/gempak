	SUBROUTINE AW_PROD ( bdata, irept, isub, lenbyt, iff,
     +							 iend, iret )
C************************************************************************
C* AW_PROD                                                              *
C*                                                                      *
C* This subroutine sends the data block to the proper product data set  *
C* control submode block.  This subroutine represents mode 1.		*
C* 									*
C* AW_PROD ( BDATA, IREPT, ISUB, LENBYT, IFF, IEND, IRET )		*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE		Byte data array                         *
C*      IREPT   INTEGER		Beginning byte position for the block   *
C*      ISUB    INTEGER		Submode number                          *
C*      LENBYT  INTEGER		Length of the block in bytes            *
C*      IFF     INTEGER		Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IEND	INTEGER		Reached end of file                     *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Added iff to awpend calling sequence    *
C* A. Hardy/GSC          3/99   Switched dbug/iend in callin sequence   *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
 	INCLUDE 	'awcmn.cmn'
C*
	BYTE		bdata(*)
C------------------------------------------------------------------------
	iret=0
C
C*	Check the submode byte to call proper subroutine.
C
        IF ( isub .eq. 1 ) THEN
C
C*          Product identification block.
C

	    CALL AW_PIDB ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 2 ) THEN
C
C*          End of product block.
C
	    CALL AW_PEND (iff, iend, iret )
          ELSE IF ( isub .eq. 3 ) THEN
	    write(6,*)'Mode 1 Submode 3 subroutine does not exist.'
C
C*          Classification block.
C
C           CALL AW_PCLS ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 4 ) THEN
C
C*          Define plot parameters block.
C
	    CALL AW_PPLT ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 5 ) THEN
	    write(6,*)'Mode 1 Submode 5 subroutine does not exist.'
C
C*          Define datawidth/fieldwidth block.
C
C           CALL AW_PDFB ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 6 ) THEN
C
C*          Product information block.
C
	    CALL AW_PINF ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 7 ) THEN
C
C*          Line information block.
C
            CALL AW_PLIN ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 10 ) THEN
	    write(6,*)'Mode 1 Submode 10 subroutine does not exist.'
C
C*          Map background definition block.
C
C           CALL AW_PMAP ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 11 ) THEN
	    write(6,*)'Mode 1 Submode 11 subroutine does not exist.'
C
C*          Set active font block.
C
C           CALL AW_PFNT ( bdata, irept, lenbyt, iff, iret )   
          ELSE IF ( isub .eq. 12 ) THEN
	    write(6,*)'Mode 1 Submode 12 subroutine does not exist.'
C
C*          Devine color palette block.
C
C           CALL AW_PCLR ( bdata, irept, lenbyt, iff, iret )
	  ELSE
	    IF ( dbug .eq. 'y' ) THEN
                write(flun,*)'Error in reading block submode.'
                write(flun,*)'Submode was read as: ',isub
            END IF
        END IF
C*
	RETURN
	END
