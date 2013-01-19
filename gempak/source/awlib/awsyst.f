	SUBROUTINE AW_SYST (bdata, irept, isub, lenbyt, iff, iret )
C************************************************************************
C* AW_SYST                                                              *
C*                                                                      *
C* This subroutine sends the systems data block to the proper submode   *
C* block.  This represents mode 2.                                      *
C* 								        *
C* AW_SYST  ( BDATA, IREPT, ISUB, LENBYT, IFF, IRET )                 	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) CHAR*1         Byte data array                         *
C*      IREPT   INTEGER         Beginning byte postion for the block    *
C*      ISUB    INTEGER         Submode number                          *
C*      LENBYT  INTEGER         Length of the block in bytes            *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* A. Hardy/GSC          3/99	Updated prolog; added byte declaration	*
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
        INCLUDE 	'awcmn.cmn'
C*
	BYTE		bdata(*)
C------------------------------------------------------------------------
	iret=0
C
C*      Check the submode byte to call proper subroutine.	
C
        IF ( isub .eq. 1 ) THEN
C
C*          System product ID number block.
C
            CALL AW_SDEF (bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 2 ) THEN
C
C*          System binary data block. It holds the communications
C*          header of an AWIPS product.
C
 	    CALL AW_SCOM ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 5 ) THEN
C
C*          System version block.
C
	    CALL AW_SVER ( bdata, irept, iff, iret )
	  ELSE
	    IF ( dbug .eq. 'y' ) THEN
                write(flun,*)'Error in reading block submode.'
	        write(flun,*)'Submode was read as: ',isub
            END IF
        END IF
C*
	RETURN
	END
