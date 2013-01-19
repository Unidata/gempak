	SUBROUTINE AW_ALPH ( bdata, irept, isub, lenbyt, iff, iret)
C************************************************************************
C* AW_ALPH                                                              *
C*                                                                      *
C* This subroutine sends the alphanumeric block to the proper submode   *
C* block.  This subroutine is represents mode 5.                        *
C* 								        *
C* AW_ALPH ( BDATA, IREPT, ISUB, LENBYT, IFF, IRET)                   	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE	        Byte data array                         *
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
C* A. Hardy/GSC          3/99   Updated prolog, added byte declaration  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
 	INCLUDE 	'awcmn.cmn'
C*
        BYTE		bdata(*)
C------------------------------------------------------------------------
	iret=0
C
C*	Read the submode and send to appropriate subroutine.
C
        IF ( isub .eq. 20 ) THEN
	    write(6,*)'Mode 5 Submode 20 subroutine does not exist.'
C
C*          Alphanumeric product definition block.
C
C           CALL AW_ADEF ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 1 ) THEN
C
C*          Alphanumeric character block.
C
	    CALL AW_ACHR ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 2 ) THEN
C
C*          Data plot block.
C
            CALL AW_APLT ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 3 ) THEN
	    write(6,*)'Mode 5 Submode 3 subroutine does not exist.'
C
C*          Wind barbs data block.
C
C      	    CALL AW_AWBD ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 4 ) THEN
	    write(6,*)'Mode 5 Submode 4 subroutine does not exist.'
C
C*          Alphanumeric data block.
C
C      	    CALL AW_ADAT ( bdata, irept, lenbyt, iff, iret )
	  ELSE
	    IF ( dbug .eq. 'y' )  THEN
	        write(flun,*)'Error in reading block submode.'
                write(flun,*)'Submode was read as: ',isub
            END IF
        END IF
C*
	RETURN
	END
