	SUBROUTINE AW_ADBT ( bdata, icnt, itot, iret )
C************************************************************************
C* AW_ADBT                                                              *
C*                                                                      *
C* This subroutine adds two bytes together.                             *
C* 								        *
C* AW_ADBT ( BDATA, ICNT, ITOT, IRET )                                  *
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) 	BYTE	         Byte data array                *
C*      ICNT     	INTEGER          Beginning byte postion of block*
C*								        *
C* Output parameters:						        *
C*	ITOT		INTEGER		Total length of the byte        *
C*	IRET		INTEGER		Return code                     *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* A. Hardy/GSC         11/98	Added do loop; fix comments             *
C* A. Hardy/GSC          3/99   Updated prolog                          *
C* A. Hardy/GSC          2/00   Increase the file byte size 20k -> 80k  *
C* A. Hardy/GSC         10/00   Removed commented include		*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
	BYTE		bdata(*)
        INTEGER         ilen1, ilen2, itot 
        INTEGER         ibte(80000)
C------------------------------------------------------------------------
 	iret = 0
C
C*      Reassign byte array to an integer array.
C
        DO i = 1, 80000
            ibte(i) = bdata(i)
        END DO
C
C*      Get the length of the two bytes.
C
        ilen1 =  ( ibits ( ibte(icnt), 0, 8 ) ) 
        ilen2 =  ( ibits ( ibte(icnt+1), 0, 8 ) ) 
C
C*      These next two lines prevents the numbers from going
C*      out of the -128 to + 127 range.
C
        IF ( ilen1 .gt. 127 ) ilen1 = ilen1 - 256 
        IF ( ilen2 .lt. 0 ) ilen2 = ilen2 + 256
C
	itot = ( 256 * ilen1 ) + ilen2
C*
	RETURN
	END
