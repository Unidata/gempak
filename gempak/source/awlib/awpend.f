	SUBROUTINE AW_PEND ( iff, iend, iret ) 
C************************************************************************
C* AW_PEND                                                              *
C*                                                                      *
C* This subroutine is the End of Product Block. This subroutine         *
C* represents mode 1, submode 2.                                        *
C* 								        *
C* AW_PEND ( IFF, IEND, IRET)                                     	*
C*								        *
C* Input parameters:                                                    *
C*	IFF 	INTEGER		Check sum flag                          *
C*								        *
C* Output parameters:						        *
C*	IEND	INTEGER		Return code                             *
C*	IRET	INTEGER		End of block code                       *
C**								        *
C* Log:									*
C* A. Hardy/GSC           7/98                                          *
C* A. Hardy/GSC          11/98	Added field flag                        *
C* A. Hardy/GSC           3/99  Updated prolog                          *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C************************************************************************
 	INCLUDE 	'awcmn.cmn'
C*
C------------------------------------------------------------------------
	IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In PEND'
        END IF
	iret=0
C
C*	Send return code that the end of the file has been reached.
C
        iend = 13
	IF ( dbug .eq. 'y' ) THEN
             IF ( iff .eq. 0 ) WRITE(flun,*)'Checksum found.'
        END IF
C*
	RETURN
	END
