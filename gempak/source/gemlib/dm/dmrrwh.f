	SUBROUTINE DM_RRWH  ( iflno, ipos, iheadr, iret )
C************************************************************************
C* DM_RRWH								*
C*									*
C* This subroutine reads a row header from a DM file.			*
C*									*
C* DM_RRWH  ( IFLNO, IPOS, IHEADR, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IPOS		INTEGER		Location			*
C*									*
C* Output parameters:							*
C*	IHEADR (*)	INTEGER		Header array			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					 -9 = invalid row		*
C*					-11 = undefined header		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	4/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	INTEGER		iheadr (*)
C------------------------------------------------------------------------
C*	Check that the file number is valid.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check for valid position.
C
	IF ( (ipos .le. 0) .or. (ipos .gt. krow(iflno))) THEN
	    iret = -9
	    DO  i = 1, krkeys ( iflno )
		iheadr (i) = IMISSD
	    END DO
	  ELSE 
C
C*	    Check that this header is defined.
C
	    IF  ( kheadr ( 0, ipos, iflno ) .ne. IMISSD )  THEN
C
C*		Retrieve row header.
C
		DO  i = 1, krkeys (iflno)
		    iheadr (i) = kheadr ( i, ipos, iflno )
		END DO
C
C*		Set error return.
C
	      ELSE
		iret = -11
		DO  i = 1, krkeys ( iflno )
		    iheadr (i) = IMISSD
		END DO
	    END IF
	END IF
C*
	RETURN
	END
