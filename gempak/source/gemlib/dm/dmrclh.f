	SUBROUTINE DM_RCLH  ( iflno, ipos, iheadr, iret )
C************************************************************************
C* DM_RCLH								*
C*									*
C* This subroutine reads a column header from a DM file.		*
C*									*
C* DM_RCLH  ( IFLNO, IPOS, IHEADR, IRET )				*
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
C*					 -9 = invalid column		*
C*					-11 = undefined header		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	4/87						*
C* m. gamazaychikov/CWS 04/11   Add code for A2DB connectivity          *
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
	IF ( (ipos .le. 0) .or. (ipos .gt. kcol (iflno))) THEN
	    iret = -9
	    DO  i = 1, kckeys ( iflno )
		iheadr (i) = IMISSD
	    END DO
	  ELSE 
C
C*	    Check that this header is defined.
C
	    jloc = ipos + krow ( iflno )
	    IF  ( kheadr ( 0, jloc, iflno ) .ne. IMISSD )  THEN
C
C*		Retrieve row header.
C
		DO  i = 1, kckeys (iflno)
		    iheadr (i) = kheadr ( i, jloc, iflno )
		END DO
C
C*		Set error return.
C
	      ELSE
		iret = -11
		DO  i = 1, kckeys ( iflno )
		    iheadr (i) = IMISSD
		END DO
	    END IF
	END IF
C*
	RETURN
	END
