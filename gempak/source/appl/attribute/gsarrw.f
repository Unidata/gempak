	SUBROUTINE GSARRW  ( szarrw, szarrh, iarwid, iartyp, iret )
C************************************************************************
C* GSARRW								*
C* 									*
C* This subroutine sets the wind arrow size, arrow head size, line      *
C* width and arrow type.  If these parameters are not positive, no      *
C* change is made.							*
C*									*
C* GSARRW  ( SZARRW, SZARRH, IARWID, IARTYP, IRET )			*
C*									*
C* Input parameters:							*
C*	SZARRW		REAL		Wind arrow size 		*
C*	SZARRH		REAL		Arrow head size			*
C*	IARWID		INTEGER		Line width			*
C*	IARTYP		INTEGER         Arrow type			*
C*                                        1 = plot arrow for calm wind	*
C*					  2 = do not plot for calm wind	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Make size = 0 no change			*
C* S. Schotz/GSC	 1/90	Added arrow width and type		*
C* S. Schotz/GSC	 8/90	Added arrow head size			*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC		 6/98	Cleaned up prolog			*
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	INTEGER		isend (2)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FSARRW
C
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR ( szarrw, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	CALL GPUTR ( szarrh, 1, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
	isend (1) = iarwid
	isend (2) = iartyp
	CALL GPUT ( isend, 2, iret)
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get the return code.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
