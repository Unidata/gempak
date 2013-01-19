	SUBROUTINE HSCTBL  ( ctblnm, iret )
C************************************************************************
C* HSCTBL - XW								*
C* 									*
C* This subroutine loads common area /COLTBL/ with RGB color components	*
C* from the device color table (COLTBL.XWP), unless another color table	*
C* is specified.							*
C* 									*
C* HSCTBL  ( CTBLNM, IRET )						*
C* 									*
C* Input parameters:							*
C* 									*
C*	CTBLNM		CHAR*		Color table name		*
C*									*
C* Output parameters:							*
C*									*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Chang/EAI		12/93						*
C* S. Jacobs/NMC	 7/94	General clean up			*
C* G. Krueger/EAI	11/95	Changed RGB range from 0-1 to 0-255.	*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C* S. Wang/GSC		11/97	Change coltbl.xw to coltbl.xwp		*
C************************************************************************
	CHARACTER*(*)	ctblnm
C*
	CHARACTER	cnam*16, cab*4, cxnam*24
C------------------------------------------------------------------------
C*	If the table name is blank, use the default table.
C
	IF  ( ctblnm .eq. ' ' )  ctblnm = 'coltbl.xwp'
	CALL FL_TBOP ( ctblnm, 'colors', lun, iret )
C
C*	If the file is open...
C
	IF  ( iret .eq. 0 )  THEN
C
C*	    Initialize the first color as the background.
C
	    READ ( lun, 1000 ) cnam, cab, irg, igg, ibg, cxnam
1000	    FORMAT ( A, A, 3 (1X, I6), A )
	    CALL HSCRGB  ( 101, irg, igg, ibg, ier )
C
C*	    Initialize the remaining colors.
C
	    ncol  = 32
	    icolr = 0
	    iend  = 0
	    DO WHILE ( iend .eq. 0 )
		READ ( lun, 1000, IOSTAT = iend ) cnam, cab,
     +						  irg, igg, ibg,
     +						  cxnam
C
C*		Set the color, if this is not a comment.
C
		IF  ( cnam(1:1) .ne. '!' )  THEN
		    icolr = icolr + 1
		    CALL DSCNAM  ( icolr, cnam, ier )
		END IF
C
C*		Check for too many colors.
C
		IF  ( icolr .eq. ncol ) iend = -1
	    END DO
C
C*	    Close the file.
C
	    CALL FL_CLOS ( lun, ier )
	END IF
C*
	RETURN
	END
