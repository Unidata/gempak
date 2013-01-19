	SUBROUTINE ICTABL  ( iret )
C************************************************************************
C* ICTABL								*
C*									*
C* This subroutine loads common area /COLTBL/ with RGB color components	*
C* from the default color table (COLTBL.TBL).				*
C*									*
C* ICTABL  ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 9/85						*
C* M. desJardins/GSFC	 2/86	Removed system service call		*
C* M. desJardins/GSFC	 5/88	Added common block initialization	*
C* M. desJardins/GSFC	 5/89	Add correct standard FL calls		*
C* S. Schotz/GSC	 8/90	Use parameter for table path name	*
C* M. desJardins/NMC	12/91	Call even if colcmp = .false.		*
C* S. Jacobs/EAI	10/93	Added cxnam to the READ;		*
C*				  Changed input values to integers	*
C* G. Krueger/EAI	11/95	Removed HLS;Added XNAME; Mod. RGB range	*
C* D. Keiser/GSC	12/95	Changed FL_TOPN to FL_TBOP		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE 	'COLTBL.CMN'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER 	cnam*16, cab*4, cxnam*32
C*
C------------------------------------------------------------------------
	iret = NORMAL
	DO  i = 1, MXCLNM
	    cname  (i) = ' '
	    icred  (i) = -1
	    icgrn  (i) = -1
	    icblue (i) = -1
	    cxname (i) = ' '
	END DO
C
C*	Initialize the number of colors in the color table.
C
	numcol = 0
C
C*	Return if color components cannot be set.
C
	IF  ( .not. colcmp )  RETURN
C
C*	Open the color table file.
C
	CALL FL_TBOP  ( 'coltbl.tbl', 'colors', lun, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = NOCTBL
	    RETURN
	END IF
C	
C*	Read till end of file.
C
	iend = 0
	DO WHILE  ( iend .eq. 0 )
	    READ  ( lun, 1000, IOSTAT = iend )  cnam, cab, irg, igg,
     +						ibg, cxnam
1000	    FORMAT ( A, A, 3 (1X, I6), A )
C
C*	    Load common if not a comment record
C
	    IF ( ( cnam (1:1) .ne. '!' ) .and. ( iend .eq. 0 ) )  THEN
		numcol = numcol + 1 
		colnam (numcol) = cnam
		cabbr  (numcol) = cab
		irgun  (numcol) = irg
		iggun  (numcol) = igg
		ibgun  (numcol) = ibg
	    END IF
C
C*	    Check for too many colors.
C
	    IF  ( numcol .eq. MXCOL )  iend = -1
	END DO
C
C*	End of file.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
