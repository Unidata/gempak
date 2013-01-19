	SUBROUTINE IP_SAVE  ( file, filflg, iret )
C************************************************************************
C* IP_SAVE								*
C*									*
C* This subroutine saves a parameter file to a .NTS file.  If FILFLG	*
C* is false, file names will not be saved.				*
C*									*
C* IP_SAVE  ( FILE, FILFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	FILE		CHAR*		Input file name			*
C*	FILFLG		LOGICAL		Save file names flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					 -9 = invalid file name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89						*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* M. desJardins/NMC	 1/92	Added filflg				*
C* M. desJardins/NMC	 2/92	Fix bug to save last parameter		*
C* K. Tyle/GSC		 7/96	Renamed from NT_SAVE			*
C* D.W.Plummer/NCEP	 6/97	Increased string length from 72 to 132	*
C* A. Hardy/NCEP	12/04	Check for leading spaces in file name	*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	file
	LOGICAL		filflg
C*
	CHARACTER	filnam*132
C-----------------------------------------------------------------------
	iret = 0
C
C*	Make full file name.
C
	CALL IP_SAVF  ( file, filnam, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'IP', iret, file, ier )
	    RETURN
	END IF
C
C*	Remove any leading spaces from the file name.
C
 	CALL ST_LDSP ( filnam, filnam, len, ier )
C
C*	Open the output file.
C
	CALL FL_SWOP  ( filnam, lun, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'IP', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Loop through writing parameters from this program to the file.
C
	i = ihead
	DO WHILE  ( i .gt. 0 )
	    IF  ( filflg .or. ( ( cparmn (i) .ne. 'GDFILE' )
     +						.and.
     +				 ( cparmn (i) .ne. 'SFFILE' )
     +						.and.
     +				 ( cparmn (i) .ne. 'SNFILE' ) ) )
     +							THEN
		WRITE ( lun, 1000, IOSTAT = iostat ) cparmn (i),
     +						     cparmv (i)
1000		FORMAT ( A, A )
	    END IF
	    i = iplink (i)
	END DO
C
C*	Close the output file.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
