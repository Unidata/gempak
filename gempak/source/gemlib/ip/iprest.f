	SUBROUTINE IP_REST  ( file, iret )
C************************************************************************
C* IP_REST								*
C*									*
C* This subroutine restores a parameter file.				*
C*									*
C* The restore file name is split into the path and the file name and	*
C* and the file is located by searching in the following order:		*
C*									*
C*	1. filename as given (local)					*
C*	2. filename with ".nts" added (local)				*
C*	3. path/filename as given					*
C*	4. path/filename with ".nts" added				*
C*	5. $GEMNTS/path/filename as given				*
C*	6. $GEMNTS/path/filename with ".nts" added			*
C*									*
C* IP_REST  ( FILE, IRET )						*
C*									*
C* Input parameters:							*
C*	FILE		CHAR*		Input file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C*					 -9 = invalid file name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/89						*
C* S. Jacobs/EAI	 9/93	Added check for files and to 		*
C*				   get global files from GEMNTS		*
C* S. Jacobs/NMC         2/94	Added file name return to FL_INQR	*
C* P. Bruehl/Unidata	10/94	Corrected "//" of nts filename		*
C* K. Tyle/GSC		 7/96	Renamed from NT_REST			*
C* S. Jacobs/NCEP	 7/96	Changed environ vars to "$.../" format	*
C* D.W.Plummer/NCEP	 6/97	Increased string lengths from 72	*
C* S. Jacobs/NCEP	12/97	Changed file checking and selection	*
C* S. Jacobs/NCEP	12/97	Added check for blank input file name	*
C* S. Jacobs/NCEP	12/97	Fixed check for a local file		*
C* A. Hardy/NCEP	12/04	Check for leading spaces in file name	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	file
C*
	CHARACTER	patfil*132, path*132, filnam*132, newfil*132,
     +			cp1*8, cp2*128, tfil*132, name*132, name2*132,
     +			tmpfil*132
	LOGICAL		exist, found, done
C-----------------------------------------------------------------------
	iret  = 0
	found = .false.
C
C*	Check for a blank file name.
C
	IF  ( file .eq. ' ' )  THEN
	    CALL IP_SAVF ( file, tmpfil, ier )
	  ELSE
	    tmpfil = file
	END IF
C
C*	Remove any leading spaces from the file name.
C
	CALL ST_LDSP ( tmpfil, tmpfil, lent, ier )
C
C*	Strip GEMNTS from beginning of the restore file name if present.
C
	CALL ST_RMST ( tmpfil, '$GEMNTS/', ipos, patfil, ier )
	IF  ( ipos .eq. 0 )  THEN
	    CALL ST_RMST ( tmpfil, 'GEMNTS:', ipos, patfil, ier )
	END IF
	IF ( ipos .eq. 0 ) patfil = tmpfil
C
C*	Extract the file name from the end of the path.
C
	CALL ST_LSTR ( patfil, ncnt, ier )
	done = .false.
	ipnt = ncnt + 1
	DO WHILE  ( ( .not. done ) .and. ( ncnt .gt. 0 ) )
	    ipnt = ipnt - 1
	    IF  ( ( patfil(ipnt:ipnt) .eq. '/' ) .or.
     +		  ( patfil(ipnt:ipnt) .eq. ':' ) )  done = .true.
	    ncnt = ncnt - 1
	END DO
C
	IF  ( .not. done )  THEN
	    filnam = patfil
	    path   = ' '
	  ELSE
	    filnam = patfil ( ipnt+1: )
	    path   = patfil ( :ipnt )
	END IF
C
	name = patfil
C
C*	Check for the file locally.
C
	CALL ST_LSTR ( filnam, lenf, ier )
	CALL FL_INQR ( filnam, exist, newfil, ierr )
	IF  ( exist )  THEN
	    name  = filnam(1:lenf)
	    found = .true.
	END IF
C
C*	Check for the file locally, with ".nts" added.
C
	IF  ( .not. found )  THEN
	    CALL IP_SAVF ( filnam, tfil, ier )
	    IF  ( filnam .ne. tfil )  THEN
		CALL FL_INQR ( tfil, exist, newfil, ierr )
		IF  ( exist )  THEN
		    name  = tfil
		    found = .true.
		END IF
	    END IF
	END IF
C
C*	Check for the file in the given path.
C
	IF  ( ( .not. found ) .and. ( path .ne. ' ' ) )  THEN
	    CALL FL_INQR ( patfil, exist, newfil, ierr )
	    IF  ( exist )  THEN
		name  = patfil
		found = .true.
	    END IF
	END IF
C
C*	Check for the file in the given path, with ".nts" added.
C
	IF  ( ( .not. found ) .and. ( path .ne. ' ' ) )  THEN
	    CALL IP_SAVF ( patfil, tfil, ier )
	    IF  ( patfil .ne. tfil )  THEN
		CALL FL_INQR ( tfil, exist, newfil, ierr )
		IF  ( exist )  THEN
		    name  = tfil
		    found = .true.
		END IF
	    END IF
	END IF
C
C*	Check for the file in $GEMNTS.
C
	IF  ( ( .not. found ) .and. ( path .ne. ' ' ) )  THEN
	    name2 = '$GEMNTS/' // patfil
	    CALL FL_INQR ( name2, exist, newfil, ierr )
	    IF  ( exist )  THEN
		name  = name2
		found = .true.
	    END IF
	END IF
C
C*	Check for the file in $GEMNTS, with ".nts" added.
C
	IF  ( ( .not. found ) .and. ( path .ne. ' ' ) )  THEN
	    name2 = '$GEMNTS/' // patfil
	    CALL IP_SAVF ( name2, tfil, ier )
	    IF  ( name2 .ne. tfil )  THEN
		CALL FL_INQR ( tfil, exist, newfil, ierr )
		IF  ( exist )  THEN
		    name  = tfil
		    found = .true.
		END IF
	    END IF
	END IF
	CALL ST_LDSP ( name, name, lent, ier )
C
C*	Open the input file.
C
	CALL FL_SOPN  ( name, lun, iret )
	IF  ( iret .ne. 0 )  THEN
	    iret = -9
	    CALL ER_WMSG  ( 'IP', iret, tmpfil, ier )
	    RETURN
	END IF
C
C*	Loop through reading parameters from the file.
C
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
C
C*	    Read the next record.
C
	    READ   ( lun, 1000, IOSTAT = iostat )  cp1, cp2
1000	    FORMAT ( A, A )
	    CALL ST_LCUC  ( cp1, cp1, ier )
	    IF  ( iostat .eq. 0 )  THEN
C
C*		Check for parameter in list.
C
		knt = 1
		DO WHILE  ( knt .le. ncparm )
		    IF  ( cparmn ( knt ) .eq. cp1 )  THEN
			cparmv ( knt ) = cp2
			knt = ncparm + 1
		      ELSE
			knt = knt + 1
		    END IF
		END DO
	    END IF
	END DO
C
C*	Close the input file.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
