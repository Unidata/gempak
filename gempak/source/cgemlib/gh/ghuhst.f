	SUBROUTINE GH_UHST ( flpart, nstrm, strmid, advno, iret )
C************************************************************************
C* GH_UHST								*
C*									*
C* This subroutine updates the history file using the storm identifiers *
C* and advisory numbers processed in the current run.                   *
C*									*
C* GH_UHST ( FLPART, NSTRM, STRMID, ADVNO, IRET )                       *
C*									*
C* Input parameters:                                                    *
C*      FLPART		CHAR*		History file name (w/out year)  *
C*	NSTRM		INTEGER		Number of new storm advisories  *
C*	STRMID (*)	CHAR*		Storm ids for new advisories    *
C* 	ADVNO (*)	CHAR*		New advisory numbers            *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*					 -1 = error opening history file*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 5/01  						*
C* D. Kidwell/NCEP	10/01	Added input parm flpart                 *
C* D. Kidwell/NCEP	 4/02   Used format for lunhst integer i/o      *
C* S. Gilbert/NCEP	 1/06   Allow for four character advisory num   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	strmid (*), advno (*), flpart
C*
	CHARACTER*(MXFLSZ)      filnam
C*
	CHARACTER	strmhs (500)*13, strnew (500)*13
	LOGICAL		done
C-----------------------------------------------------------------------
	iret  = 0
C
C*	Open the history file.
C
	CALL ST_LSTR ( flpart, lenf, ier )
	filnam = flpart ( :lenf) // '.' // strmid ( 1 ) ( 5:8 )
	CALL FL_SWOP ( filnam, lunhst, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Read the history file.
C
	READ (lunhst,200,IOSTAT=iostat) numhst
200	FORMAT ( I4 )
	IF ( iostat .ne. 0 ) numhst = 0
	IF ( numhst .gt. 0 ) THEN
	    READ (lunhst,100) ( strmhs (ii), ii = 1, numhst )
100         FORMAT (A)
	  ELSE
C
C*	    There are no entries in the history file.  The current
C*	    file(s) will be added.
C
	    strmhs ( 1 ) = ' '
	END IF
C
C*	Loop to update current storm advisories and add new storms.
C
	CALL FL_REWD ( lunhst, ier )
	nnew = 0
	DO nn = 1, nstrm
	    done = .false.
	    ii   = 1
	    DO WHILE ( .not. done )
		IF ( strmid ( nn ) .eq. strmhs ( ii ) ( :8 ) ) THEN
C
C*		    Update the advisory number for an existing storm.
C
		    strmhs ( ii ) ( 10:13 ) = advno ( nn ) ( :4 )
		    done = .true.
		  ELSE
		    ii = ii + 1 
		    IF ( ii .gt. numhst ) THEN
C
C*			Add a new storm.
C
			nnew = nnew + 1
			strnew ( nnew ) = strmid (nn) ( :8 ) // '_' //
     +					  advno (nn) ( :4 )
			done = .true.
		    END IF
		END IF
	    END DO
	END DO
C
C*	Write out the updated history file, with new storms first.
C
	numtot = numhst + nnew
	WRITE (lunhst,200) numtot
	DO ii = 1, nnew
	    WRITE (lunhst,100) strnew ( ii )
	END DO
	DO ii = 1, numhst
	    WRITE (lunhst,100) strmhs ( ii )
	END DO
	CALL FL_CLOS ( lunhst, ier )	
C*
	RETURN
	END
