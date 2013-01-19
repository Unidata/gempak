	SUBROUTINE GR_LIST ( nexp, nlun, luns, igdfln, pause, mesage,
     +	                     titl, levtyp, nlev, rlevel, icord, nparm,
     +			     prmlst, ntime, timfnd, answer, iret )
C************************************************************************
C* GR_LIST								*
C*									*
C* This subroutine lists all the grids in a grid file and prompts the 	*
C* user for input.  The input will be returned in ANSWER.  The list may *
C* be sent to as many as four output units.				*
C*									*
C* GR_LIST  ( NEXP, NLUN, LUNS, IGDFLN, PAUSE, MESAGE, TITL, LEVTYP,	*
C*            NLEV, RLEVEL, ICORD, NPARM, PRMLST, NTIME, TIMFND,	*
C*            ANSWER, IRET )						*
C*									*
C* Input parameters:							*
C*      NEXP            INTEGER         Max number of levels            *
C*	NLUN		INTEGER		Number of output units		*
C*	LUNS (4)	INTEGER		Logical output unit numbers	*
C*	IGDFLN		INTEGER		Grid file number		*
C*	PAUSE		LOGICAL		Flag to page output		*
C*	MESAGE		CHAR*		Message to write 		*
C*      TITL		LOGICAL		Title				*
C*      LEVTYP          INTEGER         Level type from the user        *
C*                                        0 = no levels input           *
C*                                        1 = list of levels            *
C*                                        2 = range of levels           *
C*                                        3 = all levels                *
C*      NLEV            INTEGER         Number of levels                *
C*      RLEVEL (NEXP,2) REAL            Levels or range from the user   *
C*      ICORD           INTEGER         Vert coord from the user        *
C*      NPARM           INTEGER         Number of parameters            *
C*      PRMLST (NPARM)  CHAR*           Parameter list from the user    *
C*      NTIME           INTEGER         Number of times                 *
C*      TIMFND (NTIME)  CHAR*           List of times from the user     *
C*									*
C* Output parameters:							*
C*	ANSWER		CHAR*		User input			*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C*					-10 = less than 1 output LUN	*
C**									*
C* Log:									*
C* M. desJardins/GSFC    2/85                                           *
C* M. desJardins/GSFC    7/87   Changed to GEMPAK4 GD library           *
C* M. desJardins/GSFC    1/88   Added grid forecast time                *
C* M. desJardins/GSFC    7/88   Changed to list all grids               *
C* G. Huffman/GSC       11/88   Add writing to multiple LUNs            *
C* S. Schotz/GSC         6/90   Get respnd locally from IP_RESP         *
C* M. desJardins/GSFC    9/90   Elim clear page, last msg when ' '      *
C* M. desJardins/NMC     1/92   Corrected return code for no EXIT       *
C* S. Jacobs/EAI         3/93   Added pause flag to page output;        *
C*                                Added PAGLEN for page length          *
C* S. Jacobs/NCEP        6/96   Changed PAGLEN to LENPAG                *
C* S. Maxwell/GSC	10/96	Changed to match based on user input	*
C* S. Jacobs/NCEP	11/96	Added a warning if no grids were found	*
C* S. Jacobs/NCEP	12/96	Fixed warning for no grids found	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* D.W.Plummer/NCEP	 4/00	Calling seq chg for title initializer	*
C* R. Tian/SAIC		 3/05	Removed GDU_GINP, calling seq chg	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( LENPAG = 35 )
C*
	CHARACTER*(*)	mesage, answer, prmlst(*), timfnd(*)
	INTEGER		luns (4)
	REAL            rlevel (NEXP, 2)
	LOGICAL		pause, titl

C*
	CHARACTER	time (2)*20, ppp*12
	INTEGER		lev (2), jvcord
	LOGICAL		respnd, pagflg, newlin, title, done, match
	SAVE		ngd
C------------------------------------------------------------------------
	iret   = 0
	answer = ' '
	newlin = .false.
	title  = titl
	pagflg = .true.
C
C*	Write an error message if there are no grids.
C
	IF  ( nlun .lt. 1 )  THEN
	    CALL  ER_WMSG  ( 'GDU', -10, ' ', ier )
	    RETURN
	END IF
C
C*	Get length of message.
C
	CALL ST_LSTR  ( mesage, length, ier )
C
C*	Get respond flag.
C
	CALL IP_RESP  ( respnd, ier )
C
C*	Set up counters to list LENPAG grids on a page and to get
C*	grids by grid number from the file.
C
	igrid  = 1
	if ( title )  ngd = 0
	ians   = 1
	knt    = 0
C
C*	Loop through grids printing LENPAG at a time.
C
	done = .false.
	DO WHILE  ( .not. done )
C
C*	    Get next grid header.
C
	    CALL GD_GIDN ( igdfln, igrid, time, lev, jvcord, ppp, ier )
C
C*	    Check for end of grids.
C
	    IF  ( ier .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*		Write out message after LENPAG grids.
C
		IF  ( ( ngd .eq. LENPAG ) .and. ( respnd ) .and.
     +		      ( pause ) )  THEN
		    CALL TM_STR ( mesage, pagflg, newlin, answer, ians )
		    IF  ( ians .eq. 2 )  THEN
			done = .true.
			iret = 3
		      ELSE IF  ( ians .eq. 0 )  THEN
			done = .true.
			iret = 0
		    END IF
		    ngd = 0
		END IF
	    END IF
C
C*	    If not finished, write out this grid.
C
	    IF  ( .not. done )  THEN
C
C*              Loop through all grids to find matches.
C
                CALL GDU_MTCH ( NEXP, time, lev, jvcord, ppp, levtyp,
     +                          nlev, rlevel, icord, nparm, prmlst,
     +                          ntime, timfnd, match, iret )
C
           	IF ( match ) THEN
		     ngd = ngd + 1
		     knt = knt + 1
		     DO  ilun = 1, nlun
		         lunx = luns ( ilun )
		         CALL GR_WTRM  ( lunx, title, igrid, time, lev,
     +			     	         jvcord, ppp, ier )
		     END DO
		     title = .false.
		END IF
	    END IF
	    igrid = igrid + 1
	END DO
C
C*	If grids have been output, print message.
C
	IF  ( ( ngd .gt. 0 ) .and. ( respnd ) .and.
     +	      ( length .gt. 0 ) )  THEN
	    pagflg = .false.
	    CALL TM_STR  ( mesage, pagflg, newlin, answer, ians )
	END IF
	IF  ( ians .eq. 2 )  iret = 3
C
C*	Check for no grids found.
C
	IF  ( knt .eq. 0 )  CALL ER_WMSG ( 'GDU', 2, ' ', ierr )
C*
	RETURN
	END
