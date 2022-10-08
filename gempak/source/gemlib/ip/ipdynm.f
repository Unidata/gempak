	SUBROUTINE IP_DYNM  ( done, iret )
C************************************************************************
C* IP_DYNM								*
C*									*
C* This subroutine will allow new values to be entered by the user	*
C* for the current program until the user types 'EXIT'.			*
C* Error messages which are encountered in processing user input	*
C* will be written to the user's terminal.				*
C*									*
C* IP_DYNM  ( DONE, IRET )						*
C*									*
C* Output parameters:							*
C*	DONE		LOGICAL		Program exit flag		*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* M. desJardins/GSFC	 4/89	Eliminate error for <CR>		*
C* M. desJardins/GSFC	 7/89	Added SAVE and RESTORE			*
C* M. desJardins/GSFC	 5/90	Add LAST.NTS as save file		*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* J. Whistler/SSAI	 4/91	Added option to page through help PHLP	*
C* J. Whistler/SSAI	 4/91	Fixed for input equal to full size	*
C* M. desJardins/NMC	 5/93	Check abbreviations more efficiently	*
C* K. Brill/NMC		 5/93	Use INPUT2 in setting FIRST		*
C* S. Jacobs/EAI	 6/93	Added CURSOR option			*
C* S. Jacobs/EAI	 9/93	Added a comment indicator		*
C* S. Jacobs/NMC	12/94	Added WINDOW option			*
C* J. Cowie/COMET	12/94	Added LUT file option			*
C* S. Jacobs/NMC	 1/95	Changed the LUT option to add image type*
C* S. Jacobs/NMC	 2/95	Removed image type from LUT command	*
C* S. Jacobs/NCEP	 5/96	Added CLOSE option			*
C* K. Tyle/GSC		 7/96	Renamed from NT_DYNM			*
C* K. Tyle/GSC		 8/96	Check for FIRSTT and RESP		*
C* D.W.Plummer/NCEP	 6/97	Increased string lengths from 80 to 164	*
C* C. Bailey/HPC	 1/05	Added GSAVE option			*
C* S. Jacobs/NCEP	 3/13	Allow mixed case for restoring NTS files*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	LOGICAL         done, resp
C*
	LOGICAL		more, exit, run, list, help, save, rest, disp
	LOGICAL		phlp, wsav, curs, swin, lutf, vers, cwin, gsav
	CHARACTER	input*164, input2*164, tutor*16, 
     +			first*12, secnd*164, cmmnt*1,
     +			lcntl*1, lcinp*164, frstnc*12, scndnc*164
C*
	DATA		cmmnt / '!' /
	DATA		lcntl / ';' /
C-----------------------------------------------------------------------
	iret  = 0
	done = .true.
C*
	CALL IP_RESP ( resp, ier )
	IF ( resp .or. firstt ) THEN
	  IF  ( firstt )  THEN
	    CALL IP_LIST  ( ' ', .false., ier )
	    firstt = .false.
	  END IF
C
C*	  Build tutor prompt which says  "GEMPAK-PROGRAM>".
C
	  tutor = 'GEMPAK-' // cprogm
	  CALL ST_LSTR  ( tutor, lent, ier )
	  IF  ( lent .gt. 7 )  lent  = lent + 1
	  tutor ( lent : lent ) = '>'
C
C*	  Write line containing list of parameters.
C
	  CALL IP_RQST  ( ier )
C
C*	  Loop until user enters EXIT or RUN.
C
	  more = .true.
	  DO WHILE  ( more )
	    WRITE  ( 6, 1000 ) tutor ( 1 : lent )
1000	    FORMAT ( 1X, A, $ )
	    READ   ( 5, 1001, IOSTAT = iostat ) input
1001	    FORMAT ( A )
	    IF  ( iostat .ne. 0 )  input = 'EXIT'

C
C*	    Check whether the user wants to exit or run program.
C
	    CALL ST_LDSP  ( input, input, nc, ier )
C
C*	    Check for a comment.
C
	    IF  ( input(1:1) .ne. cmmnt )  THEN
	      IF  ( input(1:1) .ne. lcntl )  THEN
		CALL ST_LCUC  ( input, input2, ier )
		CALL ST_ABBR  ( 'EXIT', input2, exit, ier )
		CALL ST_ABBR  ( 'RUN',  input2, run,  ier )
C
C*		If not done, get substring before first blank.
C*
C*		Parse the uppercase string and the original string. 
C*		
C
		IF  ( ( .not. exit ) .and. ( .not. run ) )  THEN
		  iblnk = INDEX ( input, ' ' )
		  IF  ( ( iblnk .gt. 0 ) .and. ( iblnk .lt. nc ) )
     +		    THEN
		    first = input2 ( : iblnk-1 )
		    secnd = input2 ( iblnk+1 : )
		    frstnc = input ( : iblnk-1 )
		    scndnc = input ( iblnk+1 : )
		  ELSE
		    first = input2
		    secnd = ' '
		    frstnc = input
		    scndnc = ' '
		  END IF
C
C*		  Check to see which command this is.
C
		  disp = .false.
		  list = .false.
		  help = .false.
		  rest = .false.
		  phlp = .false.
		  save = .false.
		  wsav = .false.
		  curs = .false.
		  swin = .false.
		  lutf = .false.
		  vers = .false.
		  cwin = .false.
                  gsav = .false.
		  CALL ST_ABBR  ( 'DISPLAY', first, disp, ier )
		  IF  ( .not. disp )  THEN
		   CALL ST_ABBR  ( 'LIST', first, list, ier )
		   IF  ( .not. list )  THEN
		    CALL ST_ABBR  ( 'HELP', first, help, ier )
		    IF  ( .not. help )  THEN
		     CALL ST_ABBR  ( 'RESTORE', first, rest, ier )
		     IF  ( .not. rest )  THEN
		      CALL ST_ABBR  ( 'WINDOW', first, swin, ier )
		      IF  ( .not. swin )  THEN
                       CALL ST_ABBR  ( 'GSAVE', first, gsav, ier )
                       IF  ( .not. gsav )  THEN 
		        CALL ST_ABBR  ( 'PHELP', first, phlp, ier )
		        IF  ( .not. phlp )  THEN
			 CALL ST_ABBR  ( 'SAVE', first, save, ier )
			 IF  ( .not. save )  THEN
			  CALL ST_ABBR  ( 'WSAVE', first, wsav, ier )
			  IF  ( .not. wsav )  THEN
			   CALL ST_ABBR  ( 'CURSOR', first, curs, ier )
			   IF  ( .not. curs )  THEN
			    CALL ST_ABBR  ( 'LUT', first, lutf, ier )
			    IF  ( .not. lutf )  THEN
			     CALL ST_ABBR ( 'VERSION', first, vers, ier)
			     IF  ( .not. vers )  THEN
			      CALL ST_ABBR ( 'CLOSE', first, cwin, ier )
			     END IF
			    END IF
			   END IF
			  END IF
			 END IF
		        END IF
		       END IF
		      END IF
		     END IF
		    END IF
		   END IF
		  END IF
		END IF
C
C*		An = indicates user is entering new value for parameter.
C
		iequal = INDEX ( input, '=' )
C
C*		Process user input.
C
		IF  ( exit )  THEN
		  done = .true.
		  more = .false.
		ELSE IF  ( run )  THEN
C
C*		Save parameters in file LAST.NTS .
C
		  CALL IP_SAVE  ( 'LAST', .true., ier )
		  done = .false.
		  more = .false.
		ELSE IF  ( iequal .ne. 0 )  THEN
		  CALL IP_SVAR  ( input, ier )
		ELSE IF  ( list .or. disp )  THEN
		  CALL IP_LIST  ( secnd, list, ier )
		ELSE IF  ( help )  THEN
		  CALL IP_HELP  ( secnd, .false., ier )
		ELSE IF  ( save )  THEN
		  CALL IP_SAVE  ( secnd, .true., ier )
		ELSE IF  ( wsav )  THEN
		  CALL IP_SAVE  ( secnd, .false., ier )
		ELSE IF  ( rest )  THEN
		  CALL IP_REST  ( scndnc, ier )
		ELSE IF  ( phlp )  THEN
		  CALL IP_HELP  ( secnd, .true., ier )
		ELSE IF  ( curs )  THEN
		  CALL IP_GTPT  ( secnd, ier )
		ELSE IF  ( swin )  THEN
C
C*		  Use the original window name, NOT the uppercase name.
C
		  CALL IP_SWIN  ( scndnc, ier )
		ELSE IF  ( gsav )  THEN
C
C*                Use the original window name, NOT the uppercase name.
C
                  CALL IP_GSAV  ( scndnc, 1, 1, ier ) 
		ELSE IF  ( lutf )  THEN
C
C*		  Use the original filename, NOT the uppercase name.
C
		  CALL IP_LUTF  ( scndnc, ier )
		ELSE IF  ( vers )  THEN
		  CALL IP_VERS  ( ier )
		ELSE IF  ( cwin )  THEN
C
C*		  Use the original window name, NOT the uppercase name.
C
		  CALL IP_CWIN  ( scndnc, ier )
		ELSE IF  ( nc .gt. 0 )  THEN
		  ier = -5
		  CALL ER_WMSG  ( 'IP', ier, input, ierr )
		END IF
	      ELSE
		lcinp = input(2:)
		CALL IP_LCTL ( lcinp, ier )
	      END IF
	    END IF
	  END DO
	END IF
C*
	RETURN
	END
