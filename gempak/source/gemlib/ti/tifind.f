	SUBROUTINE TI_FIND  ( dattim, ntimin,  timlst, timout, 
     +			      ntime, timfnd, iret )
C************************************************************************
C* TI_FIND								*
C*									*
C* This subroutine converts the user input for DATTIM into a list	*
C* of times.  The times may be entered as a list or range of times.	*
C* The requested times are returned in TIMFND.  The times in the file	*
C* are input in TIMLST, where the times must be in the standard		*
C* GEMPAK format and must be sorted from earliest to latest.  This	*
C* subroutine will write error messages for any error encountered.	*
C*									*
C* TI_FIND  ( DATTIM, NTIMIN, TIMLST, TIMOUT, NTIME, TIMFND,		*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		User input time			*
C*	NTIMIN		INTEGER		Number of data set times 	*
C*	TIMLST (NTIMIN)	CHAR*		Data set times			*
C*									*
C* Output parameters:							*
C*	TIMOUT		CHAR*		User input time 		*
C*	NTIME		INTEGER		Number of times requested	*
C*	TIMFND (NTIME)	CHAR*		Requested times			*
C*	IRET		INTEGER		Return code			*
C*					 +2 = time not in data set	*
C*					 +1 = EXIT entered		*
C*					  0 = normal return		*
C*					 -1 = DATTIM is invalid		*
C*					 -2 = no valid times entered	*
C*					 -3 = no times in range		*
C*					 -4 = cannot list times		*
C*					 -5 = data set has no times	*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/84						*
C* M. desJardins/GSFC	 4/86	Changed call to ST_LIST to ST_CLST	*
C* M. desJardins/GSFC	11/87	GEMPAK4 version				*
C* I. Graffman/RDS	12/87	Return character times			*
C* M. desJardins/GSFC	 4/90   Add time with increment			*
C* S. Schotz/GSC	 5/90	Get value of rspnd via IP_RESP		*
C* K. Brill/NMC          9/90   Return only times in data set for time  *
C*                              with increment				*
C* D. Kidwell/NCEP       3/99   Fixed for Y2K                           *
C* S. Jacobs/NCEP	 4/99	Removed NEXP, replaced with LLMXTM	*
C* T. Piper/SAIC	12/01	Changed tmp* lengths from 22 to 20	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)  	dattim, timlst (*), timout, timfnd (*)
	LOGICAL		respnd
C*
	CHARACTER*20	strtim, endtim, stntim, inc
	CHARACTER*20	list (LLMXTM), lastim, firtim
	CHARACTER*20	tmplst (LLMXTM), tmpstr, tmpend
	CHARACTER	time*80, date*6
C--------------------------------------------------------------------------
	ntime = 0
C
C*	No times in list is an error.
C*	Otherwise, initialize output variables and convert input to
C*	upper case.
C
	IF  ( ntimin .le. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'TI', iret, ' ', ier )
	    RETURN
	  ELSE 
	    iret   = 0
	    timout = dattim
	END IF
	CALL ST_LCUC  ( dattim, time, ier )
C
C*	Check for 'LIST' to list all times in file.
C
	IF  ( time .eq. 'LIST' )  THEN
C
C*	    Only list time if RESPOND flag is set.
C
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  THEN
		CALL TI_DSPL  ( ntimin, timlst, timout, ier )
		IF  ( ier .eq. 1 )  THEN
		    timout = dattim
		    iret   = 1
		    RETURN
		  ELSE
		    CALL ST_LCUC  ( timout, time, ier )
		END IF
	      ELSE
		iret   = -4
		CALL ER_WMSG  ( 'TI', iret, ' ', ier )
		RETURN
	    END IF
	END IF
C
C*	Check for special case where all times are requested.
C
	IF  ( time .eq. 'ALL' )  THEN
	    DO  i = 1, ntimin
		timfnd (i) = timlst (i)
	    END DO
	    ntime = ntimin
	    RETURN
	END IF
C
C*	Check for all times on one day requested.
C
	ipos = INDEX  ( time, '/ALL' )
	IF  ( ipos .gt. 0 )  THEN
C
C*	    Get date using last time to fill in missing parts.
C
	    IF  ( time (1:ipos) .eq. 'FIRST' )  THEN
		stntim = timlst (1)
		date   = stntim (1:6)
	      ELSE
		CALL TI_STAN  ( time ( 1: ipos ), timlst (ntimin), 
     +				stntim, ier )
		IF  ( ier .ne. 0 )  THEN
		    iret = -1
		    CALL ER_WMSG  ( 'TI', iret, time, ier )
		    RETURN
		  ELSE
		    date = stntim ( 1: 6 )
		END IF
	    END IF
C
C*	    Find the times for this one day.
C
	    DO  i = 1, ntimin
		IF  ( date .eq. timlst (i) (1:6) )  THEN
		    ntime = ntime + 1
		    timfnd (ntime) = timlst (i)
		END IF
	    END DO
	    IF  ( ntime .eq. 0 )  THEN
		iret = -2
		CALL ER_WMSG  ( 'TI', iret, ' ', ier )
	    END IF
	    RETURN
	END IF
C
C*	Break user input into a list of times.
C
	CALL ST_CLST  ( time, ';', ' ', LLMXTM, list, nlist, ier )
C
C*	Exit if there are no times in list.
C
	IF  ( nlist .eq. 0 )  THEN
	    iret = -1
	    CALL ER_WMSG  ( 'TI', iret, ' ', ier )
	    RETURN
C
C*	    If there are more than two times, this must be a list.
C
	  ELSE IF  ( nlist .ge. 2 )  THEN
	    itype = 0
C
C*	    If there was one item, check to see if this is a range.
C
	  ELSE IF  ( nlist .eq. 1 )  THEN
	    CALL ST_RANG  ( time, firtim, lastim, inc, itype, ier )
	END IF
C
C*	Get the times from a list.
C
	IF  ( itype .eq. 0 )  THEN
C
C*	    Put each time in the list into a standard format.
C
	    DO  i = 1, nlist
		IF  ( list (i) .eq. 'FIRST' )  THEN
		    stntim = timlst (1)
		    ier    = 0
		  ELSE
		    CALL TI_STAN  ( list (i), timlst (ntimin), stntim, 
     +				    ier )
		    IF  ( ier .ne. 0 )  THEN
			CALL ER_WMSG  ( 'TI', ier, list (i), jer )
		    END IF
		END IF
		IF  ( ier .eq. 0 )  THEN
		    CALL ST_FIND  ( stntim, timlst, ntimin, indx, ier )
		    IF  ( indx .ne. 0 )  THEN
			ntime = ntime + 1
		    	timfnd (ntime) = timlst (indx)
		      ELSE
			CALL ER_WMSG  ( 'TI',  +2, stntim, ier )
		    END IF
		END IF
	    END DO
C
C*	    Check that at least one time was found.
C
	    IF  ( ntime .eq. 0 )  THEN
		iret = -2
		CALL ER_WMSG  ( 'TI', iret, ' ', ier )
	    END IF
C
C*	    Check for the case of a range without an increment.
C
	  ELSE IF  ( itype .eq. 1 )  THEN
C
C*	    Get the standard time for the first and last time in range.
C
	    CALL TI_STAN  ( firtim, timlst (ntimin), strtim, ier )
	    CALL TI_STAN  ( lastim, timlst (ntimin), endtim, ier )
C
C*	    Use 4-digit years to compare dates.
C
	    CALL TI_DTM4 ( strtim, tmpstr, ier )
	    CALL TI_DTM4 ( endtim, tmpend, ier )
	    DO n = 1,ntimin
	        CALL TI_DTM4 ( timlst ( n ), tmplst ( n ), ier )
	    END DO
C
C*	    Make sure that times are in proper order and in range of
C*	    data set.
C
	    IF ( ( tmpstr .gt. tmplst ( ntimin ) )  .or. 
     +		 ( tmpend .lt. tmplst ( 1 ) )  .or.
     +		 ( tmpstr .gt. tmpend ) )  THEN
		iret= -3
		CALL ER_WMSG  ( 'TI', iret, ' ', ier )
C
C*		Check each time to see if it is in range.
C
	      ELSE 
		DO  n = 1, ntimin
		    IF  ( ( tmplst ( n ) .ge. tmpstr ) .and. 
     +			  ( tmplst ( n ) .le. tmpend ) )  THEN
			ntime = ntime + 1
			timfnd ( ntime ) = timlst (n)
		    END IF
		END DO
		IF  ( ntime .eq. 0 )   THEN
		    iret= -3
		    CALL ER_WMSG  ( 'TI', iret, ' ', ier )
		END IF
	    END IF
C
C*	    Check for range with increment.
C
	  ELSE IF  ( itype .ge. 2 )  THEN
	    CALL TG_RANG ( dattim, ntimin, timlst, ntime, timfnd, iret )
	    CALL ER_WMSG ( 'TG', iret, dattim, ier )
C
C*	    Keep only those times that are in the data set.
C
	    nntim = 0
	    DO itmr = 1, ntime
	        DO itmds = 1, ntimin
	            IF ( timlst ( itmds ) .eq.
     +			 timfnd ( itmr ) ) THEN
	                nntim = nntim + 1
	                timfnd ( nntim ) = timlst ( itmds )
	            END IF
	        END DO
	    END DO
	    ntime = nntim
	    IF ( nntim .eq. 0 ) THEN
	        iret = -3
		CALL ER_WMSG  ( 'TI', iret, ' ', ier )
	        RETURN
	    END IF
	END IF
C*
	RETURN
	END		
