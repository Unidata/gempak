	SUBROUTINE SFC_GTIM  ( isffln, maxtim, ntime, timlst, iret )
C************************************************************************
C* SFC_GTIM 								*
C*									*
C* This subroutine returns a list of times available in a surface 	*
C* data file.  The times are ordered from the earliest to the latest.	*
C*									*
C* SFC_GTIM  ( ISFFLN, MAXTIM, NTIME, TIMLST, IRET )			*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER	 	Surface file number		*
C*	MAXTIM		INTEGER	 	Maximum number of times		*
C*									*
C* Output parameters:							*
C*	NTIME		INTEGER	 	Number of times returned	*
C*	TIMLST        	CHAR*		GEMPAK times			*
C*	IRET		INTEGER	 	Return code			*
C*				    	   0 = normal return		*
C*				   	  -3 = file not open		*
C*				  	 -14 = too many times in file	*
C**									*
C* Log:									*
C* I.Graffman/RDS 	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	CHARACTER*(*)	timlst
	CHARACTER*20 	times (48)
C------------------------------------------------------------------------
C
	timlst = ' '
C
	CALL SF_GTIM  ( isffln, maxtim, ntime, times, iret )
C
	IF ( ntime .ge. 1 )  THEN
	    CALL ST_LSTR( times(1), lens, ier )
	    timlst = times(1)(1:lens) // ';'
	    DO  i = 2, ntime
	        CALL ST_LSTR( timlst, lent, ier )
	        CALL ST_LSTR( times(1), lens, ier )
	        timlst = timlst(1:lent) // times(i)(1:lens) // ';'
	    END DO
	END IF
C
	CALL ST_NULL( timlst, timlst, lens, ier )
C*
	RETURN
	END
