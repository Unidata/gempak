	SUBROUTINE OAGSTN  ( iflno, source, rlevl, ivert, slat, slon, 
     +			     number, iret )
C************************************************************************
C* OAGSTN								*
C*									*
C* This subroutine reads in the latitudes and longitudes of stations	*
C* reporting data in a given area.					*
C*									*
C* OAGSTN  ( IFLNO, SOURCE, RLEVL, IVERT, SLAT, SLON, NUMBER, IRET )	*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	SOURCE		CHAR*		Source ( SN or SF )		*
C*	RLEVL		REAL		Vertical level	 		*
C*	IVERT		INTEGER		Vertical coordinates		*
C*	SLAT (NUMBER)	REAL		Latitudes 			*
C*	SLON (NUMBER)	REAL		Longitudes			*
C*	NUMBER		INTEGER		Number of stations		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	11/89	Changes to station time			*
C* A. Hardy/GSC		 3/99   Added priority parameter to PC_SSTN     *
C* A. Hardy/GSC		 3/99   Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99   Removed ispri = 0			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	source
	REAL		slat (*), slon (*)
	LOGICAL		more
	CHARACTER	cchar*4, chdata*8
	LOGICAL		bounds
	REAL		datarr (LLMXDT)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	more   = .true.
	number = 0
	IF  ( source .eq. 'SF' )  THEN
	    CALL SF_BEGS  ( iflno, ier )
	  ELSE
	    CALL SN_BEGS  ( iflno, ier )
	END IF
C
C*	Loop through each station.
C
	DO WHILE  ( more )
	    IF  ( source .eq. 'SN' )  THEN
		CALL SN_SNXT  ( iflno, cchar, id, rlat, rlon, re, ier )
		IF  ( ier .ne. 0 )  more = .false.
	      ELSE
		CALL SF_SNXT  ( iflno, cchar, id, rlat, rlon, re, ispri,
     +				ier )
		IF  ( ier .ne. 0 )  more = .false.
	    END IF
C*
	    IF  ( more )  THEN
	        CALL LC_INBN  ( rlat, rlon, bounds, ier )
	      ELSE
	        bounds = .false.
	    END IF
C*
	    IF  ( bounds )  THEN
		IF  ( source .eq. 'SN' )  THEN
		    CALL SN_RDAT  ( iflno, numlev, datarr, ihhmm, ier )
		  ELSE
		    CALL SF_RDAT  ( iflno, datarr, ihhmm, ier )
		    numlev = 1
		END IF
		IF  ( ier .ne. 0 )  bounds = .false.
	    END IF
C*
	    IF  ( bounds )  THEN
		ispri  = 0
		CALL PC_SSTN ( cchar, id, rlat, rlon, re, ispri, ihhmm, 
     +				numlev, ier )
		CALL PC_CMVR  ( rlevl, ivert, datarr, data, chdata, 
     +				ier )
		IF  ( ( .not. ERMISS ( data ) ) .and.
     +		      ( number .lt. LLSTFL ) )  THEN
		    number = number + 1
		    slat ( number ) = rlat
		    slon ( number ) = rlon
		END IF
	    END IF
	END DO
C*
	RETURN
	END
