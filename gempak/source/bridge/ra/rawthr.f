	SUBROUTINE RA_WTHR  ( ipvaft, iptbef, wcod, wnum, iret )
C************************************************************************
C* RA_WTHR								*
C*									*
C* This subroutine get the weather information from an airways report.	*
C*									*
C* RA_WTHR  ( IPVAFT, IPTBEF, WCOD, WNUM, IRET )			*
C*									*
C* Input parameters:							*
C*	IPVAFT		INTEGER		Pointer to field after vis	*
C*	IPTBEF		INTEGER		Pointer to field before temp	*
C*									*
C* Output parameters:							*
C*	WCOD		CHAR*		GEMPAK char weather code	*
C*	WNUM		REAL		GEMPAK numeric weather code	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/88						*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*
	CHARACTER*(*)	wcod
C*
	CHARACTER*8	PT_WCOD
C------------------------------------------------------------------------
	wcod = ' '
C
C*	Get all the fields and put them together.
C
	lenw = 0
	DO  ii = ipvaft, iptbef
C
C*	    Get all character fields.  Eliminate some things that
C*	    we see with Mexican stations.
C
	    IF  ( ( iftype (ii) .eq. 1 ) .and. ( cfield (ii) .ne. 'MAS' )
     +		    .and. ( cfield (ii) .ne. 'NUB' ) 
     +		    .and. ( cfield (ii) (1:2) .ne. 'CD' ) )  THEN
		CALL ST_LSTR  ( cfield (ii), lenp, ier )
		IF  ( lenw .eq. 0 )  THEN
		    wcod = cfield (ii)
		    lenw = lenp
		  ELSE
		    wcod = wcod ( : lenw ) // cfield (ii)
		    lenw = lenw + lenp
		END IF
	    END IF
	END DO
C
C*	Convert the characters to a number and reconvert to get an
C*	actual weather code.
C
	wnum = PT_WNUM ( wcod )
	wcod = PT_WCOD ( wnum )
C*
	iret = 0
C*
	RETURN
	END
