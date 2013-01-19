	SUBROUTINE G2T_RDZONE ( alias, lunz, nzone, iret )
C************************************************************************
C* G2T_RDZONE								*
C*									*
C* This subroutine reads the zone information and store in the common.	*
C*									*
C* G2T_RDZONE ( ALIAS, LUNZ, NZONE, IRET )				*
C*									*
C* Input parameters:							*
C*	ALIAS		CHAR*		Alias for bound type		*
C*	LUNZ		INTEGER		LUN for G2T zone table		*
C*	NZONE		INTEGER		Number of zones under alias	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				 	  0 = normal return		*
C*					-14 = can't find the zone alias	*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER*(*)   alias
C*
	CHARACTER*256	bufr, carr(10)*36
	LOGICAL		done
C------------------------------------------------------------------------
	iret = 0
C
	bndtyp = ' '
C
C*	Initialize subzone names.
C
	DO nz = 1, MAXZON
	    DO ns = 1, MXSZON
		squad ( nz, ns ) = ' '
		bndtag ( nz, ns ) = ' '
	    END DO
	END DO
C
	nzone = 0
	done = .false.
	CALL ST_LCUC ( alias, alias, ier )
	REWIND ( lunz )
	DO WHILE  ( .not. done )
	    READ   ( lunz, 1000, IOSTAT = ios ) bufr
1000	    FORMAT ( A )
	    IF  ( ios .ne. 0 )  THEN
		done = .true.
	      ELSE
		IF ( bufr ( 1:1 ) .ne. '!' )  THEN
		    CALL ST_CLST ( bufr, ' ', ' ', 10, carr, num, iret )
		    IF ( alias .eq. carr ( 1 ) )  THEN
			nzone = nzone + 1
			bndtyp = carr ( 1 ) 
			nb = 0
			DO kk = 2, num
			    nb = nb + 1
			    bndtag ( nzone, nb ) = carr ( kk )
			END DO
			nsubzn ( nzone ) = nb
		    END IF
		END IF
		numzon = nzone
	    END IF
	END DO  
C
	IF ( numzon .lt. 1 )  iret = -14
C*
	RETURN
	END
