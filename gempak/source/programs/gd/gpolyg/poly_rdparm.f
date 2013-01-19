	SUBROUTINE POLY_RDPARM ( iret )
C************************************************************************
C* POLY_RDPARM								*
C*									*
C* This subroutine reads user-defined arameters from a table for	*
C* warning polygons creation.						*
C*									*
C* G2T_RDPARM ( IRET )							*
C*									*
C* Input parameters:							*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-1 = cannot open parm table	*
C*					-7 = too many points on bounds	*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/08	Created					*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	CHARACTER	bufr*80, polypm*15, carr(2)*20, ccc*10
	INTEGER		indx(15)
	LOGICAL		done
	DATA		polypm /'poly_parm.tbl'/
C*-----------------------------------------------------------------------
	iret = 0
C
	IF ( .not. tbread )  THEN
	    tbread = .true.
	    CALL FL_TBOP ( polypm, 'gpolyg', lunp, ier )
	    IF ( ier .ne. 0 )  THEN
		iret = -1
		RETURN
	    END IF
C
	    done = .false.
	    DO WHILE  ( .not. done )
		READ   ( lunp, 1000, iostat = ier ) bufr
1000		FORMAT ( A )
2000		FORMAT ( 8F9.3 )
		IF  ( ier .ne. 0 )  THEN
		    done = .true.
	          ELSE
		    CALL ST_LCUC ( bufr, bufr, ier )
		    CALL ST_LCUC ( cdxzon, ccc, ier ) 
		    CALL ST_LSTR ( ccc, nccc, ier )
		    IF ( bufr ( 1:1 ) .ne. '!' )  THEN
			indx ( 1 ) = INDEX ( bufr, 'DISTNM' )
			indx ( 2 ) = INDEX ( bufr, 'REDPCT' )
			indx ( 3 ) = INDEX ( bufr, 'SKIPCT' )
			indx ( 4 ) = INDEX ( bufr, 'REDIST' )
			indx ( 5 ) = INDEX ( bufr, 'MAXCLP' )
			indx ( 6 ) = INDEX ( bufr, 'WHURR' )
			indx ( 7 ) = INDEX ( bufr, 'WSTORM' )
			indx ( 8 ) = INDEX ( bufr, 'WGALE' )
			indx ( 9 ) = INDEX ( bufr, 'WSCA' )
			indx ( 10 ) = INDEX ( bufr, 'RAW_ON' )
			indx ( 11 ) = INDEX ( bufr, 'MAP_ON' )
C
			IF ( indx ( 1 ) .gt. 0 )  THEN  
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), distnm, ier )
C
		          ELSE IF ( indx ( 2 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), redpct, ier )
C
		          ELSE IF ( indx ( 3 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), skipct, ier )
C
		          ELSE IF ( indx ( 4 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), redist, ier )
C
		          ELSE IF ( indx ( 5 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_NUMB ( carr ( 2 ), maxclp, ier )
C
		          ELSE IF ( indx ( 6 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), whurr, ier )
C
		          ELSE IF ( indx ( 7 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), wstrm, ier )
C
		          ELSE IF ( indx ( 8 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), wgale, ier )
C
		          ELSE IF ( indx ( 9 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_CRNM ( carr ( 2 ), wsca, ier )
C
		          ELSE IF ( indx ( 10 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    IF ( INDEX ( carr ( 2 ), 'T' ) .ne. 0 ) THEN
				raw_on = .true.
			      ELSE
				raw_on = .false.
			    END IF
C
		          ELSE IF ( indx ( 11 ) .gt. 0 )  THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    IF ( INDEX ( carr ( 2 ), 'T' ) .ne. 0 ) THEN
				map_on = .true.
			      ELSE
				map_on = .false.
			    END IF
C
		          ELSE IF ( INDEX ( bufr, ccc ( : nccc ) ) 
     +				     .ne. 0 ) THEN
			    CALL ST_CLST ( bufr, ' ', ' ', 2, carr, 
     +					   num, ier)
			    CALL ST_NUMB ( carr ( 2 ), nop, ier )
			    npzon = nop
			    nol = nop / 4
			    IF ( MOD ( nop, 4 ) .ne. 0 ) nol = nol + 1
			    DO nl = 1, nol
			        is = ( nl - 1 ) * 4 + 1
			        READ ( lunp, 2000, iostat = ier ) 
     +			        (zzlat (kk), zzlon (kk), kk =is,is + 3)
			        IF ( kk .ge. MXBPTS )  THEN
				    iret = -7
			            RETURN
			        END IF
			    END DO
		        END IF
		    END IF
	        END IF
	    END DO
	END IF
C*
	RETURN
	END
