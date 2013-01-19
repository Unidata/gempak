        SUBROUTINE GG_ALIS ( lun, numc, lclr, ldum, type, iret )
C************************************************************************
C* GG_ALIS                                                              *
C*                                                                      *
C* This subroutine gets the colors and types for one data alias in the  *
C* miscellaneous data type table.  The file should be positioned at the *
C* record following an 'ALIAS' record for this routine to work properly.*
C*                                                                      *
C* GG_ALIS ( LUN, NUMC, LCLR, LDUM, TYPE, IRET )                        *
C*                                                                      *
C* Input parameters:                                                    *
C*	LUN		INTEGER		Logical unit number for table   *
C*      NUMC            INTEGER		Maximum number of colors/types  *
C*                                                                      *
C* Output parameters:                                                   *
C*	LCLR(*)		INTEGER		Color numbers for types         *
C*	LDUM(*)		INTEGER		Second Color numbers currently	*
C*					valid for airmets and QSCT only	*
C*	TYPE(*)		CHAR*		Data type names                 *
C*      IRET            INTEGER         Return code                     *
C*                                         0 = normal return            *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* D. Kidwell/NCEP      6/00                                            *
C* m.gamazaychikov/SAIC	01/04	Added LDUM to output parameters list	*
C* M. Li/SAIC		10/04	Added QSCT for ldum in the prolog	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   type(*)
	INTEGER		lclr(*), ldum(*)
C*
        CHARACTER       buffer*80, carr (3)*8 
	INTEGER		ld (2)
	LOGICAL		done
C------------------------------------------------------------------------
C
C*  Initialize outputs
C
	DO ii = 1, numc 
	    lclr(ii) = 0
	    ldum(ii) = 0
	    type(ii) = ' '
	ENDDO
	iret = 0
C
2       FORMAT ( A )
C
	numt   = 0
        iostat = 0
	done   = .false.
C
C*      Get the data types and colors for one alias.
C
	DO WHILE ( ( iostat .eq. 0) .and. ( .not. done ) )
	    READ ( lun, 2, IOSTAT = iostat ) buffer
	    IF ( ( iostat .ne. 0 ) .or. ( buffer ( :5 ) .eq. 'ALIAS' ) ) 
     +		   THEN
		done = .true.
		CALL FL_BKSP ( lun, ier )
	      ELSE
		IF ( buffer ( :4 ) .eq. 'TYPE' ) THEN
		    numt = numt + 1
		    type( numt ) = buffer ( 5:24 )
		    CALL ST_LDSP ( type(numt), type(numt), nc, ier )
		    CALL ST_CLST ( buffer ( 25: ), '/', ' ', 3,
     +				   carr, num, ier )
		    IF ( num .ge. 2 ) THEN

                       CALL ST_ILST ( carr ( 2 ), ';', IMISSD, 2, 
     +                                ld, nnn, ier )
                       lclr (numt) = ld (1)
                       ldum (numt) = ld (2)

		      ELSE
			ier = -2
		    END IF
		    IF ( ier .lt. 0 ) lclr ( numt ) = 1
		END IF
	    END IF
	    IF ( numt .eq. numc )  done = .true.
	END DO
C*
	RETURN
	END
