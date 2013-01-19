	SUBROUTINE SNECPT ( lunedt, ptnam, ihhmm, nparm, kntflg, ddpflg,
     +                       zwind, iret )
C************************************************************************
C* SNECPT                                                               *
C*                                                                      *
C* This routine gets the part name, time, significant wind flag and     *
C* number of part parameters and checks the validity of the part        *
C* header.  								*
C*									*
C* SNECPT ( LUNEDT, PTNAM, IHHMM, NPARM, KNTFLG, DDPFLG, ZWIND, IRET )  *
C*									*
C* Input Parameters:							*
C*	LUNEDT		INTEGER		LUN for edit file		*
C*									*
C* Output Parameters:							*
C*	PTNAM		CHAR*		Part name			*
C*	IHHMM		INTEGER         Part time HHMM format		*
C* 	NPARM		INTEGER		# of part parameters		*
C*	KNTFLG		LOGICAL		Flag for SKNT parameter		*
C*      DDPFLG		LOGICAL		Flag for DDPC parameter		*
C*	ZWIND		LOGICAL		Flag for sig wind in Z coord	*
C* 	IRET		INTEGER		Return code			*
C* 					  0 = normal			*
C*					  2 = new station		*
C*					  3 = End of file		*
C* 					 -1 = invalid part parameters   *
C**									*
C* Log:									*
C* S. Schotz/GSC	11/89						*
C* D. Kidwell/NCEP	 2/01	Added more parts                        *
C************************************************************************
	INCLUDE 'GEMPRM.PRM'
C*
	CHARACTER 	ptnam*(*)
	LOGICAL		kntflg, ddpflg, zwind
C*
	PARAMETER       ( MXPARM = 12 )
	CHARACTER	record*132, parts(MXPARM)*4, timstr*4
	LOGICAL  	done, found
C*
        DATA 		parts / 'TTAA', 'TTBB', 'PPBB', 'TTCC', 'TTDD',
     +			        'PPDD', 'PPAA', 'PPCC', 'TRPA', 'TRPC',
     +				'MXWA', 'MXWC' /
C------------------------------------------------------------------------
	done = .false.
	iret = 0
C
C*	Loop through edit file records, look for valid part name
C
	DO WHILE ( .not. done ) 
	    READ ( lunedt, 10, IOSTAT = iostat ) record
10	    FORMAT (A)
	    IF ( iostat .eq. 0 ) THEN
   	       CALL ST_LCUC ( record, record, ier )
	       CALL ST_LDSP ( record, record, nchar, ier )
               IF ( nchar .ne. 0 ) THEN
C
C*	          Check for new station
C
	          ip = INDEX ( record, '=' ) 
                  IF ( ip .ne. 0 ) THEN
C
C*	             New station
C
		     done = .true.
		     iret = 2
                     RETURN
                  END IF
C
C*                Determine whether record is part name header
C
		  CALL ST_FIND ( record(1:4), parts, MXPARM, ipos, ier )
		  IF ( ipos .ne. 0 ) THEN
C
C*                    Part found, decode part time
C
		      ptnam = record ( 1:4 )
		      CALL ST_LDSP ( record(5:), timstr, nchar, ier )
		      IF ( nchar .gt. 0 ) THEN
                          CALL ST_LSTR ( timstr, nchar, ier )
                          CALL ST_INTG ( timstr ( :nchar ), ihhmm, ier )
			  IF ( ier .ne. 0) ihhmm = IMISSD
		        ELSE
		          ihhmm = IMISSD
		      END IF
C
C*                    Find first non-blank record to check paramter list
C
                      found = .false.
		      DO WHILE ( .not. found )
                         READ ( lunedt, 10, IOSTAT = iostat ) record
		         IF ( iostat .eq. 0) THEN
	                    CALL ST_LDSP ( record, record, nchar, ier )
                            IF ( nchar .ne. 0 ) THEN
                               found = .true.
C
C*                             Check validity of parameter list, get
C*                             parameter flags
C
                               CALL SNECPR ( ptnam, record, nparm, 
     +                                     zwind, kntflg, ddpflg, iret )
                            END IF
		          ELSE IF ( iostat .eq. -1 ) THEN
			    found = .true.
                            iret = -1
                         END IF
                      END DO
                      done = .true.
                  END IF
               END IF
C 
             ELSE IF ( iostat .eq. -1 ) THEN
C
C*             End of file
C
               iret = 3
	       done =  .true.
	    END IF
C
	END DO
C*
	RETURN
	END
