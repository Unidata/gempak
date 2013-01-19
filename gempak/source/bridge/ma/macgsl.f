       SUBROUTINE MA_CGSL ( cgrpt, mszrpt, cgrpt2, mszrp2, iret )
C************************************************************************
C* MA_CGSL                                                              *
C*                                                                      *
C* This subroutine inserts "/" delimeters in a Coast Guard Report that	*
C* does not have any.							*
C*                                                                      *
C* MA_CGSL  ( CGRPT, MSZRPT, CGRPT2, MSZRP2, IRET )                     *
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report array                    *
C*      MSZRPT          INTEGER         Length of mszrpt in bytes       *
C*                                                                      *
C* Output parameters:                                                   *
C*      CGRPT2          CHAR*           Report array with "/" delimeter *
C*      MSZRP2          INTEGER         Length of cgrpt2 in bytes       *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C**                                                                     *
C* Log:                                                                 *
C* F. J. Yen/NCEP	 4/01	Created.				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
 	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	cgrpt, cgrpt2
C*
	LOGICAL		done, setsl
	INTEGER		ipossl(6)
	EQUIVALENCE	( ipossl(1), iwind )
C------------------------------------------------------------------------
	iret = 0
	done = .false.
	i = 1
	DO WHILE ( .not. done .and. i .lt. mszrpt )  
	    IF ( cgrpt(i:i) .ne. CHLF .and. cgrpt(i:i) .ne. CHCR ) THEN
		mszrp2 = mszrpt - i + 1
		cgrpt2(1:mszrp2) = cgrpt(i:mszrpt)
		done = .true.
	    END IF
	    i = i + 1
	END DO
	IF ( .not. done ) THEN
	    cgrpt2 = ' '
	    mszrpt = 0
	    RETURN
	END IF
	ipend = index ( cgrpt2 (1:mszrp2), CHLF )
	IF ( ipend .eq. 0 ) ipend = mszrp2
	IF ( ipossl(6) .ne. 0 ) THEN
	    ipend = MIN(ipossl(6), ipend)
	  ELSE IF ( ipossl(5) .ne. 0 ) THEN
	    ipend = MIN(ipossl(5), ipend)
	END IF
	isl = index ( cgrpt2 (1:ipend), '/' )
	IF ( isl .ne. 0 ) RETURN
	DO k = 1, 6
	    IF ( ipossl(k) .ne. 0 .and. ipossl(k) .le. ipend ) THEN
		setsl = .false.
		jst = ipossl(k)
		DO WHILE ( .not. setsl .and. jst .ge. ipossl(k) - 3)
		    IF ( cgrpt2 (jst:jst) .eq. ' ' ) THEN
			cgrpt2 (jst:jst) = '/'
			setsl = .true.
		    END IF
		    jst = jst - 1
		END DO
	    END IF
	END DO
C*
	RETURN
	END
