	SUBROUTINE MA_CGDC ( mszrpt, cgrpt, ipt, iret )
C************************************************************************
C* MA_CGDC						                *
C*								        *
C* This subroutine decodes the data in one Coast Guard report.  It	*
C* assumes that the order of the fields will be the same for each	*
C* report, but that some fields may be missing from their respective	*
C* positions from time to time.  Parameters used which are not in the   *
C* calling sequence are found in common /block6/.			*
C*			 					        *
C* MA_CGDC  ( MSZRPT, CGRPT, IPT, IRET )   			        *
C*								        *
C* Input parameters:						        *
C*      IWXVSB          INTEGER         Indicates position of wx/vis    *
C*      IWIND           INTEGER         Indicates position of wind      *
C*      IWAVE           INTEGER         Indicates position of wave data *
C*      ISEA            INTEGER         Indicates position of sst data  *
C*      IAIR            INTEGER         Indicates position of air temp. *
C*      IPRES           INTEGER         Indicates position of slp data  *
C*      IRMK            INTEGER         Indicates position of remarks   *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*      CGRPT           CHAR*           Report array                    *
C*                                                                      *
C* Input and Output parameters:						*
C*	IPT		INTEGER		Pointer to start of report.     *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*				   	  0 = Normal return 	        *
C*                                       -1 = station not found         *
C*                                        1 = err rtn from MA_CGWV      *
C*                                        2 = err rtn from MA_CGWN      *
C*                                        3 = err rtn from MA_CGWA      *
C*                                        4 = err rtn from MA_CGST      *
C*                                        5 = err rtn from MA_CGAR      *
C**								        *
C* Log:								        *
C* C. Caruso Magee/NCEP	 4/01   Modifying for Coast Guard data          *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_DCD1.	*
C*				Changed station id search to be		*
C*				independent of order.  Inserted "/"	*
C*				for stations N11 and N34.		*
C************************************************************************
        INCLUDE   	'GEMPRM.PRM'
        INCLUDE   	'BRIDGE.PRM'
        INCLUDE 	'macmn.cmn'
C* 
      	CHARACTER*(*) 	cgrpt
C*
      	CHARACTER*(DCMXBF) cgrpto, cgrpt2
	LOGICAL		again
      	INCLUDE   	'ERMISS.FNC'
C------------------------------------------------------------------------
      	iret = 0
      	ipt = 0
C
C*      Set report date/time.   
C
      	rivals(iryear) = FLOAT ( irptdt(1) )
      	rivals(irmnth) = FLOAT ( irptdt(2) )
      	rivals(irdays) = FLOAT ( irptdt(3) )
     	rivals(irhour) = FLOAT ( irptdt(4) )
      	rivals(irminu) = FLOAT ( irptdt(5) )
C
C*      Remove any unprintable (non-alphanumeric) chars from cgrpt.
C
      	CALL ST_UNPR ( cgrpt, mszrpt, cgrpto, mszrpo, iret ) 
C
C*      Remove any leading blanks from cgrpto.
C
      	IF ( cgrpto(1:1) .eq. ' ' ) THEN
            CALL ST_LDSP ( cgrpto, cgrpt2, ncout, iret )
            mszrp2 = ncout
          ELSE
      	    cgrpt2 = cgrpto
            mszrp2 = mszrpo
        END IF
C
C*      Decode station id group.
C
        CALL  MA_CGID ( cgrpt2, mszrp2, ipt, jret )
        IF ( jret .lt. 0 ) THEN
            IF ( jret .eq. -1 ) THEN
                logmsg = 'Missing station id'
                CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
              ELSE IF ( jret .eq. -2 ) THEN
                logmsg = 'Bad station id format'
                CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
            END IF
            RETURN
        END IF
C
C*      Compare stid with station table info and set lat/lon/elev.
C
        IF ( civals(icstid) .ne. '        ' ) THEN
	    again = .true.
	    i = 0
	    DO WHILE ( again )
		i = i + 1
		IF ( civals(icstid) (1:4) .eq. jstnid (i) (1:4) ) THEN
		    rivals ( irselv ) = elev ( i )
		    rivals ( irslat ) = ylat ( i )
		    rivals ( irslon ) = ylong ( i )
		    again = .false.
		END IF
		IF ( i .gt. jstn ) THEN
	            logmsg = 'Could not find '
     +                      //civals(icstid)//' in station table'
                    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
                    iret = -1
                    RETURN
		END IF
	    END DO
        END IF
C
C*	Check for stid of N11 or N34 which does not have "/" delimeters
C
	IF ( civals(icstid) (1:4) .eq. 'KN11' .or.
     +		civals(icstid) (1:4) .eq. 'KN34' ) THEN
      	    CALL MA_CGSL ( cgrpt, mszrpt, cgrpt2, mszrp2, jret )
	END IF
        IF ( iwxvsb .ne. 0 ) THEN
C
C*          Decode the weather/visibility group.
C
            CALL  MA_CGWV ( cgrpt2, mszrp2, ipt, jret )
            IF ( jret .ne. 0 ) THEN
                iret = 1
                RETURN
	    END IF
        END IF
C
        IF ( iwind .ne. 0 ) THEN
C
C*          Decode the wind group.
C
            CALL  MA_CGWN ( cgrpt2, mszrp2, ipt, jret )
            IF ( jret .ne. 0 ) THEN 
                iret = 2
                RETURN
            END IF
        END IF
C
        IF ( iwave .ne. 0 ) THEN
C
C*          Decode the wave group. 
C
            CALL  MA_CGWA ( cgrpt2, mszrp2, ipt, jret )

            IF ( jret .ne. 0 ) THEN
                iret = 3
                RETURN
            END IF
        END IF
C
        IF ( isea .ne. 0 ) THEN
C
C*          Decode the sst group. 
C
            CALL  MA_CGST ( cgrpt2, mszrp2, ipt, jret )
            IF ( jret .ne. 0 ) THEN
                iret = 4
                RETURN
            END IF
        END IF
C
        IF ( iair .ne. 0 ) THEN
C
C*          Decode the air temp. group. 
C
            CALL  MA_CGAR ( cgrpt2, mszrp2, ipt, jret )
            IF ( jret .ne. 0 ) THEN
                iret = 5
                RETURN
            END IF
        END IF
C
        IF ( ipres .ne. 0 .or. irmk .ne. 0 ) THEN
C
C*          Decode the pressure, remarks, and station name groups. 
C
            CALL  MA_CGPR ( cgrpt2, mszrp2, ipt, jret )
        END IF
C
C*      Check to see if ircorn is missing.  If so (means it wasn't set
C*	by either bulletin header or within report Remarks), set it to
C*	0 (indicates report is not a correction).
C
        IF ( ERMISS ( rivals ( ircorn ) ) )  rivals ( ircorn ) = 0.
C*
        RETURN
        END
