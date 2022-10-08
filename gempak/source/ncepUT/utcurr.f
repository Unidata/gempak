        SUBROUTINE  UT_CURR ( istrt, iends3, report, iret )
C************************************************************************
C* UT_CURR                                                              *
C*                                                                      *
C* This subroutine decodes the groups zzzz, dd, and ccc which follow    *
C* the 66k(6)k(4)k(3) group in section 3 of a tesac report or the       *
C* 66k(6)9k(3) group in section 3 of a buoy report.  These groups       *
C* contain current direction (dd) and current speed (ccc) at            *
C* selected or significant depths (zzzz).                               *
C*                                                                      *
C* UT_CURR ( ISTRT, IENDS3, REPORT, IRET )                              *
C*                                                                      *
C* Input parameters:                                                    *
C*      REPORT 		CHAR*    	Report array                    *
C*      ISTRT           INTEGER         Points to where to start search *
C*                                      in report array for section 3   *
C*                                      data groups                     *
C*      IENDS3          INTEGER         Points to where to end search in*
C*                                      report array for sect 3 groups  *
C*					                                *
C* Output parameters:                                                   *
C*	RNDDC         	REAL		Number of levels of data        *
C*	RDBSC    	REAL		Depth in meters (for current)   *
C*	RDROC     	REAL		Direction of current (tens of   *
C*	                                degrees)                        *
C*	RSPOC    	REAL		Speed of current (cm per sec)   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = No problems               *
C*                                        1 = Problems                  *
C*                                                                      *
C**								        *
C* Log:							                *
C* C. Caruso Magee/NCEP 03/02  New subroutine.                          *
C* C. Caruso Magee/NCEP 11/02  Rename from BT_CURR to UT_CURR.          *
C************************************************************************
        INCLUDE 	'GEMPRM.PRM'
        INCLUDE  	'CURR.CMN'
C*
        CHARACTER*(*)  	 report
C*
        CHARACTER     	 fld4*4, fld2*2, fld3*3
C------------------------------------------------------------------------
        iret = 0
        ipt = istrt -1
C
        lvl = 0
C
        DO WHILE ( ipt .lt. iends3 )
C
            ipt = ipt + 1
C
            IF ( report ( ipt:ipt+1 ) .eq. ' 2' ) THEN
C
                lvl = lvl + 1
		IF ( lvl .gt. MXCLYR ) lvl = MXCLYR
C
C*              Get selected and/or significant depth, in meters.
C
                ipt = ipt + 2
                fld4 = report ( ipt:ipt+3 )
                CALL  ST_INTG ( fld4, ival, ier )
                IF ( ier .eq. 0 ) 
     +               rdbsc ( lvl ) = FLOAT ( ival )
                ipt = ipt + 5
C
C*              Get current direction, in tens of degrees.               
C*              Store into CURR.CMN array.
C
                fld2 = report ( ipt:ipt+1 )
                CALL  ST_INTG ( fld2, ival, ier )
                IF ( ier .eq. 0 ) THEN
                   rdroc ( lvl )  = FLOAT ( ival )
                END IF
                ipt = ipt + 2
C
C*              Get current speed, in centimeters per second.          
C*              Store into CURR.CMN array.     
C
                fld3 = report ( ipt:ipt+2 )
                CALL  ST_INTG ( fld3, ival, ier )
                IF ( ier .eq. 0 ) THEN
                   rspoc ( lvl ) = FLOAT ( ival ) 
                END IF
                ipt = ipt + 2
            END IF
        END DO
C
	IF ( lvl .gt. 0 ) rnddc = FLOAT ( lvl )
C*
        RETURN
        END
