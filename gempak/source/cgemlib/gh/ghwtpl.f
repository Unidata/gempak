	SUBROUTINE GH_WTPL ( ipnum, iblk, iret)
C************************************************************************
C* GH_WTPL								*
C*									*
C* This subroutine plots the wind forecast probability values for	*
C* the speed intensity table.						*
C*									*
C* GH_WTPL (IPNUM, IBLK, IRET) 						*
C*									*
C* Input parameters:							*
C*      IPNUM (5,*)	INTEGER         Array of wind speed probs.	*
C*      IBLK		INTEGER         Device color of black		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Created					*
C* A. Hardy/SAIC	 8/01   Increased font size from 1.2 -> 2.      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INTEGER		ipnum(5,*)
C*
        CHARACTER       calcnm*4, storm*4, cat*4 
C*
C------------------------------------------------------------------------
        iret = 0
C
        CALL GSCOLR ( iblk, iret )
	CALL GSTEXT ( 2, 2, 2., 1, 111, 1, 2, ier )
C
C*      Place the values for storm type.
C
        DO jj = 1,4
            ypos = .36
            IF ( jj .eq. 1 ) THEN
                xpos = .145
              ELSE IF ( jj .eq. 2 ) THEN
                xpos = .282
              ELSE IF ( jj .eq. 3 ) THEN
                xpos = .415
              ELSE IF ( jj .eq. 4 ) THEN
                xpos = .537
            END IF
	    DO ii = 1,5
                IF ( (ipnum (ii,jj) .ge. 2 )  .and.
     +                (ipnum (ii,jj) .le. 98 ) ) THEN
                    CALL ST_INCH (ipnum (ii,jj), calcnm, ier) 
                    CALL ST_LSTR ( calcnm, lens, ier )
                    storm = calcnm (:lens) // '%'
                  ELSE IF ( (ipnum (ii,jj) ) .eq. -1) THEN
                    storm =' NA'
                  ELSE IF ( ( (ipnum (ii,jj) ) .eq. 1 ) .or.
     +                        (ipnum (ii,jj) ) .eq. 0 )  THEN
                    storm =' <2%'
                  ELSE IF ( (ipnum (ii,jj) ) .ge. 99) THEN
                    storm ='>98%'
                  ELSE IF ( (ipnum (ii,jj) ) .lt. -1) THEN
                    storm =' NA'
                END IF
                CALL ST_LSTR ( storm , lens, ier )
                CALL GTEXT  ( 'N',xpos, ypos , storm (:lens), 
     +                                            0.0, 0, 0, ier)
                ypos = ypos - .08
            END DO
        END DO
C
C*      Place the values for catagories.
C
        DO jj = 5,8
            ypos = .36
            IF ( jj .eq. 5 ) THEN
                xpos = .65
              ELSE IF ( jj .eq. 6 ) THEN
                xpos = .75
              ELSE IF ( jj .eq. 7 ) THEN
                xpos = .85
              ELSE IF ( jj .eq. 8 ) THEN
                xpos = .95
            END IF
	    DO ii = 1,5
                IF ( (ipnum (ii,jj) .ge. 2 )  .and.
     +                (ipnum (ii,jj) .le. 98 ) ) THEN
                    CALL ST_INCH (ipnum (ii,jj), calcnm, ier) 
                    CALL ST_LSTR ( calcnm, lens, ier )
                    cat = calcnm (:lens) // '%'
                  ELSE IF ( (ipnum (ii,jj) ) .eq. -1) THEN
                    cat =' NA'
                  ELSE IF ( ( (ipnum (ii,jj) ) .eq. 1 ) .or.
     +                        (ipnum (ii,jj) ) .eq. 0 )  THEN
                    cat =' <2%'
                  ELSE IF ( (ipnum (ii,jj) ) .ge. 99) THEN
                    cat ='>98%'
                  ELSE IF ( (ipnum (ii,jj) ) .lt. -1) THEN
                    cat =' NA'
                END IF
                CALL ST_LSTR ( cat , lens, ier )
                CALL GTEXT  ( 'N',xpos, ypos , cat (:lens), 
     +                                            0.0, 0, 0, ier)
                ypos = ypos - .08
            END DO
        END DO
C*
        RETURN
	END
