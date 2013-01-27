	SUBROUTINE	GGSLS ( svrl, iret )


	CHARACTER*(*)	svrl
	INTEGER		iret
C*
	PARAMETER       ( NS = 2 )
	CHARACTER	warr(4)*72
	INTEGER		iwclr(NS), mrktyp(NS), iwidth(NS), iflag(NS)
	REAL		ssize(NS)



	CALL ST_CLST ( svrl, '|', ' ', 4, warr, numw, ier )
        CALL ST_ILST ( warr(2), ';', -1, NS, iwclr, numc, ier )

        DO ii = 1, NS
            IF ( iwclr ( ii ) .lt. 0 )
     +           iwclr ( ii ) = 1
        END DO
        CALL ST_LCUC ( warr(3), warr(3), ier )
        CALL ST_LCUC ( warr(4), warr(4), ier )

        IF  ( warr(3)(1:1) .eq. 'Y' )  THEN
            iflag(1) = 1
        ELSE
            iflag(1) = 0
        END IF

        IF  ( warr(4)(1:1) .eq. 'Y' )  THEN
            iflag(2) = 1
        ELSE
            iflag(2) = 0
        END IF

        DO  i = 1, NS
            mrktyp(i) = 0
            ssize(i)  = 0.0
            iwidth(i) = 0
        END DO

C
        CALL GGWSLS ( 'SVRL', warr(1), NS, iwclr,
     +      mrktyp, ssize, iwidth, iflag, iret )

	RETURN
	END
