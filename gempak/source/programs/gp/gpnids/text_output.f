	SUBROUTINE OUTPUT_LUNS (nlun, luns)
	INTEGER		nlun, luns(4)

	INTEGER		icnlun, icluns(4)
	COMMON	/ OUTCMN / icnlun, icluns

	DO i=1, nlun
	   icluns(i) = luns(i)
	END DO
	icnlun = nlun

	RETURN
	END



	SUBROUTINE TEXT_OUTPUT (outstr)
	CHARACTER*(*)	outstr

	INTEGER		nlun, luns(4)
	COMMON / OUTCMN / nlun, luns

C       CALL ST_LSTR ( outstr, lens, ier )
C	CALL ST_UNPR ( outstr, lens, outstr, lenout, ier )
	DO i=1,nlun
	   write(luns(i),*) outstr
	END DO

	RETURN
	END
