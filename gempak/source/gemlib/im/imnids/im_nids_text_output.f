	BLOCK DATA IM_NIDS_OUTPUT_INIT

	INTEGER		nlun, luns(4)
	COMMON / OUTCMN / nlun, luns

	DATA	nlun /1/
	DATA	luns /6, 3*0/

	END



	SUBROUTINE IM_NIDS_OUTPUT_LUNS (inlun, iluns)
	INTEGER		inlun, iluns(*)

	INTEGER		nlun, luns(4)
	COMMON	/ OUTCMN / nlun, luns

	DO i=1, inlun
	   if ( i .le. 4 ) luns(i) = iluns(i)
	END DO
	nlun = inlun

	RETURN
	END



	SUBROUTINE IM_NIDS_TEXT_OUTPUT (outstr)
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

