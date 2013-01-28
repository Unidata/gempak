/* McIdas AIX area directory */
struct Directory {
long exist;	/* zero if exists */
long type;	/* type 4 - new format */
long sat_id;	/* satellite id */
long yyddd;	/* year & day of year */
long time;	/* hhmmss */
long lcor;	/* top most line in area */
long ecor;	/* left most element in area */
long zcor;	/* z coordinate - not used */
long nlin;	/* number of lines */
long nele;	/* number of elements per line 		w10*/
long data_bytes;/* number of bytes per data element (per band per pixel */
long lres;	/* line resolution 1= one for one >1 reduction (every lres pix)*/
long eres;	/* element resolution */
long nbands;	/* number of bands per element */
long line_pref;	/* bytes in line prefix (mult of 4) */
long proj_no;	/* project number */
long creat_date;/*create date */
long creat_time;/*create time */
long chan_map;	/* channel map for multiband right most bit is band 1 1=band present */
long ident[5];	/* identification */
long memo[8];
long primary_key;	/* unit number, -1 if not used */
long data_loc;	/* location of data in file (0 based) */
long nav_loc;	/* location of navigation codicil in file (zero based byte) w35*/
long validity_code;	/* validity code, must match line prefix, 0 - no code */
long pdl[8];	/* for Mode AA */
long xband8;	/* for Mode AA */
long actual_start_day;
long actual_start_time;
long actual_start_scan;
long len_doc;	/* length of documentation section in prefix mult of 4 bytes*/
long len_cal;	/* length of calibration section in prefix mult of 4 bytes*/
long len_lev;	/* length of level section in prefix mult of 4 bytes*/
long source_type;	/* source type (VISR VAS AVHR ...) w 52 */
long calib_type;	/* calibration type (RAW BRIT TEMP RAD) */
long internal_use[9];
long cal_loc;	/* location of calibration block */
long Num_comment_blok;	/* number of 80 byte comment blocks */
};

typedef struct Directory *Direct_ptr;

#ifdef OPEN
Direct_ptr directptr;
#else
extern Direct_ptr directptr;
#endif
