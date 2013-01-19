struct ARACOM {
 int fd;	/* file number from open call */
 char *area_name;	/* file name with optional path */
 int dir[64];		/* the directory */
 int data_loc;		/* starting byte of data, zero based */
 int line_len;		/* length of data portion of line */
 int pref_len;		/* length of prefix portion of line */
 int elem_len;		/* length of an element = number of bands * bytes per band*/
 int slot;
 int cin;
 int cout;
 int options[5];	/* List of options */
 int flip;	/* set to 0 for no flip, 1 for flip, -1 for invalid file */
};

#define N_AREAS 6

#ifdef OPEN
 int new_area = -1;
 struct ARACOM aracom[N_AREAS];
#else
 extern int new_area;
 extern struct ARACOM aracom[N_AREAS];
#endif
