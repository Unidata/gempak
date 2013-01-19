#define MAX_MODEL  100
#define MAX_FILENO 200 

typedef struct  {
        char model_name[50];
        char model_fullpath[150];
        int  nfile;
        char file_names[MAX_FILENO][100];
        char file_fullnames[MAX_FILENO][200];
        }       Model_str;

#ifdef MODEL

Model_str ModelStr[MAX_MODEL];
int       ModelNo;
int	  SelectFileNo=-1;
int       SelectModelNo=-1;

#else

extern Model_str ModelStr[MAX_MODEL];
extern int       ModelNo;
extern int	 SelectFileNo;
extern int       SelectModelNo;

#endif
