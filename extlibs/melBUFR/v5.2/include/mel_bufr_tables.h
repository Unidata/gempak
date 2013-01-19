#ifndef _MEL_BUFR_TABLES_H
#define _MEL_BUFR_TABLES_H

/*********************************************************************/
/* Code Table 0 (Identification of Center) -- From GRIB Code Table 0 */
/*********************************************************************/

#define MAX_TABLE_0_ENTRIES 256

#define MAX_LOCAL_VERSION_NUMBER 255
#define MAX_MINOR_VERSION_NUMBER 255

typedef struct
{
    int   UsesLocalMinorVersion;
    char* name;
} Table0_t;


/********************************/
/* Code Table A (Data Category) */
/********************************/

#define MAX_TABLE_A_ENTRIES 256

typedef char* TableA_t;


/********************************************************/
/* Code Table B (Classification of Elements) descriptor */
/********************************************************/

typedef struct
{
    FXY_t   fxy_value;
    Units_t units_type;
    char*   units;
    char*   description;

    int data_width;
    int scale;
    short local_flag;

    /*
     * Stack of reference values to allow changes with 2-03-YYY.
     * The tail->val contains the default reference value.
     */

    ValStack_t RefValStack;

} Descriptor_t;

/*
 * Table B is a linked-list implementation of an array of descriptors.
 * Because so few Table B descriptors are actually declared (typically
 * less than 1% of the number of available descriptors) a linked-list
 * is used instead of a memory-wasting fixed-length array.
 */

typedef struct table_b_entry
{
    Descriptor_t*         item;
    struct table_b_entry* next;

} TableB_Entry_t;

typedef struct
{
    TableB_Entry_t* head;
    TableB_Entry_t* tail;

} TableB_t;


/************************************/
/* Code Table D Descriptor Sequence */
/************************************/

typedef struct table_d_entry
{
    FXY_t                 fxy_value;    /* Used for expansion and debugging */
    Descriptor_t*         item;         /* Pointer to Table B entry */
    struct table_d_entry* next;

} TableD_Entry_t;

typedef struct table_d_sequence
{
    TableD_Entry_t*          head;          /* 3-XX-YYY stored within  */
    TableD_Entry_t*          tail;          /* 3-XX-YYY stored as well */
    struct table_d_sequence* next;
    int                      num_entries;   /* # of TableD_Entry_t's   */

} TableD_Sequence_t;

/*
 * Table D is a linked-list implementation of an array (of lists of
 * descriptors) for the same reason as Table B.
 */

typedef struct
{
    TableD_Sequence_t* head;
    TableD_Sequence_t* tail;

} TableD_t;


#endif      /* #ifndef _MEL_BUFR_TABLES_H */
