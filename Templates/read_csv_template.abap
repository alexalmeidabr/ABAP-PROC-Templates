*&---------------------------------------------------------------------*
*& Report  ZA_READ_CSV_TEMPLATE
*&
*&----------------------------------------------------------------------------*
*& Functionality:
*&
*& Read a csv file, display the data on a ALV Report and save in the Database
*&
*& Make sure the Table is created with the required fields: P_DISP 'Display only'
*&                                                          P_FILE 'CSV File'
*&                                                          P_SAVE 'Save and Display'
*&
*& Create the selection texts
*&
*& Create a Text-Symbol text-001 with 'Choose csv File'
*&                      text-002 with 'Action'
*&
*& Create the message class ZAMES1
*& TEXT 003: Some records already existed in the database table!
*& TEXT 004: & records added into the table
*& TEXT 005: Database insert error
*&
*&----------------------------------------------------------------------------*

REPORT za_read_csv_template.

*&---------------------------------------------------------------------*
*& Data Declaration
*&---------------------------------------------------------------------*

TABLES: zaemployees2.

DATA lv_count TYPE i.

* Field category for ALV Report
DATA: it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv.

* Internal table declaration
TYPES BEGIN OF ty_line_empl.

        INCLUDE STRUCTURE zaemployees2.

TYPES END OF ty_line_empl.

TYPES ty_empl TYPE STANDARD TABLE OF ty_line_empl.

DATA: it_empl TYPE ty_empl,
      wa_empl TYPE ty_line_empl.

* Fields required to load CSV file
DATA: v_rc TYPE i.
DATA: lt_filetab TYPE filetable.
DATA: ls_filetab LIKE LINE OF lt_filetab.

* Build field catalog for ALV Report

wa_fieldcat-fieldname  = 'EMPLOYEE'.
wa_fieldcat-seltext_m  = 'Employee'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'LASTNAME'.
wa_fieldcat-seltext_m  = 'Last Name'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'FIRSTNAME'.
wa_fieldcat-seltext_m  = 'Name'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'TITLE'.
wa_fieldcat-seltext_m  = 'Title'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'BIRTHDATE'.
wa_fieldcat-seltext_m  = 'Birthdate'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'INITIALS'.
wa_fieldcat-seltext_m  = 'Initials'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'GENDER'.
wa_fieldcat-seltext_m  = 'Gender'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'SALARY'.
wa_fieldcat-seltext_m  = 'Salary'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'ECURRENCY'.
wa_fieldcat-seltext_m  = 'Curency'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'ZZLOCATION'.
wa_fieldcat-seltext_m  = 'Location'.
APPEND wa_fieldcat TO it_fieldcat.

wa_fieldcat-fieldname  = 'ZZDEPT'.
wa_fieldcat-seltext_m  = 'Department'.
APPEND wa_fieldcat TO it_fieldcat.


*&---------------------------------------------------------------------*
*& Selection-Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK select_file WITH FRAME TITLE text-001.

SELECTION-SCREEN SKIP.

PARAMETERS: p_file TYPE localfile OBLIGATORY.

SELECTION-SCREEN END OF BLOCK select_file.

SELECTION-SCREEN SKIP.

* Radio buttons to choose the department
SELECTION-SCREEN BEGIN OF BLOCK team WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP.

PARAMETERS: p_save RADIOBUTTON GROUP grp1,
            p_disp RADIOBUTTON GROUP grp1 DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK team.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = lt_filetab
      rc                      = v_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  READ TABLE lt_filetab INTO ls_filetab INDEX 1.
  IF sy-subrc = 0.
    p_file = ls_filetab-filename.
  ENDIF.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM readfile USING p_file.

  IF p_save = 'X'.
    PERFORM db_save.
  ENDIF.

  PERFORM alv_display.

*&---------------------------------------------------------------------*
*& Read CSV File
*&---------------------------------------------------------------------*

FORM readfile USING p_file TYPE localfile.

  DATA: c_sep         TYPE c VALUE ',',
        lv_index      TYPE i,
        lt_split_data TYPE STANDARD TABLE OF string,
        gt_intern     TYPE kcde_intern,
        gwa_intern    TYPE kcde_intern_struc.

*  DATA itab LIKE TABLE OF kcde_cells WITH HEADER LINE.

  CALL FUNCTION 'KCD_CSV_FILE_TO_INTERN_CONVERT'
    EXPORTING
      i_filename      = p_file
      i_separator     = c_sep
    TABLES
      e_intern        = gt_intern
    EXCEPTIONS
      upload_csv      = 1
      upload_filetype = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR it_empl.

  LOOP AT gt_intern INTO gwa_intern.

    IF gwa_intern-row <> '0001'.

      CASE gwa_intern-col.
        WHEN 0001.
          wa_empl-employee     = gwa_intern-value.
        WHEN 0002.
          wa_empl-lastname     = gwa_intern-value.
        WHEN 0003.
          wa_empl-firstname    = gwa_intern-value.
        WHEN 0004.
          wa_empl-title        = gwa_intern-value.
        WHEN 0005.
          wa_empl-birthdate    = gwa_intern-value.
        WHEN 0006.
          wa_empl-initials     = gwa_intern-value.
        WHEN 0007.
          wa_empl-gender       = gwa_intern-value.
        WHEN 0008.
          wa_empl-salary       = gwa_intern-value.
        WHEN 0009.
          wa_empl-ecurrency    = gwa_intern-value.
        WHEN 0010.
          wa_empl-zzlocation   = gwa_intern-value.
        WHEN 0011.
          wa_empl-zzdept       = gwa_intern-value.
      ENDCASE.

      AT END OF row.
        APPEND wa_empl TO it_empl.
        CLEAR wa_empl.
      ENDAT.

    ENDIF.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& ALV Report
*&---------------------------------------------------------------------*

FORM alv_display.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = 'SY-REPID'
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = it_empl.

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Save data into Database
*&---------------------------------------------------------------------*

FORM db_save.

  INSERT zaemployees2 FROM TABLE it_empl ACCEPTING DUPLICATE KEYS.

  CASE sy-subrc.
    WHEN 0.
      MESSAGE s004(zames1) WITH sy-dbcnt.
    WHEN 4.
      MESSAGE i003(zames1).
    WHEN OTHERS.
      MESSAGE e005(zames1).
  ENDCASE.

ENDFORM.