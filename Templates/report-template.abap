*&---------------------------------------------------------------------*
*& Report  ZA_REPORT_TEMPLATE
*&
*&----------------------------------------------------------------------------*
*& Functionality:
*&
*&   This is a simple Report with a selection screen displaying data
*&   from a table called ZAEMPLOYEE2
*&
*&   Make sure the Table is created with the required fields
*&   Create the selection texts for labels 'Date of Birth', 'Employee Number',
*&   'Gender', 'Sales', 'Purchasing' and 'Finance'
*&
*&   Create a Text-Symbol text-001 with 'Employee Data'
*&
*&   Create the message class ZAMES1
*&   TEXT 001: & entries found.
*&   TEXT 002: No entries found!
*&
*&----------------------------------------------------------------------------*

REPORT za_report_template.

* Data Declaration ***************

TABLES: zaemployees2.

DATA: lv_employee LIKE zaemployees2-employee,
      lv_count    TYPE i.

* Internal table declaration
TYPES ty_empl TYPE STANDARD TABLE OF zaemployees2.

DATA: it_empl TYPE ty_empl,
      wa_empl TYPE zaemployees2.

* Selection-Screen ***************

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK empl WITH FRAME TITLE text-001.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:  p_ee     FOR zaemployees2-employee NO-EXTENSION,
                 p_gender FOR zaemployees2-gender   NO INTERVALS NO-EXTENSION,
                 p_birth  FOR zaemployees2-birthdate.

SELECTION-SCREEN END OF BLOCK empl.

SELECTION-SCREEN SKIP.

* Radio buttons to choose the department
SELECTION-SCREEN BEGIN OF BLOCK team WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP.

PARAMETERS: p_no    RADIOBUTTON GROUP grp1 DEFAULT 'X',
            p_sales RADIOBUTTON GROUP grp1,
            p_purch RADIOBUTTON GROUP grp1,
            p_fi    RADIOBUTTON GROUP grp1.

SELECTION-SCREEN END OF BLOCK team.


* Program flow *****************

SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_empl
  FROM zaemployees2 WHERE birthdate IN    p_birth
                                    AND   employee IN p_ee
                                    AND   gender IN p_gender.

* Checking if a record was returned
IF sy-subrc <> 0.

  MESSAGE w002(zames1).
  EXIT.

ELSE.

  DESCRIBE TABLE it_empl LINES lv_count.

  MESSAGE s001(zames1) WITH lv_count.

ENDIF.

LOOP AT it_empl INTO wa_empl.

* Check and save the department in the Database table
  IF p_sales = 'X'.
    wa_empl-zzdept = 'SALES'.
  ELSEIF p_purch = 'X'.
    wa_empl-zzdept = 'PURCHASING'.
  ELSEIF p_fi = 'X'.
    wa_empl-zzdept = 'FINANCE'.
  ENDIF.

  MODIFY it_empl FROM wa_empl.
  UPDATE zaemployees2 FROM wa_empl.

* Write the records in the screen
  WRITE: / wa_empl-employee, wa_empl-lastname, wa_empl-firstname, wa_empl-title,
           wa_empl-birthdate, wa_empl-initials, wa_empl-gender, wa_empl-salary,
           wa_empl-ecurrency, wa_empl-zzlocation, wa_empl-zzdept.

ENDLOOP.