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
*&   Create the selection texts for labels 'Date of Birth', 'Employee Number'
*&   and 'Gender'
*&
*&   Create a Text-Symbol text-001 with 'Employee Data'
*&
*&----------------------------------------------------------------------------*

REPORT ZA_REPORT_TEMPLATE.

TABLES: ZAEMPLOYEES2.

DATA lv_employee LIKE ZAEMPLOYEES2-EMPLOYEE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK empl WITH FRAME TITLE text-001.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS:  p_ee     FOR ZAEMPLOYEES2-EMPLOYEE NO-EXTENSION,
                 p_gender FOR ZAEMPLOYEES2-GENDER NO INTERVALS NO-EXTENSION,
                 p_birth  FOR ZAEMPLOYEES2-BIRTHDATE.

SELECTION-SCREEN END OF BLOCK empl.

SELECT * FROM ZAEMPLOYEES2 WHERE BIRTHDATE IN p_birth
                           AND   EMPLOYEE IN p_ee
                           AND   GENDER IN p_gender.

  WRITE: / ZAEMPLOYEES2-EMPLOYEE, ZAEMPLOYEES2-LASTNAME, ZAEMPLOYEES2-FIRSTNAME, ZAEMPLOYEES2-TITLE,
           ZAEMPLOYEES2-BIRTHDATE, ZAEMPLOYEES2-INITIALS, ZAEMPLOYEES2-GENDER, ZAEMPLOYEES2-SALARY,
           ZAEMPLOYEES2-ECURRENCY, ZAEMPLOYEES2-ZZLOCATION, ZAEMPLOYEES2-ZZDEPT.

ENDSELECT.

MESSAGE s001(ZAMES1).