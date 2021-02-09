*&---------------------------------------------------------------------*
*&  Include           ZXAUFU03
*&---------------------------------------------------------------------*

DATA : user     TYPE syuname,
       usr_data TYPE qisrsuser_data.


user = sy-uname.

CALL FUNCTION 'ISR_GET_USER_DETAILS'
  EXPORTING
    id_user_id     = sy-uname
  CHANGING
    is_user_data   = usr_data
  EXCEPTIONS
    user_not_found = 1
    OTHERS         = 2.

c_user_ci-user2 = usr_data-fullname.    "User Name
c_user_ci-user3 = usr_data-tel_number.  "User Phone Number
c_user_ci-user4 = 10000.                "Estimated Total Cost
c_user_ci-user5 = sy-datum.             "Application Date
c_user_ci-user6 = usr_data-department.  "Department