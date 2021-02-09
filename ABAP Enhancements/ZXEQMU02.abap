*&---------------------------------------------------------------------*
*&  Include           ZXEQMU02
*&---------------------------------------------------------------------*

CHECK activity_type = '1'.

IF data_iloa-swerk IS INITIAL.

  MESSAGE 'Please enter field Maintenance Plant' TYPE 'E' RAISING posting_not_allowed_ext.

ENDIF.

update_data_eq-ansdt = sy-datum.
update_flags_eq-ansdt = 'X'.