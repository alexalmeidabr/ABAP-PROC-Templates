*&---------------------------------------------------------------------*
*& Report  ZA_FLIGHTS_CLASS
*&
*&---------------------------------------------------------------------*
*&
*& Functionality:
*&
*&   Select data from Flight table SPFLI and store it into a
*&   internal table in the class flight
*&
*&   The data can be displyed in text or ALV using methods in the
*&   class flight
*&
*&   Create a Text-Symbol text-001 with 'Flight Data'
*&                        text-002 with 'Display Options'
*&
*&---------------------------------------------------------------------*

REPORT za_flights_class.

CLASS flight DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA: numflights TYPE i.

    CLASS-DATA it_fieldcat TYPE slis_t_fieldcat_alv.

    TYPES ty_flight TYPE STANDARD TABLE OF spfli.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING sel_flights TYPE ty_flight.

    METHODS viewflights.

    METHODS viewflightsalv.

    METHODS numberoffights
      IMPORTING airport           TYPE s_fromairp
      RETURNING VALUE(numflights) TYPE i.

  PRIVATE SECTION.

*   Internal table declaration

    DATA: it_flight TYPE  ty_flight.


ENDCLASS.


CLASS flight IMPLEMENTATION.

  METHOD class_constructor.

    DATA: wa_fieldcat TYPE slis_fieldcat_alv.

* Build field catalog for ALV Report

    wa_fieldcat-fieldname  = 'CARRID'.
    wa_fieldcat-seltext_m  = 'Airline'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'CONNID'.
    wa_fieldcat-seltext_m  = 'Flight Number'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'COUNTRYFR'.
    wa_fieldcat-seltext_m  = 'Country From'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'CITYFROM'.
    wa_fieldcat-seltext_m  = 'City From'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'AIRPFROM'.
    wa_fieldcat-seltext_m  = 'Airport From'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'COUNTRYTO'.
    wa_fieldcat-seltext_m  = 'Country To'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'CITYTO'.
    wa_fieldcat-seltext_m  = 'City To'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'AIRPTO'.
    wa_fieldcat-seltext_m  = 'Airport To'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'FLTIME'.
    wa_fieldcat-seltext_m  = 'Flight Time'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'DEPTIME'.
    wa_fieldcat-seltext_m  = 'Departure Time'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'ARRTIME'.
    wa_fieldcat-seltext_m  = 'Arrival Time'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'DISTANCE'.
    wa_fieldcat-seltext_m  = 'Distance'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'DISTID'.
    wa_fieldcat-seltext_m  = 'UOM'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'FLTYPE'.
    wa_fieldcat-seltext_m  = 'Flight Type'.
    APPEND wa_fieldcat TO it_fieldcat.

    wa_fieldcat-fieldname  = 'PERIOD'.
    wa_fieldcat-seltext_m  = 'Arrival n days later'.
    APPEND wa_fieldcat TO it_fieldcat.

  ENDMETHOD.

  METHOD constructor.

    DATA: wa_selflight TYPE spfli,
          wa_flights   TYPE spfli.

    LOOP AT sel_flights INTO wa_selflight.

      CLEAR wa_flights.
      MOVE-CORRESPONDING wa_selflight TO wa_flights.
      APPEND wa_flights TO it_flight.

    ENDLOOP.

  ENDMETHOD.

  METHOD viewflights.

    DATA: wa_flight TYPE spfli.

    LOOP AT it_flight INTO wa_flight.

      WRITE: / wa_flight-carrid, wa_flight-connid, wa_flight-countryfr, wa_flight-cityfrom, wa_flight-airpfrom,
               wa_flight-countryto, wa_flight-cityto, wa_flight-airpto, wa_flight-fltime, wa_flight-deptime,
               wa_flight-arrtime, wa_flight-distance, wa_flight-distid, wa_flight-fltype, wa_flight-period.

    ENDLOOP.

  ENDMETHOD.

  METHOD viewflightsalv.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = 'SY-REPID'
        it_fieldcat        = it_fieldcat
      TABLES
        t_outtab           = it_flight.

  ENDMETHOD.

  METHOD numberoffights.

    DATA: wa_flight TYPE spfli.

    LOOP AT it_flight INTO wa_flight.

      IF wa_flight-airpfrom = airport.
        numflights = numflights + 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

TABLES: spfli.

TYPES: typ_flight TYPE STANDARD TABLE OF spfli.

DATA: lt_flight     TYPE typ_flight,
      wa_selflights TYPE spfli.

DATA: obj_flight TYPE REF TO flight.

*&---------------------------------------------------------------------*
*& Selection-Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK flightdata WITH FRAME TITLE text-001.

SELECTION-SCREEN SKIP.

SELECT-OPTIONS: p_carrid FOR spfli-carrid     NO-EXTENSION,
                p_connid FOR spfli-connid     NO-EXTENSION,
                p_cfr    FOR spfli-countryfr  NO-EXTENSION NO INTERVALS,
                p_cfrom  FOR spfli-cityfrom   NO-EXTENSION NO INTERVALS,
                p_afrom  FOR spfli-airpfrom   NO-EXTENSION NO INTERVALS,
                p_cto    FOR spfli-countryto  NO-EXTENSION NO INTERVALS,
                p_cityto FOR spfli-cityto     NO-EXTENSION NO INTERVALS,
                p_airpto FOR spfli-airpto     NO-EXTENSION NO INTERVALS,
                p_fltime FOR spfli-fltime     NO-EXTENSION,
                p_dtime  FOR spfli-deptime    NO-EXTENSION,
                p_atime  FOR spfli-arrtime    NO-EXTENSION,
                p_dist   FOR spfli-distance   NO-EXTENSION,
                p_period FOR spfli-period     NO-EXTENSION NO INTERVALS.

SELECTION-SCREEN END OF BLOCK flightdata.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK viewopt WITH FRAME TITLE text-002.

PARAMETERS: p_text RADIOBUTTON GROUP grp1,
            p_alv  RADIOBUTTON GROUP grp1 DEFAULT 'X',
            p_numf RADIOBUTTON GROUP grp1.

SELECTION-SCREEN END OF BLOCK viewopt.

AT SELECTION-SCREEN.

  IF p_numf IS NOT INITIAL.

    IF p_afrom IS INITIAL.

      MESSAGE e006(zames1).

    ENDIF.

  ENDIF.


***********************************************************************
*
*  START-OF-SELECTION
*
***********************************************************************

START-OF-SELECTION.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_flight

    FROM spfli WHERE carrid     IN p_carrid
                 AND connid     IN p_connid
                 AND countryfr  IN p_cfr
                 AND cityfrom   IN p_cfrom
                 AND airpfrom   IN p_afrom
                 AND countryto  IN p_cto
                 AND cityto     IN p_cityto
                 AND airpto     IN p_airpto
                 AND fltime     IN p_fltime
                 AND deptime    IN p_dtime
                 AND arrtime    IN p_atime
                 AND distance   IN p_dist
                 AND period     IN p_period.

  CREATE OBJECT obj_flight EXPORTING sel_flights = lt_flight.

  IF p_text = 'X'.

    ULINE.

    obj_flight->viewflights( ).

  ELSEIF p_alv = 'X'.

    obj_flight->viewflightsalv( ).

  ELSE.

    WRITE: / 'Number of flights: ', obj_flight->numberoffights( P_AFROM-LOW ).

  ENDIF.

*