*&---------------------------------------------------------------------*
*& Report  ZA_FLIGHTS_CLASS
*&
*&---------------------------------------------------------------------*
*&
*& Functionality:
*&
*&   Simple class inheritance template
*&
*&---------------------------------------------------------------------*

REPORT za_vehicle_class.

************************************************************************
* Abstract CLASS vehicle - CANNOT BE INSTANTIATED
************************************************************************

CLASS vehicle DEFINITION ABSTRACT.

  PUBLIC SECTION.

    CLASS-DATA: numvehicles TYPE i.

    METHODS constructor.

    METHODS gofaster.

    METHODS writespeed.

  PROTECTED SECTION.

    DATA: speed TYPE i.

ENDCLASS.

CLASS vehicle IMPLEMENTATION.

  METHOD constructor.

    numvehicles = numvehicles + 1.

    WRITE: / 'A new vehicle has been instantiated.',
           / 'Total number of vehicles: ', numvehicles.

  ENDMETHOD.

  METHOD gofaster.

    speed = speed + 1.

  ENDMETHOD.

  METHOD writespeed.

    WRITE: / 'The vehicle speed is: ', speed LEFT-JUSTIFIED.

  ENDMETHOD.

ENDCLASS.

************************************************************************
* CLASS car inheriting from CLASS vehicle
* It is a FINAL class, it cannot be inherited
************************************************************************

CLASS car DEFINITION INHERITING FROM vehicle FINAL.

  PUBLIC SECTION.

    METHODS: refuel,

      writespeed REDEFINITION.

  PROTECTED SECTION.

    DATA: fuellevel TYPE i.

ENDCLASS.

CLASS car IMPLEMENTATION.

  METHOD refuel.

    fuellevel = 60.

    WRITE: / 'You have just filled up your fuel tank'.

  ENDMETHOD.

  METHOD writespeed.

    WRITE: / 'The CAR speed is: ', speed LEFT-JUSTIFIED.

  ENDMETHOD.

ENDCLASS.

************************************************************************
* CLASS boat inheriting from CLASS vehicle
************************************************************************

CLASS boat DEFINITION INHERITING FROM vehicle.

  PUBLIC SECTION.

    METHODS writespeed REDEFINITION.

ENDCLASS.

CLASS boat IMPLEMENTATION.

  METHOD writespeed.

    WRITE: / 'The BOAT speed is: ', speed LEFT-JUSTIFIED.

*   calling a method from the Super Class
    CALL METHOD super->writespeed.

  ENDMETHOD.

ENDCLASS.

***********************************************************************
*
*  START-OF-SELECTION
*
***********************************************************************

START-OF-SELECTION..

  DATA: obj_car  TYPE REF TO car,
        obj_boat TYPE REF TO boat.

  CREATE OBJECT obj_car.

  WRITE: / 'Car'.
  ULINE.
  ULINE.

  obj_car->gofaster( ).

  obj_car->writespeed( ).

  obj_car->gofaster( ).
  obj_car->gofaster( ).
  obj_car->gofaster( ).

  ULINE.

  obj_car->writespeed( ).

  ULINE.

  obj_car->refuel( ).

  ULINE.

  CREATE OBJECT obj_boat.

  ULINE.

  WRITE: / 'Boat'.
  ULINE.


  obj_boat->gofaster( ).

  obj_boat->writespeed( ).

  obj_boat->gofaster( ).
  obj_boat->gofaster( ).
  obj_boat->gofaster( ).
  obj_boat->gofaster( ).
  obj_boat->gofaster( ).

  ULINE.

  obj_boat->writespeed( ).