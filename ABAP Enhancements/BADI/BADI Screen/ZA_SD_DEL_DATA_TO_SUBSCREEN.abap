FUNCTION za_sd_del_data_to_subscreen.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_ADDFIELD) TYPE  ZALIKPFIELDS
*"----------------------------------------------------------------------

  zalikpfields-zzafield1 = i_addfield-zzafield1.
  zalikpfields-zzafield2 = i_addfield-zzafield2.

ENDFUNCTION.