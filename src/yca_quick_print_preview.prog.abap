*-------------------------------------------------------------------------------*
*MIT License
*
*Copyright (c) 2019 Jacek Kopcinski (indevo.pl)
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
*-------------------------------------------------------------------------------*
REPORT  yca_quick_print_preview.

TYPE-POOLS: slis, icon.

TABLES: yca_output_set, nast.

TYPES:

  BEGIN OF ygs_key,
    kappl TYPE nast-kappl,
    objky TYPE nast-objky,
    kschl TYPE nast-kschl,
    spras TYPE nast-spras,
    parvw TYPE nast-parvw,
    parnr TYPE nast-parnr,
    ldest TYPE nast-ldest,
  END OF ygs_key,

  ygts_keys TYPE SORTED TABLE OF ygs_key WITH NON-UNIQUE KEY
    kappl objky kschl spras parvw parnr ldest,

  BEGIN OF ygs_preview.
        INCLUDE TYPE ygs_key AS key.
TYPES:
    descr TYPE yca_output-descr,
    icon TYPE balimsgty,
    preview TYPE icon_4,
    print TYPE icon_4,
    pdf TYPE icon_4,
    hotspot TYPE c,
  END OF ygs_preview,

  ygt_preview TYPE STANDARD TABLE OF ygs_preview.

CONSTANTS:
  c_mode_edit TYPE c VALUE 'E',
  c_mode_display TYPE c VALUE 'D'.

DATA:
  gv_mode TYPE char1,
  gv_debug TYPE char1,
  gv_ok_code TYPE sy-ucomm,
  go_container TYPE REF TO cl_gui_custom_container,
  go_grid TYPE REF TO cl_gui_alv_grid,
  gt_preview TYPE ygt_preview.

CALL SCREEN 100.

*----------------------------------------------------------------------*
*       CLASS gcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "gcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS gcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS gcl_event_handler IMPLEMENTATION .
  METHOD handle_hotspot_click .
    CASE e_column_id.
      WHEN 'OBJKY'.
        PERFORM display_doc USING es_row_no-row_id.
      WHEN 'PREVIEW'.
        PERFORM print_preview USING es_row_no-row_id.
      WHEN 'PRINT'.
        PERFORM print_spool USING es_row_no-row_id.
      WHEN 'PDF'.
        PERFORM save_pdf USING es_row_no-row_id.
    ENDCASE.
  ENDMETHOD .                    "handle_hotspot_click
ENDCLASS.                    "gcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  check_outputs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_outputs.

  DATA:
    ls_key TYPE ygs_key,
    lt_nast_keys TYPE ygts_keys.

  FIELD-SYMBOLS:
    <output> TYPE ygs_preview.

  SELECT kappl objky kschl spras parvw parnr ldest FROM nast
    INTO TABLE lt_nast_keys
    FOR ALL ENTRIES IN gt_preview
    WHERE kappl = gt_preview-kappl
      AND objky = gt_preview-objky
      AND kschl = gt_preview-kschl
      AND spras = gt_preview-spras
      AND nacha = '1'.

  LOOP AT gt_preview ASSIGNING <output> WHERE key IS NOT INITIAL.
    READ TABLE lt_nast_keys TRANSPORTING NO FIELDS FROM <output>-key.
    IF sy-subrc = 0.
      <output>-preview = icon_layout_control.
      <output>-print = icon_print.
      <output>-pdf = icon_pdf.
      <output>-icon = icon_green_light.
    ELSE.
      CLEAR <output>-preview.
      CLEAR <output>-print.
      CLEAR <output>-pdf.
      <output>-icon = icon_red_light.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "check_outputs

*&---------------------------------------------------------------------*
*&      Form  retrieve_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM retrieve_data.
  DATA:
    ls_key TYPE ygs_key,
    lt_nast_keys TYPE ygts_keys.

  FIELD-SYMBOLS:
    <output> TYPE ygs_preview.

  SELECT kappl objky kschl spras parvw parnr ldest FROM nast
    INTO TABLE lt_nast_keys
    FOR ALL ENTRIES IN gt_preview
    WHERE kappl = gt_preview-kappl
      AND objky = gt_preview-objky
      AND kschl = gt_preview-kschl
      AND spras = gt_preview-spras
      AND nacha = '1'.

  LOOP AT gt_preview ASSIGNING <output> WHERE key IS NOT INITIAL.
    PERFORM find_nast_key USING lt_nast_keys <output>
      CHANGING ls_key.
    IF sy-subrc = 0.
      <output>-key = ls_key.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "retrieve_data

*&---------------------------------------------------------------------*
*&      Form  find_nast_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IT_NAST_KEYS  text
*      -->IS_OUTPUT     text
*      -->CS_KEY        text
*----------------------------------------------------------------------*
FORM find_nast_key
USING it_nast_keys TYPE ygts_keys is_output TYPE ygs_preview
CHANGING cs_key TYPE ygs_key.

  CLEAR cs_key.

  READ TABLE it_nast_keys INTO cs_key FROM is_output-key.

  CHECK sy-subrc <> 0.
  " No LDEST
  READ TABLE it_nast_keys INTO cs_key WITH KEY
    kappl = is_output-kappl
    objky = is_output-objky
    kschl = is_output-kschl
    spras = is_output-spras
    parvw = is_output-parvw
    parnr = is_output-parnr.

  CHECK sy-subrc <> 0.
  " No LDEST, PARNR
  READ TABLE it_nast_keys INTO cs_key WITH KEY
    kappl = is_output-kappl
    objky = is_output-objky
    kschl = is_output-kschl
    spras = is_output-spras
    parvw = is_output-parvw.

  CHECK sy-subrc <> 0.
  " No LDEST, PARNR, PARVW
  READ TABLE it_nast_keys INTO cs_key WITH KEY
     kappl = is_output-kappl
    objky = is_output-objky
    kschl = is_output-kschl
    spras = is_output-spras.

ENDFORM.                    "find_nast_key
*&---------------------------------------------------------------------*
*&      Form  print_preview
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_ROW_INDEX  text
*----------------------------------------------------------------------*
FORM print_preview
  USING iv_row_index TYPE i.

  DATA:
    lv_rcode TYPE sy-subrc,
    ls_output TYPE ygs_preview.

  READ TABLE gt_preview INTO ls_output INDEX iv_row_index.

  CHECK sy-subrc = 0 AND ls_output-preview IS NOT INITIAL.

  "SAPscript debugger on
  IF gv_debug = 'X'.
    SET PARAMETER ID 'TTD' FIELD 'X'.
  ENDIF.

  CLEAR nast.
  MOVE-CORRESPONDING ls_output TO nast.
  nast-nacha = '1'.
  nast-tdarmod = '1'.
  nast-anzal = '1'.
  nast-usnam = sy-uname.
  PERFORM einzelnachricht_screen(rsnast00) USING lv_rcode .
ENDFORM.                    "print_preview

FORM display_doc
  USING iv_row_index TYPE i.

  DATA:
    lv_rcode TYPE sy-subrc,
    ls_output TYPE ygs_preview.

  READ TABLE gt_preview INTO ls_output INDEX iv_row_index.
  CASE ls_output-kappl.
    WHEN 'V3'.
      PERFORM vf03 USING ls_output-objky(10).
    WHEN 'V2'.
      PERFORM vf02 USING ls_output-objky(10).

  ENDCASE.
  CHECK sy-subrc = 0 AND ls_output-preview IS NOT INITIAL.

ENDFORM.

FORM vf03 USING iv_vbeln TYPE vbrk-vbeln.
  SET PARAMETER ID 'VF' FIELD iv_vbeln.
  CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
ENDFORM.

FORM vf02 USING iv_vbeln TYPE likp-vbeln.
  SET PARAMETER ID 'VL' FIELD iv_vbeln.
  CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  print_spool
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_ROW_INDEX  text
*----------------------------------------------------------------------*
FORM print_spool
  USING iv_row_index TYPE i.

  DATA:
    lv_start TYPE i,
    lv_end TYPE i,
    lv_duration TYPE p DECIMALS 2,
    lv_msg TYPE string,
    lv_rcode TYPE sy-subrc,
    ls_output TYPE ygs_preview.

  READ TABLE gt_preview INTO ls_output INDEX iv_row_index.

  CHECK sy-subrc = 0 AND ls_output-preview IS NOT INITIAL.

  CLEAR nast.
  MOVE-CORRESPONDING ls_output TO nast.
  nast-nacha = '1'.
  nast-tdarmod = '1'.
  nast-anzal = '1'.
  nast-dimme = 'X'.
  GET RUN TIME FIELD lv_start.
  PERFORM einzelnachricht(rsnast00) USING lv_rcode .
  GET RUN TIME FIELD lv_end.
  lv_duration = ( lv_end - lv_start ) / 1000000.
  lv_msg = |Generation time { lv_duration } s|.
  MESSAGE lv_msg TYPE 'S'.

ENDFORM.                    "print_spool

*&---------------------------------------------------------------------*
*&      Form  save_pdf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_ROW_INDEX  text
*----------------------------------------------------------------------*
FORM save_pdf
  USING iv_row_index TYPE i.

  DATA:
    lv_guid22 TYPE c LENGTH 22,
    ls_spool_info TYPE tsp01,
    lv_rcode TYPE sy-subrc,
    ls_output TYPE ygs_preview,
    lt_otf TYPE tt_itcoo.
  READ TABLE gt_preview INTO ls_output INDEX iv_row_index.

  CHECK sy-subrc = 0 AND ls_output-preview IS NOT INITIAL.

  CLEAR nast.
  MOVE-CORRESPONDING ls_output TO nast.
  nast-nacha = '1'.
  nast-tdarmod = '1'.
  nast-anzal = '1'.
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_22 = lv_guid22.
  TRANSLATE lv_guid22 TO UPPER CASE.
  nast-dsnam = lv_guid22(6).
  nast-dsuf1 = lv_guid22+6(4).
  nast-dsuf2 = lv_guid22+10(12).

  PERFORM einzelnachricht(rsnast00) USING lv_rcode .

  IF lv_rcode <> 0.
    MESSAGE s007(yqpp) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " Find the generated spool request
  SELECT SINGLE * FROM tsp01
    INTO ls_spool_info
    WHERE rqfinal IN ('C' , '.' )
      AND rqclient = sy-mandt
      AND rqowner = sy-uname
      AND rq0name = nast-dsnam
      AND rq1name = nast-dsuf1
      AND rq2name = nast-dsuf2.

  IF sy-subrc <> 0.
    MESSAGE s008(yqpp) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE ls_spool_info-rqdoctype.
    WHEN 'OTF'.
      PERFORM otf_to_pdf USING ls_spool_info.
    WHEN 'ADSP'.
      PERFORM ads_to_pdf USING ls_spool_info.
  ENDCASE.

ENDFORM.                    "save_pdf

FORM ads_to_pdf
  USING is_spool_info TYPE tsp01.

  DATA:
    lv_action TYPE i,
    lv_docs_num TYPE i,
    ls_rq TYPE tsp01sys,
    ls_part TYPE adspartdesc,
    lt_parts TYPE STANDARD TABLE OF adspartdesc,
    lv_pos TYPE i,
    lt_content TYPE solix_tab,
    lv_size TYPE i,
    lv_pdf_file TYPE string,
    lv_filename TYPE c LENGTH 255,
    lv_pdf TYPE fpcontent.

  ls_rq = is_spool_info.
  CALL FUNCTION 'RSPO_ADSP_FILL_PARTLIST'
    EXPORTING
      rq       = ls_rq
    TABLES
      partlist = lt_parts.

  READ TABLE lt_parts INTO ls_part INDEX 1.
  CHECK sy-subrc = 0.

  CALL FUNCTION 'FPCOMP_CREATE_PDF_FROM_SPOOL'
    EXPORTING
      i_spoolid  = is_spool_info-rqident
      i_partnum  = ls_part-adsnum
    IMPORTING
      e_pdf      = lv_pdf
      e_pdf_file = lv_pdf_file
    EXCEPTIONS
      OTHERS     = 1.

  "special handling multi-part PDFs
  IF lv_pdf_file IS NOT INITIAL.
    FIND REGEX '\w+\.coa' IN lv_pdf_file MATCH OFFSET lv_pos.
    CHECK sy-subrc = 0.
    lv_filename = lv_pdf_file+lv_pos.
    CALL FUNCTION 'ADS_SR_READ_CONNECTED_CONTENT'
      EXPORTING
        rqident        = ls_rq-rqident
        partfilename   = lv_filename
        docnum         = ls_part-adsnodocs
      IMPORTING
        content_bin    = lv_pdf
      CHANGING
        number_of_docs = lv_docs_num
      EXCEPTIONS
        OTHERS         = 1.
  ENDIF.

  CHECK xstrlen( lv_pdf ) > 0.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_pdf
    IMPORTING
      output_length = lv_size
    TABLES
      binary_tab    = lt_content.

  PERFORM download_file USING lt_content lv_size
    CHANGING lv_action.

ENDFORM.

FORM otf_to_pdf
  USING is_spool_info TYPE tsp01.

  DATA:
    lv_action TYPE i,
    lv_pdf_spool_id TYPE tsp01-rqident,
    lv_filesize TYPE i,
    lt_pdf TYPE STANDARD TABLE OF tline.

  CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid              = is_spool_info-rqident
    IMPORTING
      pdf_bytecount            = lv_filesize
      pdf_spoolid              = lv_pdf_spool_id
    TABLES
      pdf                      = lt_pdf
    EXCEPTIONS
      err_no_otf_spooljob      = 1
      err_no_spooljob          = 2
      err_no_permission        = 3
      err_conv_not_possible    = 4
      err_bad_dstdevice        = 5
      user_cancelled           = 6
      err_spoolerror           = 7
      err_temseerror           = 8
      err_btcjob_open_failed   = 9
      err_btcjob_submit_failed = 10
      err_btcjob_close_failed  = 11
      OTHERS                   = 12.

  "Call rollback to delete the generated spool request
  ROLLBACK WORK.

  PERFORM download_file USING lt_pdf lv_filesize
    CHANGING lv_action.

ENDFORM.

FORM download_file
 USING it_content TYPE STANDARD TABLE
    iv_size TYPE i
CHANGING cv_action TYPE i.
  DATA:
    lv_lang TYPE c LENGTH 2,
    lv_filename TYPE string,
    lv_filepath TYPE string,
    lv_fullpath TYPE string.

  WRITE nast-spras TO lv_lang.
  CONCATENATE nast-objky nast-kschl lv_lang nast-ldest INTO lv_filename SEPARATED BY space.
  CONCATENATE lv_filename '.pdf' INTO lv_filename.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Save output as PDF file'
      default_extension = 'pdf'
      default_file_name = lv_filename
      file_filter       = 'All files (*.*)|*.*|PDF files (*.pdf)|*.pdf'
    CHANGING
      filename          = lv_filename
      path              = lv_filepath
      fullpath          = lv_fullpath
      user_action       = cv_action
    EXCEPTIONS
      OTHERS            = 0.

  CHECK cv_action = 0.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize = iv_size
      filename     = lv_fullpath
      filetype     = 'BIN'
    CHANGING
      data_tab     = it_content
    EXCEPTIONS
      OTHERS       = 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM build_fieldcat
CHANGING ot_fieldcat TYPE lvc_t_fcat.

  DATA:
    ls_fieldcat TYPE lvc_s_fcat.

  CLEAR ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'KAPPL'.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'OBJKY'.
  IF gv_mode = c_mode_display.
    ls_fieldcat-hotspot = 'X'.
  ENDIF.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'KSCHL'.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'SPRAS'.
  ls_fieldcat-outputlen = 3.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'PARVW'.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'PARNR'.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'LDEST'.
  ls_fieldcat-outputlen = 4.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-ref_table = 'YCA_OUTPUT'.
  ls_fieldcat-fieldname = 'DESCR'.
  ls_fieldcat-outputlen = 60.
  PERFORM set_edit_mode CHANGING ls_fieldcat-edit.
  APPEND ls_fieldcat TO ot_fieldcat.

  IF gv_mode = c_mode_display OR gv_mode = c_mode_edit.
    CLEAR ls_fieldcat.
    ls_fieldcat-ref_table = 'DV70A'.
    ls_fieldcat-ref_field = 'STATUSICON'.
    ls_fieldcat-fieldname = 'ICON'.
    ls_fieldcat-outputlen = 4.
    APPEND ls_fieldcat TO ot_fieldcat.
  ENDIF.

  IF gv_mode = c_mode_display.
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PREVIEW'.
    ls_fieldcat-coltext = 'Preview'.
    ls_fieldcat-icon = 'X'.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO ot_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PRINT'.
    ls_fieldcat-coltext = 'Print'.
    ls_fieldcat-icon = 'X'.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO ot_fieldcat.

    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'PDF'.
    ls_fieldcat-coltext = 'Save'.
    ls_fieldcat-icon = 'X'.
    ls_fieldcat-hotspot = 'X'.
    APPEND ls_fieldcat TO ot_fieldcat.
  ENDIF.
ENDFORM.                    "build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  set_edit_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OV_EDIT    text
*----------------------------------------------------------------------*
FORM set_edit_mode
CHANGING ov_edit TYPE c.
  IF gv_mode = c_mode_edit.
    ov_edit = 'X'.
  ELSE.
    ov_edit = space.
  ENDIF.
ENDFORM.                    "set_edit_mode
*&---------------------------------------------------------------------*
*&      Form  initialize_controls
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialize_controls.
  DATA:
    lo_handler TYPE REF TO gcl_event_handler.

  CREATE OBJECT go_container
    EXPORTING
      container_name = 'CONTAINER'.

  CREATE OBJECT go_grid
    EXPORTING
      i_parent = go_container.

  CREATE OBJECT lo_handler.

  SET HANDLER lo_handler->handle_hotspot_click FOR go_grid.

ENDFORM.                    "initialize_controls
*----------------------------------------------------------------------*
*  MODULE pai_0200 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE gv_ok_code.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN '&BACK' OR '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&PREVIEW'.
      CALL SCREEN 400.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
*  MODULE pbo_0200 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  SET PF-STATUS 'STATUS_DISPLAY'.
  gv_mode = c_mode_display.
  PERFORM display_grid.
ENDMODULE.                 " OUTPUT_0100  OUTPUT
*----------------------------------------------------------------------*
*  MODULE clear_ok_code OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE clear_ok_code OUTPUT.
  CLEAR gv_ok_code.
ENDMODULE.                 " clear_ok_code  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  CLEAR yca_output_set.
  GET PARAMETER ID 'SETID' FIELD yca_output_set-setid.
  SET PF-STATUS 'STATUS_MAIN'.
  SET TITLEBAR 'TITLE'.
ENDMODULE.                 " pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  pai_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  SET PARAMETER ID 'SETID' FIELD yca_output_set-setid.
  CASE gv_ok_code.
    WHEN '&BACK' OR '&EXIT' OR '&CANCEL'.
      LEAVE PROGRAM.
    WHEN 'DISPLAY'.
      PERFORM display_set.
    WHEN 'CREATE'.
      PERFORM create_set.
    WHEN 'EDIT'.
      PERFORM edit_set.
    WHEN 'COPY'.
      PERFORM copy_set.
    WHEN 'DELETE'.
      PERFORM delete_set.
  ENDCASE.
ENDMODULE.                 " pai_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  display_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_set.
  SELECT SINGLE * FROM yca_output_set
    WHERE setid = yca_output_set-setid.

  IF sy-subrc <> 0.
    MESSAGE e001(yqpp) WITH yca_output_set-setid.
  ENDIF.

  SELECT * FROM yca_output
    INTO CORRESPONDING FIELDS OF TABLE gt_preview
    WHERE setid = yca_output_set-setid.

  CALL SCREEN 400.
ENDFORM.                    "display_set
*&---------------------------------------------------------------------*
*&      Form  create_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_set.

  SELECT SINGLE * FROM yca_output_set
    WHERE setid = yca_output_set-setid.

  IF sy-subrc = 0.
    MESSAGE e000(yqpp) WITH yca_output_set-setid.
  ENDIF.

  CLEAR gt_preview.
  DO 200 TIMES.
    APPEND INITIAL LINE TO gt_preview.
  ENDDO.

  CALL SCREEN 300.
ENDFORM.                    "create_set
*&---------------------------------------------------------------------*
*&      Form  edit_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM edit_set.
  SELECT SINGLE * FROM yca_output_set
    WHERE setid = yca_output_set-setid.

  IF sy-subrc <> 0.
    MESSAGE e001(yqpp) WITH yca_output_set-setid.
  ENDIF.

  SELECT * FROM yca_output
    INTO CORRESPONDING FIELDS OF TABLE gt_preview
    WHERE setid = yca_output_set-setid.

  CALL SCREEN 300.
ENDFORM.                    "edit_set
*&---------------------------------------------------------------------*
*&      Form  copy_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM copy_set.
  DATA:
    lv_setid TYPE yca_dataset_id.

  PERFORM get_new_set_id CHANGING lv_setid.

  " Read the content of the original set
  SELECT * FROM yca_output
    INTO CORRESPONDING FIELDS OF TABLE gt_preview
    WHERE setid = yca_output_set-setid.

  yca_output_set-setid = lv_setid.
  CALL SCREEN 300.
ENDFORM.                    "copy_set
*&---------------------------------------------------------------------*
*&      Form  delete_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_set.
  DATA:
    lv_answer TYPE c,
    lv_text TYPE string.
  SELECT SINGLE * FROM yca_output_set
    WHERE setid = yca_output_set-setid.

  IF sy-subrc <> 0.
    MESSAGE e001(yqpp) WITH yca_output_set-setid.
  ENDIF.

  MESSAGE i003(yqpp) WITH yca_output_set-setid INTO lv_text.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = 'Delete Set'
      text_question  = lv_text
      default_button = '2'
    IMPORTING
      answer         = lv_answer.

  IF lv_answer = '1'.
    DELETE yca_output_set.
    DELETE FROM yca_output WHERE setid = yca_output_set-setid.
    COMMIT WORK.
    MESSAGE s004(yqpp) WITH yca_output_set-setid.
  ENDIF.
ENDFORM.                    "delete_set

*&---------------------------------------------------------------------*
*&      Form  display_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_grid.
  DATA:
    ls_layout TYPE lvc_s_layo,
    ls_variant TYPE disvariant,
    lt_exclusions TYPE ui_functions,
    lt_fieldcat TYPE lvc_t_fcat.

  IF go_container IS INITIAL.
    PERFORM initialize_controls.
  ENDIF.

  IF gv_mode = c_mode_display.
    ls_variant-report = 'YCA_QUICK_PRINT_PREVIEW'.
  ENDIF.

  PERFORM build_fieldcat
    CHANGING lt_fieldcat.

  PERFORM build_toolbar_exclusions
    CHANGING lt_exclusions.

  ls_layout-col_opt = 'X'.

  CALL METHOD go_grid->set_table_for_first_display
    EXPORTING
      is_layout            = ls_layout
      it_toolbar_excluding = lt_exclusions
      is_variant           = ls_variant
      i_save               = 'U'
    CHANGING
      it_outtab            = gt_preview
      it_fieldcatalog      = lt_fieldcat.
ENDFORM.                    "display_grid
*&---------------------------------------------------------------------*
*&      Form  build_toolbar_exclusions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->CT_FUNCTIONS  text
*----------------------------------------------------------------------*
FORM build_toolbar_exclusions
CHANGING ct_functions TYPE ui_functions.
  DATA:
    ls_exclude TYPE ui_func.

  APPEND cl_gui_alv_grid=>mc_mb_sum TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_mb_subtot TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_mb_export TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_info TO ct_functions.
  IF gv_mode = c_mode_edit.
    APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO ct_functions.
    APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO ct_functions.
    APPEND cl_gui_alv_grid=>mc_fc_current_variant TO ct_functions.
    APPEND cl_gui_alv_grid=>mc_fc_filter TO ct_functions.
  ENDIF.
  APPEND cl_gui_alv_grid=>mc_fc_find TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_check TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_print TO ct_functions.
  APPEND cl_gui_alv_grid=>mc_fc_views TO ct_functions.

ENDFORM.                    "build_toolbar_exclusions
*&---------------------------------------------------------------------*
*&      Form  save_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save_set.

  DATA:
    ls_preview TYPE ygs_preview,
    ls_output TYPE yca_output,
    lt_output TYPE STANDARD TABLE OF yca_output.

  IF yca_output_set-setid IS INITIAL.
    PERFORM get_new_set_id CHANGING yca_output_set-setid.
  ENDIF.

  CHECK yca_output_set-setid IS NOT INITIAL.

  IF yca_output_set-ernam IS INITIAL. " new set.
    yca_output_set-ernam = sy-uname.
    yca_output_set-erdat = sy-datum.
    INSERT yca_output_set.
  ELSE.
    DELETE FROM yca_output WHERE setid = yca_output_set-setid.
  ENDIF.

  DELETE gt_preview WHERE table_line IS INITIAL.
  SORT gt_preview BY key.
  DELETE ADJACENT DUPLICATES FROM gt_preview
    COMPARING key.

  LOOP AT gt_preview INTO ls_preview WHERE key IS NOT INITIAL.
    ls_output-setid = yca_output_set-setid.
    ls_output-kappl = ls_preview-kappl.
    ls_output-objky = ls_preview-objky.
    ls_output-kschl = ls_preview-kschl.
    ls_output-spras = ls_preview-spras.
    ls_output-parvw = ls_preview-parvw.
    ls_output-parnr = ls_preview-parnr.
    ls_output-ldest = ls_preview-ldest.
    ls_output-descr = ls_preview-descr.
    APPEND ls_output TO lt_output.
  ENDLOOP.

  IF lt_output IS NOT INITIAL.
    INSERT yca_output FROM TABLE lt_output.
  ENDIF.
  COMMIT WORK.
  MESSAGE s005(yqpp) WITH yca_output_set-setid.
  PERFORM preview_outputs.
ENDFORM.                    "save_set

*&---------------------------------------------------------------------*
*&      Form  get_new_set_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->OV_SET_ID  text
*----------------------------------------------------------------------*
FORM get_new_set_id
  CHANGING ov_setid TYPE yca_dataset_id.

  DATA:
    lv_setid TYPE yca_dataset_id,
    ls_field TYPE sval,
    lt_fields TYPE ty_sval,
    lv_return TYPE c.

  CLEAR ov_setid.

  ls_field-fieldname = 'SETID'.
  ls_field-tabname = 'YCA_OUTPUT_SET'.
  APPEND ls_field TO lt_fields.

  WHILE ov_setid IS INITIAL AND lv_return = space.
    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = 'Choose Set ID'
      IMPORTING
        returncode  = lv_return
      TABLES
        fields      = lt_fields.

    CHECK lv_return = space.

    READ TABLE lt_fields INTO ls_field INDEX 1.
    IF ls_field-value IS INITIAL.
      MESSAGE s002(yqpp) DISPLAY LIKE 'E'.
    ELSE.
      SELECT SINGLE setid FROM yca_output_set
        INTO lv_setid
        WHERE setid = ls_field-value.

      IF sy-subrc = 0.
        MESSAGE s000(yqpp) DISPLAY LIKE 'E' WITH ls_field-value.
      ELSE.
        ov_setid = ls_field-value.
      ENDIF.
    ENDIF.
  ENDWHILE.

ENDFORM.                    "get_new_set_id
*&---------------------------------------------------------------------*
*&      Module  pbo_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0400 OUTPUT.
  PERFORM pbo_0400.
ENDMODULE.                 " pbo_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  pai_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0400 INPUT.
  PERFORM pai_0400.
ENDMODULE.                 " pai_0400  INPUT
*&---------------------------------------------------------------------*
*&      Form  pbo_0400
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_0400.
  SET PF-STATUS 'STATUS_MAIN'.
  gv_mode = c_mode_display.
  PERFORM check_outputs.
  PERFORM display_grid.
ENDFORM.                                                    "pbo_0400
*&---------------------------------------------------------------------*
*&      Form  pai_0400
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pai_0400.
  CASE gv_ok_code.
    WHEN '&BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDFORM.                                                    "pai_0400
*&---------------------------------------------------------------------*
*&      Module  pbo_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.
  PERFORM pbo_0300.
ENDMODULE.                 " pbo_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  pai_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.
  PERFORM pai_0300.
ENDMODULE.                 " pai_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  pbo_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pbo_0300.
  SET PF-STATUS 'STATUS_EDIT'.
  gv_mode = c_mode_edit.
  PERFORM display_grid.
ENDFORM.                                                    "pbo_0300
*&---------------------------------------------------------------------*
*&      Form  pai_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pai_0300.
  "Call to update gt_preview table
  go_grid->check_changed_data( ).

  CASE gv_ok_code.
    WHEN '&SAVE'.
      PERFORM save_set.
    WHEN '&CHECK'.
      PERFORM check_outputs.
      go_grid->refresh_table_display( ).
    WHEN '&RETRIEVE'.
      PERFORM retrieve_data.
      go_grid->refresh_table_display( ).
    WHEN '&EXIT'.
      LEAVE PROGRAM.
    WHEN '&BACK' OR '&CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN '&PREVIEW'.
      PERFORM preview_outputs.
  ENDCASE.
ENDFORM.                                                    "pai_0300

*&---------------------------------------------------------------------*
*&      Form  preview_outputs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM preview_outputs.
  CALL SCREEN 400.
ENDFORM.                    "preview_outputs
