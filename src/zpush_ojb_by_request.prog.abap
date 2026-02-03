*&---------------------------------------------------------------------*
*& Report ZPUSH_OJB_BY_REQUEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpush_ojb_by_request NO STANDARD PAGE HEADING.

TYPE-POOLS:trwbo,icon.
TABLES: fmrfc,e070,e071.
DATA: gt_fldct TYPE lvc_t_fcat,
      gs_slayt TYPE lvc_s_layo.
DATA: gr_grid TYPE REF TO cl_gui_alv_grid.
DATA: go_cont TYPE REF TO cl_gui_custom_container.
DATA: gt_out  TYPE TABLE OF e071 WITH HEADER LINE.
DATA: gt_obj  TYPE TABLE OF ko100 WITH HEADER LINE.
DATA: gt_log  TYPE TABLE OF fmrfc-rfcds WITH HEADER LINE.
DATA: gt_func TYPE TABLE OF ui_func.
DATA: gv_trkor TYPE e070-trkorr.
DATA: gv_released.
DATA: BEGIN OF gs_key OCCURS 0,
        object   TYPE trobjtype,
        obj_name TYPE trobj_name,
      END OF gs_key.
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA: event_receiver TYPE REF TO lcl_event_receiver.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS WINDOW.
  PARAMETERS     cp_targ TYPE tr_target DEFAULT sy-sysid.
  SELECT-OPTIONS cs_objt FOR e071-object NO INTERVALS.
  SELECT-OPTIONS cs_dest FOR fmrfc-rfcds NO INTERVALS.
  SELECT-OPTIONS cs_pset FOR gs_key  NO INTERVALS.
  PARAMETERS     cp_delt AS CHECKBOX.
  PARAMETERS     cp_srcs AS CHECKBOX.
SELECTION-SCREEN END OF SCREEN 1001.

SELECTION-SCREEN BEGIN OF SCREEN 1002 AS WINDOW.
  SELECT-OPTIONS: s_trkor FOR e070-trkorr NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 1002.

*&---------------------------------------------------------------------*
*& lcl_event_receiver
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column es_row_no.
    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.
    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.
    METHODS handle_menu_button
      FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING e_object e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*&---------------------------------------------------------------------*
*& lcl_event_receiver
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM grid_double_clik USING e_row e_column es_row_no.
  ENDMETHOD.                    "handle_double_click
  METHOD handle_user_command.
    IF e_ucomm(4) = 'FCTR'.
      PERFORM trans_tr USING e_ucomm.
      PERFORM refresh.
    ELSEIF e_ucomm(3) = 'LOG'.
      PERFORM trans_log USING e_ucomm.
    ELSEIF e_ucomm IN cs_objt.
      PERFORM insert_obj USING e_ucomm.
      PERFORM refresh.
    ELSE.
      CASE e_ucomm.
        WHEN 'BATC'.
          PERFORM batch_insert USING ''.
          PERFORM refresh.
        WHEN 'COPY'.
          IF gv_released IS NOT INITIAL.
            MESSAGE s000(oo) WITH '请求已释放，不能再修改'.
            RETURN.
          ENDIF.
          CALL SELECTION-SCREEN 1002 STARTING AT 10 5.
          CHECK sy-subrc = 0.
          PERFORM copy_tr.
          PERFORM refresh.
        WHEN 'PSET'.
          PERFORM batch_insert USING 'X'.
          PERFORM refresh.
        WHEN 'FC_CONF'.
          CALL SELECTION-SCREEN 1001 STARTING AT 10 5.
          CHECK sy-subrc = 0.
          PERFORM set_scr_para_def USING '1001' 'S'.
        WHEN 'FC_DELE'.
          PERFORM delete_row.
          PERFORM refresh.
        WHEN 'FC_DELT'.
          PERFORM delete_tr.
          LEAVE PROGRAM.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "handle_user_command

  METHOD handle_toolbar.
    PERFORM add_button USING e_object: '3' ' ' '       ' '' '   ',
                                       '0' ' ' 'FC_CONF' '@BX@' '配置',
                                       '3' ' ' '       ' '' '   ',
                                       '2' 'X' 'FC_OBJC' '@17@' '添加对象',
                                       '0' 'X' 'FC_DELE' '@18@' '',
                                       '3' ' ' '       ' '' '    ',
                                       '2' ' ' 'FC_TRAN' '@K4@' '传输',
                                       '3' ' ' '       ' '' '    ',
                                       '2' ' ' 'FC_TLOG' '@0P@' '日志',
                                       '3' ' ' '       ' '' '    ',
                                       '0' ' ' 'FC_DELT' '    ' '退出(删除请求)'.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_menu_button .
    DATA lv_numc TYPE numc2.
    DATA lv_code TYPE char10.
    DATA lv_text TYPE string.

    CASE e_ucomm.
      WHEN 'FC_OBJC'.
        LOOP AT cs_objt INTO cs_objt.
          READ TABLE gt_obj INTO gt_obj WITH KEY pgmid = 'R3TR'
                                                 object = cs_objt-low.
          IF sy-subrc = 0.
            CONCATENATE cs_objt-low gt_obj-text INTO lv_text SEPARATED BY ''.
            PERFORM button_add_func USING e_object cs_objt-low lv_text.
          ENDIF.
        ENDLOOP.
        PERFORM button_add_func USING e_object '----' '-------------------------'.
        PERFORM button_add_func USING e_object 'COPY' '从请求/任务复制'.
        PERFORM button_add_func USING e_object 'BATC' '批量粘贴多种类型'.
        IF cs_pset[] IS NOT INITIAL.
          PERFORM button_add_func USING e_object 'PSET' '预设对象'.
        ENDIF.
      WHEN 'FC_TRAN'.
        LOOP AT cs_dest INTO cs_dest.
          lv_numc = sy-tabix.
          CONCATENATE 'FCTR' lv_numc INTO lv_code.
          PERFORM button_add_func USING e_object lv_code cs_dest-low.
        ENDLOOP.
      WHEN 'FC_TLOG'.
        LOOP AT gt_log INTO gt_log.
          lv_numc = sy-tabix.
          CONCATENATE 'LOG' lv_numc INTO lv_code.
          PERFORM button_add_func USING e_object lv_code gt_log.
        ENDLOOP.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.                    "handle_menu_button
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

INITIALIZATION.
  PERFORM set_scr_para_def USING '1001' 'R'.
  IF cs_objt[] IS INITIAL.
    APPEND 'IEQDOMA' TO cs_objt.
    APPEND 'IEQDTEL' TO cs_objt.
    APPEND 'IEQTABL' TO cs_objt.
    APPEND 'IEQVIEW' TO cs_objt.
    APPEND 'IEQTTYP' TO cs_objt.
    APPEND 'IEQSHLP' TO cs_objt.
    APPEND 'IEQENQU' TO cs_objt.
    APPEND 'IEQPROG' TO cs_objt.
    APPEND 'IEQFUGR' TO cs_objt.
    APPEND 'IEQCLAS' TO cs_objt.
    APPEND 'IEQTRAN' TO cs_objt.
    APPEND 'IEQNROB' TO cs_objt.
  ENDIF.

  CALL FUNCTION 'TR_OBJECT_TABLE'
    TABLES
      wt_object_text = gt_obj.

  PERFORM catset1 TABLES gt_fldct
                  USING: 'TRKORR  ' 'E071' 'TRKORR  ' '',
                         'PGMID   ' 'E071' 'PGMID   ' '',
                         'OBJECT  ' 'E071' 'OBJECT  ' '',
                         'OBJ_NAME' 'E071' 'OBJ_NAME' ''.

AT SELECTION-SCREEN OUTPUT.
  %_cp_targ_%_app_%-text = '目标系统'.
  %_cs_objt_%_app_%-text = '对象'.
  %_cs_dest_%_app_%-text = 'RFC连接'.
  %_cs_pset_%_app_%-text = '预设对象列表'.
  %_cp_srcs_%_app_%-text = '传输完毕更改原始系统'.
  %_cp_delt_%_app_%-text = '传输成功删除目标系统STMS请求'.
  %_s_trkor_%_app_%-text = '请求/任务'.
  PERFORM selscr_exfcode USING '%_CSP' 'GET SPOS'.

START-OF-SELECTION.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'M100'.
  SET TITLEBAR '003' OF PROGRAM 'SAPLKKBL'
                     WITH '通过副本请求推送对象到其他系统'.

  IF gr_grid IS INITIAL.
    CREATE OBJECT go_cont
      EXPORTING
        container_name = 'CON1'.
    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = go_cont.

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_double_click FOR gr_grid.
    SET HANDLER event_receiver->handle_user_command FOR gr_grid.
    SET HANDLER event_receiver->handle_toolbar      FOR gr_grid.
    SET HANDLER event_receiver->handle_menu_button  FOR gr_grid.

    APPEND cl_gui_alv_grid=>mc_mb_variant TO gt_func.
    APPEND cl_gui_alv_grid=>mc_mb_subtot  TO gt_func.
    APPEND cl_gui_alv_grid=>mc_mb_export  TO gt_func.
    APPEND cl_gui_alv_grid=>mc_mb_sum     TO gt_func.
    APPEND cl_gui_alv_grid=>mc_mb_view    TO gt_func.
    APPEND cl_gui_alv_grid=>mc_fc_info    TO gt_func.
    APPEND cl_gui_alv_grid=>mc_mb_filter  TO gt_func.
    APPEND cl_gui_alv_grid=>mc_fc_print   TO gt_func.
    APPEND cl_gui_alv_grid=>mc_fc_graph   TO gt_func.

    gs_slayt-zebra = 'X'.
    gs_slayt-sel_mode = 'D'.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        i_save                        = 'A'
        is_layout                     = gs_slayt
        it_toolbar_excluding          = gt_func[]
      CHANGING
        it_outtab                     = gt_out[]
        it_fieldcatalog               = gt_fldct[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*& ALV双击
*&---------------------------------------------------------------------*
FORM grid_double_clik USING pv_row pv_column ps_row_no TYPE lvc_s_roid.
  READ TABLE gt_out INDEX ps_row_no-row_id.
  IF sy-subrc = 0.
    CASE pv_column.
      WHEN 'TRKORR'.
        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = gv_trkor.
      WHEN OTHERS.
        CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
          EXPORTING
            iv_pgmid          = gt_out-pgmid
            iv_object         = gt_out-object
            iv_obj_name       = gt_out-obj_name
            iv_action         = 'SHOW'
          EXCEPTIONS
            jump_not_possible = 1
            OTHERS            = 2.
    ENDCASE.
  ENDIF.
ENDFORM. " grid_double_clik

*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM catset1 TABLES t_fldcat USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldct TYPE lvc_s_fcat.

  ls_fldct-fieldname  = pv_field.
  ls_fldct-reptext    = pv_text.
  ls_fldct-coltext    = pv_text.
  ls_fldct-colddictxt = 'R'.
  ls_fldct-selddictxt = 'R'.
  ls_fldct-ref_table  = pv_reftab.
  ls_fldct-ref_field  = pv_reffld.

  CASE ls_fldct-fieldname.
    WHEN 'TRKORR'.
      ls_fldct-outputlen = 12.
    WHEN 'PGMID'.
      ls_fldct-outputlen = 6.
    WHEN 'OBJ_NAME'.
      ls_fldct-outputlen = 45.
    WHEN OTHERS.
  ENDCASE.

  APPEND ls_fldct TO t_fldcat.
  CLEAR ls_fldct.
ENDFORM.                    "catset1

*&---------------------------------------------------------------------*
*&  选择界面保存默认值
*&---------------------------------------------------------------------*
FORM set_scr_para_def USING pv_dynnr pv_method.
  DATA lt_rsscr TYPE TABLE OF rsscr WITH HEADER LINE.
  DATA lt_paras TYPE TABLE OF char10 WITH HEADER LINE.
  DATA lv_strfd TYPE bf4indx-strfd.
  DATA lv_subrc TYPE sy-subrc.
  DATA lv_fnumb TYPE i.
  DATA lv_tnumb TYPE i.
  DATA lv_fsstr TYPE string.
  DATA ls_scr   TYPE rsscr.

  lv_fnumb = pv_dynnr * 1000 + 1.
  lv_tnumb = pv_dynnr * 1000 + 999.
  CONCATENATE sy-repid sy-uname sy-mandt INTO lv_strfd.
  PERFORM load_sscr(rsdbrunt) TABLES   lt_rsscr
                              USING    sy-repid
                              CHANGING lv_subrc.
  LOOP AT lt_rsscr WHERE numb BETWEEN lv_fnumb AND lv_tnumb AND
                         kind CA 'PS' AND
                         flag1 NE '20'. "NO-DISPLAY
    READ TABLE lt_rsscr INTO ls_scr WITH KEY miscell = lt_rsscr-name.
    IF sy-subrc = 0.
      lv_fsstr = ls_scr-name.
    ELSE.
      CONCATENATE '%_' lt_rsscr-name '_%_app_%-text' INTO lv_fsstr.
    ENDIF.

    IF lt_rsscr-kind = 'P'.
      lt_paras  = lt_rsscr-name.
    ELSE.
      CONCATENATE lt_rsscr-name '[]' INTO lt_paras.
    ENDIF.
    COLLECT lt_paras.
  ENDLOOP.
  CHECK lt_paras[] IS NOT INITIAL.

  CASE pv_method.
    WHEN 'S'.
      CHECK lt_paras[] IS NOT INITIAL.
      EXPORT (lt_paras) TO DATABASE bf4indx(zs) ID lv_strfd.
      MESSAGE s000(oo) WITH 'Saved'.
    WHEN 'R'.
      TRY .
          IMPORT (lt_paras) FROM DATABASE bf4indx(zs) ID lv_strfd.
        CATCH cx_root.
      ENDTRY.
  ENDCASE.
ENDFORM.                    "set_scr_para_def

*&---------------------------------------------------------------------*
*& 选择屏幕屏蔽按钮
*&---------------------------------------------------------------------*
FORM selscr_exfcode USING pv_status pv_exfcode.
  DATA: lt_exttab TYPE slis_t_extab.

  SPLIT pv_exfcode AT space INTO TABLE lt_exttab.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = pv_status
    TABLES
      p_exclude = lt_exttab.
ENDFORM. "selscreen_ext

*&---------------------------------------------------------------------*
*&      Form  button_add_func
*&---------------------------------------------------------------------*
FORM button_add_func USING pr_ctmenu TYPE REF TO cl_ctmenu
                           pv_code pv_text.
  DATA: lv_fcode TYPE ui_func,
        lv_text	 TYPE gui_text.

  lv_fcode = pv_code.
  lv_text  = pv_text.
  pr_ctmenu->add_function( EXPORTING icon  = icon_okay
                                     fcode = lv_fcode
                                     text  = lv_text ).
ENDFORM.                    "button_add_func

*&---------------------------------------------------------------------*
*& ALV自带工具栏添加按钮
*&---------------------------------------------------------------------*
FORM add_button USING pr_object TYPE REF TO cl_alv_event_toolbar_set
                      pv_type pv_disa pv_func pv_icon pv_text .
  DATA: ls_toolbar  TYPE stb_button.

  CLEAR ls_toolbar.
  ls_toolbar-butn_type = pv_type.
  ls_toolbar-function  = pv_func.
  ls_toolbar-icon      = pv_icon.
  ls_toolbar-text      = pv_text.
  IF pv_disa = 'X'.
    ls_toolbar-disabled  = gv_released.
  ENDIF.
  APPEND ls_toolbar TO pr_object->mt_toolbar.
ENDFORM.                    "add_button

*&---------------------------------------------------------------------*
*& 刷新ALV
*&---------------------------------------------------------------------*
FORM refresh.
  DATA ls_stable TYPE lvc_s_stbl.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_out
    FROM e071
    WHERE trkorr = gv_trkor ORDER BY PRIMARY KEY.

  ls_stable-row = 'X'.
  ls_stable-col = 'X'.
  CALL METHOD gr_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.
ENDFORM.                    "refresh

*&---------------------------------------------------------------------*
*& 创建副本请求
*&---------------------------------------------------------------------*
FORM create_tr USING pv_target CHANGING cv_trkor.
  DATA ls_header  TYPE trwbo_request_header.

  CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
    EXPORTING
      iv_type           = 'T'
      iv_text           = 'Push Object'
      iv_target         = pv_target
    IMPORTING
      es_request_header = ls_header
    EXCEPTIONS
      insert_failed     = 1
      enqueue_failed    = 2
      OTHERS            = 3.
  IF sy-subrc = 0.
    cv_trkor = ls_header-trkorr.
    COMMIT WORK.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4  .
  ENDIF.
ENDFORM.                    "createtr

*&---------------------------------------------------------------------*
*& 请求添加单一类型对象
*&---------------------------------------------------------------------*
FORM insert_obj USING pv_object.
  DATA lt_list  TYPE TABLE OF trobj_name WITH HEADER LINE.
  DATA lt_e071  TYPE TABLE OF e071 WITH HEADER LINE.
  DATA lt_e071k TYPE TABLE OF e071k WITH HEADER LINE.
  DATA lv_title TYPE string.
  DATA lv_mark  TYPE c.
  DATA lv_don   TYPE c.

  IF gv_released IS NOT INITIAL.
    MESSAGE s000(oo) WITH '请求已释放，不能再修改'.
    RETURN.
  ENDIF.

  IF gv_trkor IS INITIAL.
    PERFORM create_tr USING cp_targ CHANGING gv_trkor.
  ENDIF.

  lv_title = pv_object.
  CALL FUNCTION 'ISU_POPUP_TEXT_EDIT'
    EXPORTING
      x_height      = 14
      x_title       = lv_title
    CHANGING
      xy_texttab    = lt_list[]
    EXCEPTIONS
      general_fault = 1
      OTHERS        = 2.
  DELETE lt_list WHERE table_line = ''.
  CHECK lt_list[] IS NOT INITIAL.

  lt_e071-pgmid  = 'R3TR'.
  lt_e071-object = pv_object.
  LOOP AT lt_list.
    TRANSLATE lt_list TO UPPER CASE.
    CONDENSE lt_list.
    lt_e071-obj_name = lt_list.
    APPEND lt_e071.
  ENDLOOP.

  PERFORM append_objects_to_request(saplstrv)
          USING lt_e071[] gv_trkor lv_mark lv_don.
ENDFORM.                    "insert_obj

*&---------------------------------------------------------------------*
*& 请求批量添加不同类型对象
*&---------------------------------------------------------------------*
FORM batch_insert USING pv_default.
  DATA lt_list  TYPE TABLE OF trobj_name WITH HEADER LINE.
  DATA lt_e071  TYPE TABLE OF e071 WITH HEADER LINE.
  DATA lt_e071k TYPE TABLE OF e071k WITH HEADER LINE.
  DATA lv_title TYPE string.
  DATA lv_mark  TYPE c.
  DATA lv_don   TYPE c.

  IF gv_released IS NOT INITIAL.
    MESSAGE s000(oo) WITH '请求已释放，不能再修改'.
    RETURN.
  ENDIF.

  IF gv_trkor IS INITIAL.
    PERFORM create_tr USING cp_targ CHANGING gv_trkor.
  ENDIF.

  IF pv_default = ''.
    lv_title = '对象类型+对象值，可以使用空格、制表符隔开或者不隔'.
    CALL FUNCTION 'ISU_POPUP_TEXT_EDIT'
      EXPORTING
        x_height      = 14
        x_title       = lv_title
      CHANGING
        xy_texttab    = lt_list[]
      EXCEPTIONS
        general_fault = 1
        OTHERS        = 2.
    DELETE lt_list WHERE table_line = ''.
  ELSE.
    LOOP AT cs_pset INTO cs_pset.
      lt_list = cs_pset-low.
      APPEND lt_list.
    ENDLOOP.
  ENDIF.
  CHECK lt_list[] IS NOT INITIAL.

  LOOP AT lt_list.
    REPLACE ALL OCCURRENCES OF %_horizontal_tab IN lt_list WITH ''.
    TRANSLATE lt_list TO UPPER CASE.
    CONDENSE lt_list(10) NO-GAPS.
    lt_e071-pgmid    = lt_list(4).
    lt_e071-object   = lt_list+4(4).
    lt_e071-obj_name = lt_list+8.
    APPEND lt_e071.
  ENDLOOP.

  PERFORM append_objects_to_request(saplstrv)
          USING lt_e071[] gv_trkor lv_mark lv_don.
ENDFORM.                    "batch_insert

*&---------------------------------------------------------------------*
*& 复制请求/任务条目
*&---------------------------------------------------------------------*
FORM copy_tr.
  IF gv_trkor IS INITIAL.
    PERFORM create_tr USING cp_targ CHANGING gv_trkor.
  ENDIF.

  CHECK s_trkor[] IS NOT INITIAL.
  LOOP AT s_trkor.
    CALL FUNCTION 'TR_COPY_COMM'
      EXPORTING
        wi_dialog                = ''
        wi_trkorr_from           = s_trkor-low
        wi_trkorr_to             = gv_trkor
        wi_without_documentation = 'X'
      EXCEPTIONS
        db_access_error          = 1
        trkorr_from_not_exist    = 2
        trkorr_to_is_repair      = 3
        trkorr_to_locked         = 4
        trkorr_to_not_exist      = 5
        trkorr_to_released       = 6
        user_not_owner           = 7
        no_authorization         = 8
        wrong_client             = 9
        wrong_category           = 10
        object_not_patchable     = 11.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.
  CALL FUNCTION 'TR_SORT_AND_COMPRESS_COMM'
    EXPORTING
      iv_trkorr = gv_trkor
    EXCEPTIONS
      OTHERS    = 5.
  COMMIT WORK.
ENDFORM.                    "copy_tr

*&---------------------------------------------------------------------*
*& 删除请求的行
*&---------------------------------------------------------------------*
FORM delete_row.
  DATA lt_srows TYPE lvc_t_roid WITH HEADER LINE.

  IF gv_released IS NOT INITIAL.
    MESSAGE s000(oo) WITH '请求已释放，不能再修改'.
  ELSE.
    gr_grid->get_selected_rows( IMPORTING et_row_no = lt_srows[] ).
    LOOP AT lt_srows.
      READ TABLE gt_out INDEX lt_srows-row_id.
      DELETE FROM e071 WHERE trkorr = gt_out-trkorr AND
                             as4pos = gt_out-as4pos.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "delete_row

*&---------------------------------------------------------------------*
*& 释放请求
*&---------------------------------------------------------------------*
FORM release_tr.
  DATA: ls_retn TYPE bapiret2.

  CALL FUNCTION 'TRINT_RELEASE_REQUEST'
    EXPORTING
      iv_trkorr                = gv_trkor
      iv_dialog                = ''
      iv_as_background_job     = ''
      iv_success_message       = 'X'
      iv_without_objects_check = 'X'
      iv_without_locking       = 'X'
      iv_display_export_log    = ''
    EXCEPTIONS
      OTHERS                   = 18.
  IF sy-subrc = 0.
    DO 20 TIMES.
      WAIT UP TO 1 SECONDS.
      SELECT SINGLE * FROM e070
        WHERE trkorr = gv_trkor AND
              trstatus IN ('R','N').
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
    ENDDO.
    gv_released = 'X'.
    MESSAGE s000(oo) WITH '已释放'.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4  .
  ENDIF.
ENDFORM.                    "release_tr

*&---------------------------------------------------------------------*
*& 传输请求
*&---------------------------------------------------------------------*
FORM trans_tr USING pv_ucomm.
  DATA lv_tabix TYPE sy-tabix.
  DATA lv_rdest TYPE rfcdest.
  DATA lv_trdir TYPE char255.
  DATA lv_lfile TYPE char255.
  DATA lv_cfile TYPE xstring.
  DATA lv_dfile TYPE xstring.
  DATA lv_rcode TYPE stpa-retcode.

  IF gt_out[] IS INITIAL.
    MESSAGE s000(oo) WITH '列表为空'.
    RETURN.
  ENDIF.

  IF gv_released = ''.
    PERFORM release_tr.
  ENDIF.

  lv_tabix = pv_ucomm+4.
  READ TABLE cs_dest INDEX lv_tabix.
  lv_rdest = cs_dest-low.

  CALL FUNCTION 'RSPO_R_SAPGPARAM'
    EXPORTING
      name   = 'DIR_TRANS'
    IMPORTING
      value  = lv_trdir
    EXCEPTIONS
      error  = 1
      OTHERS = 0.
  IF lv_trdir IS INITIAL.
    MESSAGE e000(oo) WITH 'DIR_TRANS目录未找到'.
  ENDIF.

  CONCATENATE lv_trdir '/data/R ' gv_trkor+4 '.' gv_trkor(3) INTO lv_lfile.
  OPEN DATASET lv_lfile FOR INPUT IN BINARY MODE.
  IF sy-subrc = 0.
    READ DATASET lv_lfile INTO lv_dfile.
    CLOSE DATASET lv_lfile.
  ELSE.
    MESSAGE e000(oo) WITH '读取Data文件失败'.
  ENDIF.

  CONCATENATE lv_trdir '/cofiles/K ' gv_trkor+4 '.' gv_trkor(3) INTO lv_lfile.
  OPEN DATASET lv_lfile FOR INPUT IN BINARY MODE.
  IF sy-subrc = 0.
    READ DATASET lv_lfile INTO lv_cfile.
    CLOSE DATASET lv_lfile.
  ELSE.
    MESSAGE e000(oo) WITH '读取控制文件失败'.
  ENDIF.

  READ TABLE gt_log WITH KEY table_line = lv_rdest.
  IF sy-subrc NE 0.
    APPEND lv_rdest TO gt_log.
  ENDIF.

  PERFORM create_rfclink(zremotelogin) CHANGING lv_rdest.
  CALL FUNCTION 'ZTRANSFER_REQUEST' DESTINATION lv_rdest
    EXPORTING
      trkorr         = gv_trkor
      defbf          = cp_delt
      srcsystem      = cp_srcs
      cofile         = lv_cfile
      datafile       = lv_dfile
    IMPORTING
      ev_tp_ret_code = lv_rcode.
  PERFORM delete_rfclink(zremotelogin) IF FOUND USING lv_rdest.
  IF lv_rcode LE 4.
    MESSAGE s000(oo) WITH '成功'.
  ELSE.
    MESSAGE s000(oo) WITH '失败' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " TRANS_TR

*&---------------------------------------------------------------------*
*& 传输日志
*&---------------------------------------------------------------------*
FORM trans_log USING pv_ucomm.
  DATA lv_tabix TYPE sy-tabix.
  DATA lv_rdest TYPE rfcdest.

  CHECK gv_trkor IS NOT INITIAL.

  lv_tabix = pv_ucomm+4.
  READ TABLE gt_log INDEX lv_tabix.
  lv_rdest = gt_log.

  PERFORM create_rfclink(zremotelogin) CHANGING lv_rdest.
  CALL FUNCTION 'ZTRANSFER_VIEW_LOG' DESTINATION lv_rdest
    EXPORTING
      iv_trokr = gv_trkor.
  PERFORM delete_rfclink(zremotelogin) IF FOUND USING lv_rdest.
ENDFORM.                    "trans_log

*&---------------------------------------------------------------------*
*& 删除请求
*&---------------------------------------------------------------------*
FORM delete_tr.
  CHECK gv_trkor IS NOT INITIAL.

  UPDATE e070 SET trstatus = 'D'
    WHERE trkorr = gv_trkor.
  COMMIT WORK.

  CALL FUNCTION 'TR_DELETE_COMM'
    EXPORTING
      wi_dialog = ''
      wi_trkorr = gv_trkor
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc = 0.
    MESSAGE s000(oo) WITH gv_trkor '已删除'.
  ELSE.
    MESSAGE s000(oo) WITH gv_trkor '删除失败' DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.                    "delete_tr
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA:ok_code LIKE sy-ucomm.
  DATA:save_ok LIKE sy-ucomm.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&F03' OR '&F15' OR '&F12'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR:sy-ucomm.
ENDMODULE.
