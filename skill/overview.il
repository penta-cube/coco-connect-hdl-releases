;;; overview.il - fast design overview and targeted page statistics
;;;
;;; Depends on util.il, hdl.il, and read_model.il. design_overview only reads
;;; drawing metadata and view file names; it never switches pages. page_overview
;;; inspects one requested page and restores the original page when necessary.

(defun _coco_json_number_field (name value)
  (_coco_json_field name (safeStr value)))

(defun _coco_json_bool_field (name value)
  (_coco_json_field name (if value "true" "false")))

(defun _coco_overview_dwg_string (dwg accessor fallback_idx)
  (let ((value ""))
    (when dwg
      (setq value (safeStr (_coco_safe_apply accessor (list dwg))))
      (when (equal value "")
        (setq value (safeStr (_coco_nth fallback_idx dwg)))))
    value))

(defun _coco_overview_dwg_id (dwg)
  (safeStr (_coco_nth 0 dwg)))

(defun _coco_overview_dwg_full_name (dwg)
  (sprintf nil "@%s.%s(%s)"
    (_coco_overview_dwg_lib_name dwg)
    (_coco_overview_dwg_cell_name dwg)
    (_coco_overview_dwg_view_name dwg)))

(defun _coco_overview_dwg_lib_name (dwg)
  (_coco_overview_dwg_string dwg 'cnGetDwgLibName 6))

(defun _coco_overview_dwg_cell_name (dwg)
  (_coco_overview_dwg_string dwg 'cnGetDwgCellName 7))

(defun _coco_overview_dwg_view_name (dwg)
  (_coco_overview_dwg_string dwg 'cnGetDwgViewName 8))

(defun _coco_overview_dwg_kind (dwg)
  (safeStr (_coco_nth 10 dwg)))

(defun _coco_overview_dwg_total_pages (dwg)
  (let ((total (atoi (safeStr (_coco_nth 11 dwg)))))
    (if (greaterp total 0) total 1)))

(defun _coco_overview_page_names (handle dwg)
  (let ((page_names (_coco_design_page_names handle dwg))
        (page_num 1)
        (total (_coco_overview_dwg_total_pages dwg)))
    (when (not page_names)
      (while (leqp page_num total)
        (setq page_names
          (append page_names (list (_coco_page_canonical_name dwg page_num))))
        (setq page_num (add1 page_num))))
    page_names))

(defun _coco_overview_page_active_p (page_num active_page_num)
  (equal (safeStr page_num) (safeStr active_page_num)))

(defun _coco_overview_page_json (dwg page_name active_page_num)
  (let ((page_num (_coco_page_number_from_name page_name)))
    (_coco_json_object
      (list
        (_coco_json_string_field "id"
          (strcat (_coco_overview_dwg_id dwg) ":page" page_num))
        (_coco_json_string_field "name" (strcat "page" page_num))
        (_coco_json_string_field "full_name" page_name)
        (_coco_json_string_field "lib_name" (_coco_overview_dwg_lib_name dwg))
        (_coco_json_string_field "cell_name" (_coco_overview_dwg_cell_name dwg))
        (_coco_json_string_field "view_name" (_coco_overview_dwg_view_name dwg))
        (_coco_json_string_field "page" page_num)
        (_coco_json_string_field "kind" (_coco_overview_dwg_kind dwg))
        (_coco_json_bool_field "active"
          (_coco_overview_page_active_p page_num active_page_num))))))

(defun _coco_overview_pages_json (dwg page_names active_page_num)
  (let ((items nil))
    (foreach page_name page_names
      (setq items
        (append items
          (list (_coco_overview_page_json dwg page_name active_page_num)))))
    (_coco_json_array items)))

(defun _coco_overview_design_json (dwg page_names)
  (_coco_json_object
    (list
      (_coco_json_string_field "id" (_coco_overview_dwg_id dwg))
      (_coco_json_string_field "name" (_coco_overview_dwg_cell_name dwg))
      (_coco_json_string_field "full_name" (_coco_overview_dwg_full_name dwg))
      (_coco_json_string_field "lib_name" (_coco_overview_dwg_lib_name dwg))
      (_coco_json_string_field "cell_name" (_coco_overview_dwg_cell_name dwg))
      (_coco_json_string_field "view_name" (_coco_overview_dwg_view_name dwg))
      (_coco_json_string_field "kind" (_coco_overview_dwg_kind dwg))
      (_coco_json_number_field "page_count" (length page_names)))))

(defun _coco_project_design_json (dwg)
  (_coco_json_object
    (list
      (_coco_json_string_field "id" (_coco_overview_dwg_id dwg))
      (_coco_json_string_field "name" (_coco_overview_dwg_cell_name dwg))
      (_coco_json_string_field "full_name" (_coco_overview_dwg_full_name dwg))
      (_coco_json_string_field "lib_name" (_coco_overview_dwg_lib_name dwg))
      (_coco_json_string_field "cell_name" (_coco_overview_dwg_cell_name dwg))
      (_coco_json_string_field "view_name" (_coco_overview_dwg_view_name dwg)))))

(defun _coco_op_project_info ()
  (let ((handle (_coco_import_handle))
        (dwg nil))
    (setq dwg (_coco_active_dwg handle))
    (when (not dwg)
      (error "NO_ACTIVE_DESIGN" "No active HDL drawing was found"))
    (_coco_json_object
      (list
        (_coco_json_field "design" (_coco_project_design_json dwg))
        (_coco_json_string_field
          "view_source_path"
          (_coco_view_source_path handle dwg))))))

(defun _coco_op_design_overview ()
  (let ((handle (_coco_import_handle))
        (dwg nil)
        (active_page_num 1)
        (active_page_name "")
        (page_names nil))
    (setq dwg (_coco_active_dwg handle))
    (when (not dwg)
      (error "NO_ACTIVE_DESIGN" "No active HDL drawing was found"))
    (setq active_page_num (_coco_resolved_active_page_number handle dwg))
    (setq page_names (_coco_overview_page_names handle dwg))
    (setq active_page_name (_coco_page_canonical_name dwg active_page_num))
    (_coco_json_object
      (list
        (_coco_json_field "design" (_coco_overview_design_json dwg page_names))
        (_coco_json_field "active_page"
          (_coco_overview_page_json dwg active_page_name active_page_num))
        (_coco_json_field "pages"
          (_coco_overview_pages_json dwg page_names active_page_num))))))

(defun _coco_overview_port_symbol_p (symbol_name)
  (let ((value (upperCase (safeStr symbol_name))))
    (or (equal value "PORT_IN")
        (equal value "PORT_OUT")
        (equal value "PORT_IO")
        (equal value "PORT")
        (rexMatchp "^PORT_" value)
        (rexMatchp "OFFPAGE" value))))

(defun _coco_overview_unique_symbol_names (tuples)
  (let ((names nil)
        (name ""))
    (foreach tuple tuples
      (setq name (safeStr (_coco_nth 1 tuple)))
      (when (and (not (equal name ""))
                 (not (_coco_member_string name names)))
        (setq names (append names (list name)))))
    names))

(defun _coco_overview_port_count (tuples)
  (let ((count 0))
    (foreach tuple tuples
      (when (_coco_overview_port_symbol_p (_coco_nth 1 tuple))
        (setq count (add1 count))))
    count))

(defun _coco_overview_page_counts_json (handle page_name)
  (let ((component_tuples nil)
        (net_names nil)
        (symbol_names nil))
    (_coco_prepare_page_snapshot handle _coco_page_overview_active_dwg page_name)
    (setq component_tuples _coco_page_snapshot_component_tuples)
    (setq net_names _coco_page_snapshot_net_names)
    (setq symbol_names (_coco_overview_unique_symbol_names component_tuples))
    (_coco_json_object
      (list
        (_coco_json_number_field "component_count" (length component_tuples))
        (_coco_json_number_field "net_count" (length net_names))
        (_coco_json_number_field "symbol_count" (length symbol_names))
        (_coco_json_number_field "port_count"
          (_coco_overview_port_count component_tuples))
        (_coco_json_string_field "scope" "targeted_page_query")
        (_coco_json_string_field "port_count_status" "symbol_name_heuristic")))))

(defun _coco_overview_page_arg_number (arg)
  (let ((value (safeStr arg))
        (number ""))
    (cond
      ((rexMatchp "^[1-9][0-9]*$" value)
        (setq number value))
      ((rexMatchp "^page\\([1-9][0-9]*\\)$" (lowerCase value))
        (setq number (rexSubstitute "\\1")))
      (t
        (setq number (_coco_page_number_from_name value))))
    number))

(defun _coco_overview_resolve_page (handle dwg page_names arg)
  (let ((value (safeStr arg))
        (number "")
        (candidate ""))
    (if (equal value "")
      (_coco_resolved_active_page_name handle dwg)
      (progn
        (setq number (_coco_overview_page_arg_number value))
        (when (not (equal number ""))
          (setq candidate (_coco_page_canonical_name dwg number)))
        (if (member candidate page_names) candidate nil)))))

(defun _coco_overview_page_detail_json (dwg page_name active_page_num counts_json)
  (let ((page_num (_coco_page_number_from_name page_name)))
    (_coco_json_object
      (list
        (_coco_json_string_field "id"
          (strcat (_coco_overview_dwg_id dwg) ":page" page_num))
        (_coco_json_string_field "name" (strcat "page" page_num))
        (_coco_json_string_field "full_name" page_name)
        (_coco_json_string_field "lib_name" (_coco_overview_dwg_lib_name dwg))
        (_coco_json_string_field "cell_name" (_coco_overview_dwg_cell_name dwg))
        (_coco_json_string_field "view_name" (_coco_overview_dwg_view_name dwg))
        (_coco_json_string_field "page" page_num)
        (_coco_json_string_field "kind" (_coco_overview_dwg_kind dwg))
        (_coco_json_bool_field "active"
          (_coco_overview_page_active_p page_num active_page_num))
        (_coco_json_field "counts" counts_json)))))

(setq _coco_page_overview_active_dwg nil)

(defun _coco_op_page_overview (arg)
  (let ((handle (_coco_import_handle))
        (dwg nil)
        (page_names nil)
        (original_page "")
        (target_page "")
        (active_page_num 1)
        (switched nil)
        (counts_result nil)
        (counts_json nil))
    (setq dwg (_coco_active_dwg handle))
    (when (not dwg)
      (error "NO_ACTIVE_DESIGN" "No active HDL drawing was found"))
    (setq _coco_page_overview_active_dwg dwg)
    (setq page_names (_coco_overview_page_names handle dwg))
    (setq original_page (_coco_resolved_active_page_name handle dwg))
    (setq active_page_num (_coco_page_number_from_name original_page))
    (setq target_page (_coco_overview_resolve_page handle dwg page_names arg))
    (when (not target_page)
      (error "PAGE_NOT_FOUND" (strcat "Page not found: " (safeStr arg))))
    (when (not (equal target_page original_page))
      (_coco_send_cmd handle (sprintf nil "nmapedit %s;" target_page))
      (setq switched t))
    (setq counts_result
      (errset (_coco_overview_page_counts_json handle target_page) t))
    (when switched
      (errset (_coco_send_cmd handle (sprintf nil "nmapedit %s;" original_page)) t))
    (when (not counts_result)
      (error "PAGE_OVERVIEW_FAILED"
        (strcat "Failed to inspect page: " target_page)))
    (setq counts_json (car counts_result))
    (_coco_json_object
      (list
        (_coco_json_field "page"
          (_coco_overview_page_detail_json
            dwg target_page active_page_num counts_json))
        (_coco_json_field "limitations"
          (_coco_json_array
            (list
              (_coco_json_string
                "port_count is inferred from port/offpage-style symbol names"))))))))
