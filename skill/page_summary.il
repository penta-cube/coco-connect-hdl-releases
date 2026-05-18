;;; page_summary.il - drawing/page summary for coco-connect-hdl
;;;
;;; Depends on util.il, hdl.il, and read_model.il. The confirmed DE-HDL 22.1
;;; API can enumerate drawing metadata directly. Design-wide page summaries are
;;; built by traversing pages with nmapedit and then collecting page-scoped
;;; Select/find results immediately after each page switch.

(defun _coco_json_number_field (name value)
  (_coco_json_field name (safeStr value)))

(defun _coco_json_bool_field (name value)
  (_coco_json_field name (if value "true" "false")))

(defun _coco_json_null_field (name)
  (_coco_json_field name "null"))

(defun _coco_dwg_string (dwg accessor fallback_idx)
  (let ((value ""))
    (when dwg
      (setq value (safeStr (_coco_safe_apply accessor (list dwg))))
      (when (equal value "")
        (setq value (safeStr (_coco_nth fallback_idx dwg)))))
    value))

(defun _coco_dwg_id (dwg)
  (safeStr (_coco_nth 0 dwg)))

(defun _coco_dwg_name (dwg)
  (_coco_dwg_string dwg 'cnGetDwgName 1))

(defun _coco_dwg_full_name (dwg)
  (_coco_dwg_string dwg 'cnGetDwgFullName 1))

(defun _coco_dwg_lib_name (dwg)
  (_coco_dwg_string dwg 'cnGetDwgLibName 6))

(defun _coco_dwg_cell_name (dwg)
  (_coco_dwg_string dwg 'cnGetDwgCellName 7))

(defun _coco_dwg_view_name (dwg)
  (_coco_dwg_string dwg 'cnGetDwgViewName 8))

(defun _coco_dwg_page (dwg)
  (_coco_dwg_string dwg 'cnGetDwgPage 9))

(defun _coco_dwg_page_number (dwg)
  (let ((value (_coco_nth 9 dwg)))
    (if value value 1)))

(defun _coco_dwg_kind (dwg)
  (safeStr (_coco_nth 10 dwg)))

(defun _coco_dwg_total_pages (dwg)
  (let ((value (_coco_nth 11 dwg)))
    (if value value 1)))

(defun _coco_dwg_same_page (left right)
  (and
    (equal (_coco_dwg_full_name left) (_coco_dwg_full_name right))
    (equal (_coco_dwg_page left) (_coco_dwg_page right))))

(defun _coco_dwg_page_full_name (dwg page_num)
  (let ((total (_coco_dwg_total_pages dwg)))
    (cond
      ((equal total 1) (_coco_dwg_full_name dwg))
      ((equal page_num 1) (safeStr (_coco_nth 1 dwg)))
      ((equal page_num total) (safeStr (_coco_nth 2 dwg)))
      (t ""))))

(defun _coco_dwg_info_active_page (dwg_info)
  (let ((active_page nil)
        (head ""))
    (foreach item dwg_info
      (when (and (listp item) (not active_page))
        (setq head (safeStr (car item)))
        (when (equal head "active")
          (setq active_page (_coco_nth 1 item)))))
    (if active_page active_page 1)))

(defun _coco_dwg_resolved_active_page (dwg active_page_hint)
  (if (greaterp (_coco_dwg_total_pages dwg) 1)
    active_page_hint
    (_coco_dwg_page_number dwg)))

(defun _coco_dwg_page_is_active (page_num active_page_num)
  (equal (safeStr page_num) (safeStr active_page_num)))

(defun _coco_port_symbol_p (symbol_name)
  (let ((value (upperCase (safeStr symbol_name))))
    (or (equal value "PORT_IN")
        (equal value "PORT_OUT")
        (equal value "PORT_IO")
        (equal value "PORT")
        (rexMatchp "^PORT_" value)
        (rexMatchp "OFFPAGE" value))))

(defun _coco_unique_symbol_names (tuples)
  (let ((names nil)
        (name ""))
    (foreach tuple tuples
      (setq name (safeStr (_coco_nth 1 tuple)))
      (when (and (not (equal name ""))
                 (not (_coco_member_string name names)))
        (setq names (append names (list name)))))
    names))

(defun _coco_port_count_from_tuples (tuples)
  (let ((count 0))
    (foreach tuple tuples
      (when (_coco_port_symbol_p (_coco_nth 1 tuple))
        (setq count (add1 count))))
    count))

(defun _coco_page_name_for_number (handle dwg page_num)
  (let ((found "")
        (page_names (_coco_design_page_names handle dwg)))
    (foreach page_name page_names
      (when (and (equal found "")
                 (equal (_coco_page_number_from_name page_name) (safeStr page_num)))
        (setq found page_name)))
    found))

(defun _coco_page_scope_counts_json (handle page_name)
  (let ((component_tuples nil)
        (net_names nil)
        (symbol_names nil)
        (port_count 0))
    (_coco_send_cmd handle (sprintf nil "nmapedit %s;" page_name))
    (setq component_tuples (_coco_component_tuples handle))
    (setq net_names (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
    (setq symbol_names (_coco_unique_symbol_names component_tuples))
    (setq port_count (_coco_port_count_from_tuples component_tuples))
    (_coco_json_object
      (list
        (_coco_json_number_field "component_count" (length component_tuples))
        (_coco_json_number_field "net_count" (length net_names))
        (_coco_json_number_field "symbol_count" (length symbol_names))
        (_coco_json_number_field "port_count" port_count)
        (_coco_json_bool_field "counts_available" t)
        (_coco_json_string_field "scope" "design_wide_page_traversal")
        (_coco_json_string_field "component_count_status" "nmapedit_scoped")
        (_coco_json_string_field "net_count_status" "nmapedit_scoped")
        (_coco_json_string_field "symbol_count_status" "nmapedit_scoped")
        (_coco_json_string_field "port_count_status" "symbol_name_heuristic")))))

(defun _coco_unavailable_counts_json ()
  (_coco_json_object
    (list
      (_coco_json_null_field "component_count")
      (_coco_json_null_field "net_count")
      (_coco_json_null_field "symbol_count")
      (_coco_json_null_field "port_count")
      (_coco_json_bool_field "counts_available" nil)
      (_coco_json_string_field "scope" "unresolved_page_list")
      (_coco_json_string_field "status" "page_file_list_required"))))

(defun _coco_dwg_summary_json (handle dwg active_dwg counts_json)
  (let ((is_active nil)
        (page_counts_json counts_json))
    (setq is_active (_coco_dwg_same_page dwg active_dwg))
    (_coco_json_object
      (list
        (_coco_json_string_field "id" (_coco_dwg_id dwg))
        (_coco_json_string_field "name" (_coco_dwg_name dwg))
        (_coco_json_string_field "full_name" (_coco_dwg_full_name dwg))
        (_coco_json_string_field "lib_name" (_coco_dwg_lib_name dwg))
        (_coco_json_string_field "cell_name" (_coco_dwg_cell_name dwg))
        (_coco_json_string_field "view_name" (_coco_dwg_view_name dwg))
        (_coco_json_string_field "page" (_coco_dwg_page dwg))
        (_coco_json_string_field "kind" (_coco_dwg_kind dwg))
        (_coco_json_bool_field "active" is_active)
        (_coco_json_field "counts" page_counts_json)))))

(defun _coco_dwg_page_summary_json (handle dwg active_page_num page_num page_full_name counts_json)
  (let ((is_active nil)
        (page_counts_json counts_json))
    (setq is_active (_coco_dwg_page_is_active page_num active_page_num))
    (_coco_json_object
      (list
        (_coco_json_string_field "id" (strcat (_coco_dwg_id dwg) ":page" (safeStr page_num)))
        (_coco_json_string_field "name" page_full_name)
        (_coco_json_string_field "full_name" page_full_name)
        (_coco_json_string_field "lib_name" (_coco_dwg_lib_name dwg))
        (_coco_json_string_field "cell_name" (_coco_dwg_cell_name dwg))
        (_coco_json_string_field "view_name" (_coco_dwg_view_name dwg))
        (_coco_json_string_field "page" (safeStr page_num))
        (_coco_json_string_field "kind" (_coco_dwg_kind dwg))
        (_coco_json_bool_field "active" is_active)
        (_coco_json_field "counts" page_counts_json)))))

(defun _coco_page_counts_pair (page_name counts_json)
  (list page_name counts_json))

(defun _coco_page_counts_name (entry)
  (car entry))

(defun _coco_page_counts_json (entry)
  (_coco_nth 1 entry))

(setq _coco_page_counts_acc nil)

(defun _coco_collect_page_counts_item (handle page_name)
  (setq _coco_page_counts_acc
    (append _coco_page_counts_acc
      (list (_coco_page_counts_pair page_name (_coco_page_scope_counts_json handle page_name))))))

(defun _coco_collect_page_counts (handle dwg original_page)
  (setq _coco_page_counts_acc nil)
  (_coco_each_design_page handle dwg original_page '_coco_collect_page_counts_item)
  _coco_page_counts_acc)

(defun _coco_find_page_counts (entries page_name)
  (let ((found nil))
    (foreach entry entries
      (when (and (not found)
                 (equal (_coco_page_counts_name entry) page_name))
        (setq found (_coco_page_counts_json entry))))
    found))

(defun _coco_resolve_page_counts (handle page_counts page_name)
  (let ((counts_json (_coco_find_page_counts page_counts page_name)))
    (if counts_json
      counts_json
      (_coco_unavailable_counts_json))))

(defun _coco_append_expanded_dwg_pages (items handle dwg active_dwg active_page_num page_counts)
  (let ((page_names (_coco_design_page_names handle dwg))
        (page_full_name "")
        (page_num "")
        (counts_json ""))
    (if page_names
      (foreach page_full_name page_names
        (setq page_num (_coco_page_number_from_name page_full_name))
        (setq counts_json (_coco_resolve_page_counts handle page_counts page_full_name))
        (setq items
          (append items
            (list (_coco_dwg_page_summary_json handle dwg active_page_num page_num page_full_name counts_json))))
        )
      (progn
        (setq page_full_name (_coco_dwg_full_name dwg))
        (setq counts_json (_coco_resolve_page_counts handle page_counts page_full_name))
        (setq items (append items (list (_coco_dwg_summary_json handle dwg active_dwg counts_json))))))
    items))

(defun _coco_dwgs_json (handle active_dwg active_page_num page_counts)
  (let ((dwg_info nil)
        (dwgs nil)
        (items nil))
    (setq dwg_info (_coco_safe_apply 'cnGetDwgInfo (list handle)))
    (when dwg_info
      (setq dwgs (_coco_safe_apply 'cnGetDwgs (list dwg_info))))
    (foreach dwg dwgs
      (setq items (_coco_append_expanded_dwg_pages items handle dwg active_dwg active_page_num page_counts)))
    (_coco_json_array items)))

(defun _coco_page_summary_limitations_json ()
  (_coco_json_array
    (list
      (_coco_json_string "page counts are collected by traversing pages with nmapedit")
      (_coco_json_string "page traversal is disabled when page*.csa/csb files cannot be resolved from the current view")
      (_coco_json_string "port count is derived from port/offpage-style symbol names and may need refinement"))))

(defun _coco_page_summary_diagnostics_json (active_dwg active_page_hint active_page_num)
  (_coco_json_object
    (list
      (_coco_json_string_field "active_hint_from_dwg_info" (safeStr active_page_hint))
      (_coco_json_string_field "resolved_active_page" (safeStr active_page_num))
      (_coco_json_string_field "active_dwg_tuple_page" (_coco_dwg_page active_dwg))
      (_coco_json_string_field "active_dwg_total_pages" (safeStr (_coco_dwg_total_pages active_dwg)))
      (_coco_json_string_field "active_dwg_full_name" (_coco_dwg_full_name active_dwg)))))

(defun _coco_op_page_summary ()
  (let ((handle (_coco_import_handle))
        (dwg_info nil)
        (active_dwg nil)
        (page_counts nil)
        (active_page_hint 1)
        (active_page_num 1)
        (active_page_name ""))
    (setq dwg_info (_coco_safe_apply 'cnGetDwgInfo (list handle)))
    (when dwg_info
      (setq active_dwg (_coco_safe_apply 'cnFindActiveDwg (list dwg_info))))
    (setq active_page_hint (_coco_dwg_info_active_page dwg_info))
    (setq active_page_num (_coco_dwg_resolved_active_page active_dwg active_page_hint))
    (setq active_page_name (_coco_page_name_for_number handle active_dwg active_page_num))
    (setq page_counts (_coco_collect_page_counts handle active_dwg active_page_name))
    (_coco_json_object
      (list
        (_coco_json_field "active_page"
          (_coco_dwg_page_summary_json
            handle
            active_dwg
            active_page_num
            active_page_num
            active_page_name
            (_coco_resolve_page_counts handle page_counts active_page_name)))
        (_coco_json_field "pages" (_coco_dwgs_json handle active_dwg active_page_num page_counts))
        (_coco_json_field "limitations" (_coco_page_summary_limitations_json))
        (_coco_json_field "diagnostics" (_coco_page_summary_diagnostics_json active_dwg active_page_hint active_page_num))))))
