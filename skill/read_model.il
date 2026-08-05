;;; read_model.il - common read model helpers for coco-connect-hdl bridge
;;;
;;; This model intentionally starts from property find results. In some DE-HDL
;;; sessions cnGetComps/cnGetWires returns object handles while the direct
;;; cnGetComp*/cnGetWire* accessors fail. cnGetProps + cnGetPropName/Value is
;;; the stable path already verified by the older list commands.

(setq _coco_component_find_diagnostics nil)

(defun _coco_json_field (name value)
  (strcat (_coco_json_string name) ":" value))

(defun _coco_json_string_field (name value)
  (_coco_json_field name (_coco_json_string value)))

(defun _coco_json_array (items)
  (strcat "[" (_coco_join_list items ",") "]"))

(defun _coco_json_object (fields)
  (strcat "{" (_coco_join_list fields ",") "}"))

(defun _coco_nth (idx lst)
  (let ((i 0)
        (rest lst)
        (value nil))
    (while (and rest (leqp i idx))
      (when (equal i idx)
        (setq value (car rest)))
      (setq rest (cdr rest))
      (setq i (add1 i)))
    value))

(defun _coco_safe_apply (fn args)
  (let ((result nil))
    (setq result (errset (apply fn args) nil))
    (if result (car result) nil)))

(defun _coco_safe_dbid_string (dbid)
  (let ((result nil))
    (when dbid
      (setq result (errset (cnCreateDbIdString dbid) nil)))
    (if result (safeStr (car result)) "")))

(defun _coco_prop_owner_dbid_string (prop)
  (_coco_safe_dbid_string (_coco_safe_apply 'cnGetPropOwnerDbId (list prop))))

(defun _coco_prop_dbid_string (prop)
  (_coco_safe_dbid_string (_coco_safe_apply 'cnGetPropDbId (list prop))))

(defun _coco_prop_owner_type_string (prop)
  (safeStr (_coco_safe_apply 'cnGetPropOwnerType (list prop))))

(defun _coco_prop_json (prop)
  (_coco_json_object
    (list
      (_coco_json_string_field "name" (safeGetProp prop "name"))
      (_coco_json_string_field "value" (safeGetProp prop "value"))
      (_coco_json_string_field "property_dbid" (_coco_prop_dbid_string prop))
      (_coco_json_string_field "owner_dbid" (_coco_prop_owner_dbid_string prop))
      (_coco_json_string_field "owner_type" (_coco_prop_owner_type_string prop)))))

(defun _coco_props_json (props)
  (let ((items nil))
    (foreach prop props
      (setq items (append items (list (_coco_prop_json prop)))))
    (_coco_json_array items)))

(defun _coco_props_from_find (handle find_cmd)
  (let ((iter nil)
        (props nil))
    (setq iter (_coco_open_iterator handle find_cmd 'cnGetProps))
    (setq props (_coco_iterator_items iter))
    (_coco_cleanup_iterator handle iter)
    props))

(defun _coco_props_from_component_find (handle find_cmd)
  (let ((find_result nil)
        (body_result nil)
        (include_result nil)
        (find_group "")
        (body_group "")
        (include_group "")
        (props nil)
        (dbid_count 0)
        (result nil))
    (setq result (errset (_coco_send_cmd handle find_cmd) nil))
    (when result
      (setq find_result (car result))
      (setq find_group (safeStr (_coco_safe_apply 'cnGetGroupName (list find_result)))))
    (when (not (equal find_group ""))
      (setq body_result
        (errset
          (_coco_send_cmd handle (sprintf nil "INCLUDE %s BODIES" find_group))
          nil))
      (when body_result
        (setq body_group
          (safeStr (_coco_safe_apply 'cnGetGroupName (list (car body_result))))))
      (when (equal body_group "")
        (setq body_group find_group))
      (setq include_result
        (errset
          (_coco_send_cmd handle (sprintf nil "INCLUDE %s PROPERTIES" body_group))
          nil))
      (when include_result
        (setq include_group
          (safeStr (_coco_safe_apply 'cnGetGroupName (list (car include_result)))))
        (setq props
          (_coco_safe_apply
            'cnGetProps
            (list (_coco_safe_apply 'cnGetGroupedObjectSet (list (car include_result)))))))
      (when (and
              (not (equal include_group ""))
              (not (equal include_group body_group))
              (not (equal include_group find_group)))
        (_coco_cleanup_iterator handle (_coco_make_iterator include_group nil)))
      (when (and
              (not (equal body_group ""))
              (not (equal body_group find_group)))
        (_coco_cleanup_iterator handle (_coco_make_iterator body_group nil)))
      (_coco_cleanup_iterator handle (_coco_make_iterator find_group nil)))
    (foreach prop props
      (when (not (equal (_coco_prop_dbid_string prop) ""))
        (setq dbid_count (add1 dbid_count))))
    (setq _coco_component_find_diagnostics
      (append
        _coco_component_find_diagnostics
        (list
          (list
            find_cmd
            find_group
            body_group
            include_group
            (length props)
            dbid_count))))
    props))

(defun _coco_prop_key (prop)
  (let ((dbid (_coco_prop_dbid_string prop))
        (owner_dbid (_coco_prop_owner_dbid_string prop)))
    (strcat
      (safeGetProp prop "name")
      "="
      (safeGetProp prop "value")
      "@"
      (if (not (equal dbid "")) dbid owner_dbid))))

(defun _coco_merge_unique_props (base extra)
  (let ((result base)
        (keys nil)
        (key ""))
    (foreach prop result
      (setq keys (append keys (list (_coco_prop_key prop)))))
    (foreach prop extra
      (setq key (_coco_prop_key prop))
      (when (not (_coco_member_string key keys))
        (setq result (append result (list prop)))
        (setq keys (append keys (list key)))))
    result))

(defun _coco_first_nonempty_prop_value (props names)
  (let ((value ""))
    (foreach prop props
      (when (and (equal value "")
                 (_coco_member_string (safeGetProp prop "name") names))
        (setq value (safeGetProp prop "value"))))
    value))

(defun _coco_component_tuples (handle)
  (let ((iter nil)
        (items nil))
    (setq iter (_coco_open_iterator handle "Select Bodies All" 'cnGetComps))
    (setq items (_coco_iterator_items iter))
    (_coco_cleanup_iterator handle iter)
    items))

(defun _coco_wire_tuples (handle)
  (let ((iter nil)
        (items nil))
    (setq iter (_coco_open_iterator handle "Select Wires All" 'cnGetWires))
    (setq items (_coco_iterator_items iter))
    (_coco_cleanup_iterator handle iter)
    items))

(defun _coco_tuple_xy_string (tuple)
  (strcat
    "("
    (safeStr (_coco_nth 2 tuple))
    ","
    (safeStr (_coco_nth 3 tuple))
    ")"))

(defun _coco_find_component_tuple_by_xy (xy tuples)
  (let ((found nil))
    (foreach tuple tuples
      (when (and (not found)
                 (equal xy (_coco_tuple_xy_string tuple)))
        (setq found tuple)))
    found))

(defun _coco_pin_tuple_json (pin)
  (_coco_json_object
    (list
      (_coco_json_string_field "component_dbid" (_coco_nth 0 pin))
      (_coco_json_string_field "name" (_coco_nth 1 pin))
      (_coco_json_string_field "x" (_coco_nth 2 pin))
      (_coco_json_string_field "y" (_coco_nth 3 pin)))))

(defun _coco_component_pins_json (handle comp_tuple)
  (let ((sym_info nil)
        (pins nil)
        (items nil))
    (when comp_tuple
      (setq sym_info (_coco_safe_apply 'cnGetCompSymInfo (list handle comp_tuple)))
      (when sym_info
        (setq pins (_coco_safe_apply 'cnCreateCompPins (list comp_tuple sym_info)))))
    (foreach pin pins
      (setq items (append items (list (_coco_pin_tuple_json pin)))))
    (_coco_json_array items)))

(defun _coco_component_props_for_refdes (handle refdes)
  (let ((props nil)
        (extra nil)
        (body_props nil)
        (body_extra nil))
    (setq props (_coco_props_from_find handle (sprintf nil "find cds_location=%s" refdes)))
    (setq extra (_coco_props_from_find handle (sprintf nil "find $location=%s" refdes)))
    (setq body_props (_coco_props_from_component_find handle (sprintf nil "find cds_location=%s" refdes)))
    (setq body_extra (_coco_props_from_component_find handle (sprintf nil "find $location=%s" refdes)))
    (setq props (_coco_merge_unique_props props extra))
    (setq props (_coco_merge_unique_props props body_props))
    (_coco_merge_unique_props props body_extra)))

(defun _coco_active_dwg (handle)
  (let ((dwg_info nil)
        (dwg nil))
    (setq dwg_info (_coco_safe_apply 'cnGetDwgInfo (list handle)))
    (when dwg_info
      (setq dwg (_coco_safe_apply 'cnFindActiveDwg (list dwg_info))))
    dwg))

(defun _coco_page_json_from_name (dwg page_name)
  (let ((lib_name (safeStr (_coco_safe_apply 'cnGetDwgLibName (list dwg))))
        (cell_name (safeStr (_coco_safe_apply 'cnGetDwgCellName (list dwg))))
        (view_name (safeStr (_coco_safe_apply 'cnGetDwgViewName (list dwg)))))
    (when (equal lib_name "")
      (setq lib_name (safeStr (_coco_nth 6 dwg))))
    (when (equal cell_name "")
      (setq cell_name (safeStr (_coco_nth 7 dwg))))
    (when (equal view_name "")
      (setq view_name (safeStr (_coco_nth 8 dwg))))
    (_coco_json_object
      (list
        (_coco_json_string_field "id" (safeStr (_coco_nth 0 dwg)))
        (_coco_json_string_field "full_name" page_name)
        (_coco_json_string_field "root_full_name" page_name)
        (_coco_json_string_field "lib_name" lib_name)
        (_coco_json_string_field "cell_name" cell_name)
        (_coco_json_string_field "view_name" view_name)
        (_coco_json_string_field "page" (_coco_page_number_from_name page_name))
        (_coco_json_string_field "kind" (safeStr (_coco_nth 10 dwg)))))))

(setq _coco_page_snapshot_page_json "")
(setq _coco_page_snapshot_component_tuples nil)
(setq _coco_page_snapshot_refdes_list nil)
(setq _coco_page_snapshot_net_names nil)
(setq _coco_page_snapshot_props_cache nil)

(defun _coco_snapshot_cache_entry (refdes props)
  (list refdes props))

(defun _coco_snapshot_cache_refdes (entry)
  (car entry))

(defun _coco_snapshot_cache_props (entry)
  (_coco_nth 1 entry))

(defun _coco_reset_page_snapshot ()
  (setq _coco_page_snapshot_page_json "")
  (setq _coco_page_snapshot_component_tuples nil)
  (setq _coco_page_snapshot_refdes_list nil)
  (setq _coco_page_snapshot_net_names nil)
  (setq _coco_page_snapshot_props_cache nil)
  t)

(defun _coco_prepare_page_snapshot (handle dwg page_name)
  (_coco_reset_page_snapshot)
  (setq _coco_page_snapshot_page_json (_coco_page_json_from_name dwg page_name))
  (setq _coco_page_snapshot_component_tuples (_coco_component_tuples handle))
  (setq _coco_page_snapshot_refdes_list (_coco_component_refdes_list handle))
  (setq _coco_page_snapshot_net_names
    (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
  t)

(defun _coco_snapshot_props_for_refdes (handle refdes)
  (let ((found nil)
        (props nil))
    (foreach entry _coco_page_snapshot_props_cache
      (when (and (not found)
                 (equal (_coco_snapshot_cache_refdes entry) refdes))
        (setq found (_coco_snapshot_cache_props entry))))
    (if found
      found
      (progn
        (setq props (_coco_component_props_for_refdes handle refdes))
        (setq _coco_page_snapshot_props_cache
          (append _coco_page_snapshot_props_cache
            (list (_coco_snapshot_cache_entry refdes props))))
        props))))

(defun _coco_component_refdes_list (handle)
  (let ((props nil)
        (values nil)
        (extra_values nil))
    (setq props (_coco_props_from_component_find handle "Select Bodies All"))
    (setq values (_coco_collect_prop_values props (list "CDS_LOCATION")))
    (setq extra_values (_coco_collect_prop_values props (list "$LOCATION")))
    (_coco_merge_unique values extra_values)))

(defun _coco_active_page_json (handle)
  (let ((dwg_info nil)
        (dwg nil))
    (setq dwg_info (_coco_safe_apply 'cnGetDwgInfo (list handle)))
    (when dwg_info
      (setq dwg (_coco_safe_apply 'cnFindActiveDwg (list dwg_info))))
    (_coco_json_object
      (list
        (_coco_json_string_field "id" (_coco_nth 0 dwg))
        (_coco_json_string_field "full_name" (_coco_nth 1 dwg))
        (_coco_json_string_field "root_full_name" (_coco_nth 2 dwg))
        (_coco_json_string_field "lib_name" (_coco_nth 6 dwg))
        (_coco_json_string_field "cell_name" (_coco_nth 7 dwg))
        (_coco_json_string_field "view_name" (_coco_nth 8 dwg))
        (_coco_json_string_field "page" (_coco_nth 9 dwg))
        (_coco_json_string_field "kind" (_coco_nth 10 dwg))))))

