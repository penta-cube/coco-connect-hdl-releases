;;; read_model.il - common read model helpers for coco-connect-hdl bridge
;;;
;;; This model intentionally starts from property find results. In some DE-HDL
;;; sessions cnGetComps/cnGetWires returns object handles while the direct
;;; cnGetComp*/cnGetWire* accessors fail. cnGetProps + cnGetPropName/Value is
;;; the stable path already verified by the older list commands.

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

(defun _coco_prop_owner_type_string (prop)
  (safeStr (_coco_safe_apply 'cnGetPropOwnerType (list prop))))

(defun _coco_prop_json (prop)
  (_coco_json_object
    (list
      (_coco_json_string_field "name" (safeGetProp prop "name"))
      (_coco_json_string_field "value" (safeGetProp prop "value"))
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
        (include_result nil)
        (group "")
        (props nil)
        (result nil))
    (setq result (errset (_coco_send_cmd handle find_cmd) nil))
    (when result
      (setq find_result (car result))
      (setq group (safeStr (_coco_safe_apply 'cnGetGroupName (list find_result)))))
    (when (not (equal group ""))
      (errset (_coco_send_cmd handle (sprintf nil "INCLUDE %s BODIES" group)) nil)
      (setq include_result
        (errset (_coco_send_cmd handle (sprintf nil "INCLUDE %s PROPERTIES" group)) nil))
      (when include_result
        (setq props
          (_coco_safe_apply
            'cnGetProps
            (list (_coco_safe_apply 'cnGetGroupedObjectSet (list (car include_result)))))))
      (_coco_cleanup_iterator handle (_coco_make_iterator group nil)))
    props))

(defun _coco_prop_key (prop)
  (strcat
    (safeGetProp prop "name")
    "="
    (safeGetProp prop "value")
    "@"
    (_coco_prop_owner_dbid_string prop)))

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

(setq _coco_design_current_page_json "")

(defun _coco_component_json_from_refdes (handle refdes)
  (let ((props nil)
        (comp_tuple nil)
        (owner_dbid "")
        (part_name "")
        (device "")
        (footprint "")
        (xy "")
        (value ""))
    (setq props (_coco_component_props_for_refdes handle refdes))
    (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
    (setq comp_tuple (_coco_find_component_tuple_by_xy xy _coco_component_tuple_cache))
    (foreach prop props
      (when (equal owner_dbid "")
        (setq owner_dbid (_coco_prop_owner_dbid_string prop))))
    (when (and (equal owner_dbid "") comp_tuple)
      (setq owner_dbid (safeStr (_coco_nth 0 comp_tuple))))
    (setq part_name (_coco_first_nonempty_prop_value props (list "PART_NAME" "Part Name" "PART_NUMBER")))
    (setq device (_coco_first_nonempty_prop_value props (list "DEVICE" "Device")))
    (setq footprint (_coco_first_nonempty_prop_value props (list "PCB Footprint" "PCB_FOOTPRINT" "JEDEC_TYPE" "PACK_TYPE")))
    (setq value (_coco_first_nonempty_prop_value props (list "VALUE" "Value")))
    (_coco_json_object
      (list
        (_coco_json_string_field "refdes" refdes)
        (_coco_json_string_field "owner_dbid" owner_dbid)
        (_coco_json_string_field "part_name" part_name)
        (_coco_json_string_field "device" device)
        (_coco_json_string_field "footprint" footprint)
        (_coco_json_string_field "value" value)
        (_coco_json_string_field "symbol_name" (_coco_nth 1 comp_tuple))
        (_coco_json_string_field "x" (_coco_nth 2 comp_tuple))
        (_coco_json_string_field "y" (_coco_nth 3 comp_tuple))
        (_coco_json_string_field "angle" (_coco_nth 4 comp_tuple))
        (_coco_json_string_field "rotation" (_coco_nth 5 comp_tuple))
        (_coco_json_string_field "lib_name" (_coco_nth 8 comp_tuple))
        (_coco_json_field "page" _coco_design_current_page_json)
        (_coco_json_field "pins" (_coco_component_pins_json handle comp_tuple))
        (_coco_json_field "properties" (_coco_props_json props))))))

(defun _coco_component_refdes_list (handle)
  (let ((props nil)
        (values nil)
        (extra_values nil))
    (setq props (_coco_props_from_component_find handle "Select Bodies All"))
    (setq values (_coco_collect_prop_values props (list "CDS_LOCATION")))
    (setq extra_values (_coco_collect_prop_values props (list "$LOCATION")))
    (_coco_merge_unique values extra_values)))

(defun _coco_components_json_for_page (handle)
  (let ((items nil))
    (setq _coco_component_tuple_cache (_coco_component_tuples handle))
    (foreach refdes (_coco_component_refdes_list handle)
      (setq items (append items (list (_coco_component_json_from_refdes handle refdes)))))
    items))

(setq _coco_design_items_acc nil)
(setq _coco_design_active_dwg nil)
(setq _coco_design_original_page "")

(defun _coco_design_components_item (handle page_name)
  (let ()
    (setq _coco_design_current_page_json (_coco_page_json_from_name _coco_design_active_dwg page_name))
    (setq _coco_design_items_acc
      (append _coco_design_items_acc
        (_coco_components_json_for_page handle)))))

(defun _coco_design_array_json (items)
  (_coco_json_array items))

(defun _coco_design_components_json (handle)
  (setq _coco_design_items_acc nil)
  (setq _coco_design_active_dwg (_coco_active_dwg handle))
  (setq _coco_design_original_page (_coco_resolved_active_page_name handle _coco_design_active_dwg))
  (_coco_each_design_page
    handle
    _coco_design_active_dwg
    _coco_design_original_page
    '_coco_design_components_item)
  (_coco_design_array_json _coco_design_items_acc))

(defun _coco_net_json (name)
  (_coco_json_object
    (list
      (_coco_json_string_field "name" name)
      (_coco_json_field "page" _coco_design_current_page_json))))

(defun _coco_nets_json_for_page (handle)
  (let ((items nil))
    (foreach net (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME"))
      (setq items (append items (list (_coco_net_json net)))))
    items))

(defun _coco_design_nets_item (handle page_name)
  (let ()
    (setq _coco_design_current_page_json (_coco_page_json_from_name _coco_design_active_dwg page_name))
    (setq _coco_design_items_acc
      (append _coco_design_items_acc
        (_coco_nets_json_for_page handle)))))

(defun _coco_design_nets_json (handle)
  (setq _coco_design_items_acc nil)
  (setq _coco_design_active_dwg (_coco_active_dwg handle))
  (setq _coco_design_original_page (_coco_resolved_active_page_name handle _coco_design_active_dwg))
  (_coco_each_design_page
    handle
    _coco_design_active_dwg
    _coco_design_original_page
    '_coco_design_nets_item)
  (_coco_design_array_json _coco_design_items_acc))

(defun _coco_wire_tuple_json (wire)
  (_coco_json_object
    (list
      (_coco_json_string_field "dbid" (_coco_nth 0 wire))
      (_coco_json_string_field "x1" (_coco_nth 1 wire))
      (_coco_json_string_field "y1" (_coco_nth 2 wire))
      (_coco_json_string_field "x2" (_coco_nth 3 wire))
      (_coco_json_string_field "y2" (_coco_nth 4 wire))
      (_coco_json_string_field "style" (_coco_nth 5 wire))
      (_coco_json_string_field "flags" (_coco_nth 6 wire))
      (_coco_json_field "page" _coco_design_current_page_json))))

(defun _coco_wires_json_for_page (handle)
  (let ((items nil))
    (foreach wire (_coco_wire_tuples handle)
      (setq items (append items (list (_coco_wire_tuple_json wire)))))
    items))

(defun _coco_design_wires_item (handle page_name)
  (let ()
    (setq _coco_design_current_page_json (_coco_page_json_from_name _coco_design_active_dwg page_name))
    (setq _coco_design_items_acc
      (append _coco_design_items_acc
        (_coco_wires_json_for_page handle)))))

(defun _coco_design_wires_json (handle)
  (setq _coco_design_items_acc nil)
  (setq _coco_design_active_dwg (_coco_active_dwg handle))
  (setq _coco_design_original_page (_coco_resolved_active_page_name handle _coco_design_active_dwg))
  (_coco_each_design_page
    handle
    _coco_design_active_dwg
    _coco_design_original_page
    '_coco_design_wires_item)
  (_coco_design_array_json _coco_design_items_acc))

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

(defun _coco_op_read_model ()
  (let ((handle (_coco_import_handle)))
    (_coco_json_object
      (list
        (_coco_json_field "import_context_page" (_coco_active_page_json handle))
        (_coco_json_field "components" (_coco_design_components_json handle))
        (_coco_json_field "nets" (_coco_design_nets_json handle))
        (_coco_json_field "wires" (_coco_design_wires_json handle))))))

