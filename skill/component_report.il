;;; component_report.il - normalized component report for coco-connect-hdl
;;;
;;; Depends on util.il, hdl.il, and read_model.il. This file owns the C02
;;; reporting view while read_model.il stays focused on common read helpers.

(defun _coco_props_with_names_json (props names)
  (let ((items nil))
    (foreach prop props
      (when (_coco_member_string (safeGetProp prop "name") names)
        (setq items (append items (list (_coco_prop_json prop))))))
    (_coco_json_array items)))

(defun _coco_source_property_names ()
  (list
    "SOURCE"
    "Source"
    "SOURCE_PATH"
    "SOURCE_FILE"
    "SOURCE FILE"
    "DESIGN_SOURCE"
    "HDL_SOURCE"
    "PAGE"
    "PAGE_NAME"
    "LIBRARY"
    "LIBRARY_NAME"
    "LIB_NAME"
    "PATH"
    "FILE"))

(setq _coco_component_report_current_page_json "")

(defun _coco_component_report_json_from_refdes (handle refdes)
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
        (_coco_json_string_field "component_dbid" owner_dbid)
        (_coco_json_string_field "part_name" part_name)
        (_coco_json_string_field "value" value)
        (_coco_json_string_field "device" device)
        (_coco_json_string_field "footprint" footprint)
        (_coco_json_string_field "library_name" (_coco_nth 8 comp_tuple))
        (_coco_json_string_field "symbol_name" (_coco_nth 1 comp_tuple))
        (_coco_json_string_field "x" (_coco_nth 2 comp_tuple))
        (_coco_json_string_field "y" (_coco_nth 3 comp_tuple))
        (_coco_json_field "page" _coco_component_report_current_page_json)
        (_coco_json_field "source_properties" (_coco_props_with_names_json props (_coco_source_property_names)))
        (_coco_json_field "properties" (_coco_props_json props))))))

(defun _coco_component_report_components_json_for_page (handle)
  (let ((items nil))
    (setq _coco_component_tuple_cache (_coco_component_tuples handle))
    (foreach refdes (_coco_component_refdes_list handle)
      (setq items (append items (list (_coco_component_report_json_from_refdes handle refdes)))))
    items))

(setq _coco_component_report_items_acc nil)
(setq _coco_component_report_active_dwg nil)

(defun _coco_component_report_page_item (handle page_name)
  (let ()
    (setq _coco_component_report_current_page_json
      (_coco_page_json_from_name _coco_component_report_active_dwg page_name))
    (setq _coco_component_report_items_acc
      (append _coco_component_report_items_acc
        (_coco_component_report_components_json_for_page handle)))))

(defun _coco_component_report_components_json (handle)
  (setq _coco_component_report_items_acc nil)
  (setq _coco_component_report_active_dwg (_coco_active_dwg handle))
  (_coco_each_design_page
    handle
    _coco_component_report_active_dwg
    (_coco_resolved_active_page_name handle _coco_component_report_active_dwg)
    '_coco_component_report_page_item)
  (_coco_json_array _coco_component_report_items_acc))

(defun _coco_op_component_report ()
  (let ((handle (_coco_import_handle)))
    (_coco_json_object
      (list
        (_coco_json_field "import_context_page" (_coco_active_page_json handle))
        (_coco_json_field "components" (_coco_component_report_components_json handle))))))
