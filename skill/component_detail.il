;;; component_detail.il - detailed component/pin inspection for coco-connect-hdl
;;;
;;; Depends on util.il, hdl.il, and read_model.il. This file owns the C04
;;; detailed lookup view while C08 remains responsible for pin-to-net traversal.

(defun _coco_detail_component_dbid (comp_tuple props)
  (let ((owner_dbid ""))
    (foreach prop props
      (when (equal owner_dbid "")
        (setq owner_dbid (_coco_prop_owner_dbid_string prop))))
    (when (and (equal owner_dbid "") comp_tuple)
      (setq owner_dbid (safeStr (_coco_nth 0 comp_tuple))))
    owner_dbid))

(defun _coco_detail_component_name (comp_tuple)
  (let ((value ""))
    (when comp_tuple
      (setq value (safeStr (_coco_safe_apply 'cnGetCompName (list comp_tuple))))
      (when (equal value "")
        (setq value (safeStr (_coco_nth 1 comp_tuple)))))
    value))

(defun _coco_detail_component_lib_name (comp_tuple)
  (let ((value ""))
    (when comp_tuple
      (setq value (safeStr (_coco_safe_apply 'cnGetCompLibName (list comp_tuple))))
      (when (equal value "")
        (setq value (safeStr (_coco_nth 8 comp_tuple)))))
    value))

(defun _coco_detail_component_angle (comp_tuple)
  (let ((value ""))
    (when comp_tuple
      (setq value (safeStr (_coco_safe_apply 'cnGetCompAngle (list comp_tuple))))
      (when (equal value "")
        (setq value (safeStr (_coco_nth 4 comp_tuple)))))
    value))

(defun _coco_detail_component_mirror (comp_tuple)
  (let ((value ""))
    (when comp_tuple
      (setq value (safeStr (_coco_safe_apply 'cnGetCompMirror (list comp_tuple))))
      (when (equal value "")
        (setq value (safeStr (_coco_nth 5 comp_tuple)))))
    value))

(defun _coco_detail_origin_x (comp_tuple)
  (safeStr (_coco_nth 2 comp_tuple)))

(defun _coco_detail_origin_y (comp_tuple)
  (safeStr (_coco_nth 3 comp_tuple)))

(defun _coco_detail_pin_name (pin)
  (let ((value (safeStr (_coco_safe_apply 'cnGetCompPinName (list pin)))))
    (when (equal value "")
      (setq value (safeStr (_coco_nth 1 pin))))
    value))

(defun _coco_detail_pin_x (pin)
  (safeStr (_coco_nth 2 pin)))

(defun _coco_detail_pin_y (pin)
  (safeStr (_coco_nth 3 pin)))

(defun _coco_detail_pin_json (pin)
  (_coco_json_object
    (list
      (_coco_json_string_field "name" (_coco_detail_pin_name pin))
      (_coco_json_string_field "x" (_coco_detail_pin_x pin))
      (_coco_json_string_field "y" (_coco_detail_pin_y pin))
      (_coco_json_field "properties" (_coco_json_array nil))
      (_coco_json_string_field "connected_wire_dbid" "")
      (_coco_json_string_field "net_name" ""))))

(defun _coco_detail_pins_json (handle comp_tuple)
  (let ((sym_info nil)
        (pins nil)
        (items nil))
    (when comp_tuple
      (setq sym_info (_coco_safe_apply 'cnGetCompSymInfo (list handle comp_tuple)))
      (when sym_info
        (setq pins (_coco_safe_apply 'cnCreateCompPins (list comp_tuple sym_info)))))
    (foreach pin pins
      (setq items (append items (list (_coco_detail_pin_json pin)))))
    (_coco_json_array items)))

(setq _coco_component_detail_current_page_json "")

(defun _coco_component_detail_json_from_refdes (handle refdes)
  (let ((props nil)
        (comp_tuple nil)
        (xy "")
        (part_name "")
        (device "")
        (footprint "")
        (value ""))
    (setq props (_coco_component_props_for_refdes handle refdes))
    (if (not props)
      nil
      (progn
        (setq _coco_component_tuple_cache (_coco_component_tuples handle))
        (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
        (setq comp_tuple (_coco_find_component_tuple_by_xy xy _coco_component_tuple_cache))
        (setq part_name (_coco_first_nonempty_prop_value props (list "PART_NAME" "Part Name" "PART_NUMBER")))
        (setq device (_coco_first_nonempty_prop_value props (list "DEVICE" "Device")))
        (setq footprint (_coco_first_nonempty_prop_value props (list "PCB Footprint" "PCB_FOOTPRINT" "JEDEC_TYPE" "PACK_TYPE")))
        (setq value (_coco_first_nonempty_prop_value props (list "VALUE" "Value")))
        (_coco_json_object
          (list
            (_coco_json_string_field "refdes" refdes)
            (_coco_json_string_field "dbid" (_coco_detail_component_dbid comp_tuple props))
            (_coco_json_string_field "component_name" (_coco_detail_component_name comp_tuple))
            (_coco_json_string_field "library_name" (_coco_detail_component_lib_name comp_tuple))
            (_coco_json_string_field "part_name" part_name)
            (_coco_json_string_field "value" value)
            (_coco_json_string_field "device" device)
            (_coco_json_string_field "footprint" footprint)
            (_coco_json_string_field "x" (_coco_detail_origin_x comp_tuple))
            (_coco_json_string_field "y" (_coco_detail_origin_y comp_tuple))
            (_coco_json_string_field "angle" (_coco_detail_component_angle comp_tuple))
            (_coco_json_string_field "mirror" (_coco_detail_component_mirror comp_tuple))
            (_coco_json_field "page" _coco_component_detail_current_page_json)
            (_coco_json_field "pins" (_coco_detail_pins_json handle comp_tuple))
            (_coco_json_field "properties" (_coco_props_json props))))))))

(setq _coco_component_detail_items_acc nil)
(setq _coco_component_detail_active_dwg nil)
(setq _coco_component_detail_refdes_filter "")

(defun _coco_component_detail_page_item (handle page_name)
  (let ((detail nil))
    (setq _coco_component_detail_current_page_json
      (_coco_page_json_from_name _coco_component_detail_active_dwg page_name))
    (setq detail
      (_coco_component_detail_json_from_refdes
        handle _coco_component_detail_refdes_filter))
    (when detail
      (setq _coco_component_detail_items_acc
        (append _coco_component_detail_items_acc (list detail))))))

(defun _coco_component_detail_resolve_page_hint (handle dwg page_hint)
  (let ((found nil)
        (page_num ""))
    (foreach page_name (_coco_design_page_names handle dwg)
      (setq page_num (_coco_page_number_from_name page_name))
      (when (and (not found)
                 (or (equal page_hint page_name)
                     (equal page_hint page_num)
                     (equal (lowerCase page_hint) (strcat "page" page_num))))
        (setq found page_name)))
    found))

(defun _coco_component_detail_components_json (handle refdes page_hint)
  (let ((original_page "")
        (target_page nil)
        (switched nil)
        (result nil))
    (setq _coco_component_detail_items_acc nil)
    (setq _coco_component_detail_active_dwg (_coco_active_dwg handle))
    (setq _coco_component_detail_refdes_filter (safeStr refdes))
    (setq original_page
      (_coco_resolved_active_page_name handle _coco_component_detail_active_dwg))
    (if (equal page_hint "")
      (_coco_each_design_page
        handle
        _coco_component_detail_active_dwg
        original_page
        '_coco_component_detail_page_item)
      (progn
        (setq target_page
          (_coco_component_detail_resolve_page_hint
            handle _coco_component_detail_active_dwg page_hint))
        (when (not target_page)
          (error "PAGE_NOT_FOUND" (strcat "Page not found: " page_hint)))
        (when (not (equal target_page original_page))
          (_coco_send_cmd handle (sprintf nil "nmapedit %s;" target_page))
          (setq switched t))
        (setq result
          (errset (_coco_component_detail_page_item handle target_page) t))
        (when switched
          (errset (_coco_send_cmd handle (sprintf nil "nmapedit %s;" original_page)) t))
        (when (not result)
          (error "COMPONENT_DETAIL_FAILED"
            (strcat "Failed to inspect page: " target_page)))))
    (_coco_json_array _coco_component_detail_items_acc)))

(defun _coco_component_detail_request_parts (arg)
  (let ((parts (parseString (safeStr arg) "|")))
    (list (safeStr (_coco_nth 0 parts)) (safeStr (_coco_nth 1 parts)))))

(defun _coco_op_component_detail (arg)
  (let ((handle nil)
        (page_json "")
        (parts (_coco_component_detail_request_parts arg))
        (refdes "")
        (page_hint ""))
    (setq refdes (_coco_nth 0 parts))
    (setq page_hint (_coco_nth 1 parts))
    (when (equal refdes "")
      (error "MISSING_REFDES" "component_detail requires a refdes argument"))
    (setq handle (_coco_import_handle))
    (setq page_json (_coco_active_page_json handle))
    (_coco_json_object
      (list
        (_coco_json_string_field "refdes" refdes)
        (_coco_json_field "import_context_page" page_json)
        (_coco_json_field "components"
          (_coco_component_detail_components_json handle refdes page_hint))))))
