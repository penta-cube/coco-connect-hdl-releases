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

(defun _coco_component_detail_json_from_refdes (handle refdes page_json)
  (let ((props nil)
        (comp_tuple nil)
        (xy "")
        (part_name "")
        (device "")
        (footprint "")
        (value ""))
    (setq props (_coco_component_props_for_refdes handle refdes))
    (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
    (setq comp_tuple (_coco_find_component_tuple_by_xy xy _coco_component_tuple_cache))
    (if (and (not props) (not comp_tuple))
      nil
      (progn
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
            (_coco_json_field "page" page_json)
            (_coco_json_field "pins" (_coco_detail_pins_json handle comp_tuple))
            (_coco_json_field "properties" (_coco_props_json props))))))))

(defun _coco_component_detail_components_json (handle refdes page_json)
  (let ((items nil)
        (targets nil)
        (detail nil))
    (setq _coco_component_tuple_cache (_coco_component_tuples handle))
    (if (equal (safeStr refdes) "")
      (setq targets (_coco_component_refdes_list handle))
      (setq targets (list (safeStr refdes))))
    (foreach target targets
      (setq detail (_coco_component_detail_json_from_refdes handle target page_json))
      (when detail
        (setq items (append items (list detail)))))
    (_coco_json_array items)))

(defun _coco_op_component_detail (arg)
  (let ((handle (_coco_import_handle))
        (page_json ""))
    (setq page_json (_coco_active_page_json handle))
    (_coco_json_object
      (list
        (_coco_json_string_field "refdes" (safeStr arg))
        (_coco_json_field "page" page_json)
        (_coco_json_field "components" (_coco_component_detail_components_json handle arg page_json))))))
