;;; circuit_block.il - page-local graphical circuit block extraction
;;;
;;; Public operation:
;;;   circuit_block

(defun _coco_cb_string_starts_with (text prefix)
  (let ((hay (upperCase (safeStr text)))
        (needle (upperCase (safeStr prefix)))
        (hay_len 0)
        (needle_len 0))
    (setq hay_len (strlen hay))
    (setq needle_len (strlen needle))
    (and (greaterp needle_len 0)
         (leqp needle_len hay_len)
         (equal (substring hay 1 needle_len) needle))))

(defun _coco_cb_stop_refdes_p (refdes target_refdes)
  (let ((value (upperCase (safeStr refdes)))
        (target (upperCase (safeStr target_refdes)))
        (stop nil))
    (if (equal value target)
      nil
      (progn
        (foreach prefix (list "GND" "CN" "U" "J" "P" "V")
          (when (and (not stop) (_coco_cb_string_starts_with value prefix))
            (setq stop t)))
        stop))))

(defun _coco_cb_pin_point (pin_entry)
  (_coco_conn_point_key (_coco_nth 2 pin_entry) (_coco_nth 3 pin_entry)))

(defun _coco_cb_wire_key (wire)
  (_coco_conn_wire_dbid wire))

(defun _coco_cb_pin_key (pin_entry)
  (strcat
    (safeStr (_coco_nth 0 pin_entry))
    "|"
    (safeStr (_coco_nth 1 pin_entry))
    "|"
    (safeStr (_coco_nth 2 pin_entry))
    ","
    (safeStr (_coco_nth 3 pin_entry))))

(defun _coco_cb_component_seen_p (refdes items)
  (_coco_member_string (safeStr refdes) items))

(defun _coco_cb_wire_seen_p (wire items)
  (_coco_member_string (_coco_cb_wire_key wire) items))

(defun _coco_cb_wire_connected_to_point_p (wire point_key)
  (or (_coco_conn_wire_touches_point wire point_key)
      (let ((xy (_coco_cb_split_point point_key)))
        (_coco_conn_point_on_wire (_coco_nth 0 xy) (_coco_nth 1 xy) wire))))

(defun _coco_cb_split_point (point_key)
  (let ((str (safeStr point_key))
        (i 1)
        (len 0)
        (ch "")
        (left "")
        (right "")
        (seen nil))
    (setq len (strlen str))
    (while (leqp i len)
      (setq ch (substring str i 1))
      (if (and (not seen) (equal ch ","))
        (setq seen t)
        (if seen
          (setq right (strcat right ch))
          (setq left (strcat left ch))))
      (setq i (add1 i)))
    (list left right)))

(defun _coco_cb_wire_connected_to_wire_p (left right)
  (or
    (_coco_conn_wire_touches_point left (_coco_conn_wire_p1 right))
    (_coco_conn_wire_touches_point left (_coco_conn_wire_p2 right))
    (_coco_conn_wire_touches_point right (_coco_conn_wire_p1 left))
    (_coco_conn_wire_touches_point right (_coco_conn_wire_p2 left))
    (let ((left_p1 (_coco_cb_split_point (_coco_conn_wire_p1 left)))
          (left_p2 (_coco_cb_split_point (_coco_conn_wire_p2 left)))
          (right_p1 (_coco_cb_split_point (_coco_conn_wire_p1 right)))
          (right_p2 (_coco_cb_split_point (_coco_conn_wire_p2 right))))
      (or
        (_coco_conn_point_on_wire (_coco_nth 0 left_p1) (_coco_nth 1 left_p1) right)
        (_coco_conn_point_on_wire (_coco_nth 0 left_p2) (_coco_nth 1 left_p2) right)
        (_coco_conn_point_on_wire (_coco_nth 0 right_p1) (_coco_nth 1 right_p1) left)
        (_coco_conn_point_on_wire (_coco_nth 0 right_p2) (_coco_nth 1 right_p2) left)))))

(defun _coco_cb_pins_for_refdes (pin_entries refdes)
  (let ((items nil))
    (foreach pin_entry pin_entries
      (when (equal (safeStr (_coco_nth 0 pin_entry)) refdes)
        (setq items (append items (list pin_entry)))))
    items))

(defun _coco_cb_component_from_pin (pin_entry)
  (safeStr (_coco_nth 0 pin_entry)))

(defun _coco_cb_add_unique_string (value items)
  (if (_coco_member_string value items)
    items
    (append items (list value))))

(defun _coco_cb_add_unique_wire (wire wires)
  (let ((found nil))
    (foreach item wires
      (when (equal (_coco_cb_wire_key item) (_coco_cb_wire_key wire))
        (setq found t)))
    (if found wires (append wires (list wire)))))

(defun _coco_cb_add_unique_pin (pin_entry pins)
  (let ((key (_coco_cb_pin_key pin_entry))
        (found nil))
    (foreach item pins
      (when (equal key (_coco_cb_pin_key item))
        (setq found t)))
    (if found pins (append pins (list pin_entry)))))

(defun _coco_cb_component_tuple_for_refdes (handle refdes comp_tuples)
  (let ((props nil)
        (xy "")
        (comp_tuple nil))
    (setq props (_coco_component_props_for_refdes handle refdes))
    (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
    (setq comp_tuple (_coco_find_component_tuple_by_xy xy comp_tuples))
    comp_tuple))

(defun _coco_cb_expand (target_refdes all_pins all_wires)
  (let ((component_queue (list target_refdes))
        (wire_queue nil)
        (component_seen nil)
        (wire_seen nil)
        (blocked_components nil)
        (result_pins nil)
        (result_wires nil)
        (refdes "")
        (pin_entry nil)
        (point "")
        (wire nil)
        (front_wire nil)
        (pin_refdes "")
        (stop nil))
    (while (or component_queue wire_queue)
      (while component_queue
        (setq refdes (car component_queue))
        (setq component_queue (cdr component_queue))
        (when (not (_coco_cb_component_seen_p refdes component_seen))
          (setq component_seen (_coco_cb_add_unique_string refdes component_seen))
          (setq stop (_coco_cb_stop_refdes_p refdes target_refdes))
          (when stop
            (setq blocked_components (_coco_cb_add_unique_string refdes blocked_components)))
          (foreach pin_entry (_coco_cb_pins_for_refdes all_pins refdes)
            (setq result_pins (_coco_cb_add_unique_pin pin_entry result_pins))
            (when (not stop)
              (setq point (_coco_cb_pin_point pin_entry))
              (foreach wire all_wires
                (when (and (not (_coco_cb_wire_seen_p wire wire_seen))
                           (_coco_cb_wire_connected_to_point_p wire point))
                  (setq wire_seen (_coco_cb_add_unique_string (_coco_cb_wire_key wire) wire_seen))
                  (setq result_wires (_coco_cb_add_unique_wire wire result_wires))
                  (setq wire_queue (append wire_queue (list wire)))))))))
      (while wire_queue
        (setq front_wire (car wire_queue))
        (setq wire_queue (cdr wire_queue))
        (foreach wire all_wires
          (when (and (not (_coco_cb_wire_seen_p wire wire_seen))
                     (_coco_cb_wire_connected_to_wire_p front_wire wire))
            (setq wire_seen (_coco_cb_add_unique_string (_coco_cb_wire_key wire) wire_seen))
            (setq result_wires (_coco_cb_add_unique_wire wire result_wires))
            (setq wire_queue (append wire_queue (list wire)))))
        (foreach pin_entry all_pins
          (setq point (_coco_cb_pin_point pin_entry))
          (when (_coco_cb_wire_connected_to_point_p front_wire point)
            (setq result_pins (_coco_cb_add_unique_pin pin_entry result_pins))
            (setq pin_refdes (_coco_cb_component_from_pin pin_entry))
            (when (not (_coco_cb_component_seen_p pin_refdes component_seen))
              (setq component_queue (append component_queue (list pin_refdes))))))))
    (list component_seen blocked_components result_pins result_wires)))

(defun _coco_cb_refdes_from_pins (pins)
  (let ((items nil)
        (refdes ""))
    (foreach pin_entry pins
      (setq refdes (_coco_cb_component_from_pin pin_entry))
      (when (not (_coco_member_string refdes items))
        (setq items (append items (list refdes)))))
    items))

(defun _coco_cb_pins_json_for_refdes (pins refdes)
  (let ((items nil))
    (foreach pin_entry pins
      (when (equal refdes (_coco_cb_component_from_pin pin_entry))
        (setq items
          (append items
            (list
              (_coco_json_object
                (list
                  (_coco_json_string_field "name" (_coco_nth 1 pin_entry))
                  (_coco_json_string_field "x" (_coco_nth 2 pin_entry))
                  (_coco_json_string_field "y" (_coco_nth 3 pin_entry)))))))))
    (_coco_json_array items)))

(defun _coco_cb_component_json (handle refdes comp_tuples pins blocked_components page_json)
  (let ((props nil)
        (comp_tuple nil)
        (part_name "")
        (device "")
        (footprint "")
        (value "")
        (stop_flag "false"))
    (setq props (_coco_component_props_for_refdes handle refdes))
    (setq comp_tuple (_coco_cb_component_tuple_for_refdes handle refdes comp_tuples))
    (setq part_name (_coco_first_nonempty_prop_value props (list "PART_NAME" "Part Name" "PART_NUMBER")))
    (setq device (_coco_first_nonempty_prop_value props (list "DEVICE" "Device")))
    (setq footprint (_coco_first_nonempty_prop_value props (list "PCB Footprint" "PCB_FOOTPRINT" "JEDEC_TYPE" "PACK_TYPE")))
    (setq value (_coco_first_nonempty_prop_value props (list "VALUE" "Value")))
    (when (_coco_member_string refdes blocked_components)
      (setq stop_flag "true"))
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
        (_coco_json_bool_field "stop_expansion" (equal stop_flag "true"))
        (_coco_json_field "page" page_json)
        (_coco_json_field "pins" (_coco_cb_pins_json_for_refdes pins refdes))
        (_coco_json_field "properties" (_coco_props_json props))))))

(defun _coco_cb_components_json (handle refdes_list comp_tuples pins blocked_components page_json)
  (let ((items nil))
    (foreach refdes refdes_list
      (setq items
        (append items
          (list (_coco_cb_component_json handle refdes comp_tuples pins blocked_components page_json)))))
    (_coco_json_array items)))

(defun _coco_cb_net_names_for_wire (handle wire)
  (let ((names (_coco_conn_net_names_for_wire handle wire))
        (bbox_names nil))
    (setq names (_coco_cb_normalize_net_names names))
    (when (not names)
      (setq bbox_names (_coco_cb_net_names_by_wire_bbox handle wire))
      (foreach name bbox_names
        (setq names (_coco_cb_add_net_name name names))))
    names))

(defun _coco_cb_normalize_net_name (name)
  (let ((value (safeStr name))
        (len 0))
    (setq len (strlen value))
    (if (and (leqp 2 len)
             (equal (substring value (sub1 len) 2) "\\g"))
      (substring value 1 (difference len 2))
      value)))

(defun _coco_cb_add_net_name (name names)
  (let ((value (_coco_cb_normalize_net_name name)))
    (if (or (equal value "") (_coco_member_string value names))
      names
      (append names (list value)))))

(defun _coco_cb_normalize_net_names (names)
  (let ((items nil))
    (foreach name names
      (setq items (_coco_cb_add_net_name name items)))
    items))

(defun _coco_cb_min_int (a b)
  (if (leqp a b) a b))

(defun _coco_cb_max_int (a b)
  (if (leqp a b) b a))

(defun _coco_cb_net_name_candidates_from_props (props)
  (let ((names nil)
        (prop_name "")
        (value ""))
    (foreach prop props
      (setq prop_name (upperCase (safeGetProp prop "name")))
      (when (_coco_member_string prop_name (list "SIG_NAME" "HDL_POWER" "HDL_TAP"))
        (setq value (safeGetProp prop "value"))
        (setq names (_coco_cb_add_net_name value names))))
    names))

(defun _coco_cb_single_name_or_nil (names)
  (if (equal (length names) 1) names nil))

(defun _coco_cb_net_names_from_props_for_wire (props wire)
  (let ((names nil)
        (loose_names nil)
        (prop_name "")
        (value "")
        (x "")
        (y ""))
    (foreach prop props
      (setq prop_name (upperCase (safeGetProp prop "name")))
      (when (_coco_member_string prop_name (list "SIG_NAME" "HDL_POWER" "HDL_TAP"))
        (setq value (safeGetProp prop "value"))
        (setq x (_coco_conn_prop_attach_x prop))
        (setq y (_coco_conn_prop_attach_y prop))
        (cond
          ((and (not (equal value ""))
                (not (equal x ""))
                (not (equal y ""))
                (_coco_conn_point_on_wire x y wire))
            (setq names (_coco_cb_add_net_name value names)))
          ((and (not (equal value ""))
                (or (equal x "") (equal y "")))
            (setq loose_names (_coco_cb_add_net_name value loose_names))))))
    (if names names (_coco_cb_single_name_or_nil loose_names))))

(defun _coco_cb_props_from_wire_bbox_include (handle wire include_cmd)
  (let ((x1 (atoi (_coco_conn_wire_x1 wire)))
        (y1 (atoi (_coco_conn_wire_y1 wire)))
        (x2 (atoi (_coco_conn_wire_x2 wire)))
        (y2 (atoi (_coco_conn_wire_y2 wire)))
        (pad 25)
        (select_result nil)
        (include_result nil)
        (group "")
        (include_group "")
        (group_obj nil)
        (props nil)
        (result nil))
    (setq result
      (errset
        (_coco_send_cmd
          handle
          (sprintf nil "Select (%d %d) (%d %d)"
            (difference (_coco_cb_min_int x1 x2) pad)
            (difference (_coco_cb_min_int y1 y2) pad)
            (plus (_coco_cb_max_int x1 x2) pad)
            (plus (_coco_cb_max_int y1 y2) pad)))
        nil))
    (when result
      (setq select_result (car result))
      (setq group (safeStr (_coco_safe_apply 'cnGetGroupName (list select_result))))
      (setq include_result (errset (_coco_send_cmd handle include_cmd) nil))
      (when include_result
        (setq include_group (safeStr (_coco_safe_apply 'cnGetGroupName (list (car include_result)))))
        (setq group_obj (_coco_safe_apply 'cnGetGroupedObjectSet (list (car include_result))))
        (when group_obj
          (setq props (_coco_safe_apply 'cnGetProps (list group_obj)))))
      (when (and (not (equal include_group "")) (not (equal include_group group)))
        (_coco_cleanup_iterator handle (_coco_make_iterator include_group nil)))
      (_coco_cleanup_iterator handle (_coco_make_iterator group nil)))
    props))

(defun _coco_cb_net_names_by_wire_bbox (handle wire)
  (let ((names nil)
        (loose_names nil)
        (props nil)
        (prop_names nil)
        (candidate_names nil))
    (foreach include_cmd (list "include nets properties" "include bodies properties" "include properties")
      (setq props (_coco_cb_props_from_wire_bbox_include handle wire include_cmd))
      (setq prop_names (_coco_cb_net_names_from_props_for_wire props wire))
      (foreach name prop_names
        (when (not (_coco_member_string name names))
          (setq names (append names (list name)))))
      (when (not prop_names)
        (setq candidate_names (_coco_cb_net_name_candidates_from_props props))
        (foreach name candidate_names
          (setq loose_names (_coco_cb_add_net_name name loose_names)))))
    (if names names (_coco_cb_single_name_or_nil loose_names))))

(defun _coco_cb_wire_json (handle wire page_json)
  (let ((net_names (_coco_cb_net_names_for_wire handle wire)))
    (_coco_json_object
      (list
        (_coco_json_string_field "dbid" (_coco_conn_wire_dbid wire))
        (_coco_json_string_field "x1" (_coco_conn_wire_x1 wire))
        (_coco_json_string_field "y1" (_coco_conn_wire_y1 wire))
        (_coco_json_string_field "x2" (_coco_conn_wire_x2 wire))
        (_coco_json_string_field "y2" (_coco_conn_wire_y2 wire))
        (_coco_json_string_field "style" (_coco_nth 5 wire))
        (_coco_json_string_field "flags" (_coco_nth 6 wire))
        (_coco_json_field "net_names" (_coco_json_array (mapcar '_coco_json_string net_names)))
        (_coco_json_field "page" page_json)))))

(defun _coco_cb_wires_json (handle wires page_json)
  (let ((items nil))
    (foreach wire wires
      (setq items (append items (list (_coco_cb_wire_json handle wire page_json)))))
    (_coco_json_array items)))

(defun _coco_cb_net_names (handle wires)
  (let ((items nil)
        (names nil))
    (foreach wire wires
      (setq names (_coco_cb_net_names_for_wire handle wire))
      (foreach name names
        (when (not (_coco_member_string name items))
          (setq items (append items (list name))))))
    items))

(defun _coco_cb_nets_json (names)
  (let ((items nil))
    (foreach name names
      (setq items
        (append items
          (list (_coco_json_object (list (_coco_json_string_field "name" name)))))))
    (_coco_json_array items)))

(defun _coco_cb_summary_json (components wires nets)
  (_coco_json_object
    (list
      (_coco_json_number_field "component_count" (length components))
      (_coco_json_number_field "wire_count" (length wires))
      (_coco_json_number_field "net_count" (length nets)))))

(setq _coco_cb_result_json nil)
(setq _coco_cb_target_refdes "")
(setq _coco_cb_active_dwg nil)

(defun _coco_cb_page_item (handle page_name)
  (let ((comp_tuples nil)
        (all_pins nil)
        (all_wires nil)
        (target_pins nil)
        (page_json "")
        (expanded nil)
        (components nil)
        (blocked_components nil)
        (pins nil)
        (wires nil)
        (nets nil))
    (when (not _coco_cb_result_json)
      (setq comp_tuples (_coco_component_tuples handle))
      (setq all_pins (_coco_conn_all_page_pins handle))
      (setq target_pins (_coco_cb_pins_for_refdes all_pins _coco_cb_target_refdes))
      (when target_pins
        (setq _coco_conn_page_net_names
          (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
        (setq _coco_conn_page_signal_props (_coco_conn_signal_props_for_page handle))
        (setq all_wires (_coco_wire_tuples handle))
        (setq page_json (_coco_page_json_from_name _coco_cb_active_dwg page_name))
        (setq expanded (_coco_cb_expand _coco_cb_target_refdes all_pins all_wires))
        (setq components (_coco_cb_refdes_from_pins (_coco_nth 2 expanded)))
        (setq blocked_components (_coco_nth 1 expanded))
        (setq pins (_coco_nth 2 expanded))
        (setq wires (_coco_nth 3 expanded))
        (setq nets (_coco_cb_net_names handle wires))
        (setq _coco_cb_result_json
          (_coco_json_object
            (list
              (_coco_json_string_field "refdes" _coco_cb_target_refdes)
              (_coco_json_field "page" page_json)
              (_coco_json_field "components" (_coco_cb_components_json handle components comp_tuples all_pins blocked_components page_json))
              (_coco_json_field "wires" (_coco_cb_wires_json handle wires page_json))
              (_coco_json_field "nets" (_coco_cb_nets_json nets))
              (_coco_json_field "summary" (_coco_cb_summary_json components wires nets)))))))))

(defun _coco_cb_extract_json (handle refdes)
  (setq _coco_cb_result_json nil)
  (setq _coco_cb_target_refdes (safeStr refdes))
  (setq _coco_cb_active_dwg (_coco_active_dwg handle))
  (_coco_each_design_page
    handle
    _coco_cb_active_dwg
    (_coco_resolved_active_page_name handle _coco_cb_active_dwg)
    '_coco_cb_page_item)
  _coco_cb_result_json)

(defun _coco_op_circuit_block (arg)
  (let ((handle nil)
        (refdes (safeStr arg))
        (block_json nil))
    (when (equal refdes "")
      (error "MISSING_REFDES" "circuit_block requires a refdes argument"))
    (setq handle (_coco_import_handle))
    (setq block_json (_coco_cb_extract_json handle refdes))
    (when (not block_json)
      (error "MISSING_REFDES" (strcat "RefDes not found: " refdes)))
    (_coco_json_object
      (list
        (_coco_json_string_field "refdes" refdes)
        (_coco_json_field "import_context_page" (_coco_active_page_json handle))
        (_coco_json_field "block" block_json)))))
