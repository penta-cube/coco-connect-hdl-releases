;;; connectivity.il - C08 pin/wire/net connectivity inspection
;;;
;;; Public operation:
;;;   pin_connectivity

(defun _coco_conn_split_pipe (arg)
  (let ((str (safeStr arg))
        (i 1)
        (len 0)
        (ch "")
        (left "")
        (right "")
        (seen nil))
    (setq len (strlen str))
    (while (leqp i len)
      (setq ch (substring str i 1))
      (if (and (not seen) (equal ch "|"))
        (setq seen t)
        (if seen
          (setq right (strcat right ch))
          (setq left (strcat left ch))))
      (setq i (add1 i)))
    (list left right)))

(defun _coco_conn_point_key (x y)
  (strcat (safeStr x) "," (safeStr y)))

(defun _coco_conn_string_contains (text needle)
  (let ((hay (safeStr text))
        (target (safeStr needle))
        (hay_len 0)
        (target_len 0)
        (idx 1)
        (found nil))
    (setq hay_len (strlen hay))
    (setq target_len (strlen target))
    (if (equal target_len 0)
      t
      (progn
        (while (and (not found) (leqp idx (add1 (difference hay_len target_len))))
          (when (equal (substring hay idx target_len) target)
            (setq found t))
          (setq idx (add1 idx)))
        found))))

(defun _coco_conn_string_ends_with (text suffix)
  (let ((hay (safeStr text))
        (tail (safeStr suffix))
        (hay_len 0)
        (tail_len 0))
    (setq hay_len (strlen hay))
    (setq tail_len (strlen tail))
    (and (leqp tail_len hay_len)
         (equal (substring hay (add1 (difference hay_len tail_len)) tail_len) tail))))

(defun _coco_conn_wire_dbid (wire)
  (let ((value (_coco_safe_apply 'cnGetWireDbId (list wire))))
    (if value
      (safeStr value)
      (safeStr (_coco_nth 0 wire)))))

(defun _coco_conn_wire_dbid_value (wire)
  (let ((value (_coco_safe_apply 'cnGetWireDbId (list wire))))
    (if value value (_coco_nth 0 wire))))

(defun _coco_conn_wire_x1 (wire)
  (let ((point (_coco_safe_apply 'cnGetWireXY1 (list wire)))
        (value nil))
    (when point
      (setq value (_coco_safe_apply 'cnGetPointX (list point))))
    (if value (safeStr value) (safeStr (_coco_nth 1 wire)))))

(defun _coco_conn_wire_y1 (wire)
  (let ((point (_coco_safe_apply 'cnGetWireXY1 (list wire)))
        (value nil))
    (when point
      (setq value (_coco_safe_apply 'cnGetPointY (list point))))
    (if value (safeStr value) (safeStr (_coco_nth 2 wire)))))

(defun _coco_conn_wire_x2 (wire)
  (let ((point (_coco_safe_apply 'cnGetWireXY2 (list wire)))
        (value nil))
    (when point
      (setq value (_coco_safe_apply 'cnGetPointX (list point))))
    (if value (safeStr value) (safeStr (_coco_nth 3 wire)))))

(defun _coco_conn_wire_y2 (wire)
  (let ((point (_coco_safe_apply 'cnGetWireXY2 (list wire)))
        (value nil))
    (when point
      (setq value (_coco_safe_apply 'cnGetPointY (list point))))
    (if value (safeStr value) (safeStr (_coco_nth 4 wire)))))

(defun _coco_conn_wire_p1 (wire)
  (_coco_conn_point_key (_coco_conn_wire_x1 wire) (_coco_conn_wire_y1 wire)))

(defun _coco_conn_wire_p2 (wire)
  (_coco_conn_point_key (_coco_conn_wire_x2 wire) (_coco_conn_wire_y2 wire)))

(defun _coco_conn_wire_touches_point (wire point_key)
  (or (equal (_coco_conn_wire_p1 wire) point_key)
      (equal (_coco_conn_wire_p2 wire) point_key)))

(defun _coco_conn_same_wire_geometry (left right)
  (let ((left_p1 (_coco_conn_wire_p1 left))
        (left_p2 (_coco_conn_wire_p2 left))
        (right_p1 (_coco_conn_wire_p1 right))
        (right_p2 (_coco_conn_wire_p2 right)))
    (or
      (and (equal left_p1 right_p1) (equal left_p2 right_p2))
      (and (equal left_p1 right_p2) (equal left_p2 right_p1)))))

(defun _coco_conn_props_for_wire (handle wire include_nets)
  (let ((wire_id nil)
        (wire_group nil)
        (select_result nil)
        (group "")
        (group_obj nil)
        (props nil)
        (result nil))
    (setq wire_id (_coco_conn_wire_dbid_value wire))
    (when wire_id
      (setq wire_group (_coco_safe_apply 'cnGroupObjects (list handle (list wire_id)))))
    (when wire_group
      (setq result
        (errset
          (_coco_send_cmd
            handle
            (if include_nets "include nets properties" "include properties"))
          nil))
      (when result
        (setq select_result (car result))
        (setq group (safeStr (_coco_safe_apply 'cnGetGroupName (list select_result))))
        (setq group_obj (_coco_safe_apply 'cnGetGroupedObjectSet (list select_result)))
        (when group_obj
          (setq props (_coco_safe_apply 'cnGetProps (list group_obj)))))
      (_coco_cleanup_iterator handle (_coco_make_iterator group nil)))
    props))

(setq _coco_conn_page_net_names nil)
(setq _coco_conn_page_signal_props nil)

(defun _coco_conn_prop_attach_x (prop)
  (let ((point (_coco_safe_apply 'cnGetPropAttachXY (list prop)))
        (value nil))
    (when point
      (setq value (_coco_safe_apply 'cnGetPointX (list point))))
    (if value (safeStr value) "")))

(defun _coco_conn_prop_attach_y (prop)
  (let ((point (_coco_safe_apply 'cnGetPropAttachXY (list prop)))
        (value nil))
    (when point
      (setq value (_coco_safe_apply 'cnGetPointY (list point))))
    (if value (safeStr value) "")))

(defun _coco_conn_signal_prop_entry (prop)
  (list
    (safeGetProp prop "value")
    (_coco_conn_prop_attach_x prop)
    (_coco_conn_prop_attach_y prop)
    (_coco_prop_owner_dbid_string prop)
    (_coco_prop_owner_type_string prop)))

(defun _coco_conn_signal_props_for_page (handle)
  (let ((props (_coco_props_from_find handle "find sig_name=*"))
        (entries nil)
        (value ""))
    (foreach prop props
      (setq value (safeGetProp prop "value"))
      (when (not (equal value ""))
        (setq entries (append entries (list (_coco_conn_signal_prop_entry prop))))))
    entries))

(defun _coco_conn_between_int (value a b)
  (or
    (and (leqp a value) (leqp value b))
    (and (leqp b value) (leqp value a))))

(defun _coco_conn_point_on_wire (x_str y_str wire)
  (let ((x (atoi x_str))
        (y (atoi y_str))
        (x1 (atoi (_coco_conn_wire_x1 wire)))
        (y1 (atoi (_coco_conn_wire_y1 wire)))
        (x2 (atoi (_coco_conn_wire_x2 wire)))
        (y2 (atoi (_coco_conn_wire_y2 wire))))
    (or
      (and (equal y y1)
           (equal y1 y2)
           (_coco_conn_between_int x x1 x2))
      (and (equal x x1)
           (equal x1 x2)
           (_coco_conn_between_int y y1 y2)))))

(defun _coco_conn_net_names_by_signal_props_for_wire (wire)
  (let ((names nil)
        (name "")
        (x "")
        (y ""))
    (foreach entry _coco_conn_page_signal_props
      (setq name (_coco_nth 0 entry))
      (setq x (_coco_nth 1 entry))
      (setq y (_coco_nth 2 entry))
      (when (and (not (equal name ""))
                 (not (equal x ""))
                 (not (equal y ""))
                 (_coco_conn_point_on_wire x y wire)
                 (not (_coco_member_string name names)))
        (setq names (append names (list name)))))
    names))

(defun _coco_conn_prop_attach_key (prop)
  (let ((point (_coco_safe_apply 'cnGetPropAttachXY (list prop)))
        (x "")
        (y ""))
    (when point
      (setq x (safeStr (_coco_safe_apply 'cnGetPointX (list point))))
      (setq y (safeStr (_coco_safe_apply 'cnGetPointY (list point)))))
    (_coco_conn_point_key x y)))

(defun _coco_conn_names_from_props_at_point (props point_key)
  (let ((names nil)
        (name "")
        (value ""))
    (foreach prop props
      (setq name (upperCase (safeGetProp prop "name")))
      (when (and (_coco_member_string name (list "SIG_NAME" "HDL_POWER" "HDL_TAP"))
                 (equal (_coco_conn_prop_attach_key prop) point_key))
        (setq value (safeGetProp prop "value"))
        (when (and (not (equal value ""))
                   (not (_coco_member_string value names)))
          (setq names (append names (list value))))))
    names))

(defun _coco_conn_props_near_point (handle x_str y_str)
  (let ((x (atoi x_str))
        (y (atoi y_str))
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
            (difference x 5)
            (difference y 5)
            (plus x 5)
            (plus y 5)))
        nil))
    (when result
      (setq select_result (car result))
      (setq group (safeStr (_coco_safe_apply 'cnGetGroupName (list select_result))))
      (setq include_result (errset (_coco_send_cmd handle "include bodies properties") nil))
      (when include_result
        (setq include_group (safeStr (_coco_safe_apply 'cnGetGroupName (list (car include_result)))))
        (setq group_obj (_coco_safe_apply 'cnGetGroupedObjectSet (list (car include_result))))
        (when group_obj
          (setq props (_coco_safe_apply 'cnGetProps (list group_obj)))))
      (when (and (not (equal include_group "")) (not (equal include_group group)))
        (_coco_cleanup_iterator handle (_coco_make_iterator include_group nil)))
      (_coco_cleanup_iterator handle (_coco_make_iterator group nil)))
    props))

(defun _coco_conn_net_names_by_endpoint_props_for_wire (handle wire)
  (let ((names nil)
        (point_key1 (_coco_conn_wire_p1 wire))
        (point_key2 (_coco_conn_wire_p2 wire))
        (props nil)
        (point_names nil))
    (setq props (_coco_conn_props_near_point handle (_coco_conn_wire_x1 wire) (_coco_conn_wire_y1 wire)))
    (setq point_names (_coco_conn_names_from_props_at_point props point_key1))
    (foreach name point_names
      (when (not (_coco_member_string name names))
        (setq names (append names (list name)))))
    (setq props (_coco_conn_props_near_point handle (_coco_conn_wire_x2 wire) (_coco_conn_wire_y2 wire)))
    (setq point_names (_coco_conn_names_from_props_at_point props point_key2))
    (foreach name point_names
      (when (not (_coco_member_string name names))
        (setq names (append names (list name)))))
    names))

(defun _coco_conn_net_names_for_wire (handle wire)
  (let ((props nil)
        (names nil)
        (value ""))
    (setq props (_coco_conn_props_for_wire handle wire t))
    (foreach prop props
      (when (_coco_member_string (upperCase (safeGetProp prop "name")) (list "SIG_NAME" "HDL_POWER" "HDL_TAP"))
        (setq value (safeGetProp prop "value"))
        (when (and (not (equal value ""))
                   (not (_coco_member_string value names)))
          (setq names (append names (list value))))))
    (when (not names)
      (setq names (_coco_conn_net_names_by_signal_props_for_wire wire)))
    (when (not names)
      (setq names (_coco_conn_net_names_by_endpoint_props_for_wire handle wire)))
    names))

(defun _coco_conn_wire_visited (wire visited_dbids)
  (_coco_member_string (_coco_conn_wire_dbid wire) visited_dbids))

(defun _coco_conn_trace_wires_from_point (wires start_point)
  (let ((frontier (list start_point))
        (front "")
        (visited_points nil)
        (visited_dbids nil)
        (result nil)
        (p1 "")
        (p2 "")
        (dbid ""))
    (while frontier
      (setq front (car frontier))
      (setq frontier (cdr frontier))
      (when (not (_coco_member_string front visited_points))
        (setq visited_points (append visited_points (list front)))
        (foreach wire wires
          (setq dbid (_coco_conn_wire_dbid wire))
          (when (and (not (_coco_member_string dbid visited_dbids))
                     (_coco_conn_wire_touches_point wire front))
            (setq visited_dbids (append visited_dbids (list dbid)))
            (setq result (append result (list wire)))
            (setq p1 (_coco_conn_wire_p1 wire))
            (setq p2 (_coco_conn_wire_p2 wire))
            (when (not (_coco_member_string p1 visited_points))
              (setq frontier (append frontier (list p1))))
            (when (not (_coco_member_string p2 visited_points))
              (setq frontier (append frontier (list p2))))))))
    result))

(defun _coco_conn_points_from_wires (wires)
  (let ((points nil)
        (p1 "")
        (p2 ""))
    (foreach wire wires
      (setq p1 (_coco_conn_wire_p1 wire))
      (setq p2 (_coco_conn_wire_p2 wire))
      (when (not (_coco_member_string p1 points))
        (setq points (append points (list p1))))
      (when (not (_coco_member_string p2 points))
        (setq points (append points (list p2)))))
    points))

(defun _coco_conn_pin_entry (refdes pin comp_tuple props)
  (list
    refdes
    (_coco_detail_pin_name pin)
    (_coco_detail_pin_x pin)
    (_coco_detail_pin_y pin)
    (safeStr (_coco_nth 1 comp_tuple))
    (_coco_first_nonempty_prop_value props (list "PATH" "Path" "SOURCE_PATH" "INSTANCE" "Instance"))
    (_coco_first_nonempty_prop_value props (list "PART_NAME" "Part Name" "PART_NUMBER"))
    (_coco_first_nonempty_prop_value props (list "DEVICE" "Device"))
    (_coco_first_nonempty_prop_value props (list "PCB Footprint" "PCB_FOOTPRINT" "JEDEC_TYPE" "PACK_TYPE"))))

(defun _coco_conn_net_matches_pin_token (net_name token)
  (and
    (not (equal token ""))
    (_coco_conn_string_contains net_name (strcat "$" token "$"))))

(defun _coco_conn_generated_net_matches_pin_entry (net_name pin_entry)
  (let ((pin_name (safeStr (_coco_nth 1 pin_entry)))
        (symbol_name (safeStr (_coco_nth 4 pin_entry)))
        (path_name (safeStr (_coco_nth 5 pin_entry)))
        (part_name (safeStr (_coco_nth 6 pin_entry)))
        (device (safeStr (_coco_nth 7 pin_entry)))
        (footprint (safeStr (_coco_nth 8 pin_entry)))
        (pin_suffix ""))
    (setq pin_suffix (strcat "$" pin_name))
    (and
      (_coco_conn_string_ends_with net_name pin_suffix)
      (or
        (_coco_conn_net_matches_pin_token net_name symbol_name)
        (_coco_conn_net_matches_pin_token net_name part_name)
        (_coco_conn_net_matches_pin_token net_name device)
        (_coco_conn_net_matches_pin_token net_name footprint)
        (and
          (not (equal path_name ""))
          (_coco_conn_string_contains net_name (strcat "$" path_name "$")))))))

(defun _coco_conn_all_page_pins (handle)
  (let ((refdes_list (_coco_component_refdes_list handle))
        (comp_tuples (_coco_component_tuples handle))
        (items nil)
        (props nil)
        (xy "")
        (comp_tuple nil)
        (sym_info nil)
        (pins nil))
    (foreach refdes refdes_list
      (setq props (_coco_component_props_for_refdes handle refdes))
      (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
      (setq comp_tuple (_coco_find_component_tuple_by_xy xy comp_tuples))
      (when comp_tuple
        (setq sym_info (_coco_safe_apply 'cnGetCompSymInfo (list handle comp_tuple)))
        (when sym_info
          (setq pins (_coco_safe_apply 'cnCreateCompPins (list comp_tuple sym_info)))
          (foreach pin pins
            (setq items (append items (list (_coco_conn_pin_entry refdes pin comp_tuple props))))))))
    items))

(defun _coco_conn_connected_pin_json (pin_entry)
  (_coco_json_object
    (list
      (_coco_json_string_field "refdes" (_coco_nth 0 pin_entry))
      (_coco_json_string_field "pin_name" (_coco_nth 1 pin_entry))
      (_coco_json_string_field "x" (_coco_nth 2 pin_entry))
      (_coco_json_string_field "y" (_coco_nth 3 pin_entry)))))

(defun _coco_conn_other_connected_pins_json (pin_entries point_keys target_refdes target_pin_name)
  (let ((items nil)
        (point "")
        (refdes "")
        (pin_name ""))
    (foreach pin_entry pin_entries
      (setq point (_coco_conn_point_key (_coco_nth 2 pin_entry) (_coco_nth 3 pin_entry)))
      (setq refdes (safeStr (_coco_nth 0 pin_entry)))
      (setq pin_name (safeStr (_coco_nth 1 pin_entry)))
      (when (and
              (_coco_member_string point point_keys)
              (not (and (equal refdes target_refdes) (equal pin_name target_pin_name))))
        (setq items (append items (list (_coco_conn_connected_pin_json pin_entry))))))
    (_coco_json_array items)))

(defun _coco_conn_find_pin_entry (pin_entries refdes pin_name x y)
  (let ((found nil)
        (entry_refdes "")
        (entry_pin "")
        (entry_x "")
        (entry_y ""))
    (foreach pin_entry pin_entries
      (when (not found)
        (setq entry_refdes (safeStr (_coco_nth 0 pin_entry)))
        (setq entry_pin (safeStr (_coco_nth 1 pin_entry)))
        (setq entry_x (safeStr (_coco_nth 2 pin_entry)))
        (setq entry_y (safeStr (_coco_nth 3 pin_entry)))
        (when (and
                (equal entry_refdes refdes)
                (equal entry_pin pin_name)
                (equal entry_x x)
                (equal entry_y y))
          (setq found pin_entry))))
    found))

(defun _coco_conn_first_string (items)
  (if items (safeStr (car items)) ""))

(defun _coco_conn_generated_net_names_for_pin_entry (pin_entry)
  (let ((names nil)
        (matched nil))
    (foreach net_name _coco_conn_page_net_names
      (setq matched (_coco_conn_generated_net_matches_pin_entry net_name pin_entry))
      (when (and matched (not (_coco_member_string net_name names)))
        (setq names (append names (list net_name)))))
    names))

(defun _coco_conn_net_names_by_connected_pins (pin_entries point_keys)
  (let ((names nil)
        (point "")
        (pin_names nil))
    (foreach pin_entry pin_entries
      (setq point (_coco_conn_point_key (_coco_nth 2 pin_entry) (_coco_nth 3 pin_entry)))
      (when (_coco_member_string point point_keys)
        (setq pin_names (_coco_conn_generated_net_names_for_pin_entry pin_entry))
        (foreach name pin_names
          (when (not (_coco_member_string name names))
            (setq names (append names (list name)))))))
    names))

(defun _coco_conn_net_names_for_target_pin (target_pin_entry pin_entries point_keys)
  (let ((names nil))
    (when target_pin_entry
      (setq names (_coco_conn_generated_net_names_for_pin_entry target_pin_entry)))
    (when (not names)
      (setq names (_coco_conn_net_names_by_connected_pins pin_entries point_keys)))
    names))

(defun _coco_conn_net_names_from_wires (handle wires)
  (let ((names nil)
        (wire_names nil))
    (foreach wire wires
      (setq wire_names (_coco_conn_net_names_for_wire handle wire))
      (foreach name wire_names
        (when (not (_coco_member_string name names))
          (setq names (append names (list name))))))
    (if names (list (car names)) nil)))

(defun _coco_conn_pin_json (handle refdes pin all_pins wires)
  (let ((point (_coco_conn_point_key (_coco_detail_pin_x pin) (_coco_detail_pin_y pin)))
        (trace_wires nil)
        (trace_points nil)
        (net_names nil)
        (pin_name "")
        (x "")
        (y "")
        (target_entry nil))
    (setq pin_name (_coco_detail_pin_name pin))
    (setq x (_coco_detail_pin_x pin))
    (setq y (_coco_detail_pin_y pin))
    (setq trace_wires (_coco_conn_trace_wires_from_point wires point))
    (setq trace_points (_coco_conn_points_from_wires trace_wires))
    (setq net_names (_coco_conn_net_names_from_wires handle trace_wires))
    (when (not net_names)
      (setq target_entry (_coco_conn_find_pin_entry all_pins refdes pin_name x y))
      (setq net_names (_coco_conn_net_names_for_target_pin target_entry all_pins trace_points)))
    (when net_names
      (setq net_names (list (car net_names))))
    (_coco_json_object
      (list
        (_coco_json_string_field "pin_name" pin_name)
        (_coco_json_string_field "x" x)
        (_coco_json_string_field "y" y)
        (_coco_json_field "connected" (if trace_wires "true" "false"))
        (_coco_json_string_field "net_name" (_coco_conn_first_string net_names))
        (_coco_json_field
          "connected_parts"
          (_coco_conn_other_connected_pins_json all_pins trace_points refdes pin_name))))))

(defun _coco_conn_component_pins_json (handle comp_tuple pin_filter all_pins wires)
  (let ((sym_info nil)
        (pins nil)
        (items nil)
        (pin_name ""))
    (when comp_tuple
      (setq sym_info (_coco_safe_apply 'cnGetCompSymInfo (list handle comp_tuple)))
      (when sym_info
        (setq pins (_coco_safe_apply 'cnCreateCompPins (list comp_tuple sym_info)))))
    (foreach pin pins
      (setq pin_name (_coco_detail_pin_name pin))
      (when (or (equal pin_filter "") (equal pin_filter pin_name))
        (setq items
          (append items
            (list (_coco_conn_pin_json handle _coco_conn_refdes pin all_pins wires))))))
    (_coco_json_array items)))

(setq _coco_conn_items_acc nil)
(setq _coco_conn_active_dwg nil)
(setq _coco_conn_refdes "")
(setq _coco_conn_pin_filter "")

(defun _coco_conn_page_item (handle page_name)
  (let ((props nil)
        (xy "")
        (comp_tuples nil)
        (comp_tuple nil)
        (all_pins nil)
        (wires nil)
        (pins_json "[]")
        (page_json ""))
    (setq comp_tuples (_coco_component_tuples handle))
    (setq props (_coco_component_props_for_refdes handle _coco_conn_refdes))
    (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
    (setq comp_tuple (_coco_find_component_tuple_by_xy xy comp_tuples))
    (when comp_tuple
      (setq _coco_conn_page_net_names
        (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
      (setq _coco_conn_page_signal_props (_coco_conn_signal_props_for_page handle))
      (setq all_pins (_coco_conn_all_page_pins handle))
      (setq wires (_coco_wire_tuples handle))
      (setq page_json (_coco_page_json_from_name _coco_conn_active_dwg page_name))
      (setq pins_json
        (_coco_conn_component_pins_json
          handle
          comp_tuple
          _coco_conn_pin_filter
          all_pins
          wires))
      (setq _coco_conn_items_acc
        (append _coco_conn_items_acc
          (list
            (_coco_json_object
              (list
                (_coco_json_string_field "refdes" _coco_conn_refdes)
                (_coco_json_field "page" page_json)
                (_coco_json_field "pins" pins_json)))))))))

(defun _coco_conn_components_json (handle refdes pin_filter)
  (setq _coco_conn_items_acc nil)
  (setq _coco_conn_active_dwg (_coco_active_dwg handle))
  (setq _coco_conn_refdes (safeStr refdes))
  (setq _coco_conn_pin_filter (safeStr pin_filter))
  (_coco_each_design_page
    handle
    _coco_conn_active_dwg
    (_coco_resolved_active_page_name handle _coco_conn_active_dwg)
    '_coco_conn_page_item)
  (_coco_json_array _coco_conn_items_acc))

(defun _coco_op_pin_connectivity (arg)
  (let ((parts (_coco_conn_split_pipe arg))
        (refdes "")
        (pin_filter "")
        (handle nil))
    (setq refdes (safeStr (_coco_nth 0 parts)))
    (setq pin_filter (safeStr (_coco_nth 1 parts)))
    (when (equal refdes "")
      (error "INVALID_ARG" "pin_connectivity requires a RefDes argument"))
    (setq handle (_coco_import_handle))
    (_coco_json_object
      (list
        (_coco_json_string_field "refdes" refdes)
        (_coco_json_string_field "pin_name_filter" pin_filter)
        (_coco_json_field "import_context_page" (_coco_active_page_json handle))
        (_coco_json_field "components" (_coco_conn_components_json handle refdes pin_filter))))))
