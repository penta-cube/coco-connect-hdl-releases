;;; net_detail.il - design-wide logical net inspection
;;;
;;; Public operation:
;;;   net_detail
;;;
;;; Same-name net segments from every page are merged. Page identity is kept
;;; only on wire coordinates because those coordinates are page-local.

(setq _coco_net_detail_target "")
(setq _coco_net_detail_active_dwg nil)
(setq _coco_net_detail_pin_items nil)
(setq _coco_net_detail_pin_keys nil)
(setq _coco_net_detail_component_keys nil)
(setq _coco_net_detail_wire_items nil)
(setq _coco_net_detail_wire_keys nil)

(defun _coco_net_detail_name_key (name)
  (upperCase (_coco_cb_normalize_net_name name)))

(defun _coco_net_detail_name_matches (name)
  (equal (_coco_net_detail_name_key name) _coco_net_detail_target))

(defun _coco_net_detail_wire_key (page wire)
  (let ((dbid (_coco_conn_wire_dbid wire)))
    (when (equal dbid "")
      (setq dbid
        (strcat
          (_coco_conn_wire_p1 wire)
          "|"
          (_coco_conn_wire_p2 wire))))
    (strcat (safeStr page) "|" dbid)))

(defun _coco_net_detail_add_unique_wire (wire wires)
  (let ((found nil)
        (key (_coco_conn_wire_dbid wire)))
    (foreach item wires
      (when (or
              (and (not (equal key ""))
                   (equal key (_coco_conn_wire_dbid item)))
              (_coco_conn_same_wire_geometry wire item))
        (setq found t)))
    (if found wires (append wires (list wire)))))

(defun _coco_net_detail_wire_in_list (wire wires)
  (let ((found nil)
        (key (_coco_conn_wire_dbid wire)))
    (foreach item wires
      (when (or
              (and (not (equal key ""))
                   (equal key (_coco_conn_wire_dbid item)))
              (_coco_conn_same_wire_geometry wire item))
        (setq found t)))
    found))

(defun _coco_net_detail_target_signal_props ()
  (let ((items nil))
    (foreach entry _coco_conn_page_signal_props
      (when (_coco_net_detail_name_matches (_coco_nth 0 entry))
        (setq items (append items (list entry)))))
    items))

(defun _coco_net_detail_seed_wires (handle wires target_props)
  (let ((seeds nil)
        (x "")
        (y "")
        (names nil)
        (matched nil))
    ; Fast path: use SIG_NAME attachment points to identify seed wires.
    (foreach entry target_props
      (setq x (safeStr (_coco_nth 1 entry)))
      (setq y (safeStr (_coco_nth 2 entry)))
      (when (and (not (equal x "")) (not (equal y "")))
        (foreach wire wires
          (when (_coco_conn_point_on_wire x y wire)
            (setq seeds (_coco_net_detail_add_unique_wire wire seeds))))))
    ; Fallback for net names stored directly on a wire or its endpoint.
    (when (not seeds)
      (foreach wire wires
        (setq matched nil)
        (setq names (_coco_cb_net_names_for_wire handle wire))
        (foreach name names
          (when (_coco_net_detail_name_matches name)
            (setq matched t)))
        (when matched
          (setq seeds (_coco_net_detail_add_unique_wire wire seeds)))))
    seeds))

(defun _coco_net_detail_trace_wires (all_wires seeds)
  (let ((frontier seeds)
        (result nil)
        (front nil))
    (foreach wire seeds
      (setq result (_coco_net_detail_add_unique_wire wire result)))
    (while frontier
      (setq front (car frontier))
      (setq frontier (cdr frontier))
      (foreach wire all_wires
        (when (and
                (not (_coco_net_detail_wire_in_list wire result))
                (_coco_cb_wire_connected_to_wire_p front wire))
          (setq result (_coco_net_detail_add_unique_wire wire result))
          (setq frontier (append frontier (list wire))))))
    result))

(defun _coco_net_detail_pin_on_wires (pin_entry wires)
  (let ((found nil)
        (x (safeStr (_coco_nth 2 pin_entry)))
        (y (safeStr (_coco_nth 3 pin_entry))))
    (foreach wire wires
      (when (_coco_conn_point_on_wire x y wire)
        (setq found t)))
    found))

(defun _coco_net_detail_add_pin (pin_entry)
  (let ((refdes (safeStr (_coco_nth 0 pin_entry)))
        (pin_name (safeStr (_coco_nth 1 pin_entry)))
        (pin_key "")
        (component_key ""))
    (setq pin_key (strcat (upperCase refdes) "|" (upperCase pin_name)))
    (setq component_key (upperCase refdes))
    (when (not (_coco_member_string pin_key _coco_net_detail_pin_keys))
      (setq _coco_net_detail_pin_keys
        (append _coco_net_detail_pin_keys (list pin_key)))
      (setq _coco_net_detail_pin_items
        (append _coco_net_detail_pin_items
          (list
            (_coco_json_object
              (list
                (_coco_json_string_field "refdes" refdes)
                (_coco_json_string_field "pin" pin_name)))))))
    (when (and
            (not (equal component_key ""))
            (not (_coco_member_string component_key _coco_net_detail_component_keys)))
      (setq _coco_net_detail_component_keys
        (append _coco_net_detail_component_keys (list component_key))))))

(defun _coco_net_detail_wire_json (page wire)
  (_coco_json_object
    (list
      (_coco_json_string_field "dbid" (_coco_conn_wire_dbid wire))
      (_coco_json_string_field "page" page)
      (_coco_json_field
        "points"
        (_coco_json_array
          (list
            (_coco_json_object
              (list
                (_coco_json_number_field "x" (_coco_conn_wire_x1 wire))
                (_coco_json_number_field "y" (_coco_conn_wire_y1 wire))))
            (_coco_json_object
              (list
                (_coco_json_number_field "x" (_coco_conn_wire_x2 wire))
                (_coco_json_number_field "y" (_coco_conn_wire_y2 wire))))))))))

(defun _coco_net_detail_add_wire (page wire)
  (let ((key (_coco_net_detail_wire_key page wire)))
    (when (not (_coco_member_string key _coco_net_detail_wire_keys))
      (setq _coco_net_detail_wire_keys
        (append _coco_net_detail_wire_keys (list key)))
      (setq _coco_net_detail_wire_items
        (append _coco_net_detail_wire_items
          (list (_coco_net_detail_wire_json page wire)))))))

(defun _coco_net_detail_page_item (handle page_name)
  (let ((target_props nil)
        (wires nil)
        (seeds nil)
        (matched_wires nil)
        (pins nil)
        (page (_coco_page_number_from_name page_name)))
    ; Property-first page filtering avoids loading wire/component tuples on
    ; pages that do not contain the requested net name.
    (setq _coco_conn_page_signal_props (_coco_conn_signal_props_for_page handle))
    (setq target_props (_coco_net_detail_target_signal_props))
    (when target_props
      (setq wires (_coco_wire_tuples handle))
      (setq seeds (_coco_net_detail_seed_wires handle wires target_props))
      (setq matched_wires (_coco_net_detail_trace_wires wires seeds))
      (when matched_wires
        (foreach wire matched_wires
          (_coco_net_detail_add_wire page wire))
        ; Component and pin construction is deferred until a matching wire
        ; exists on the page.
        (setq pins (_coco_conn_all_page_pins handle))
        (foreach pin_entry pins
          (when (_coco_net_detail_pin_on_wires pin_entry matched_wires)
            (_coco_net_detail_add_pin pin_entry)))))))

(defun _coco_net_detail_result (handle net_name)
  (let ((original_page ""))
    (setq _coco_net_detail_target (_coco_net_detail_name_key net_name))
    (setq _coco_net_detail_active_dwg (_coco_active_dwg handle))
    (setq _coco_net_detail_pin_items nil)
    (setq _coco_net_detail_pin_keys nil)
    (setq _coco_net_detail_component_keys nil)
    (setq _coco_net_detail_wire_items nil)
    (setq _coco_net_detail_wire_keys nil)
    (setq original_page
      (_coco_resolved_active_page_name handle _coco_net_detail_active_dwg))
    (_coco_each_design_page
      handle
      _coco_net_detail_active_dwg
      original_page
      '_coco_net_detail_page_item)
    (_coco_json_object
      (list
        (_coco_json_string_field "name" (_coco_cb_normalize_net_name net_name))
        (_coco_json_bool_field "found" _coco_net_detail_wire_items)
        (_coco_json_field "connected_pins" (_coco_json_array _coco_net_detail_pin_items))
        (_coco_json_field "wires" (_coco_json_array _coco_net_detail_wire_items))
        (_coco_json_field
          "summary"
          (_coco_json_object
            (list
              (_coco_json_number_field "component_count" (length _coco_net_detail_component_keys))
              (_coco_json_number_field "pin_count" (length _coco_net_detail_pin_items))
              (_coco_json_number_field "wire_count" (length _coco_net_detail_wire_items)))))))))

(defun _coco_op_net_detail (arg)
  (let ((net_name (safeStr arg))
        (handle nil))
    (when (equal net_name "")
      (error "MISSING_NET_NAME" "net_detail requires a net name argument"))
    (setq handle (_coco_import_handle))
    (_coco_net_detail_result handle net_name)))
