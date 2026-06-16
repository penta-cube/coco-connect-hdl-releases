;;; ui_navigation.il - C06 UI navigation helpers
;;;
;;; Public operations:
;;;   ui_navigate_part
;;;   ui_navigate_net

(defun _coco_ui_json_result (fields)
  (_coco_json_object fields))

(defun _coco_ui_send_interactive (handle cmd)
  (cnSendCommand handle cmd))

(defun _coco_ui_json_navigate (kind target result count mode)
  (_coco_ui_json_result
    (list
      (_coco_json_string_field "status" "ok")
      (_coco_json_string_field "action" "navigate")
      (_coco_json_string_field "kind" kind)
      (_coco_json_string_field "target" target)
      (_coco_json_string_field "result" result)
      (_coco_json_string_field "match_count" count)
      (_coco_json_string_field "mode" mode))))

(defun _coco_ui_json_not_found (kind target)
  (_coco_ui_json_result
    (list
      (_coco_json_string_field "status" "not_found")
      (_coco_json_string_field "action" "navigate")
      (_coco_json_string_field "kind" kind)
      (_coco_json_string_field "target" target))))

(defun _coco_ui_parse_xy (xy)
  (let ((x "")
        (y ""))
    (when (and (not (equal xy ""))
               (rexMatchp "^[ \t]*[(]?[ \t]*\\([-0-9]+\\)[ \t]*,[ \t]*\\([-0-9]+\\)[ \t]*[)]?[ \t]*$" xy))
      (setq x (rexSubstitute "\\1"))
      (setq y (rexSubstitute "\\2")))
    (list x y)))

(defun _coco_ui_point_from_xy_strings (x y)
  (let ((result nil))
    (setq result (errset (list (atoi x) (atoi y)) nil))
    (if result (car result) nil)))

(defun _coco_ui_try_point_highlight (handle x_str y_str)
  (let ((point nil)
        (result nil))
    (setq point (_coco_ui_point_from_xy_strings x_str y_str))
    (when point
      (setq result (errset (cnHighlightPoint handle point 2) nil))
      (when (not result)
        (setq result (errset (cnHighlightPoint handle point 3) nil)))
      (when (not result)
        (setq result (errset (cnHighlightPoint handle point 1) nil))))
    (if result t nil)))

(defun _coco_ui_try_name_highlight (handle name)
  (let ((result nil))
    (when (not (equal name ""))
      (setq result (errset (_coco_ui_send_interactive handle (sprintf nil "highlight %s" name)) nil)))
    (if result t nil)))

(defun _coco_ui_try_part_highlight (handle refdes path dbid x_str y_str)
  (let ((ok nil))
    (when (not (equal path ""))
      (setq ok (_coco_ui_try_name_highlight handle path)))
    (when (not ok)
      (setq ok (_coco_ui_try_name_highlight handle refdes)))
    (when (and (not ok) (not (equal dbid "")))
      (setq ok (_coco_ui_try_name_highlight handle dbid)))
    (when (not ok)
      (setq ok (_coco_ui_try_point_highlight handle x_str y_str)))
    (if ok 1 0)))

(setq _coco_ui_part_detail_active_dwg nil)
(setq _coco_ui_part_detail_refdes "")
(setq _coco_ui_part_detail_result nil)

(defun _coco_ui_part_detail_page_item (handle page_name)
  (let ((props nil)
        (xy "")
        (comp_tuple nil)
        (dbid "")
        (path "")
        (x "")
        (y ""))
    (when (not _coco_ui_part_detail_result)
      (setq _coco_component_tuple_cache (_coco_component_tuples handle))
      (setq props (_coco_component_props_for_refdes handle _coco_ui_part_detail_refdes))
      (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
      (setq comp_tuple (_coco_find_component_tuple_by_xy xy _coco_component_tuple_cache))
      (when (or props comp_tuple)
        (when comp_tuple
          (setq x (safeStr (_coco_nth 2 comp_tuple)))
          (setq y (safeStr (_coco_nth 3 comp_tuple))))
        (when (and (equal x "") (not (equal xy "")))
          (let ((parsed (_coco_ui_parse_xy xy)))
            (setq x (safeStr (_coco_nth 0 parsed)))
            (setq y (safeStr (_coco_nth 1 parsed)))))
        (setq dbid (_coco_detail_component_dbid comp_tuple props))
        (setq path (_coco_first_nonempty_prop_value props (list "PATH" "Path")))
        (setq _coco_ui_part_detail_result (list page_name dbid path x y))))))

(defun _coco_ui_find_part_detail (handle refdes)
  (setq _coco_ui_part_detail_active_dwg (_coco_active_dwg handle))
  (setq _coco_ui_part_detail_refdes (safeStr refdes))
  (setq _coco_ui_part_detail_result nil)
  (_coco_each_design_page
    handle
    _coco_ui_part_detail_active_dwg
    (_coco_resolved_active_page_name handle _coco_ui_part_detail_active_dwg)
    '_coco_ui_part_detail_page_item)
  _coco_ui_part_detail_result)

(defun _coco_ui_zoom_part (handle page_name x_str y_str)
  (let ((x (atoi x_str))
        (y (atoi y_str)))
    (_coco_ui_send_interactive handle (sprintf nil "nmapedit %s;" page_name))
    (_coco_ui_send_interactive handle
      (sprintf nil "ZOOM (%s,%s,1) (%s,%s,1)"
        (safeStr (difference x 700))
        (safeStr (plus y 700))
        (safeStr (plus x 700))
        (safeStr (difference y 700))))))

(defun _coco_op_ui_navigate_part (arg)
  (let ((handle (_coco_import_handle))
        (refdes (safeStr arg))
        (detail nil)
        (page_name "")
        (dbid "")
        (path "")
        (x_str "")
        (y_str "")
        (highlight_count 0))
    (when (equal refdes "")
      (error "INVALID_ARG" "refdes is required"))
    (setq detail (_coco_ui_find_part_detail handle refdes))
    (when detail
      (setq page_name (_coco_nth 0 detail))
      (setq dbid (_coco_nth 1 detail))
      (setq path (_coco_nth 2 detail))
      (setq x_str (_coco_nth 3 detail))
      (setq y_str (_coco_nth 4 detail)))
    (if (or (not detail) (equal page_name "") (equal x_str "") (equal y_str ""))
      (_coco_ui_json_not_found "part" refdes)
      (progn
        (_coco_ui_zoom_part handle page_name x_str y_str)
        (setq highlight_count (_coco_ui_try_part_highlight handle refdes path dbid x_str y_str))
        (_coco_ui_json_navigate
          "part"
          refdes
          (cond
            ((not (equal path "")) path)
            ((not (equal dbid "")) dbid)
            (t refdes))
          highlight_count
          "bbox")))))

(defun _coco_ui_zoom_box_result (handle kind target x1 y1 x2 y2 mode)
  (_coco_send_cmd handle (sprintf nil "ZOOM (%s,%s,1) (%s,%s,1)" x1 y1 x2 y2))
  (_coco_ui_json_result
    (list
      (_coco_json_string_field "status" "ok")
      (_coco_json_string_field "action" "navigate")
      (_coco_json_string_field "kind" kind)
      (_coco_json_string_field "target" target)
      (_coco_json_string_field "mode" mode)
      (_coco_json_field
        "bbox"
        (_coco_json_object
          (list
            (_coco_json_string_field "x1" x1)
            (_coco_json_string_field "y1" y1)
            (_coco_json_string_field "x2" x2)
            (_coco_json_string_field "y2" y2)))))))

(defun _coco_ui_wire_box_for_net (handle net_name)
  (let ((iter nil)
        (wires nil)
        (x1 0)
        (y1 0)
        (x2 0)
        (y2 0)
        (x_min nil)
        (x_max nil)
        (y_min nil)
        (y_max nil))
    (setq iter (_coco_open_iterator handle (sprintf nil "find sig_name=%s" net_name) 'cnGetWires))
    (setq wires (_coco_iterator_items iter))
    (_coco_cleanup_iterator handle iter)
    (foreach wire wires
      (setq x1 (atoi (safeStr (_coco_nth 1 wire))))
      (setq y1 (atoi (safeStr (_coco_nth 2 wire))))
      (setq x2 (atoi (safeStr (_coco_nth 3 wire))))
      (setq y2 (atoi (safeStr (_coco_nth 4 wire))))
      (when (or (not x_min) (lessp x1 x_min)) (setq x_min x1))
      (when (or (not x_min) (lessp x2 x_min)) (setq x_min x2))
      (when (or (not x_max) (greaterp x1 x_max)) (setq x_max x1))
      (when (or (not x_max) (greaterp x2 x_max)) (setq x_max x2))
      (when (or (not y_min) (lessp y1 y_min)) (setq y_min y1))
      (when (or (not y_min) (lessp y2 y_min)) (setq y_min y2))
      (when (or (not y_max) (greaterp y1 y_max)) (setq y_max y1))
      (when (or (not y_max) (greaterp y2 y_max)) (setq y_max y2)))
    (if wires
      (list
        (safeStr (difference x_min 200))
        (safeStr (plus y_max 200))
        (safeStr (plus x_max 200))
        (safeStr (difference y_min 200)))
      nil)))

(defun _coco_ui_page_has_net (handle net_name)
  (let ((values nil))
    (setq values
      (_coco_collect_values_from_find handle "find sig_name=*" (list "SIG_NAME")))
    (_coco_member_string net_name values)))

(setq _coco_ui_net_detail_active_dwg nil)
(setq _coco_ui_net_detail_name "")
(setq _coco_ui_net_detail_result nil)

(defun _coco_ui_net_detail_page_item (handle page_name)
  (let ((box nil))
    (when (not _coco_ui_net_detail_result)
      (setq box (_coco_ui_wire_box_for_net handle _coco_ui_net_detail_name))
      (when box
        (setq _coco_ui_net_detail_result (list page_name box "bbox")))
      (when (and (not _coco_ui_net_detail_result)
                 (_coco_ui_page_has_net handle _coco_ui_net_detail_name))
        (setq _coco_ui_net_detail_result (list page_name nil "page"))))))

(defun _coco_ui_find_net_detail (handle net_name)
  (setq _coco_ui_net_detail_active_dwg (_coco_active_dwg handle))
  (setq _coco_ui_net_detail_name (safeStr net_name))
  (setq _coco_ui_net_detail_result nil)
  (_coco_each_design_page
    handle
    _coco_ui_net_detail_active_dwg
    (_coco_resolved_active_page_name handle _coco_ui_net_detail_active_dwg)
    '_coco_ui_net_detail_page_item)
  _coco_ui_net_detail_result)

(defun _coco_op_ui_navigate_net (arg)
  (let ((handle (_coco_import_handle))
        (net_name (safeStr arg))
        (detail nil)
        (page_name "")
        (box nil)
        (mode ""))
    (when (equal net_name "")
      (error "INVALID_ARG" "net name is required"))
    (setq detail (_coco_ui_find_net_detail handle net_name))
    (when detail
      (setq page_name (_coco_nth 0 detail))
      (setq box (_coco_nth 1 detail))
      (setq mode (_coco_nth 2 detail)))
    (if (equal page_name "")
      (_coco_ui_json_not_found "net" net_name)
      (if (and box (equal mode "bbox"))
        (progn
          (_coco_ui_send_interactive handle (sprintf nil "nmapedit %s;" page_name))
          (_coco_ui_try_name_highlight handle net_name)
          (_coco_ui_zoom_box_result
            handle
            "net"
            net_name
            (_coco_nth 0 box)
            (_coco_nth 1 box)
            (_coco_nth 2 box)
            (_coco_nth 3 box)
            "bbox"))
        (progn
          (_coco_ui_send_interactive handle (sprintf nil "nmapedit %s;" page_name))
          (_coco_ui_try_name_highlight handle net_name)
          (_coco_ui_json_navigate "net" net_name page_name 1 "page"))))))
