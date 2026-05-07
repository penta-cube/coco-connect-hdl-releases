;;; highlight.il - highlight operations for coco-connect-hdl bridge

(setq _coco_highlight_groups nil)

(defun _coco_highlight_json (status kind target selected)
  (strcat
    "{"
    "\"status\":" (_coco_json_string status)
    ",\"kind\":" (_coco_json_string kind)
    ",\"target\":" (_coco_json_string target)
    ",\"selected\":" (sprintf nil "%d" selected)
    "}"))

(defun _coco_clear_json ()
  "{\"status\":\"cleared\"}")

(defun _coco_direct_highlight (handle kind target)
  (errset (_coco_send_cmd handle (sprintf nil "HIGHLIGHT %s %s" kind target)) t))

(defun _coco_direct_unhighlight (handle kind target)
  (errset (_coco_send_cmd handle (sprintf nil "UNHIGHLIGHT %s %s" kind target)) t))

(defun _coco_prepare_body_group (handle group)
  (when (not (equal group ""))
    (errset (_coco_send_cmd handle (sprintf nil "INCLUDE %s BODIES" group)) t)
    (errset (_coco_send_cmd handle (sprintf nil "EXCLUDE %s PROPERTIES" group)) t)))

(defun _coco_clear_highlight_groups (handle)
  (foreach group _coco_highlight_groups
    (when (not (equal group ""))
      (_coco_cleanup_iterator handle (_coco_make_iterator group nil))))
  (setq _coco_highlight_groups nil))

(defun _coco_op_highlight_net (net_name)
  (let ((handle (_coco_import_handle))
        (target (_coco_to_string net_name))
        (found nil)
        (group nil)
        (selected 0))
    (if (equal target "")
      (_coco_highlight_json "invalid_arg" "net" target 0)
      (progn
        (_coco_clear_highlight_groups handle)
        (setq found (_coco_find_group handle (sprintf nil "find sig_name=%s" target) (list "SIG_NAME")))
        (setq group (car found))
        (setq selected (cadr found))
        (if group
          (progn
            (_coco_prepare_body_group handle group)
            (setq _coco_highlight_groups (list group))
            (_coco_direct_highlight handle "NET" target)
            (_coco_highlight_json "highlighted" "net" target selected))
          (_coco_highlight_json "no_match" "net" target 0))))))

(defun _coco_op_highlight_part (refdes)
  (let ((handle (_coco_import_handle))
        (target (_coco_to_string refdes))
        (found nil)
        (group nil)
        (selected 0))
    (if (equal target "")
      (_coco_highlight_json "invalid_arg" "part" target 0)
      (progn
        (_coco_clear_highlight_groups handle)
        (setq found (_coco_find_group handle (sprintf nil "find cds_location=%s" target) (list "CDS_LOCATION")))
        (setq group (car found))
        (setq selected (cadr found))
        (unless group
          (setq found (_coco_find_group handle (sprintf nil "find $location=%s" target) (list "$LOCATION")))
          (setq group (car found))
          (setq selected (cadr found)))
        (if group
          (progn
            (_coco_prepare_body_group handle group)
            (setq _coco_highlight_groups (list group))
            (_coco_direct_highlight handle "PART" target)
            (_coco_highlight_json "highlighted" "part" target selected))
          (_coco_highlight_json "no_match" "part" target 0))))))

(defun _coco_op_clear_highlight ()
  (let ((handle (_coco_import_handle)))
    (_coco_clear_highlight_groups handle)
    (_coco_clear_json)))
