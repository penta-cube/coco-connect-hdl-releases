;;; property_crud.il - direct component property access and RefDes rename
;;;
;;; Public operations:
;;;   property_get
;;;   property_set
;;;   rename

(setq _coco_property_find_refdes "")
(setq _coco_property_find_result nil)
(setq _coco_property_original_page "")

(defun _coco_property_decode_field (value)
  (if (equal (safeStr value) "__COCO_EMPTY__") "" (safeStr value)))

(defun _coco_property_request_fields (arg)
  (let ((items nil))
    (foreach field (parseString (safeStr arg) "|")
      (setq items (append items (list (_coco_property_decode_field field)))))
    items))

(defun _coco_property_props_named (props prop_name)
  (let ((items nil))
    (foreach prop props
      (when (equal (safeGetProp prop "name") prop_name)
        (setq items (append items (list prop)))))
    items))

(defun _coco_property_component_props (handle refdes)
  (let ((props nil)
        (extra nil))
    ; CRUD must operate on properties included from the component body.
    ; Direct find results may be searchable display/inherited objects whose
    ; property DB IDs cannot be mutated reliably.
    (setq props
      (_coco_props_from_component_find
        handle
        (sprintf nil "find cds_location=%s" refdes)))
    (setq extra
      (_coco_props_from_component_find
        handle
        (sprintf nil "find $location=%s" refdes)))
    (setq props (_coco_merge_unique_props props extra))
    (if props props (_coco_component_props_for_refdes handle refdes))))

(defun _coco_property_resolve_page_hint (handle dwg page_hint)
  (let ((found nil)
        (page_num ""))
    (foreach page_name (_coco_design_page_names handle dwg)
      (setq page_num (_coco_page_number_from_name page_name))
      (when (and
              (not found)
              (or
                (equal page_hint page_name)
                (equal page_hint page_num)
                (equal (lowerCase page_hint) (strcat "page" page_num))))
        (setq found page_name)))
    found))

(defun _coco_property_candidate_pages (handle dwg active_page page_hint)
  (let ((pages (_coco_design_page_names handle dwg))
        (target nil)
        (ordered nil))
    (if (not (equal page_hint ""))
      (progn
        (setq target (_coco_property_resolve_page_hint handle dwg page_hint))
        (when (not target)
          (error "PAGE_NOT_FOUND" (strcat "Page not found: " page_hint)))
        (list target))
      (progn
        (when (_coco_member_string active_page pages)
          (setq ordered (list active_page)))
        (foreach page_name pages
          (when (not (equal page_name active_page))
            (setq ordered (append ordered (list page_name)))))
        ordered))))

(defun _coco_property_find_page_item (handle page_name)
  (let ((props nil))
    (when (not _coco_property_find_result)
      (setq props (_coco_property_component_props handle _coco_property_find_refdes))
      (when props
        (setq _coco_property_find_result (list page_name props))))))

(defun _coco_property_find_component (handle refdes page_hint)
  (let ((dwg (_coco_active_dwg handle))
        (current_page "")
        (pages nil)
        (page_name ""))
    (setq _coco_property_find_refdes (safeStr refdes))
    (setq _coco_property_find_result nil)
    (setq _coco_component_find_diagnostics nil)
    (setq _coco_property_original_page (_coco_resolved_active_page_name handle dwg))
    (setq current_page _coco_property_original_page)
    (setq pages
      (_coco_property_candidate_pages
        handle dwg _coco_property_original_page page_hint))
    (while (and pages (not _coco_property_find_result))
      (setq page_name (car pages))
      (setq pages (cdr pages))
      (when (not (equal page_name current_page))
        (_coco_send_cmd handle (sprintf nil "nmapedit %s;" page_name))
        (setq current_page page_name))
      (_coco_property_find_page_item handle page_name))
    _coco_property_find_result))

(defun _coco_property_restore_page (handle)
  (when (not (equal _coco_property_original_page ""))
    (errset
      (_coco_send_cmd handle (sprintf nil "nmapedit %s;" _coco_property_original_page))
      nil)))

(defun _coco_property_status_json (status refdes page prop_name value count)
  (_coco_json_object
    (list
      (_coco_json_string_field "status" status)
      (_coco_json_string_field "refdes" refdes)
      (_coco_json_string_field "page" page)
      (_coco_json_string_field "property_name" prop_name)
      (_coco_json_string_field "value" value)
      (_coco_json_number_field "count" count))))

(defun _coco_property_delete_props (handle props)
  (let ((dbids nil)
        (group nil)
        (group_name "")
        (result nil))
    (foreach prop props
      (let ((dbid (_coco_safe_apply 'cnGetPropDbId (list prop))))
        (when dbid
          (setq dbids (append dbids (list dbid))))))
    (when dbids
      (setq group (cnGroupObjects handle dbids))
      (when group
        (setq group_name (safeStr (cnGetGroupName group)))
        (when (not (equal group_name ""))
          (setq result
            (errset (_coco_send_cmd handle (sprintf nil "Delete %s \n;" group_name)) nil))
          (_coco_cleanup_iterator handle (_coco_make_iterator group_name nil)))))
    (if result t nil)))

(defun _coco_property_update_template (prop prop_name prop_value)
  (if (equal (cnGetPropOwnerType prop) cnOWNER_PIN)
    (cnCreatePropTemplate
      ?name prop_name
      ?value prop_value
      ?attachXY (cnGetPropAttachXY prop)
      ?XY (cnGetPropXY prop)
      ?color (cnGetPropColor prop)
      ?visibility (cnGetPropVisibility prop)
      ?justification (cnGetPropJustification prop)
      ?size (cnGetPropSize prop)
      ?angle (cnGetPropAngle prop)
      ?propDbid (cnGetPropDbId prop))
    (cnCreatePropTemplate
      ?name prop_name
      ?value prop_value
      ?ownerDbId (cnGetPropOwnerDbId prop)
      ?attachXY (cnGetPropAttachXY prop)
      ?XY (cnGetPropXY prop)
      ?color (cnGetPropColor prop)
      ?visibility (cnGetPropVisibility prop)
      ?justification (cnGetPropJustification prop)
      ?size (cnGetPropSize prop)
      ?angle (cnGetPropAngle prop)
      ?propDbid (cnGetPropDbId prop))))

(defun _coco_property_apply_updates (handle props prop_name prop_value)
  (let ((templates nil)
        (result nil))
    ; Build templates while the source properties are still valid.
    (foreach prop props
      (setq templates
        (append templates
          (list (_coco_property_update_template prop prop_name prop_value)))))
    (when (and templates (_coco_property_delete_props handle props))
      (setq result (errset (cnCreateProps handle templates) nil)))
    (if result t nil)))

(defun _coco_property_apply_values (handle props prop_value)
  (let ((templates nil)
        (result nil))
    (foreach prop props
      (setq templates
        (append templates
          (list
            (_coco_property_update_template
              prop
              (safeGetProp prop "name")
              prop_value)))))
    (when (and templates (_coco_property_delete_props handle props))
      (setq result (errset (cnCreateProps handle templates) nil)))
    (if result t nil)))

(defun _coco_property_anchor_prop (props)
  (let ((found nil)
        (name ""))
    (foreach prop props
      (setq name (upperCase (safeGetProp prop "name")))
      (when (and
              (not found)
              (_coco_member_string name (list "CDS_LOCATION" "$LOCATION" "LOCATION")))
        (setq found prop)))
    found))

(defun _coco_property_apply_create (handle anchor prop_name prop_value)
  (let ((template nil)
        (result nil))
    (when anchor
      (setq template
        (cnCreatePropTemplate
          ?name prop_name
          ?value prop_value
          ?ownerDbId (cnGetPropOwnerDbId anchor)
          ?attachXY (cnGetPropAttachXY anchor)
          ?XY (cnGetPropXY anchor)
          ?color "GREEN"
          ?visibility 1
          ?justification (cnGetPropJustification anchor)
          ?size (cnGetPropSize anchor)
          ?angle 0))
      (setq result (errset (cnCreateProps handle (list template)) nil)))
    (if result t nil)))

(defun _coco_op_property_get (arg)
  (let ((fields (_coco_property_request_fields arg))
        (handle (_coco_import_handle))
        (refdes "")
        (prop_name "")
        (page_hint "")
        (found nil)
        (page "")
        (props nil)
        (result ""))
    (setq refdes (safeStr (_coco_nth 0 fields)))
    (setq prop_name (safeStr (_coco_nth 1 fields)))
    (setq page_hint (safeStr (_coco_nth 2 fields)))
    (when (equal refdes "")
      (error "INVALID_ARG" "property_get requires a refdes"))
    (setq found (_coco_property_find_component handle refdes page_hint))
    (if found
      (progn
        (setq page (_coco_nth 0 found))
        (setq props (_coco_nth 1 found))
        (when (not (equal prop_name ""))
          (setq props (_coco_property_props_named props prop_name)))
        (setq result
          (_coco_json_object
            (list
              (_coco_json_string_field "status" "ok")
              (_coco_json_string_field "refdes" refdes)
              (_coco_json_string_field "page" page)
              (_coco_json_string_field "property_name" prop_name)
              (_coco_json_field "properties" (_coco_props_json props))))))
      (setq result (_coco_property_status_json "not_found" refdes "" prop_name "" 0)))
    (_coco_property_restore_page handle)
    result))

(defun _coco_op_property_set (arg)
  (let ((fields (_coco_property_request_fields arg))
        (handle (_coco_import_handle))
        (refdes "")
        (prop_name "")
        (prop_value "")
        (page_hint "")
        (found nil)
        (page "")
        (props nil)
        (targets nil)
        (anchor nil)
        (ok nil)
        (status "error")
        (count 0)
        (result ""))
    (setq refdes (safeStr (_coco_nth 0 fields)))
    (setq prop_name (safeStr (_coco_nth 1 fields)))
    (setq prop_value (safeStr (_coco_nth 2 fields)))
    (setq page_hint (safeStr (_coco_nth 3 fields)))
    (when (or (equal refdes "") (equal prop_name ""))
      (error "INVALID_ARG" "property_set requires refdes and property_name"))
    (setq found (_coco_property_find_component handle refdes page_hint))
    (if found
      (progn
        (setq page (_coco_nth 0 found))
        (setq props (_coco_nth 1 found))
        (setq targets (_coco_property_props_named props prop_name))
        (if targets
          (progn
            (setq count (length targets))
            (setq ok (_coco_property_apply_updates handle targets prop_name prop_value))
            (setq status (if ok "updated" "error")))
          (progn
            (setq anchor (_coco_property_anchor_prop props))
            (setq ok (_coco_property_apply_create handle anchor prop_name prop_value))
            (setq count (if ok 1 0))
            (setq status (if ok "created" "error"))))
        (setq result
          (_coco_property_status_json status refdes page prop_name prop_value count)))
      (setq result (_coco_property_status_json "not_found" refdes "" prop_name prop_value 0)))
    (_coco_property_restore_page handle)
    result))

(defun _coco_property_refdes_aliases (props)
  (let ((items nil)
        (name ""))
    (foreach prop props
      (setq name (upperCase (safeGetProp prop "name")))
      (when (_coco_member_string name (list "CDS_LOCATION" "$LOCATION"))
        (setq items (append items (list prop)))))
    items))

(defun _coco_op_rename (arg)
  (let ((fields (_coco_property_request_fields arg))
        (handle (_coco_import_handle))
        (from_refdes "")
        (to_refdes "")
        (page_hint "")
        (found nil)
        (page "")
        (aliases nil)
        (ok nil)
        (status "not_found")
        (count 0)
        (result ""))
    (setq from_refdes (safeStr (_coco_nth 0 fields)))
    (setq to_refdes (safeStr (_coco_nth 1 fields)))
    (setq page_hint (safeStr (_coco_nth 2 fields)))
    (when (or (equal from_refdes "") (equal to_refdes ""))
      (error "INVALID_ARG" "rename requires from_refdes and to_refdes"))
    (setq found (_coco_property_find_component handle from_refdes page_hint))
    (when found
      (setq page (_coco_nth 0 found))
      (setq aliases (_coco_property_refdes_aliases (_coco_nth 1 found)))
      (when aliases
        (setq count (length aliases))
        (setq ok (_coco_property_apply_values handle aliases to_refdes))
        (setq status (if ok "renamed" "error"))))
    (setq result
      (_coco_json_object
        (list
          (_coco_json_string_field "status" status)
          (_coco_json_string_field "from_refdes" from_refdes)
          (_coco_json_string_field "to_refdes" to_refdes)
          (_coco_json_string_field "page" page)
          (_coco_json_number_field "count" count))))
    (_coco_property_restore_page handle)
    result))
