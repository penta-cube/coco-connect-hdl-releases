;;; batch_edit.il - C07 write helpers for coco-connect-hdl
;;;
;;; Public operations:
;;;   batch_edit_snapshot
;;;   property_batch_edit
;;;   refdes_rename

;;; JSON helpers

(defun _coco_batch_json_number_field (name value)
  (_coco_json_field name (safeStr value)))

(defun _coco_batch_json_bool_field (name value)
  (_coco_json_field name (if value "true" "false")))

(defun _coco_batch_json_optional_string_field (name value)
  (if (equal (safeStr value) "")
    (_coco_json_field name "null")
    (_coco_json_string_field name value)))

(defun _coco_batch_audit_json (operation refdes owner_dbid property_name before_value after_value status reason)
  (_coco_json_object
    (list
      (_coco_json_string_field "operation" operation)
      (_coco_json_string_field "refdes" refdes)
      (_coco_json_string_field "owner_dbid" owner_dbid)
      (_coco_batch_json_optional_string_field "property_name" property_name)
      (_coco_batch_json_optional_string_field "before_value" before_value)
      (_coco_batch_json_optional_string_field "after_value" after_value)
      (_coco_json_string_field "status" status)
      (_coco_batch_json_optional_string_field "reason" reason))))

(defun _coco_batch_result_json (mode changes skipped conflicts errors)
  (let ((audit_log nil))
    (setq audit_log (append audit_log changes))
    (setq audit_log (append audit_log skipped))
    (setq audit_log (append audit_log conflicts))
    (setq audit_log (append audit_log errors))
    (_coco_json_object
      (list
        (_coco_json_string_field "mode" mode)
        (_coco_batch_json_bool_field "ok" (and (not conflicts) (not errors)))
        (_coco_json_field
          "summary"
          (_coco_json_object
            (list
              (_coco_batch_json_number_field "total_targets" (length audit_log))
              (_coco_batch_json_number_field "planned" 0)
              (_coco_batch_json_number_field "applied" (length changes))
              (_coco_batch_json_number_field "skipped" (length skipped))
              (_coco_batch_json_number_field "conflicts" (length conflicts))
              (_coco_batch_json_number_field "errors" (length errors)))))
        (_coco_json_field "changes" (_coco_json_array changes))
        (_coco_json_field "skipped" (_coco_json_array skipped))
        (_coco_json_field "conflicts" (_coco_json_array conflicts))
        (_coco_json_field "audit_log" (_coco_json_array audit_log))))))

;;; Payload parsing

(defun _coco_batch_record_tokens (arg)
  (if (equal (safeStr arg) "")
    nil
    (parseString arg "\t")))

(defun _coco_batch_record_fields (record)
  (parseString record "|"))

(defun _coco_batch_decode_field (value)
  (if (equal (safeStr value) "__COCO_EMPTY__")
    ""
    (safeStr value)))

(defun _coco_batch_mode_from_arg (tokens)
  (let ((mode "dry_run"))
    (foreach token tokens
      (when (and (greaterp (strlen token) 2)
                 (equal (substring token 1 2) "m:"))
        (setq mode (substring token 3 (strlen token)))))
    mode))

(defun _coco_batch_records_with_prefix (tokens prefix)
  (let ((records nil)
        (prefix_len (strlen prefix)))
    (foreach token tokens
      (when (and (greaterp (strlen token) prefix_len)
                 (equal (substring token 1 prefix_len) prefix))
        (setq records
          (append records
            (list (_coco_batch_record_fields
              (substring token (add1 prefix_len) (strlen token))))))))
    records))

(defun _coco_batch_property_records (tokens)
  (_coco_batch_records_with_prefix tokens "e:"))

(defun _coco_batch_rename_records (tokens)
  (_coco_batch_records_with_prefix tokens "r:"))

(defun _coco_batch_props_named (props prop_name)
  (let ((matches nil))
    (foreach prop props
      (when (equal (safeGetProp prop "name") prop_name)
        (setq matches (append matches (list prop)))))
    matches))

(defun _coco_batch_prop_named_with_value (props prop_name prop_value)
  (let ((found nil))
    (foreach prop props
      (when (and (not found)
                 (equal (safeGetProp prop "name") prop_name)
                 (equal (safeGetProp prop "value") prop_value))
        (setq found prop)))
    found))

(defun _coco_batch_update_prop_for_record (target_props property_name before_value)
  (let ((prop nil))
    (setq prop (_coco_batch_prop_named_with_value target_props property_name before_value))
    (when (and (not prop) (equal (length target_props) 1))
      (setq prop (car target_props)))
    prop))

;;; Shared state and cleanup

(setq _coco_batch_snapshot_current_page_json "")
(setq _coco_batch_snapshot_items_acc nil)
(setq _coco_batch_snapshot_active_dwg nil)
(setq _coco_batch_snapshot_refdes_prefix "")
(setq _coco_batch_snapshot_refdes_exact "")
(setq _coco_batch_snapshot_page_filter "")
(setq _coco_batch_original_page "")
(setq _coco_batch_find_refdes "")
(setq _coco_batch_find_result nil)
(setq _coco_batch_find_active_dwg nil)
(setq _coco_batch_last_create_variant "")

(defun _coco_batch_cleanup_state ()
  (setq _coco_batch_snapshot_current_page_json "")
  (setq _coco_batch_snapshot_items_acc nil)
  (setq _coco_batch_snapshot_active_dwg nil)
  (setq _coco_batch_snapshot_refdes_prefix "")
  (setq _coco_batch_snapshot_refdes_exact "")
  (setq _coco_batch_snapshot_page_filter "")
  (setq _coco_batch_find_refdes "")
  (setq _coco_batch_find_result nil)
  (setq _coco_batch_find_active_dwg nil)
  (setq _coco_batch_last_create_variant "")
  (_coco_reset_page_snapshot)
  t)

(defun _coco_batch_restore_original_page (handle)
  (when (not (equal _coco_batch_original_page ""))
    (errset (_coco_send_cmd handle (sprintf nil "nmapedit %s;" _coco_batch_original_page)) nil))
  t)

;;; Snapshot operation

(defun _coco_batch_snapshot_apply_hint_token (token)
  (cond
    ((and (greaterp (strlen token) 3)
          (equal (substring token 1 3) "rp:"))
      (setq _coco_batch_snapshot_refdes_prefix
        (_coco_batch_decode_field (substring token 4 (strlen token)))))
    ((and (greaterp (strlen token) 3)
          (equal (substring token 1 3) "re:"))
      (setq _coco_batch_snapshot_refdes_exact
        (_coco_batch_decode_field (substring token 4 (strlen token)))))
    ((and (greaterp (strlen token) 3)
          (equal (substring token 1 3) "pg:"))
      (setq _coco_batch_snapshot_page_filter
        (_coco_batch_decode_field (substring token 4 (strlen token)))))))

(defun _coco_batch_snapshot_apply_hints (arg)
  (_coco_batch_cleanup_state)
  (foreach token (_coco_batch_record_tokens arg)
    (_coco_batch_snapshot_apply_hint_token token))
  t)

(defun _coco_batch_snapshot_page_matches (page_name)
  (or (equal _coco_batch_snapshot_page_filter "")
      (equal _coco_batch_snapshot_page_filter page_name)
      (equal _coco_batch_snapshot_page_filter (_coco_page_number_from_name page_name))))

(defun _coco_batch_starts_with (value prefix)
  (let ((value_len (strlen value))
        (prefix_len (strlen prefix)))
    (and (leqp prefix_len value_len)
         (equal (substring value 1 prefix_len) prefix))))

(defun _coco_batch_snapshot_refdes_matches (refdes)
  (and
    (or (equal _coco_batch_snapshot_refdes_prefix "")
        (_coco_batch_starts_with refdes _coco_batch_snapshot_refdes_prefix))
    (or (equal _coco_batch_snapshot_refdes_exact "")
        (equal _coco_batch_snapshot_refdes_exact refdes))))

(defun _coco_batch_snapshot_component_json (handle refdes)
  (let ((props nil)
        (comp_tuple nil)
        (owner_dbid "")
        (part_name "")
        (device "")
        (footprint "")
        (xy "")
        (value ""))
    (setq props (_coco_snapshot_props_for_refdes handle refdes))
    (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
    (setq comp_tuple (_coco_find_component_tuple_by_xy xy _coco_page_snapshot_component_tuples))
    (when comp_tuple
      (setq owner_dbid (safeStr (_coco_nth 0 comp_tuple))))
    (foreach prop props
      (when (equal owner_dbid "")
        (setq owner_dbid (_coco_prop_owner_dbid_string prop))))
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
        (_coco_json_field "page" _coco_batch_snapshot_current_page_json)
        (_coco_json_field "properties" (_coco_props_json props))))))

(defun _coco_batch_snapshot_page_item (handle page_name)
  (when (_coco_batch_snapshot_page_matches page_name)
    (_coco_prepare_page_snapshot handle _coco_batch_snapshot_active_dwg page_name)
    (setq _coco_batch_snapshot_current_page_json _coco_page_snapshot_page_json)
    (foreach refdes _coco_page_snapshot_refdes_list
      (when (_coco_batch_snapshot_refdes_matches refdes)
        (setq _coco_batch_snapshot_items_acc
          (append _coco_batch_snapshot_items_acc
            (list (_coco_batch_snapshot_component_json handle refdes))))))))

(defun _coco_op_batch_edit_snapshot (arg)
  (let ((handle (_coco_import_handle))
        (result ""))
    (_coco_batch_snapshot_apply_hints arg)
    (setq _coco_batch_snapshot_active_dwg (_coco_active_dwg handle))
    (setq _coco_batch_original_page (_coco_resolved_active_page_name handle _coco_batch_snapshot_active_dwg))
    (_coco_each_design_page
      handle
      _coco_batch_snapshot_active_dwg
      _coco_batch_original_page
      '_coco_batch_snapshot_page_item)
    (setq result
      (_coco_json_object
        (list
          (_coco_json_field "components" (_coco_json_array _coco_batch_snapshot_items_acc)))))
    (_coco_batch_restore_original_page handle)
    (_coco_batch_cleanup_state)
    result))

;;; Property write helpers

(defun _coco_batch_delete_props (handle props)
  (let ((dbids nil)
        (group nil)
        (group_name ""))
    (foreach prop props
      (setq dbids (append dbids (list (cnGetPropDbId prop)))))
    (when dbids
      (setq group (cnGroupObjects handle dbids))
      (when group
        (setq group_name (safeStr (cnGetGroupName group)))
        (when (not (equal group_name ""))
          (_coco_send_cmd handle (sprintf nil "Delete %s \n;" group_name))
          (when (isCallable 'csFreeGroup)
            (csFreeGroup handle group))))))
  t)

(defun _coco_batch_build_update_template (prop new_name new_value)
  (if (equal (cnGetPropOwnerType prop) cnOWNER_PIN)
    (cnCreatePropTemplate
      ?name (if (equal new_name "") (cnGetPropName prop) new_name)
      ?value (if (equal new_value "") (cnGetPropValue prop) new_value)
      ?attachXY (cnGetPropAttachXY prop)
      ?XY (cnGetPropXY prop)
      ?color (cnGetPropColor prop)
      ?visibility (cnGetPropVisibility prop)
      ?justification (cnGetPropJustification prop)
      ?size (cnGetPropSize prop)
      ?angle (cnGetPropAngle prop)
      ?propDbid (cnGetPropDbId prop))
    (cnCreatePropTemplate
      ?name (if (equal new_name "") (cnGetPropName prop) new_name)
      ?value (if (equal new_value "") (cnGetPropValue prop) new_value)
      ?ownerDbId (cnGetPropOwnerDbId prop)
      ?attachXY (cnGetPropAttachXY prop)
      ?XY (cnGetPropXY prop)
      ?color (cnGetPropColor prop)
      ?visibility (cnGetPropVisibility prop)
      ?justification (cnGetPropJustification prop)
      ?size (cnGetPropSize prop)
      ?angle (cnGetPropAngle prop)
      ?propDbid (cnGetPropDbId prop))))

(defun _coco_batch_apply_update (handle prop new_name new_value)
  (let ((tpl_list nil)
        (result nil))
    (setq result
      (errset
        (progn
          (_coco_batch_delete_props handle (list prop))
          (setq tpl_list (list (_coco_batch_build_update_template prop new_name new_value)))
          (cnCreateProps handle tpl_list))
        nil))
    (if result t nil)))

(defun _coco_batch_anchor_prop (props)
  (let ((found nil)
        (name ""))
    (foreach prop props
      (setq name (upperCase (safeGetProp prop "name")))
      (when (and (not found)
                 (or (equal name "LOCATION")
                     (equal name "$LOCATION")
                     (equal name "CDS_LOCATION")))
        (setq found prop)))
    found))

(defun _coco_batch_apply_create (handle anchor_prop prop_name prop_value)
  (let ((template nil)
        (result nil))
    (when anchor_prop
      (setq template
        (ncons
          (cnCreatePropTemplate
            ?name prop_name
            ?value prop_value
            ?color "GREEN"
            ?attachXY (cnGetPropAttachXY anchor_prop)
            ?XY (cnGetPropXY anchor_prop)
            ?visibility 1
            ?justification (cnGetPropJustification anchor_prop)
            ?size (cnGetPropSize anchor_prop)
            ?angle 0)))
      (setq result (errset (cnCreateProps handle template) nil)))
    (if result t nil)))

(defun _coco_batch_has_prop_value (props prop_name prop_value)
  (let ((found nil))
    (foreach prop props
      (when (and (equal (safeGetProp prop "name") prop_name)
                 (equal (safeGetProp prop "value") prop_value))
        (setq found t)))
    found))

;;; Component lookup

(defun _coco_batch_find_refdes_page_item (handle page_name)
  (let ((props nil))
    (when (not _coco_batch_find_result)
      (setq props (_coco_component_props_for_refdes handle _coco_batch_find_refdes))
      (when props
        (setq _coco_batch_find_result (list page_name props))))))

(defun _coco_batch_find_refdes_page_and_props (handle refdes)
  (setq _coco_batch_find_refdes (safeStr refdes))
  (setq _coco_batch_find_result nil)
  (setq _coco_batch_find_active_dwg (_coco_active_dwg handle))
  (_coco_each_design_page
    handle
    _coco_batch_find_active_dwg
    (_coco_resolved_active_page_name handle _coco_batch_find_active_dwg)
    '_coco_batch_find_refdes_page_item)
  _coco_batch_find_result)

(defun _coco_batch_refdes_exists (handle refdes)
  (let ((found (_coco_batch_find_refdes_page_and_props handle refdes)))
    (if found t nil)))

;;; Property batch edit

(defun _coco_batch_apply_property_record (handle mode record)
  (let ((refdes "")
        (owner_dbid "")
        (payload_page "")
        (property_name "")
        (before_value "")
        (after_value "")
        (action "")
        (found nil)
        (page_name "")
        (props nil)
        (target_props nil)
        (target_prop nil)
        (anchor_prop nil)
        (applied nil))
    (if (not (equal (length record) 7))
      (list "error" (_coco_batch_audit_json "property_batch_edit" "" "" "" "" "" "error" "invalid_record"))
      (progn
        (setq refdes (_coco_batch_decode_field (_coco_nth 0 record)))
        (setq owner_dbid (_coco_batch_decode_field (_coco_nth 1 record)))
        (setq payload_page (_coco_batch_decode_field (_coco_nth 2 record)))
        (setq property_name (_coco_batch_decode_field (_coco_nth 3 record)))
        (setq before_value (_coco_batch_decode_field (_coco_nth 4 record)))
        (setq after_value (_coco_batch_decode_field (_coco_nth 5 record)))
        (setq action (_coco_batch_decode_field (_coco_nth 6 record)))
        (when (not (equal payload_page ""))
          (_coco_send_cmd handle (sprintf nil "nmapedit %s;" payload_page))
          (setq props (_coco_component_props_for_refdes handle refdes))
          (when props
            (setq found (list payload_page props))))
        (when (not found)
          (setq found (_coco_batch_find_refdes_page_and_props handle refdes)))
        (if (not found)
          (list "conflict" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "conflict" "source_not_found"))
          (progn
            (setq page_name (_coco_nth 0 found))
            (_coco_send_cmd handle (sprintf nil "nmapedit %s;" page_name))
            (setq props (_coco_component_props_for_refdes handle refdes))
            (setq target_props (_coco_batch_props_named props property_name))
            (setq target_prop (_coco_batch_update_prop_for_record target_props property_name before_value))
            (cond
              ((equal mode "dry_run")
                (list "change" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "planned" "")))
              ((equal action "update")
                (if (not target_prop)
                  (list "conflict" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "conflict" "property_state_mismatch"))
                  (progn
                    (setq applied (_coco_batch_apply_update handle target_prop property_name after_value))
                    (setq props (_coco_component_props_for_refdes handle refdes))
                    (if (and applied (_coco_batch_has_prop_value props property_name after_value))
                      (list "change" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "applied" ""))
                      (list "error" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "error" "update_failed"))))))
              ((equal action "create")
                (if target_props
                  (list "conflict" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "conflict" "property_already_exists"))
                  (progn
                    (setq anchor_prop (_coco_batch_anchor_prop props))
                    (if (not anchor_prop)
                      (list "conflict" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "conflict" "missing_anchor_property"))
                      (progn
                        (setq applied (_coco_batch_apply_create handle anchor_prop property_name after_value))
                        (setq props (_coco_component_props_for_refdes handle refdes))
                        (if (and applied (_coco_batch_has_prop_value props property_name after_value))
                          (list "change" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "applied" ""))
                          (list "error" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "error" "create_failed"))))))))
              (t
                (list "conflict" (_coco_batch_audit_json "property_batch_edit" refdes owner_dbid property_name before_value after_value "conflict" "property_state_mismatch"))))))))))

(defun _coco_batch_collect_record_result (record_result changes skipped conflicts errors)
  (let ((kind (_coco_nth 0 record_result))
        (entry (_coco_nth 1 record_result)))
    (cond
      ((equal kind "change") (list (append changes (list entry)) skipped conflicts errors))
      ((equal kind "skip") (list changes (append skipped (list entry)) conflicts errors))
      ((equal kind "conflict") (list changes skipped (append conflicts (list entry)) errors))
      (t (list changes skipped conflicts (append errors (list entry)))))))

(defun _coco_batch_apply_property_records (handle mode records)
  (let ((changes nil)
        (skipped nil)
        (conflicts nil)
        (errors nil)
        (record_result nil)
        (buckets nil))
    (foreach record records
      (setq record_result (_coco_batch_apply_property_record handle mode record))
      (setq buckets (_coco_batch_collect_record_result record_result changes skipped conflicts errors))
      (setq changes (_coco_nth 0 buckets))
      (setq skipped (_coco_nth 1 buckets))
      (setq conflicts (_coco_nth 2 buckets))
      (setq errors (_coco_nth 3 buckets)))
    (_coco_batch_result_json mode changes skipped conflicts errors)))

;;; RefDes rename

(defun _coco_batch_alias_names_from_field (field)
  (if (equal (safeStr field) "")
    nil
    (parseString field ",")))

(defun _coco_batch_preflight_rename_conflicts (handle records)
  (let ((conflicts nil)
        (record nil)
        (from_refdes "")
        (to_refdes "")
        (owner_dbid ""))
    (foreach record records
      (setq from_refdes (_coco_batch_decode_field (_coco_nth 0 record)))
      (setq to_refdes (_coco_batch_decode_field (_coco_nth 1 record)))
      (setq owner_dbid (_coco_batch_decode_field (_coco_nth 2 record)))
      (when (_coco_batch_refdes_exists handle to_refdes)
        (setq conflicts
          (append conflicts
            (list (_coco_batch_audit_json "refdes_rename" from_refdes owner_dbid "" from_refdes to_refdes "conflict" "target_already_exists"))))))
    conflicts))

(defun _coco_batch_apply_rename_records (handle mode records)
  (let ((changes nil)
        (skipped nil)
        (conflicts nil)
        (errors nil)
        (record nil)
        (from_refdes "")
        (to_refdes "")
        (owner_dbid "")
        (payload_page "")
        (alias_names nil)
        (found nil)
        (page_name "")
        (props nil)
        (alias_props nil)
        (alias_name "")
        (rename_failed nil))
    (setq conflicts (_coco_batch_preflight_rename_conflicts handle records))
    (when (not conflicts)
      (foreach record records
        (if (not (equal (length record) 5))
          (setq errors
            (append errors
              (list (_coco_batch_audit_json "refdes_rename" "" "" "" "" "" "error" "invalid_record"))))
          (progn
            (setq from_refdes (_coco_batch_decode_field (_coco_nth 0 record)))
            (setq to_refdes (_coco_batch_decode_field (_coco_nth 1 record)))
            (setq owner_dbid (_coco_batch_decode_field (_coco_nth 2 record)))
            (setq payload_page (_coco_batch_decode_field (_coco_nth 3 record)))
            (setq alias_names (_coco_batch_alias_names_from_field (_coco_batch_decode_field (_coco_nth 4 record))))
            (setq found nil)
            (setq props nil)
            (setq alias_props nil)
            (when (not (equal payload_page ""))
              (_coco_send_cmd handle (sprintf nil "nmapedit %s;" payload_page))
              (setq props (_coco_component_props_for_refdes handle from_refdes))
              (when props
                (setq found (list payload_page props))))
            (when (not found)
              (setq found (_coco_batch_find_refdes_page_and_props handle from_refdes)))
            (if (not found)
              (setq conflicts
                (append conflicts
                  (list (_coco_batch_audit_json "refdes_rename" from_refdes owner_dbid "" from_refdes to_refdes "conflict" "source_not_found"))))
              (progn
                (setq page_name (_coco_nth 0 found))
                (_coco_send_cmd handle (sprintf nil "nmapedit %s;" page_name))
                (setq props (_coco_component_props_for_refdes handle from_refdes))
                (setq alias_props nil)
                (foreach alias_name alias_names
                  (setq alias_props (append alias_props (_coco_batch_props_named props alias_name))))
                (cond
                  ((not alias_props)
                    (setq conflicts
                      (append conflicts
                        (list (_coco_batch_audit_json "refdes_rename" from_refdes owner_dbid "" from_refdes to_refdes "conflict" "missing_alias_property")))))
                  ((equal mode "dry_run")
                    (setq changes
                      (append changes
                        (list (_coco_batch_audit_json "refdes_rename" from_refdes owner_dbid "" from_refdes to_refdes "planned" "")))))
                  (t
                    (setq rename_failed nil)
                    (foreach alias_name alias_names
                      (foreach prop (_coco_batch_props_named props alias_name)
                        (when (not (_coco_batch_apply_update handle prop alias_name to_refdes))
                          (setq rename_failed t))))
                    (_coco_send_cmd handle (sprintf nil "nmapedit %s;" page_name))
                    (if (and (not rename_failed) (_coco_component_props_for_refdes handle to_refdes))
                      (setq changes
                        (append changes
                          (list (_coco_batch_audit_json "refdes_rename" from_refdes owner_dbid "" from_refdes to_refdes "applied" ""))))
                      (setq errors
                        (append errors
                          (list (_coco_batch_audit_json "refdes_rename" from_refdes owner_dbid "" from_refdes to_refdes "error" "rename_verify_failed")))))))))))))
    (_coco_batch_result_json mode changes skipped conflicts errors)))

;;; Public bridge operations

(defun _coco_batch_non_json_result (mode operation)
  (_coco_batch_result_json
    mode
    nil
    nil
    nil
    (list (_coco_batch_audit_json operation "" "" "" "" "" "error" "non_json_result"))))

(defun _coco_op_property_batch_edit (arg)
  (let ((handle (_coco_import_handle))
        (tokens (_coco_batch_record_tokens arg))
        (mode "dry_run")
        (active_dwg nil)
        (result ""))
    (setq mode (_coco_batch_mode_from_arg tokens))
    (setq active_dwg (_coco_active_dwg handle))
    (setq _coco_batch_original_page (_coco_resolved_active_page_name handle active_dwg))
    (setq result (_coco_batch_apply_property_records handle mode (_coco_batch_property_records tokens)))
    (when (not (stringp result))
      (setq result (_coco_batch_non_json_result mode "property_batch_edit")))
    (_coco_batch_restore_original_page handle)
    (_coco_batch_cleanup_state)
    result))

(defun _coco_op_refdes_rename (arg)
  (let ((handle (_coco_import_handle))
        (tokens (_coco_batch_record_tokens arg))
        (mode "dry_run")
        (active_dwg nil)
        (result ""))
    (setq mode (_coco_batch_mode_from_arg tokens))
    (setq active_dwg (_coco_active_dwg handle))
    (setq _coco_batch_original_page (_coco_resolved_active_page_name handle active_dwg))
    (setq result (_coco_batch_apply_rename_records handle mode (_coco_batch_rename_records tokens)))
    (when (not (stringp result))
      (setq result (_coco_batch_non_json_result mode "refdes_rename")))
    (_coco_batch_restore_original_page handle)
    (_coco_batch_cleanup_state)
    result))
