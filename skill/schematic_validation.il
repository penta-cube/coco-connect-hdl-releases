;;; schematic_validation.il - read-only schematic validation for coco-connect-hdl
;;;
;;; Depends on util.il, hdl.il, and read_model.il. Keep validation rules here so
;;; read_model.il remains a reusable component/page/property read layer.

(defun _coco_validation_placeholder_values ()
  (list "" "?" "??" "TBD" "TODO" "UNKNOWN" "UNASSIGNED" "N/A" "NA"))

(defun _coco_validation_placeholder_refdes (refdes)
  (let ((value (safeStr refdes)))
    (or (_coco_member_string value (_coco_validation_placeholder_values))
        (equal value "U?")
        (equal value "R?")
        (equal value "C?")
        (equal value "L?")
        (equal value "D?")
        (equal value "Q?")
        (equal value "J?")
        (equal value "P?")
        (equal value "TP?"))))

(defun _coco_validation_placeholder_value (value)
  (_coco_member_string (safeStr value) (_coco_validation_placeholder_values)))

(defun _coco_validation_prop_values (props names)
  (let ((values nil)
        (name "")
        (value ""))
    (foreach prop props
      (setq name (safeGetProp prop "name"))
      (when (_coco_member_string name names)
        (setq value (safeGetProp prop "value"))
        (when (not (_coco_member_string value values))
          (setq values (append values (list value))))))
    values))

(defun _coco_validation_nonempty_values (values)
  (let ((result nil))
    (foreach value values
      (when (not (equal value ""))
        (setq result (append result (list value)))))
    result))

(defun _coco_validation_issue (severity rule_id comp_dbid refdes page prop_name current_value message)
  (list severity rule_id comp_dbid refdes page prop_name current_value message))

(defun _coco_validation_issue_json (severity rule_id comp_dbid refdes page prop_name current_value message)
  (_coco_json_object
    (list
      (_coco_json_string_field "severity" severity)
      (_coco_json_string_field "rule_id" rule_id)
      (_coco_json_string_field "object_dbid" comp_dbid)
      (_coco_json_string_field "refdes" refdes)
      (_coco_json_string_field "page" page)
      (_coco_json_string_field "property_name" prop_name)
      (_coco_json_string_field "current_value" current_value)
      (_coco_json_string_field "message" message))))

(defun _coco_validation_count_severity (issues severity)
  (let ((count 0))
    (foreach issue issues
      (when (equal (_coco_nth 0 issue) severity)
        (setq count (add1 count))))
    count))

(defun _coco_validation_issue_to_json (issue)
  (_coco_validation_issue_json
    (_coco_nth 0 issue)
    (_coco_nth 1 issue)
    (_coco_nth 2 issue)
    (_coco_nth 3 issue)
    (_coco_nth 4 issue)
    (_coco_nth 5 issue)
    (_coco_nth 6 issue)
    (_coco_nth 7 issue)))

(defun _coco_validation_issues_json (issues)
  (let ((items nil))
    (foreach issue issues
      (setq items (append items (list (_coco_validation_issue_to_json issue)))))
    (_coco_json_array items)))

(defun _coco_validation_refdes_count (refdes counts)
  (let ((count 0))
    (foreach entry counts
      (when (equal (car entry) refdes)
        (setq count (_coco_nth 1 entry))))
    count))

(defun _coco_validation_refdes_counts_add (refdes counts)
  (let ((result nil)
        (matched nil))
    (foreach entry counts
      (if (equal (car entry) refdes)
        (progn
          (setq matched t)
          (setq result (append result (list (list refdes (add1 (_coco_nth 1 entry)))))))
        (setq result (append result (list entry)))))
    (when (not matched)
      (setq result (append result (list (list refdes 1)))))
    result))

(defun _coco_validation_component_record (handle refdes page)
  (let ((props nil)
        (comp_tuple nil)
        (owner_dbid "")
        (xy "")
        (footprint "")
        (value "")
        (device ""))
    (setq props (_coco_snapshot_props_for_refdes handle refdes))
    (setq xy (_coco_first_nonempty_prop_value props (list "XY")))
    (setq comp_tuple (_coco_find_component_tuple_by_xy xy _coco_page_snapshot_component_tuples))
    (foreach prop props
      (when (equal owner_dbid "")
        (setq owner_dbid (_coco_prop_owner_dbid_string prop))))
    (when (and (equal owner_dbid "") comp_tuple)
      (setq owner_dbid (safeStr (_coco_nth 0 comp_tuple))))
    (setq footprint (_coco_first_nonempty_prop_value props (list "PCB Footprint" "PCB_FOOTPRINT" "JEDEC_TYPE" "PACK_TYPE")))
    (setq value (_coco_first_nonempty_prop_value props (list "VALUE" "Value")))
    (setq device (_coco_first_nonempty_prop_value props (list "DEVICE" "Device")))
    (list owner_dbid refdes page footprint value device)))

(defun _coco_validation_component_issues_from_record (record refdes_counts)
  (let ((owner_dbid (_coco_nth 0 record))
        (refdes (_coco_nth 1 record))
        (page (_coco_nth 2 record))
        (footprint (_coco_nth 3 record))
        (value (_coco_nth 4 record))
        (device (_coco_nth 5 record))
        (issues nil))
    (cond
      ((equal refdes "")
        (setq issues
          (append issues
            (list (_coco_validation_issue "error" "missing_refdes" owner_dbid refdes page "RefDes" refdes "Component RefDes is empty")))))
      ((_coco_validation_placeholder_refdes refdes)
        (setq issues
          (append issues
            (list (_coco_validation_issue "error" "placeholder_refdes" owner_dbid refdes page "RefDes" refdes "Component RefDes is still a placeholder"))))))
    (when (greaterp (_coco_validation_refdes_count refdes refdes_counts) 1)
      (setq issues
        (append issues
          (list (_coco_validation_issue "error" "duplicate_refdes" owner_dbid refdes page "RefDes" refdes "More than one component appears to use this RefDes")))))
    (when (equal footprint "")
      (setq issues
        (append issues
          (list (_coco_validation_issue "error" "missing_footprint" owner_dbid refdes page "footprint" footprint "No footprint alias value was found")))))
    (when (and (not (equal value "")) (_coco_validation_placeholder_value value))
      (setq issues
        (append issues
          (list (_coco_validation_issue "warning" "placeholder_value" owner_dbid refdes page "value" value "Component value looks unresolved")))))
    (when (and (not (equal device "")) (_coco_validation_placeholder_value device))
      (setq issues
        (append issues
          (list (_coco_validation_issue "warning" "placeholder_device" owner_dbid refdes page "device" device "DEVICE value looks unresolved")))))
    issues))

(defun _coco_validation_summary_json (component_count issues)
  (_coco_json_object
    (list
      (_coco_json_field "component_count" (safeStr component_count))
      (_coco_json_field "issue_count" (safeStr (length issues)))
      (_coco_json_field "error_count" (safeStr (_coco_validation_count_severity issues "error")))
      (_coco_json_field "warning_count" (safeStr (_coco_validation_count_severity issues "warning")))
      (_coco_json_field "info_count" (safeStr (_coco_validation_count_severity issues "info"))))))

(setq _coco_validation_component_count 0)
(setq _coco_validation_issues_acc nil)
(setq _coco_validation_records_acc nil)
(setq _coco_validation_refdes_counts nil)
(setq _coco_validation_active_dwg nil)

(defun _coco_validation_page_item (handle page_name)
  (let ((refdes_list nil)
        (record nil))
    (_coco_prepare_page_snapshot handle _coco_validation_active_dwg page_name)
    (setq refdes_list _coco_page_snapshot_refdes_list)
    (setq _coco_validation_component_count
      (plus _coco_validation_component_count (length _coco_page_snapshot_component_tuples)))
    (foreach refdes refdes_list
      (setq _coco_validation_refdes_counts (_coco_validation_refdes_counts_add refdes _coco_validation_refdes_counts))
      (setq record (_coco_validation_component_record handle refdes page_name))
      (setq _coco_validation_records_acc (append _coco_validation_records_acc (list record))))))

(defun _coco_schematic_validation_json (handle)
  (let ((active_dwg nil)
        (page_json "")
        (rules_json ""))
    (setq active_dwg (_coco_active_dwg handle))
    (setq _coco_validation_active_dwg active_dwg)
    (setq page_json (_coco_active_page_json handle))
    (setq _coco_validation_component_count 0)
    (setq _coco_validation_issues_acc nil)
    (setq _coco_validation_records_acc nil)
    (setq _coco_validation_refdes_counts nil)
    (_coco_each_design_page
      handle
      active_dwg
      (_coco_resolved_active_page_name handle active_dwg)
      '_coco_validation_page_item)
    (foreach record _coco_validation_records_acc
      (setq _coco_validation_issues_acc
        (append _coco_validation_issues_acc
          (_coco_validation_component_issues_from_record record _coco_validation_refdes_counts))))
    (setq rules_json
      (_coco_json_array
        (list
          (_coco_json_string "missing_refdes")
          (_coco_json_string "placeholder_refdes")
          (_coco_json_string "duplicate_refdes")
          (_coco_json_string "missing_footprint")
          (_coco_json_string "placeholder_value")
          (_coco_json_string "placeholder_device"))))
    (_coco_json_object
      (list
        (_coco_json_field "import_context_page" page_json)
        (_coco_json_field "summary" (_coco_validation_summary_json _coco_validation_component_count _coco_validation_issues_acc))
        (_coco_json_field "rules" rules_json)
        (_coco_json_field "issues" (_coco_validation_issues_json _coco_validation_issues_acc))))))

(defun _coco_op_schematic_validation ()
  (let ((handle (_coco_import_handle)))
    (_coco_schematic_validation_json handle)))
