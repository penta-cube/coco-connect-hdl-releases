;;; Launcher used by the packaged HDL SKILL bridge.
(unless (boundp '_coco_bridge_dir)
  (setq _coco_bridge_dir "."))
(setq _coco_module_suffix ".ile")
(let ((payload (strcat _coco_bridge_dir "\\" "bridge.ile")))
  (unless (and (isFile payload) (load payload))
    (error "Cannot load HDL bridge: %s" payload)))
