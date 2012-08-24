
; test for guile-gnome-2 working properly
(use-modules (gnome-2))
(use-modules
  (gnome glib)
  (gnome gtk)
  (gnome gtk gdk-event))
(if (module-bound? (current-module) 'g-idle-add)
    (exit 0)
    (exit 1))
