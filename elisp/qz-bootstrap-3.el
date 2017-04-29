;; Copyright (C) 2017  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Version: 0.0.3
;; Created: 07 April 2017
;; Keywords: bootstrap3 qzuma

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; glyphicons

(defvar qz-twbs-glyphicons
  '(("menu-up" . "glyphicon glyphicon-menu-up")
    ("asterisk" . "glyphicon glyphicon-asterisk")
    ("plus" . "glyphicon glyphicon-plus")
    ("euro" . "glyphicon glyphicon-euro")
    ("eur" . "glyphicon glyphicon-eur")
    ("minus" . "glyphicon glyphicon-minus")
    ("cloud" . "glyphicon glyphicon-cloud")
    ("envelope" . "glyphicon glyphicon-envelope")
    ("pencil" . "glyphicon glyphicon-pencil")
    ("glass" . "glyphicon glyphicon-glass")
    ("music" . "glyphicon glyphicon-music")
    ("search" . "glyphicon glyphicon-search")
    ("heart" . "glyphicon glyphicon-heart")
    ("star" . "glyphicon glyphicon-star")
    ("star-empty" . "glyphicon glyphicon-star-empty")
    ("user" . "glyphicon glyphicon-user")
    ("film" . "glyphicon glyphicon-film")
    ("th-large" . "glyphicon glyphicon-th-large")
    ("th" . "glyphicon glyphicon-th")
    ("th-list" . "glyphicon glyphicon-th-list")
    ("ok" . "glyphicon glyphicon-ok")
    ("remove" . "glyphicon glyphicon-remove")
    ("zoom-in" . "glyphicon glyphicon-zoom-in")
    ("zoom-out" . "glyphicon glyphicon-zoom-out")
    ("off" . "glyphicon glyphicon-off")
    ("signal" . "glyphicon glyphicon-signal")
    ("cog" . "glyphicon glyphicon-cog")
    ("trash" . "glyphicon glyphicon-trash")
    ("home" . "glyphicon glyphicon-home")
    ("file" . "glyphicon glyphicon-file")
    ("time" . "glyphicon glyphicon-time")
    ("road" . "glyphicon glyphicon-road")
    ("download-alt" . "glyphicon glyphicon-download-alt")
    ("download" . "glyphicon glyphicon-download")
    ("upload" . "glyphicon glyphicon-upload")
    ("inbox" . "glyphicon glyphicon-inbox")
    ("play-circle" . "glyphicon glyphicon-play-circle")
    ("repeat" . "glyphicon glyphicon-repeat")
    ("refresh" . "glyphicon glyphicon-refresh")
    ("list-alt" . "glyphicon glyphicon-list-alt")
    ("lock" . "glyphicon glyphicon-lock")
    ("flag" . "glyphicon glyphicon-flag")
    ("headphones" . "glyphicon glyphicon-headphones")
    ("volume-off" . "glyphicon glyphicon-volume-off")
    ("volume-down" . "glyphicon glyphicon-volume-down")
    ("volume-up" . "glyphicon glyphicon-volume-up")
    ("qrcode" . "glyphicon glyphicon-qrcode")
    ("barcode" . "glyphicon glyphicon-barcode")
    ("tag" . "glyphicon glyphicon-tag")
    ("tags" . "glyphicon glyphicon-tags")
    ("book" . "glyphicon glyphicon-book")
    ("bookmark" . "glyphicon glyphicon-bookmark")
    ("print" . "glyphicon glyphicon-print")
    ("camera" . "glyphicon glyphicon-camera")
    ("font" . "glyphicon glyphicon-font")
    ("bold" . "glyphicon glyphicon-bold")
    ("italic" . "glyphicon glyphicon-italic")
    ("text-height" . "glyphicon glyphicon-text-height")
    ("text-width" . "glyphicon glyphicon-text-width")
    ("align-left" . "glyphicon glyphicon-align-left")
    ("align-center" . "glyphicon glyphicon-align-center")
    ("align-right" . "glyphicon glyphicon-align-right")
    ("align-justify" . "glyphicon glyphicon-align-justify")
    ("list" . "glyphicon glyphicon-list")
    ("indent-left" . "glyphicon glyphicon-indent-left")
    ("indent-right" . "glyphicon glyphicon-indent-right")
    ("facetime-video" . "glyphicon glyphicon-facetime-video")
    ("picture" . "glyphicon glyphicon-picture")
    ("map-marker" . "glyphicon glyphicon-map-marker")
    ("adjust" . "glyphicon glyphicon-adjust")
    ("tint" . "glyphicon glyphicon-tint")
    ("edit" . "glyphicon glyphicon-edit")
    ("share" . "glyphicon glyphicon-share")
    ("check" . "glyphicon glyphicon-check")
    ("move" . "glyphicon glyphicon-move")
    ("step-backward" . "glyphicon glyphicon-step-backward")
    ("fast-backward" . "glyphicon glyphicon-fast-backward")
    ("backward" . "glyphicon glyphicon-backward")
    ("play" . "glyphicon glyphicon-play")
    ("pause" . "glyphicon glyphicon-pause")
    ("stop" . "glyphicon glyphicon-stop")
    ("forward" . "glyphicon glyphicon-forward")
    ("fast-forward" . "glyphicon glyphicon-fast-forward")
    ("step-forward" . "glyphicon glyphicon-step-forward")
    ("eject" . "glyphicon glyphicon-eject")
    ("chevron-left" . "glyphicon glyphicon-chevron-left")
    ("chevron-right" . "glyphicon glyphicon-chevron-right")
    ("plus-sign" . "glyphicon glyphicon-plus-sign")
    ("minus-sign" . "glyphicon glyphicon-minus-sign")
    ("remove-sign" . "glyphicon glyphicon-remove-sign")
    ("ok-sign" . "glyphicon glyphicon-ok-sign")
    ("question-sign" . "glyphicon glyphicon-question-sign")
    ("info-sign" . "glyphicon glyphicon-info-sign")
    ("screenshot" . "glyphicon glyphicon-screenshot")
    ("remove-circle" . "glyphicon glyphicon-remove-circle")
    ("ok-circle" . "glyphicon glyphicon-ok-circle")
    ("ban-circle" . "glyphicon glyphicon-ban-circle")
    ("arrow-left" . "glyphicon glyphicon-arrow-left")
    ("arrow-right" . "glyphicon glyphicon-arrow-right")
    ("arrow-up" . "glyphicon glyphicon-arrow-up")
    ("arrow-down" . "glyphicon glyphicon-arrow-down")
    ("share-alt" . "glyphicon glyphicon-share-alt")
    ("resize-full" . "glyphicon glyphicon-resize-full")
    ("resize-small" . "glyphicon glyphicon-resize-small")
    ("exclamation-sign" . "glyphicon glyphicon-exclamation-sign")
    ("gift" . "glyphicon glyphicon-gift")
    ("leaf" . "glyphicon glyphicon-leaf")
    ("fire" . "glyphicon glyphicon-fire")
    ("eye-open" . "glyphicon glyphicon-eye-open")
    ("eye-close" . "glyphicon glyphicon-eye-close")
    ("warning-sign" . "glyphicon glyphicon-warning-sign")
    ("plane" . "glyphicon glyphicon-plane")
    ("calendar" . "glyphicon glyphicon-calendar")
    ("random" . "glyphicon glyphicon-random")
    ("comment" . "glyphicon glyphicon-comment")
    ("magnet" . "glyphicon glyphicon-magnet")
    ("chevron-up" . "glyphicon glyphicon-chevron-up")
    ("chevron-down" . "glyphicon glyphicon-chevron-down")
    ("retweet" . "glyphicon glyphicon-retweet")
    ("shopping-cart" . "glyphicon glyphicon-shopping-cart")
    ("folder-close" . "glyphicon glyphicon-folder-close")
    ("folder-open" . "glyphicon glyphicon-folder-open")
    ("resize-vertical" . "glyphicon glyphicon-resize-vertical")
    ("resize-horizontal" . "glyphicon glyphicon-resize-horizontal")
    ("hdd" . "glyphicon glyphicon-hdd")
    ("bullhorn" . "glyphicon glyphicon-bullhorn")
    ("bell" . "glyphicon glyphicon-bell")
    ("certificate" . "glyphicon glyphicon-certificate")
    ("thumbs-up" . "glyphicon glyphicon-thumbs-up")
    ("thumbs-down" . "glyphicon glyphicon-thumbs-down")
    ("hand-right" . "glyphicon glyphicon-hand-right")
    ("hand-left" . "glyphicon glyphicon-hand-left")
    ("hand-up" . "glyphicon glyphicon-hand-up")
    ("hand-down" . "glyphicon glyphicon-hand-down")
    ("circle-arrow-right" . "glyphicon glyphicon-circle-arrow-right")
    ("circle-arrow-left" . "glyphicon glyphicon-circle-arrow-left")
    ("circle-arrow-up" . "glyphicon glyphicon-circle-arrow-up")
    ("circle-arrow-down" . "glyphicon glyphicon-circle-arrow-down")
    ("globe" . "glyphicon glyphicon-globe")
    ("wrench" . "glyphicon glyphicon-wrench")
    ("tasks" . "glyphicon glyphicon-tasks")
    ("filter" . "glyphicon glyphicon-filter")
    ("briefcase" . "glyphicon glyphicon-briefcase")
    ("fullscreen" . "glyphicon glyphicon-fullscreen")
    ("dashboard" . "glyphicon glyphicon-dashboard")
    ("paperclip" . "glyphicon glyphicon-paperclip")
    ("heart-empty" . "glyphicon glyphicon-heart-empty")
    ("link" . "glyphicon glyphicon-link")
    ("phone" . "glyphicon glyphicon-phone")
    ("pushpin" . "glyphicon glyphicon-pushpin")
    ("usd" . "glyphicon glyphicon-usd")
    ("gbp" . "glyphicon glyphicon-gbp")
    ("sort" . "glyphicon glyphicon-sort")
    ("sort-by-alphabet" . "glyphicon glyphicon-sort-by-alphabet")
    ("sort-by-alphabet-alt" . "glyphicon glyphicon-sort-by-alphabet-alt")
    ("sort-by-order" . "glyphicon glyphicon-sort-by-order")
    ("sort-by-order-alt" . "glyphicon glyphicon-sort-by-order-alt")
    ("sort-by-attributes" . "glyphicon glyphicon-sort-by-attributes")
    ("sort-by-attributes-alt" . "glyphicon glyphicon-sort-by-attributes-alt")
    ("unchecked" . "glyphicon glyphicon-unchecked")
    ("expand" . "glyphicon glyphicon-expand")
    ("collapse-down" . "glyphicon glyphicon-collapse-down")
    ("collapse-up" . "glyphicon glyphicon-collapse-up")
    ("log-in" . "glyphicon glyphicon-log-in")
    ("flash" . "glyphicon glyphicon-flash")
    ("log-out" . "glyphicon glyphicon-log-out")
    ("new-window" . "glyphicon glyphicon-new-window")
    ("record" . "glyphicon glyphicon-record")
    ("save" . "glyphicon glyphicon-save")
    ("open" . "glyphicon glyphicon-open")
    ("saved" . "glyphicon glyphicon-saved")
    ("import" . "glyphicon glyphicon-import")
    ("export" . "glyphicon glyphicon-export")
    ("send" . "glyphicon glyphicon-send")
    ("floppy-disk" . "glyphicon glyphicon-floppy-disk")
    ("floppy-saved" . "glyphicon glyphicon-floppy-saved")
    ("floppy-remove" . "glyphicon glyphicon-floppy-remove")
    ("floppy-save" . "glyphicon glyphicon-floppy-save")
    ("floppy-open" . "glyphicon glyphicon-floppy-open")
    ("credit-card" . "glyphicon glyphicon-credit-card")
    ("transfer" . "glyphicon glyphicon-transfer")
    ("cutlery" . "glyphicon glyphicon-cutlery")
    ("header" . "glyphicon glyphicon-header")
    ("compressed" . "glyphicon glyphicon-compressed")
    ("earphone" . "glyphicon glyphicon-earphone")
    ("phone-alt" . "glyphicon glyphicon-phone-alt")
    ("tower" . "glyphicon glyphicon-tower")
    ("stats" . "glyphicon glyphicon-stats")
    ("sd-video" . "glyphicon glyphicon-sd-video")
    ("hd-video" . "glyphicon glyphicon-hd-video")
    ("subtitles" . "glyphicon glyphicon-subtitles")
    ("sound-stereo" . "glyphicon glyphicon-sound-stereo")
    ("sound-dolby" . "glyphicon glyphicon-sound-dolby")
    ("copyright-mark" . "glyphicon glyphicon-copyright-mark")
    ("registration-mark" . "glyphicon glyphicon-registration-mark")
    ("cloud-download" . "glyphicon glyphicon-cloud-download")
    ("cloud-upload" . "glyphicon glyphicon-cloud-upload")
    ("tree-conifer" . "glyphicon glyphicon-tree-conifer")
    ("tree-deciduous" . "glyphicon glyphicon-tree-deciduous")
    ("cd" . "glyphicon glyphicon-cd")
    ("save-file" . "glyphicon glyphicon-save-file")
    ("open-file" . "glyphicon glyphicon-open-file")
    ("level-up" . "glyphicon glyphicon-level-up")
    ("copy" . "glyphicon glyphicon-copy")
    ("paste" . "glyphicon glyphicon-paste")
    ("alert" . "glyphicon glyphicon-alert")
    ("equalizer" . "glyphicon glyphicon-equalizer")
    ("king" . "glyphicon glyphicon-king")
    ("queen" . "glyphicon glyphicon-queen")
    ("pawn" . "glyphicon glyphicon-pawn")
    ("bishop" . "glyphicon glyphicon-bishop")
    ("knight" . "glyphicon glyphicon-knight")
    ("baby-formula" . "glyphicon glyphicon-baby-formula")
    ("tent" . "glyphicon glyphicon-tent")
    ("blackboard" . "glyphicon glyphicon-blackboard")
    ("bed" . "glyphicon glyphicon-bed")
    ("apple" . "glyphicon glyphicon-apple")
    ("erase" . "glyphicon glyphicon-erase")
    ("hourglass" . "glyphicon glyphicon-hourglass")
    ("lamp" . "glyphicon glyphicon-lamp")
    ("duplicate" . "glyphicon glyphicon-duplicate")
    ("piggy-bank" . "glyphicon glyphicon-piggy-bank")
    ("scissors" . "glyphicon glyphicon-scissors")
    ("bitcoin" . "glyphicon glyphicon-bitcoin")
    ("btc" . "glyphicon glyphicon-btc")
    ("xbt" . "glyphicon glyphicon-xbt")
    ("yen" . "glyphicon glyphicon-yen")
    ("jpy" . "glyphicon glyphicon-jpy")
    ("ruble" . "glyphicon glyphicon-ruble")
    ("rub" . "glyphicon glyphicon-rub")
    ("scale" . "glyphicon glyphicon-scale")
    ("ice-lolly" . "glyphicon glyphicon-ice-lolly")
    ("ice-lolly-tasted" . "glyphicon glyphicon-ice-lolly-tasted")
    ("education" . "glyphicon glyphicon-education")
    ("option-horizontal" . "glyphicon glyphicon-option-horizontal")
    ("option-vertical" . "glyphicon glyphicon-option-vertical")
    ("menu-hamburger" . "glyphicon glyphicon-menu-hamburger")
    ("modal-window" . "glyphicon glyphicon-modal-window")
    ("oil" . "glyphicon glyphicon-oil")
    ("grain" . "glyphicon glyphicon-grain")
    ("sunglasses" . "glyphicon glyphicon-sunglasses")
    ("text-size" . "glyphicon glyphicon-text-size")
    ("text-color" . "glyphicon glyphicon-text-color")
    ("text-background" . "glyphicon glyphicon-text-background")
    ("object-align-top" . "glyphicon glyphicon-object-align-top")
    ("object-align-bottom" . "glyphicon glyphicon-object-align-bottom")
    ("object-align-horizontal" . "glyphicon glyphicon-object-align-horizontal")
    ("object-align-left" . "glyphicon glyphicon-object-align-left")
    ("object-align-vertical" . "glyphicon glyphicon-object-align-vertical")
    ("object-align-right" . "glyphicon glyphicon-object-align-right")
    ("triangle-right" . "glyphicon glyphicon-triangle-right")
    ("triangle-left" . "glyphicon glyphicon-triangle-left")
    ("triangle-bottom" . "glyphicon glyphicon-triangle-bottom")
    ("triangle-top" . "glyphicon glyphicon-triangle-top")
    ("console" . "glyphicon glyphicon-console")
    ("superscript" . "glyphicon glyphicon-superscript")
    ("subscript" . "glyphicon glyphicon-subscript")
    ("menu-left" . "glyphicon glyphicon-menu-left")
    ("menu-right" . "glyphicon glyphicon-menu-right")
    ("menu-down" . "glyphicon glyphicon-menu-down")))

(defvar qz-twbs-default-glyphicon "home")

(defun qz-twbs-read-glyphicon ()
  "Read from minibuffer with completion."
  (completing-read
   (format "Glyphicon Name (%s): "
           qz-twbs-default-glyphicon)
   qz-twbs-glyphicons
   nil
   t))

(defun qz-twbs-insert-glyphicon (name &optional html)
  "Insert twitter bootstrap glyphicon class at where point is.
If non-nil prefix HTML, insert glyphicon class wrapped in html tag."
  (interactive
   (list (qz-twbs-read-glyphicon)
         current-prefix-arg))
  (setq qz-twbs-default-glyphicon name)
  (if html
      (qz-twbs--insert-glyphicon name 1)
    (qz-twbs--insert-glyphicon name)))

(defun qz-twbs--insert-glyphicon (name &optional html)
  "Non-interactive insert twitter bootstrap glyphicon class at where
point is. If non-nil prefix HTML, insert glyphicon class wrapped in
html tag."
  (let ((template "%s")
        (glyphicon (cdr (assoc name qz-twbs-glyphicons))))
    (when html
      (setq template "<span class=\"%s\" aria-hidden=\"true\"></span> "))
    (insert (format template glyphicon))))

(defun qz-twbs-sr-only (&optional html)
  "Insert twitter bootstrap class sr-only.
If non-nil prefix HTML, insert sr-only wrapped in html tag with value
'Error:'."
  (interactive "P")
  (if html
      (qz-twbs--sr-only 1)
    (qz-twbs--sr-only)))

(defun qz-twbs--sr-only (&optional html)
  "Non-interactive insert twitter bootstrap class sr-only.
If non-nil prefix HTML, insert sr-only wrapped in html tag with value
'Error:'."
  (let ((template "%s"))
    (when html
      (setq template "<span class=\"%s\">Error:</span>"))
    (insert (format template "sr-only"))))

;;; group unknown

(defvar qz-twbs-arias
  '((controls . "aria-controls=\"\"")
    (expanded . "aria-expanded=\"\"")
    (hidden . "aria-hidden=\"\"")
    (label . "aria-label=\"\"")
    (haspopup . "aria-haspopup=\"\"")
    (labelledby . "aria-labelledby=\"\"")
    (describedby . "aria-describedby=\"\"")
    (valuenow . "aria-valuenow=\"\"")
    (valuemin . "aria-valuemin=\"\"")
    (valuemax . "aria-valuemax=\"\"")
    (pressed . "aria-pressed=\"\"")))

(defvar qz-twbs-roles
  '((banner . "role-banner=\"\"")
    (main . "role-main=\"\"")
    (toolbar . "role-toolbar=\"\"")
    (alert . "role-alert=\"\"")
    (separator . "role-separator=\"\"")
    (group . "role-group=\"\"")
    (button . "role-button=\"\"")
    (navigation . "role-navigation=\"\"")
    (presentation . "role-presentation=\"\"")
    (search . "role-search=\"\"")
    (tablist . "role-tablist=\"\"")
    (progressbar . "role-progressbar=\"\"")
    (complementary . "role-complementary=\"\"")
    (contentinfo . "role-contentinfo=\"\"")))

;;; TODO dropdowns

(defvar qz-twbs-menu-separator
  "<li role=\"separator\" class=\"divider\"/>")

;;; qz-bootstrap-3.el ends here
