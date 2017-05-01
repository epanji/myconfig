;; Copyright (C) 2017  Panji Kusuma

;; Author: Panji Kusuma <epanji@gmail.com>
;; Version: 0.0.5
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

;;; macros

(defmacro qz-define-read-prompt (prefix read-name key-string alist-string
                                        &optional html-template)
  "Create variable default, function non-interactive and interactive
from association list string with optional template as wrapper.
When call-interactively, minibuffer will prompt available options from
association list.

The argument to this command are as follow:

PREFIX:         a string to make it uniq.
READ-NAME:      a string as a name of single collection alist.
KEY-STRING:     a string as default value taken from alist key.
ALIST-STRING:   an association list with key and value as string.
HTML-TEMPLATE:  an optional string as template with single '%s' inside."
  (unless html-template
    (setq html-template "%s"))
  (when (and (stringp prefix)
             (stringp read-name)
             (stringp key-string)
             (stringp (cdar (symbol-value alist-string)))
             (stringp html-template))
    (let((default (concat prefix "-default-" read-name))
         (read (concat prefix "-read-" read-name))
         (get (concat prefix "-get-" read-name))
         (insert (concat prefix "-insert-" read-name)))
      `(progn
         (defvar ,(intern default) ,key-string)
         (defun ,(intern read) ()
           ,(concat "Read from minibuffer with completion.\n\n"
                    "This function generated by "
                    "`qz-define-read-prompt'.")
           (completing-read
            (format
             ,(concat (capitalize read-name) " Name (%s): ")
             ,(intern default)) ; prompt
            ,alist-string       ; collection
            nil                 ; predicate
            t                   ; require-match
            nil                 ; initial-input
            nil                 ; hist
            ,(intern default)   ; def
            ))
         (defun ,(intern get) (name &optional html)
           ,(concat "Non-interactive get association value from \`"
                    (symbol-name alist-string) "\'.\nIf non-nil HTML, "
                    "wrapped this value as defined.\n\n"
                    "This function generated by "
                    "`qz-define-read-prompt'.")
           (let ((template "%s")
                 (,(intern read-name) (cdr (assoc name ,alist-string))))
             (unless ,(intern read-name)
               (setq ,(intern read-name) ""))
             (when html
               (setq template ,html-template))
             (format template ,(intern read-name))))
         (defun ,(intern insert) (name &optional html)
           ,(concat "Insert association value from \`"
                    (symbol-name alist-string)
                    "\' at the point.\nIf non-nil HTML, "
                    "wrapped this value as defined.\n\n"
                    "This function generated by "
                    "`qz-define-read-prompt'.")
           (interactive
            (list (,(intern read))
                  current-prefix-arg))
           (setq ,(intern default) name)
           (if html
               (insert (,(intern get) name 1))
             (insert (,(intern get) name))))))))

;;; roles

(defvar qz-twbs-roles
  '(("alert" . "role=\"alert\"")
    ("banner" . "role=\"banner\"")
    ("button" . "role=\"button\"")
    ("complementary" . "role=\"complementary\"")
    ("contentinfo" . "role=\"contentinfo\"")
    ("group" . "role=\"group\"")
    ("main" . "role=\"main\"")
    ("navigation" . "role=\"navigation\"")
    ("presentation" . "role=\"presentation\"")
    ("progressbar" . "role=\"progressbar\"")
    ("search" . "role=\"search\"")
    ("separator" . "role=\"separator\"")
    ("tablist" . "role=\"tablist\"")
    ("toolbar" . "role=\"toolbar\"")))

(qz-define-read-prompt
 "qz-twbs" "role" "group" qz-twbs-roles)

;;; arias

(defvar qz-twbs-arias
  '(("controls" . "aria-controls=\"\"")
    ("describedby" . "aria-describedby=\"\"")
    ("expanded" . "aria-expanded=\"\"")
    ("haspopup" . "aria-haspopup=\"\"")
    ("hidden" . "aria-hidden=\"\"")
    ("label" . "aria-label=\"\"")
    ("labelledby" . "aria-labelledby=\"\"")
    ("pressed" . "aria-pressed=\"\"")
    ("valuemax" . "aria-valuemax=\"\"")
    ("valuemin" . "aria-valuemin=\"\"")
    ("valuenow" . "aria-valuenow=\"\"")))

(qz-define-read-prompt
 "qz-twbs" "aria" "hidden" qz-twbs-arias)

;;; datas

(defvar qz-twbs-datas
  '(("count" . "data-count=\"\"")
    ("dismiss" . "data-dismiss=\"\"")
    ("example-id" . "data-example-id=\"\"")
    ("lang" . "data-lang=\"\"")
    ("show-count" . "data-show-count=\"\"")
    ("toggle" . "data-toggle=\"\"")
    ("via" . "data-via=\"\"")))

(qz-define-read-prompt
 "qz-twbs" "data" "toggle" qz-twbs-datas)

;;; classes

(defvar qz-twbs-classes
  '(("active" . "active")
    ("affix" . "affix")
    ("alert" . "alert")
    ("arrow" . "arrow")
    ("badge" . "badge")
    ("bottom" . "bottom")
    ("breadcrumb" . "breadcrumb")
    ("btn" . "btn")
    ("caption" . "caption")
    ("caret" . "caret")
    ("carousel" . "carousel")
    ("checkbox" . "checkbox")
    ("close" . "close")
    ("collapse" . "collapse")
    ("collapsing" . "collapsing")
    ("container" . "container")
    ("css" . "css")
    ("danger" . "danger")
    ("disabled" . "disabled")
    ("divider" . "divider")
    ("dropdown" . "dropdown")
    ("dropup" . "dropup")
    ("empty" . "")
    ("fade" . "fade")
    ("focus" . "focus")
    ("glyphicon" . "glyphicon")
    ("hidden" . "hidden")
    ("hide" . "hide")
    ("in" . "in")
    ("info" . "info")
    ("initialism" . "initialism")
    ("invisible" . "invisible")
    ("item" . "item")
    ("jumbotron" . "jumbotron")
    ("label" . "label")
    ("lead" . "lead")
    ("left" . "left")
    ("map" . "map")
    ("mark" . "mark")
    ("media" . "media")
    ("modal" . "modal")
    ("nav" . "nav")
    ("navbar" . "navbar")
    ("next" . "next")
    ("open" . "open")
    ("pager" . "pager")
    ("pagination" . "pagination")
    ("panel" . "panel")
    ("popover" . "popover")
    ("prev" . "prev")
    ("previous" . "previous")
    ("progress" . "progress")
    ("radio" . "radio")
    ("right" . "right")
    ("row" . "row")
    ("show" . "show")
    ("small" . "small")
    ("success" . "success")
    ("table" . "table")
    ("thumbnail" . "thumbnail")
    ("tooltip" . "tooltip")
    ("top" . "top")
    ("warning" . "warning")
    ("well" . "well")))

(qz-define-read-prompt
 "qz-twbs" "class" "alert" qz-twbs-classes
 "class=\"%s\"")

;;; srs

(defvar qz-twbs-srs
  '(("only" . "sr-only")
    ("only-focusable" . "sr-only-focusable")))

(qz-define-read-prompt
 "qz-twbs" "sr" "only" qz-twbs-srs)

;;; glyphicons

(defvar qz-twbs-glyphicons
  '(("adjust" . "glyphicon-adjust")
    ("alert" . "glyphicon-alert")
    ("align-center" . "glyphicon-align-center")
    ("align-justify" . "glyphicon-align-justify")
    ("align-left" . "glyphicon-align-left")
    ("align-right" . "glyphicon-align-right")
    ("apple" . "glyphicon-apple")
    ("arrow-down" . "glyphicon-arrow-down")
    ("arrow-left" . "glyphicon-arrow-left")
    ("arrow-right" . "glyphicon-arrow-right")
    ("arrow-up" . "glyphicon-arrow-up")
    ("asterisk" . "glyphicon-asterisk")
    ("baby-formula" . "glyphicon-baby-formula")
    ("backward" . "glyphicon-backward")
    ("ban-circle" . "glyphicon-ban-circle")
    ("barcode" . "glyphicon-barcode")
    ("bed" . "glyphicon-bed")
    ("bell" . "glyphicon-bell")
    ("bishop" . "glyphicon-bishop")
    ("bitcoin" . "glyphicon-bitcoin")
    ("blackboard" . "glyphicon-blackboard")
    ("bold" . "glyphicon-bold")
    ("book" . "glyphicon-book")
    ("bookmark" . "glyphicon-bookmark")
    ("briefcase" . "glyphicon-briefcase")
    ("btc" . "glyphicon-btc")
    ("bullhorn" . "glyphicon-bullhorn")
    ("calendar" . "glyphicon-calendar")
    ("camera" . "glyphicon-camera")
    ("cd" . "glyphicon-cd")
    ("certificate" . "glyphicon-certificate")
    ("check" . "glyphicon-check")
    ("chevron-down" . "glyphicon-chevron-down")
    ("chevron-left" . "glyphicon-chevron-left")
    ("chevron-right" . "glyphicon-chevron-right")
    ("chevron-up" . "glyphicon-chevron-up")
    ("circle-arrow-down" . "glyphicon-circle-arrow-down")
    ("circle-arrow-left" . "glyphicon-circle-arrow-left")
    ("circle-arrow-right" . "glyphicon-circle-arrow-right")
    ("circle-arrow-up" . "glyphicon-circle-arrow-up")
    ("class" . "glyphicon")
    ("cloud" . "glyphicon-cloud")
    ("cloud-download" . "glyphicon-cloud-download")
    ("cloud-upload" . "glyphicon-cloud-upload")
    ("cog" . "glyphicon-cog")
    ("collapse-down" . "glyphicon-collapse-down")
    ("collapse-up" . "glyphicon-collapse-up")
    ("comment" . "glyphicon-comment")
    ("compressed" . "glyphicon-compressed")
    ("console" . "glyphicon-console")
    ("copy" . "glyphicon-copy")
    ("copyright-mark" . "glyphicon-copyright-mark")
    ("credit-card" . "glyphicon-credit-card")
    ("cutlery" . "glyphicon-cutlery")
    ("dashboard" . "glyphicon-dashboard")
    ("download" . "glyphicon-download")
    ("download-alt" . "glyphicon-download-alt")
    ("duplicate" . "glyphicon-duplicate")
    ("earphone" . "glyphicon-earphone")
    ("edit" . "glyphicon-edit")
    ("education" . "glyphicon-education")
    ("eject" . "glyphicon-eject")
    ("envelope" . "glyphicon-envelope")
    ("equalizer" . "glyphicon-equalizer")
    ("erase" . "glyphicon-erase")
    ("eur" . "glyphicon-eur")
    ("euro" . "glyphicon-euro")
    ("exclamation-sign" . "glyphicon-exclamation-sign")
    ("expand" . "glyphicon-expand")
    ("export" . "glyphicon-export")
    ("eye-close" . "glyphicon-eye-close")
    ("eye-open" . "glyphicon-eye-open")
    ("facetime-video" . "glyphicon-facetime-video")
    ("fast-backward" . "glyphicon-fast-backward")
    ("fast-forward" . "glyphicon-fast-forward")
    ("file" . "glyphicon-file")
    ("film" . "glyphicon-film")
    ("filter" . "glyphicon-filter")
    ("fire" . "glyphicon-fire")
    ("flag" . "glyphicon-flag")
    ("flash" . "glyphicon-flash")
    ("floppy-disk" . "glyphicon-floppy-disk")
    ("floppy-open" . "glyphicon-floppy-open")
    ("floppy-remove" . "glyphicon-floppy-remove")
    ("floppy-save" . "glyphicon-floppy-save")
    ("floppy-saved" . "glyphicon-floppy-saved")
    ("folder-close" . "glyphicon-folder-close")
    ("folder-open" . "glyphicon-folder-open")
    ("font" . "glyphicon-font")
    ("forward" . "glyphicon-forward")
    ("fullscreen" . "glyphicon-fullscreen")
    ("gbp" . "glyphicon-gbp")
    ("gift" . "glyphicon-gift")
    ("glass" . "glyphicon-glass")
    ("globe" . "glyphicon-globe")
    ("grain" . "glyphicon-grain")
    ("hand-down" . "glyphicon-hand-down")
    ("hand-left" . "glyphicon-hand-left")
    ("hand-right" . "glyphicon-hand-right")
    ("hand-up" . "glyphicon-hand-up")
    ("hd-video" . "glyphicon-hd-video")
    ("hdd" . "glyphicon-hdd")
    ("header" . "glyphicon-header")
    ("headphones" . "glyphicon-headphones")
    ("heart" . "glyphicon-heart")
    ("heart-empty" . "glyphicon-heart-empty")
    ("home" . "glyphicon-home")
    ("hourglass" . "glyphicon-hourglass")
    ("ice-lolly" . "glyphicon-ice-lolly")
    ("ice-lolly-tasted" . "glyphicon-ice-lolly-tasted")
    ("import" . "glyphicon-import")
    ("inbox" . "glyphicon-inbox")
    ("indent-left" . "glyphicon-indent-left")
    ("indent-right" . "glyphicon-indent-right")
    ("info-sign" . "glyphicon-info-sign")
    ("italic" . "glyphicon-italic")
    ("jpy" . "glyphicon-jpy")
    ("king" . "glyphicon-king")
    ("knight" . "glyphicon-knight")
    ("lamp" . "glyphicon-lamp")
    ("leaf" . "glyphicon-leaf")
    ("level-up" . "glyphicon-level-up")
    ("link" . "glyphicon-link")
    ("list" . "glyphicon-list")
    ("list-alt" . "glyphicon-list-alt")
    ("lock" . "glyphicon-lock")
    ("log-in" . "glyphicon-log-in")
    ("log-out" . "glyphicon-log-out")
    ("magnet" . "glyphicon-magnet")
    ("map-marker" . "glyphicon-map-marker")
    ("menu-down" . "glyphicon-menu-down")
    ("menu-hamburger" . "glyphicon-menu-hamburger")
    ("menu-left" . "glyphicon-menu-left")
    ("menu-right" . "glyphicon-menu-right")
    ("menu-up" . "glyphicon-menu-up")
    ("minus" . "glyphicon-minus")
    ("minus-sign" . "glyphicon-minus-sign")
    ("modal-window" . "glyphicon-modal-window")
    ("move" . "glyphicon-move")
    ("music" . "glyphicon-music")
    ("new-window" . "glyphicon-new-window")
    ("object-align-bottom" . "glyphicon-object-align-bottom")
    ("object-align-horizontal" . "glyphicon-object-align-horizontal")
    ("object-align-left" . "glyphicon-object-align-left")
    ("object-align-right" . "glyphicon-object-align-right")
    ("object-align-top" . "glyphicon-object-align-top")
    ("object-align-vertical" . "glyphicon-object-align-vertical")
    ("off" . "glyphicon-off")
    ("oil" . "glyphicon-oil")
    ("ok" . "glyphicon-ok")
    ("ok-circle" . "glyphicon-ok-circle")
    ("ok-sign" . "glyphicon-ok-sign")
    ("open" . "glyphicon-open")
    ("open-file" . "glyphicon-open-file")
    ("option-horizontal" . "glyphicon-option-horizontal")
    ("option-vertical" . "glyphicon-option-vertical")
    ("paperclip" . "glyphicon-paperclip")
    ("paste" . "glyphicon-paste")
    ("pause" . "glyphicon-pause")
    ("pawn" . "glyphicon-pawn")
    ("pencil" . "glyphicon-pencil")
    ("phone" . "glyphicon-phone")
    ("phone-alt" . "glyphicon-phone-alt")
    ("picture" . "glyphicon-picture")
    ("piggy-bank" . "glyphicon-piggy-bank")
    ("plane" . "glyphicon-plane")
    ("play" . "glyphicon-play")
    ("play-circle" . "glyphicon-play-circle")
    ("plus" . "glyphicon-plus")
    ("plus-sign" . "glyphicon-plus-sign")
    ("print" . "glyphicon-print")
    ("pushpin" . "glyphicon-pushpin")
    ("qrcode" . "glyphicon-qrcode")
    ("queen" . "glyphicon-queen")
    ("question-sign" . "glyphicon-question-sign")
    ("random" . "glyphicon-random")
    ("record" . "glyphicon-record")
    ("refresh" . "glyphicon-refresh")
    ("registration-mark" . "glyphicon-registration-mark")
    ("remove" . "glyphicon-remove")
    ("remove-circle" . "glyphicon-remove-circle")
    ("remove-sign" . "glyphicon-remove-sign")
    ("repeat" . "glyphicon-repeat")
    ("resize-full" . "glyphicon-resize-full")
    ("resize-horizontal" . "glyphicon-resize-horizontal")
    ("resize-small" . "glyphicon-resize-small")
    ("resize-vertical" . "glyphicon-resize-vertical")
    ("retweet" . "glyphicon-retweet")
    ("road" . "glyphicon-road")
    ("rub" . "glyphicon-rub")
    ("ruble" . "glyphicon-ruble")
    ("save" . "glyphicon-save")
    ("save-file" . "glyphicon-save-file")
    ("saved" . "glyphicon-saved")
    ("scale" . "glyphicon-scale")
    ("scissors" . "glyphicon-scissors")
    ("screenshot" . "glyphicon-screenshot")
    ("sd-video" . "glyphicon-sd-video")
    ("search" . "glyphicon-search")
    ("send" . "glyphicon-send")
    ("share" . "glyphicon-share")
    ("share-alt" . "glyphicon-share-alt")
    ("shopping-cart" . "glyphicon-shopping-cart")
    ("signal" . "glyphicon-signal")
    ("sort" . "glyphicon-sort")
    ("sort-by-alphabet" . "glyphicon-sort-by-alphabet")
    ("sort-by-alphabet-alt" . "glyphicon-sort-by-alphabet-alt")
    ("sort-by-attributes" . "glyphicon-sort-by-attributes")
    ("sort-by-attributes-alt" . "glyphicon-sort-by-attributes-alt")
    ("sort-by-order" . "glyphicon-sort-by-order")
    ("sort-by-order-alt" . "glyphicon-sort-by-order-alt")
    ("sound-dolby" . "glyphicon-sound-dolby")
    ("sound-stereo" . "glyphicon-sound-stereo")
    ("star" . "glyphicon-star")
    ("star-empty" . "glyphicon-star-empty")
    ("stats" . "glyphicon-stats")
    ("step-backward" . "glyphicon-step-backward")
    ("step-forward" . "glyphicon-step-forward")
    ("stop" . "glyphicon-stop")
    ("subscript" . "glyphicon-subscript")
    ("subtitles" . "glyphicon-subtitles")
    ("sunglasses" . "glyphicon-sunglasses")
    ("superscript" . "glyphicon-superscript")
    ("tag" . "glyphicon-tag")
    ("tags" . "glyphicon-tags")
    ("tasks" . "glyphicon-tasks")
    ("tent" . "glyphicon-tent")
    ("text-background" . "glyphicon-text-background")
    ("text-color" . "glyphicon-text-color")
    ("text-height" . "glyphicon-text-height")
    ("text-size" . "glyphicon-text-size")
    ("text-width" . "glyphicon-text-width")
    ("th" . "glyphicon-th")
    ("th-large" . "glyphicon-th-large")
    ("th-list" . "glyphicon-th-list")
    ("thumbs-down" . "glyphicon-thumbs-down")
    ("thumbs-up" . "glyphicon-thumbs-up")
    ("time" . "glyphicon-time")
    ("tint" . "glyphicon-tint")
    ("tower" . "glyphicon-tower")
    ("transfer" . "glyphicon-transfer")
    ("trash" . "glyphicon-trash")
    ("tree-conifer" . "glyphicon-tree-conifer")
    ("tree-deciduous" . "glyphicon-tree-deciduous")
    ("triangle-bottom" . "glyphicon-triangle-bottom")
    ("triangle-left" . "glyphicon-triangle-left")
    ("triangle-right" . "glyphicon-triangle-right")
    ("triangle-top" . "glyphicon-triangle-top")
    ("unchecked" . "glyphicon-unchecked")
    ("upload" . "glyphicon-upload")
    ("usd" . "glyphicon-usd")
    ("user" . "glyphicon-user")
    ("volume-down" . "glyphicon-volume-down")
    ("volume-off" . "glyphicon-volume-off")
    ("volume-up" . "glyphicon-volume-up")
    ("warning-sign" . "glyphicon-warning-sign")
    ("wrench" . "glyphicon-wrench")
    ("xbt" . "glyphicon-xbt")
    ("yen" . "glyphicon-yen")
    ("zoom-in" . "glyphicon-zoom-in")
    ("zoom-out" . "glyphicon-zoom-out")))

(qz-define-read-prompt
 "qz-twbs" "glyphicon" "class" qz-twbs-glyphicons
 "<span class=\"glyphicon %s\" aria-hidden=\"true\"></span> ")

;;; dropdowns

(defvar qz-twbs-dropdowns
  '(("backdrop" . "dropdown-backdrop")
    ("class" . "dropdown")
    ("header" . "dropdown-header")
    ("menu" . "dropdown-menu")
    ("menu-left" . "dropdown-menu-left")
    ("menu-right" . "dropdown-menu-right")
    ("toggle" . "dropdown-toggle")))

(qz-define-read-prompt
 "qz-twbs" "dropdown" "class" qz-twbs-dropdowns
 "class=\"%s\"")

;;; btns

(defvar qz-twbs-btns
  '(("block" . "btn-block")
    ("class" . "btn")
    ("danger" . "btn-danger")
    ("default" . "btn-default")
    ("group" . "btn-group")
    ("group-justified" . "btn-group-justified")
    ("group-lg" . "btn-group-lg")
    ("group-sm" . "btn-group-sm")
    ("group-vertical" . "btn-group-vertical")
    ("group-xs" . "btn-group-xs")
    ("info" . "btn-info")
    ("lg" . "btn-lg")
    ("link" . "btn-link")
    ("primary" . "btn-primary")
    ("sm" . "btn-sm")
    ("success" . "btn-success")
    ("toolbar" . "btn-toolbar")
    ("warning" . "btn-warning")
    ("xs" . "btn-xs")))

(qz-define-read-prompt
 "qz-twbs" "btn" "class" qz-twbs-btns
 "class=\"btn %s\"")

;;; inputs

(defvar qz-twbs-inputs
  '(("class" . "input")
    ("group" . "input-group")
    ("group-addon" . "input-group-addon")
    ("group-btn" . "input-group-btn")
    ("group-lg" . "input-group-lg")
    ("group-sm" . "input-group-sm")
    ("lg" . "input-lg")
    ("sm" . "input-sm")))

(qz-define-read-prompt
 "qz-twbs" "input" "class" qz-twbs-inputs
 "class=\"btn %s\"")

;;; cols

(defvar qz-twbs-cols
  '(("lg-1" . "col-lg-1")
    ("lg-10" . "col-lg-10")
    ("lg-11" . "col-lg-11")
    ("lg-12" . "col-lg-12")
    ("lg-2" . "col-lg-2")
    ("lg-3" . "col-lg-3")
    ("lg-4" . "col-lg-4")
    ("lg-5" . "col-lg-5")
    ("lg-6" . "col-lg-6")
    ("lg-7" . "col-lg-7")
    ("lg-8" . "col-lg-8")
    ("lg-9" . "col-lg-9")
    ("lg-offset-0" . "col-lg-offset-0")
    ("lg-offset-1" . "col-lg-offset-1")
    ("lg-offset-10" . "col-lg-offset-10")
    ("lg-offset-11" . "col-lg-offset-11")
    ("lg-offset-12" . "col-lg-offset-12")
    ("lg-offset-2" . "col-lg-offset-2")
    ("lg-offset-3" . "col-lg-offset-3")
    ("lg-offset-4" . "col-lg-offset-4")
    ("lg-offset-5" . "col-lg-offset-5")
    ("lg-offset-6" . "col-lg-offset-6")
    ("lg-offset-7" . "col-lg-offset-7")
    ("lg-offset-8" . "col-lg-offset-8")
    ("lg-offset-9" . "col-lg-offset-9")
    ("lg-pull-0" . "col-lg-pull-0")
    ("lg-pull-1" . "col-lg-pull-1")
    ("lg-pull-10" . "col-lg-pull-10")
    ("lg-pull-11" . "col-lg-pull-11")
    ("lg-pull-12" . "col-lg-pull-12")
    ("lg-pull-2" . "col-lg-pull-2")
    ("lg-pull-3" . "col-lg-pull-3")
    ("lg-pull-4" . "col-lg-pull-4")
    ("lg-pull-5" . "col-lg-pull-5")
    ("lg-pull-6" . "col-lg-pull-6")
    ("lg-pull-7" . "col-lg-pull-7")
    ("lg-pull-8" . "col-lg-pull-8")
    ("lg-pull-9" . "col-lg-pull-9")
    ("lg-push-0" . "col-lg-push-0")
    ("lg-push-1" . "col-lg-push-1")
    ("lg-push-10" . "col-lg-push-10")
    ("lg-push-11" . "col-lg-push-11")
    ("lg-push-12" . "col-lg-push-12")
    ("lg-push-2" . "col-lg-push-2")
    ("lg-push-3" . "col-lg-push-3")
    ("lg-push-4" . "col-lg-push-4")
    ("lg-push-5" . "col-lg-push-5")
    ("lg-push-6" . "col-lg-push-6")
    ("lg-push-7" . "col-lg-push-7")
    ("lg-push-8" . "col-lg-push-8")
    ("lg-push-9" . "col-lg-push-9")
    ("md-1" . "col-md-1")
    ("md-10" . "col-md-10")
    ("md-11" . "col-md-11")
    ("md-12" . "col-md-12")
    ("md-2" . "col-md-2")
    ("md-3" . "col-md-3")
    ("md-4" . "col-md-4")
    ("md-5" . "col-md-5")
    ("md-6" . "col-md-6")
    ("md-7" . "col-md-7")
    ("md-8" . "col-md-8")
    ("md-9" . "col-md-9")
    ("md-offset-0" . "col-md-offset-0")
    ("md-offset-1" . "col-md-offset-1")
    ("md-offset-10" . "col-md-offset-10")
    ("md-offset-11" . "col-md-offset-11")
    ("md-offset-12" . "col-md-offset-12")
    ("md-offset-2" . "col-md-offset-2")
    ("md-offset-3" . "col-md-offset-3")
    ("md-offset-4" . "col-md-offset-4")
    ("md-offset-5" . "col-md-offset-5")
    ("md-offset-6" . "col-md-offset-6")
    ("md-offset-7" . "col-md-offset-7")
    ("md-offset-8" . "col-md-offset-8")
    ("md-offset-9" . "col-md-offset-9")
    ("md-pull-0" . "col-md-pull-0")
    ("md-pull-1" . "col-md-pull-1")
    ("md-pull-10" . "col-md-pull-10")
    ("md-pull-11" . "col-md-pull-11")
    ("md-pull-12" . "col-md-pull-12")
    ("md-pull-2" . "col-md-pull-2")
    ("md-pull-3" . "col-md-pull-3")
    ("md-pull-4" . "col-md-pull-4")
    ("md-pull-5" . "col-md-pull-5")
    ("md-pull-6" . "col-md-pull-6")
    ("md-pull-7" . "col-md-pull-7")
    ("md-pull-8" . "col-md-pull-8")
    ("md-pull-9" . "col-md-pull-9")
    ("md-push-0" . "col-md-push-0")
    ("md-push-1" . "col-md-push-1")
    ("md-push-10" . "col-md-push-10")
    ("md-push-11" . "col-md-push-11")
    ("md-push-12" . "col-md-push-12")
    ("md-push-2" . "col-md-push-2")
    ("md-push-3" . "col-md-push-3")
    ("md-push-4" . "col-md-push-4")
    ("md-push-5" . "col-md-push-5")
    ("md-push-6" . "col-md-push-6")
    ("md-push-7" . "col-md-push-7")
    ("md-push-8" . "col-md-push-8")
    ("md-push-9" . "col-md-push-9")
    ("sm-1" . "col-sm-1")
    ("sm-10" . "col-sm-10")
    ("sm-11" . "col-sm-11")
    ("sm-12" . "col-sm-12")
    ("sm-2" . "col-sm-2")
    ("sm-3" . "col-sm-3")
    ("sm-4" . "col-sm-4")
    ("sm-5" . "col-sm-5")
    ("sm-6" . "col-sm-6")
    ("sm-7" . "col-sm-7")
    ("sm-8" . "col-sm-8")
    ("sm-9" . "col-sm-9")
    ("sm-offset-0" . "col-sm-offset-0")
    ("sm-offset-1" . "col-sm-offset-1")
    ("sm-offset-10" . "col-sm-offset-10")
    ("sm-offset-11" . "col-sm-offset-11")
    ("sm-offset-12" . "col-sm-offset-12")
    ("sm-offset-2" . "col-sm-offset-2")
    ("sm-offset-3" . "col-sm-offset-3")
    ("sm-offset-4" . "col-sm-offset-4")
    ("sm-offset-5" . "col-sm-offset-5")
    ("sm-offset-6" . "col-sm-offset-6")
    ("sm-offset-7" . "col-sm-offset-7")
    ("sm-offset-8" . "col-sm-offset-8")
    ("sm-offset-9" . "col-sm-offset-9")
    ("sm-pull-0" . "col-sm-pull-0")
    ("sm-pull-1" . "col-sm-pull-1")
    ("sm-pull-10" . "col-sm-pull-10")
    ("sm-pull-11" . "col-sm-pull-11")
    ("sm-pull-12" . "col-sm-pull-12")
    ("sm-pull-2" . "col-sm-pull-2")
    ("sm-pull-3" . "col-sm-pull-3")
    ("sm-pull-4" . "col-sm-pull-4")
    ("sm-pull-5" . "col-sm-pull-5")
    ("sm-pull-6" . "col-sm-pull-6")
    ("sm-pull-7" . "col-sm-pull-7")
    ("sm-pull-8" . "col-sm-pull-8")
    ("sm-pull-9" . "col-sm-pull-9")
    ("sm-push-0" . "col-sm-push-0")
    ("sm-push-1" . "col-sm-push-1")
    ("sm-push-10" . "col-sm-push-10")
    ("sm-push-11" . "col-sm-push-11")
    ("sm-push-12" . "col-sm-push-12")
    ("sm-push-2" . "col-sm-push-2")
    ("sm-push-3" . "col-sm-push-3")
    ("sm-push-4" . "col-sm-push-4")
    ("sm-push-5" . "col-sm-push-5")
    ("sm-push-6" . "col-sm-push-6")
    ("sm-push-7" . "col-sm-push-7")
    ("sm-push-8" . "col-sm-push-8")
    ("sm-push-9" . "col-sm-push-9")
    ("xs-1" . "col-xs-1")
    ("xs-10" . "col-xs-10")
    ("xs-11" . "col-xs-11")
    ("xs-12" . "col-xs-12")
    ("xs-2" . "col-xs-2")
    ("xs-3" . "col-xs-3")
    ("xs-4" . "col-xs-4")
    ("xs-5" . "col-xs-5")
    ("xs-6" . "col-xs-6")
    ("xs-7" . "col-xs-7")
    ("xs-8" . "col-xs-8")
    ("xs-9" . "col-xs-9")
    ("xs-offset-0" . "col-xs-offset-0")
    ("xs-offset-1" . "col-xs-offset-1")
    ("xs-offset-10" . "col-xs-offset-10")
    ("xs-offset-11" . "col-xs-offset-11")
    ("xs-offset-12" . "col-xs-offset-12")
    ("xs-offset-2" . "col-xs-offset-2")
    ("xs-offset-3" . "col-xs-offset-3")
    ("xs-offset-4" . "col-xs-offset-4")
    ("xs-offset-5" . "col-xs-offset-5")
    ("xs-offset-6" . "col-xs-offset-6")
    ("xs-offset-7" . "col-xs-offset-7")
    ("xs-offset-8" . "col-xs-offset-8")
    ("xs-offset-9" . "col-xs-offset-9")
    ("xs-pull-0" . "col-xs-pull-0")
    ("xs-pull-1" . "col-xs-pull-1")
    ("xs-pull-10" . "col-xs-pull-10")
    ("xs-pull-11" . "col-xs-pull-11")
    ("xs-pull-12" . "col-xs-pull-12")
    ("xs-pull-2" . "col-xs-pull-2")
    ("xs-pull-3" . "col-xs-pull-3")
    ("xs-pull-4" . "col-xs-pull-4")
    ("xs-pull-5" . "col-xs-pull-5")
    ("xs-pull-6" . "col-xs-pull-6")
    ("xs-pull-7" . "col-xs-pull-7")
    ("xs-pull-8" . "col-xs-pull-8")
    ("xs-pull-9" . "col-xs-pull-9")
    ("xs-push-0" . "col-xs-push-0")
    ("xs-push-1" . "col-xs-push-1")
    ("xs-push-10" . "col-xs-push-10")
    ("xs-push-11" . "col-xs-push-11")
    ("xs-push-12" . "col-xs-push-12")
    ("xs-push-2" . "col-xs-push-2")
    ("xs-push-3" . "col-xs-push-3")
    ("xs-push-4" . "col-xs-push-4")
    ("xs-push-5" . "col-xs-push-5")
    ("xs-push-6" . "col-xs-push-6")
    ("xs-push-7" . "col-xs-push-7")
    ("xs-push-8" . "col-xs-push-8")
    ("xs-push-9" . "col-xs-push-9")))

(qz-define-read-prompt
 "qz-twbs" "col" "sm-12" qz-twbs-cols
 "class=\"%s\"")

;;; navs

(defvar qz-twbs-navs
  '(("class" . "nav")
    ("divider" . "nav-divider")
    ("justified" . "nav-justified")
    ("pills" . "nav-pills")
    ("stacked" . "nav-stacked")
    ("tabs" . "nav-tabs")
    ("tabs-justified" . "nav-tabs-justified")))

(qz-define-read-prompt
 "qz-twbs" "nav" "class" qz-twbs-navs
 "class=\"%s\"")

;;; navbars

(defvar qz-twbs-navbars
  '(("brand" . "navbar-brand")
    ("btn" . "navbar-btn")
    ("class" . "navbar")
    ("collapse" . "navbar-collapse")
    ("default" . "navbar-default")
    ("fixed-bottom" . "navbar-fixed-bottom")
    ("fixed-top" . "navbar-fixed-top")
    ("form" . "navbar-form")
    ("header" . "navbar-header")
    ("inverse" . "navbar-inverse")
    ("left" . "navbar-left")
    ("link" . "navbar-link")
    ("nav" . "navbar-nav")
    ("right" . "navbar-right")
    ("static-top" . "navbar-static-top")
    ("text" . "navbar-text")
    ("toggle" . "navbar-toggle")))

(qz-define-read-prompt
 "qz-twbs" "navbar" "class" qz-twbs-navbars
 "class=\"%s\"")

;;; qz-bootstrap-3.el ends here
