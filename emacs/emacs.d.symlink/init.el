;; -*- auto-byte-compile: t -*-
;;; init.el
;;
;; Filename: init.el
;; Description: Emacs Setup
;; Author: Martial Boniou
;; Maintainer: Martial Boniou (hondana.net/about)
;; Created: Wed Nov 18 11:53:01 2006
;; Version: 5.1
;; Last-Updated: Tue Jun 11 13:52:15 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 2071
;; URL: https://github.com/martialboniou/Dots.git
;; Keywords: .emacs, init
;; Compatibility: 
;;
;; Features that might be required by this library:
;;
;;   CEDET 1.0 normally loaded..
;;   REMARK: `load-path' and `loaddefs' generated in 'WALKER and 'ADAPTER;
;;           'PACKS should be replaced by 'EL-SELECT and may soon not be
;;           required:
;;           - `el-select' <-- `walker' <-- `packs'
;;           - `el-select' <-- `walker' <-- `adapter'
;;           you should be able to load 'KERNEL w/o 'PACKS but only 'ADAPTER
;;   TIPS: http://cheat.errtheblog.com/s/emacs_tips/
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(delete-dups load-path)

(provide 'emacs-normal-startup)

(require 'kernel)

;;; SERVER
;;
(start-named-server user-login-name)

;;; ALL THE FUN
;; these packages are not loaded when Emacs boots from `kernel'
;;
(require 'mail)         ; emacs as a MUA, a web browser, a syndicate and an organizer
(require 'rectify)      ; emacs as a programming environment including smart code validation
(require 'version)      ; emacs as a VC tool
(require 'vt)           ; emacs as a virtual terminal
(require 'media)        ; emacs as a multimedia player
(require 'toolbox)      ; emacs as a swiss army knife

;; minibuffer histories
(savehist-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
