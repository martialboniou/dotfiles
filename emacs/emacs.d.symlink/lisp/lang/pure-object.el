;;; pure-object.el --- 
;; 
;; Filename: pure-object.el
;; Description: Every OO programming languages
;; Author: Martial Boniou
;; Maintainer: 
;; Created: Sat Mar  5 23:58:09 2011 (+0100)
;; Version: 
;; Last-Updated: Tue Jun 11 13:48:25 2013 (+0200)
;;           By: Martial Boniou
;;     Update #: 33
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: gst / factorcode 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
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
;;; Code:

(provide 'one-language-spoken)
(require 'code-preamble)
(unintern 'one-language-spoken obarray)

;;; GNU SMALLTALK
;;
(require-if-located 'smalltalk-mode-init)

;;; RUBY
;;
;; - Rinari Is Not A Rails IDE
(when (locate-library "rinari")
  (add-lambda-hook 'ruby-mode-hook
    (defadvice ruby-mode-set-encoding
      (around ruby-mode-set-encoding-disable activate) nil))
  (mars/add-hooks (mars/generate-mode-hook-list '(rhtml ruby)) #'rinari-launch))

;; - ruby electric
;; NOTE: AUTOPAIR case is managed in 'CODE

;;; FACTOR
;;
(when (boundp 'factorcode-source-rep)
  (when (file-directory-p factorcode-source-rep)
    (load (joindirs factorcode-source-rep "misc" "fuel" "fu") t))) ; fu.el autoloads

(provide 'pure-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pure-object.el ends here
