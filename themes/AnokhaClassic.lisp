;;; -*- Mode: Lisp -*-
;;; Anokha Classic
;;; $Id: AnokhaClassic.lisp,v 1.4 2005-11-19 18:50:49 piso Exp $

(unless (find-package '#:anokha-classic)
  (defpackage #:anokha-classic (:use #:cl #:j)))

(in-package #:anokha-classic)

;;; Colors.
(defconstant white      "255 255 255")
(defconstant black      "  0   0   0")
(defconstant red        "255   0   0")
(defconstant blue       "  0   0 255")

;;; Styles.
(defconstant plain      "0")
(defconstant bold       "1")
(defconstant italic     "2")

(set-global-property "color.text"                           white)
(set-global-property "color.background"                     "  0  51  51")
(set-global-property "color.currentLineBackground"          "  0   0   0")
(set-global-property "color.selectionBackground"            " 51 102 102")
(set-global-property "color.matchingBracketBackground"      " 51 102 102")
(set-global-property "color.caret"                          "204 255 204")

(set-global-property "color.preprocessor"                   "255 255   0")
(set-global-property "color.comment"                        "180 180 128")
(set-global-property "color.keyword"                        "204 102   0")
(set-global-property "color.brace"                          "255 255   0")
(set-global-property "color.number"                         "255 153   0")
(set-global-property "color.function"                       "255 153   0")
(set-global-property "color.string"                         "153 204 153")
(set-global-property "color.operator"                       "255 255   0")
(set-global-property "color.disabled"                       "153 153 153")
(set-global-property "color.verticalRule"                   " 96  96  72")
(set-global-property "color.lineNumber"                     "128 128  96")
(set-global-property "color.gutterBorder"                   " 96  96  72")
(set-global-property "color.change"                         "255 164   0")
(set-global-property "color.savedChange"                    "128  96   0")
(set-global-property "color.delimiter"                      "255 255   0")

(set-global-property "color.headerName"                     "204 102   0")
(set-global-property "color.headerValue"                    "255 255 204")
(set-global-property "color.matchingText"                   "255 153   0")
(set-global-property "color.signature"                      "204 204 153")

(set-global-property "MailboxMode.color.deleted"            "153 153 102")
(set-global-property "MailboxMode.color.to"                 "255 255   0")
(set-global-property "MailboxMode.color.flags"              "255 255 204")
(set-global-property "MailboxMode.color.date"               "204 204 153")
(set-global-property "MailboxMode.color.from"               "255 153   0")
(set-global-property "MailboxMode.color.size"               "204 204 153")
(set-global-property "MailboxMode.color.subject"            white)
(set-global-property "MailboxMode.style.subject"            bold)

(set-global-property "MailboxMode.color.flaggedTo"          "255 204   0")
(set-global-property "MailboxMode.style.flaggedTo"          bold)
(set-global-property "MailboxMode.color.flaggedSubject"     "255 204   0")
(set-global-property "MailboxMode.style.flaggedSubject"     bold)
(set-global-property "MailboxMode.color.flaggedFrom"        "255 204   0")
(set-global-property "MailboxMode.style.flaggedFrom"        bold)

(set-global-property "PerlMode.color.scalar"                "255 255 204")
(set-global-property "PerlMode.color.list"                  "204 204 153")

(set-global-property "LispMode.color.substitution"          "255 204 102")
(set-global-property "LispMode.color.parenthesis"           "153 153 102")
(set-global-property "LispMode.color.punctuation"           "153 153 102")
(set-global-property "LispMode.color.secondaryKeyword"      "153 102  51")

(set-global-property "PHPMode.color.var"                    "255 255 204")
(set-global-property "PHPMode.color.attribute"              "204 204 153")
(set-global-property "PHPMode.color.equals"                 "255 255   0")
(set-global-property "PHPMode.color.tag"                    "255 153   0")

(set-global-property "color.prompt"                         "153 204 153")
(set-global-property "color.input"                          "255 153   0")

(set-global-property "XmlMode.color.attribute"              "204 204 153")
(set-global-property "XmlMode.color.equals"                 "255 255   0")
(set-global-property "XmlMode.color.namespace"              "255 204   0")
(set-global-property "XmlMode.color.tag"                    "255 153   0")

(set-global-property "HtmlMode.color.tag"                   "153 204 153")
(set-global-property "HtmlMode.color.anchor"                "255 204   0")
(set-global-property "HtmlMode.color.image"                 "255 153   0")
(set-global-property "HtmlMode.color.table"                 "153 153 102")
(set-global-property "HtmlMode.color.tableRow"              "204 204 153")
(set-global-property "HtmlMode.color.tableData"             "255 255 204")
(set-global-property "HtmlMode.color.comment"               "102 153 153")
(set-global-property "HtmlMode.color.script"                "255   0   0")

(set-global-property "MakefileMode.color.target"            "255 153   0")

(set-global-property "PropertiesMode.color.key"             "255 153   0")
(set-global-property "PropertiesMode.style.key"             plain)
(set-global-property "PropertiesMode.color.value"           white)
(set-global-property "PropertiesMode.style.value"           plain)
(set-global-property "PropertiesMode.color.section"         "255 153   0")
(set-global-property "PropertiesMode.style.section"         bold)

(set-global-property "DirectoryMode.color.directory"        "255 153   0")
(set-global-property "DirectoryMode.color.symlink"          "255 204   0")
(set-global-property "color.marked"                         "255 255   0")

(set-global-property "DiffMode.color.file"                  "255 255   0")
(set-global-property "DiffMode.style.file"                  plain)
(set-global-property "DiffMode.color.header"                "180 180 128")
(set-global-property "DiffMode.style.header"                plain)
(set-global-property "DiffMode.color.deleted"               "180 180 180")
(set-global-property "DiffMode.color.inserted"              "255 153   0")
(set-global-property "DiffMode.color.context"               white)

 ;; List Registers mode
(set-global-property "color.registerHeader"                 "255 153   0")
(set-global-property "style.registerHeader"                 plain)
(set-global-property "color.registerName"                   "255 255   0")
(set-global-property "style.registerName"                   plain)

(set-global-property "style.text"                           plain)
(set-global-property "style.keyword"                        plain)
(set-global-property "style.function"                       plain)
(set-global-property "DirectoryMode.style.directory"        plain)
(set-global-property "DirectoryMode.style.marked"           plain)
(set-global-property "style.prompt"                         plain)
(set-global-property "style.comment"                        plain)
(set-global-property "style.tag"                            plain)
(set-global-property "style.delimiter"                      plain)
