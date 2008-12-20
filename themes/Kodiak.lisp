;;; -*- Mode: Lisp -*-
;;; Kodiak 4/14/2000-11/1/2000
;;; $Id: Kodiak.lisp,v 1.4 2005-11-19 18:50:42 piso Exp $

(unless (find-package '#:kodiak)
  (defpackage #:kodiak (:use #:cl #:j)))

(in-package #:kodiak)

;;; Colors.
(defconstant black      "  0   0   0")
(defconstant red        "255   0   0")
(defconstant blue       "  0   0 255")

;;; Styles.
(defconstant plain      "0")
(defconstant bold       "1")
(defconstant italic     "2")

(set-global-property "color.text"                           black)
(set-global-property "color.background"                     "255 255 224")
(set-global-property "color.currentLineBackground"          "235 235 204")
(set-global-property "color.selectionBackground"            "153 204 255")
(set-global-property "color.matchingBracketBackground"      "153 204 255")
(set-global-property "color.caret"                          red)

(set-global-property "color.keyword"                        "  0   0 153")
(set-global-property "color.string"                         "153  51   0")
(set-global-property "color.comment"                        "  0 102   0")
(set-global-property "color.function"                       black)
(set-global-property "color.operator"                       blue)
(set-global-property "color.brace"                          "  0 128 128")
(set-global-property "color.number"                         "153 102  51")
(set-global-property "color.preprocessor"                   red)
(set-global-property "color.disabled"                       "153 153 153")
(set-global-property "color.verticalRule"                   "204 204 204")
(set-global-property "color.lineNumber"                     "153 153 153")
(set-global-property "color.gutterBorder"                   "153 153 153")
(set-global-property "color.change"                         "255 164   0")
(set-global-property "color.savedChange"                    "180 180 180")
(set-global-property "color.delimiter"                      "  0 153 153")

(set-global-property "CSSMode.color.selector"               black)
(set-global-property "CSSMode.style.selector"               bold)
(set-global-property "CSSMode.color.property"               "  0   0 204")
(set-global-property "CSSMode.style.property"               plain)

(set-global-property "PerlMode.color.scalar"                " 51  51   0")
(set-global-property "PerlMode.color.list"                  "  0  51  51")

(set-global-property "LispMode.color.substitution"          "153   0 153")
(set-global-property "LispMode.color.punctuation"           "102 102 102")
(set-global-property "LispMode.color.parenthesis"           "102 102 102")
(set-global-property "LispMode.color.secondaryKeyword"      "  0 102 153")

(set-global-property "TclMode.color.brace"                  "153   0  51")
(set-global-property "TclMode.style.brace"                  bold)
(set-global-property "TclMode.color.bracket"                "204 102   0")
(set-global-property "TclMode.style.bracket"                bold)

(set-global-property "VHDLMode.color.type"                  blue)

(set-global-property "PHPMode.color.var"                    " 51  51   0")
(set-global-property "PHPMode.color.tag"                    black)
(set-global-property "PHPMode.color.attribute"              "  0   0 128")
(set-global-property "PHPMode.color.equals"                 "  0 153 153")

(set-global-property "color.prompt"                         black)
(set-global-property "color.input"                          blue)

(set-global-property "XmlMode.color.attribute"              "0   0 128")
(set-global-property "XmlMode.color.equals"                 "0 153 153")
(set-global-property "XmlMode.color.namespace"              black)
(set-global-property "XmlMode.color.tag"                    black)

(set-global-property "HtmlMode.color.tag"                   "  0   0 204")
(set-global-property "HtmlMode.color.image"                 "204 102   0")
(set-global-property "HtmlMode.color.anchor"                " 51 153  51")
(set-global-property "HtmlMode.color.table"                 "204   0   0")
(set-global-property "HtmlMode.color.tableRow"              "153   0   0")
(set-global-property "HtmlMode.color.tableData"             "153  51   0")
(set-global-property "HtmlMode.color.comment"               "128 128 128")
(set-global-property "HtmlMode.color.script"                blue)

(set-global-property "MakefileMode.color.target"            black)

(set-global-property "DiffMode.color.file"                  plain)
(set-global-property "DiffMode.style.file"                  bold)
(set-global-property "DiffMode.color.header"                "  0 102   0")
(set-global-property "DiffMode.style.header"                italic)
(set-global-property "DiffMode.color.context"               black)
(set-global-property "DiffMode.color.inserted"              "153   0   0")
(set-global-property "DiffMode.style.inserted"              plain)
(set-global-property "DiffMode.color.deleted"               "  0   0 153")
(set-global-property "DiffMode.style.deleted"               plain)
(set-global-property "DiffMode.color.changed"               "  0 102 153")
(set-global-property "DiffMode.style.changed"               plain)

(set-global-property "DirectoryMode.color.directory"        black)
(set-global-property "DirectoryMode.style.directory"        bold)
(set-global-property "DirectoryMode.color.symlink"          blue)
(set-global-property "DirectoryMode.color.marked"           "153   0   0")
(set-global-property "DirectoryMode.style.marked"           bold)

(set-global-property "WebMode.color.link"                   blue)
(set-global-property "WebMode.style.link"                   plain)

(set-global-property "MailboxMode.color.date"               " 51  51  51")
(set-global-property "MailboxMode.style.date"               plain)
(set-global-property "MailboxMode.color.from"               black)
(set-global-property "MailboxMode.style.from"               bold)
(set-global-property "MailboxMode.color.size"               "51   51  51")
(set-global-property "MailboxMode.color.subject"            "51  102 102")
(set-global-property "MailboxMode.style.subject"            bold)

(set-global-property "MailboxMode.color.flaggedTo"          "204  51   0")
(set-global-property "MailboxMode.style.flaggedTo"          bold)
(set-global-property "MailboxMode.style.flaggedFlags"       plain)
(set-global-property "MailboxMode.style.flaggedDate"        plain)
(set-global-property "MailboxMode.color.flaggedFrom"        "204  51   0")
(set-global-property "MailboxMode.style.flaggedFrom"        bold)
(set-global-property "MailboxMode.style.flaggedSize"        plain)
(set-global-property "MailboxMode.color.flaggedSubject"     "204  51   0")
(set-global-property "MailboxMode.style.flaggedSubject"     bold)

(set-global-property "MailboxMode.color.marked"             "153   0   0")
(set-global-property "MailboxMode.style.marked"             bold)

(set-global-property "MessageMode.color.headerValue"        " 51 102 102")
(set-global-property "MessageMode.style.headerValue"        bold)
(set-global-property "MessageMode.color.signature"          "102 102 102")
(set-global-property "MessageMode.color.string"             "  0 102   0")
(set-global-property "MessageMode.style.string"             plain)
(set-global-property "MessageMode.color.comment"            "102 102 102")
(set-global-property "MessageMode.style.comment"            plain)

(set-global-property "WebMode.color.headerValue"            " 51 102 102")
(set-global-property "WebMode.style.headerValue"            bold)

(set-global-property "color.matchingText"                   "204 102   0")
(set-global-property "style.matchingText"                   bold)
(set-global-property "color.status"                         "  0   0 153")
(set-global-property "style.status"                         italic)

 ;; Properties mode.
(set-global-property "color.key"                            "  0   0 153")
(set-global-property "style.key"                            plain)
(set-global-property "color.value"                          "128   0   0")
(set-global-property "style.value"                          plain)
(set-global-property "color.section"                        "  0   0 153")
(set-global-property "style.section"                        bold)

 ;; List Registers mode
(set-global-property "color.registerPrefix"                 "  0   0 153")
(set-global-property "style.registerPrefix"                 bold)
(set-global-property "color.registerName"                   "204 102   0")
(set-global-property "style.registerName"                   bold)

 ;; Styles.
(set-global-property "style.text"                           plain)
(set-global-property "style.keyword"                        bold)
(set-global-property "style.function"                       bold)
(set-global-property "style.prompt"                         bold)
(set-global-property "style.comment"                        italic)
(set-global-property "style.delimiter"                      bold)
(set-global-property "XmlMode.style.tag"                    bold)
(set-global-property "PHPMode.style.tag"                    bold)
(set-global-property "MakefileMode.style.target"            bold)
