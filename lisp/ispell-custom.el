;;; ispell-custom --- ISpell customization -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this package with:
;; (require 'ispell-custom)

;;; Code:
;;** Spell checking


;;     spc accepts the word, this time only,
;;     i   accepts the word and inserts it in your personal dictionary,
;;     a   accepts the word for this session,
;;     A   accepts the word for this file, and inserts it in the local file dictionary
;;     r   allows you to correct the word by hand
;;     R   allows you to correct all the occurrences of the misspelled word,
;;     x   stops the checking, and puts the cursor back in place,
;;     X   stops the checking and leaves the cursor where it is, letting you correct your file; you will be able to continue the spell-checking later if you type Meta-x ispell-continue,
;;     ?   gives you online help.


;; hunspell aliase:
;;  ("american" "/usr/share/hunspell/en_US.dic" "/usr/share/hunspell/en_US.aff")
;;  ("english" "/usr/share/hunspell/en_US.dic" "/usr/share/hunspell/en_US.aff")
;;  ("en_US" "/usr/share/hunspell/en_US.dic" "/usr/share/hunspell/en_US.aff"))
;;  ("deutsch" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("deutsch8" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("german" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("german8" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")
;;  ("de_DE" "/usr/share/hunspell/de_DE.dic" "/usr/share/hunspell/de_DE.aff")


;; The default personal dictionary depends on the locale settings.
;; The following environment variables are searched:
;; LC_ALL, LC_MESSAGES, and LANG.
;; If none  are  set  then  the  default  personal  dictionary  is
;; $HOME/.hunspell_default.
;; Setting -d or  the DICTIONARY environmental variable, personal dictionary will be
;; $HOME/.hunspell_dicname

;; Example
;; -d en_US,en_geo,en_med,de_DE,de_med
;; Meaning:
;; Base dictionary: en_US, Personal dictionaries: en_geo, en_med
;; Base dictionary: de_DE, Personal dictionaries: de_med

;; ispell-program-name: Program invoked by M-$ and M-x ispell-region commands.
;; ispell-dictionary: Default dictionary to use if `ispell-local-dictionary' is nil (nil -> use hunspell standard dictionary).
;; ispell-local-dictionary: Buffer local dictionary
;; ispell-personal-dictionary: Personal dictionary (nil -> use hunspell standard location $HOME/.hunspell_dicname)
;;    $HOME/.hunspell_default.  Default path to personal dictionary.
;; ispell-dictionary-alist: (nil) List of ispell-dictionaries
;; ispell-local-dictionary-alist: (nil) Override definitions in ispell-dictionary-alist
;; ispell-alternate-dictionary: ("/usr/share/dict/words") Plain word-list dictionary for spelling help
;; ispell-complete-word-dict: (nil) Plain word-list dictionary used for word completion if different from `ispell-alternate-dictionary'.
(setq ispell-program-name "hunspell"
      ispell-dictionary "en_US"
      ispell-dictionary-alist
      '(("en_US" ; DICTIONARY-NAME used by ispell-dictionary
         "[[:alpha:]]" ; CASECHARS valid characters that comprise a word
         "[^[:alpha:]]" ; NOT-CASECHARS is the opposite regexp of CASECHARS
         "[']" ; OTHERCHARS from NOT-CASECHARS parsed as part of a word in special cases
         nil ; MANY-OTHERCHARS-P only one OTHERCHAR is allowed in a single word
         ("-d" "en_US") ; ISPELL-ARGS contains ACTUAL parameters passed to hunspell ("-d" "en_US,en_US-med")
         nil ; EXTENDED-CHARACTER-MODE For example, umlauts can be encoded as \"a, a\", "a, ...
         utf-8) ; CHARACTER-SET text coding sent to hunspell when the text contains non-ASCII characters))
        ("de_DE" ; DICTIONARY-NAME used by ispell-dictionary
         "[[:alpha:]]" ; CASECHARS valid characters that comprise a word
         "[^[:alpha:]]" ; NOT-CASECHARS is the opposite regexp of CASECHARS
         "[']" ; OTHERCHARS from NOT-CASECHARS parsed as part of a word in special cases
         nil ; MANY-OTHERCHARS-P only one OTHERCHAR is allowed in a single word
         ("-d" "de_DE") ; ISPELL-ARGS contains ACTUAL parameters passed to hunspell ("-d" "en_US,en_US-med")
         nil ; EXTENDED-CHARACTER-MODE For example, umlauts can be encoded as \"a, a\", "a, ...
         utf-8)) ; CHARACTER-SET text coding sent to hunspell when the text contains non-ASCII characters))
)
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^"))



(provide 'ispell-custom)

;;; ispell-custom.el ends here
