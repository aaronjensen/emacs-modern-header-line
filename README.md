# Modern Header Line

A fork of https://github.com/rougier/nano-modeline with most of its functionality removed and pared down to my specific use case.

## Installation With Elpaca

```el
(use-package modern-header-line
  :ensure (:host github :repo "aaronjensen/emacs-modern-header-line" :protocol ssh)
  :demand t

  :config
  (setq modern-header-line-abbreviations
        '(("some-long-name" . "sln")))

  (modern-header-line-mode)

  (with-current-buffer "*Messages*"
    (modern-header-line)))
```
