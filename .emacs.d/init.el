(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

; Load themes
(load-theme 'pink-ocean t)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; This is only needed once, near the top of the file
;(package-refresh-contents)

(use-package ido
  :config (ido-mode t))
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(use-package which-key
  :config (which-key-mode))
(use-package lsp-mode
  :init  (setq lsp-keymap-prefix "C-c l")
  :requires (company lsp-ui rust-mode)
  :after (lsp lsp-haskell)
  :hook (haskell-mode . lsp)
  (haskell-iterate-mode . lsp)
  (purescript-mode . lsp)
  (rust-mode . lsp)
  (typescript-mode . lsp)
  (javacript-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(add-hook 'after-init-hook 'global-company-mode)
(use-package company-box
  :hook (company-mode . company-box-mode))

;(use-package rust-mode)

;; Hooks so haskell and literate haskell major modes trigger LSP setup

(use-package nix-mode)
(use-package evil
  :config
  (evil-mode 1)
  )
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package unicode-fonts
  :hook (unicode-fonts-setup))



(use-package ligature
  :load-path "~/.emacs.d/ligature.el"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<*" "<*>" "<+>" "<$>" "***" "<|" "|>" "<|>"
				       "!!" "||" "===" "==>" "<<<" ">>>" "<>" "+++"
				       "<-" "->" "=>" ">>" "<<" ">>=" "=<<" ".."
				       "..." "::" "-<" ">-" "-<<" ">>-" "++" "/=" "=="))

  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

; (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
; (add-to-list 'default-frame-alist '(alpha . (85 . 85)))
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-enabled-themes '(pink-ocean))
 '(custom-safe-themes
   '("88412a1061046873673502ea004933c281f70df2f0c2d0178f667b37a730cb7f" "3d9119ace62fcd77416c59da0ffbacd3720af0c8677f873bae1a64e6dce65a44" "297abb6594360bea8b52d8ab707afe33f71427cbff5b8d5b11d3a05d7f35223e" "99a1b30e075eb460d9c65cd957bad14b895bc6e85599c7ceb56cb3ccc18091e9" "04d29d5415cd42e1a37b424237b4b332e520a696aee7d449511cfbcfd5e71343" "81dd7ac9ca429fa9952a9a35ae215e6cc97874d34f818fdcffb37c0257106204" "f42974ccb1046f9ba58b5b9d0c11005b2e01502878c5ef937c203e2f520ba60b" "147e630051be15bc3062b336c09912da642334fb7ab79f88dd1f376eb8f65d36" "3bee18505207b2a474aed3059e8f515b47ab9270801c0ce1972786a8c90eca8b" "b163d8e20ba38b5012a5b54de0ac4dbac827519682943a01512c353d89123b08" "9107c801421f9577481d8b49352a09aac3624562768e2982fac819eb6c93a05f" "0965a3bdf3b1c4dc9dde431956d3039994175b2c697446f07ecfee32757da3c5" "753bb0c8819b1fc06a7ac5e92f2be94c4a4f5eb7840f80aea5f5562bdb59a5e2" "e740f544ded8d5f32826afa4d62a71519184bf051d1eeece0a41d482a74d25fc" "f2d74bed382b250ebe9876af0f9366cc8fd827a59cb2006244f8d72a5926dbd6" "30ebaa3376ec5ae933af79253504294c6a2b78650752f842dbe015a2736a9a06" "fc142df7cc00fd5a6d9ab1b65f9d21f9103ab7ac2639657f1e9a323bfd725385" "a9cd2b648208e9d4fdd81be02eed51535e3325de505cead65a855b500a642df4" "17fc84f99d5358712a014481fb24ffc527c137d52c4d1f5c62e1a25c6ca65f30" "f6563f7e1b37a148b0f77d3742c2a058d0efd1f104afec867d2542632ee41140" "dde66647610c613e26a5d52d5b192dbec3871891dacbd6a380a00814061028b7" "decc9b7a7408b0150d03e34d09ca2c7e4dc5bd424d31892c4ef5ffaf6678920c" "9e39a8334e0e476157bfdb8e42e1cea43fad02c9ec7c0dbd5498cf02b9adeaf1" default))
 '(display-line-numbers-type 'relative)
 '(fci-rule-color "#3E4451")
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(org which-key-posframe company-box company-cabal rust-mode unicode-fonts doom-modeline atom-one-dark-theme purescript-mode company which-key tide nix-mode flycheck lsp-ui use-package lsp-mode evil telephone-line))
 '(scroll-bar-mode nil)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hasklug Nerd Font" :foundry "ADBO" :slant normal :weight normal :height 103 :width normal)))))


