(deftheme pink-ocean
  "Created 2021-11-17.")

(custom-theme-set-faces
 'pink-ocean
 '(cursor ((t (:background "#ccccc7"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "misty rose"))))
 '(highlight ((t (:foreground "#ccccc7" :background "#464752"))))
 '(region ((t (:inherit match :extend t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "SkyBlue4")) (((class color) (min-colors 8)) (:extend t :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(trailing-whitespace ((t (:foreground "unspecified-fg" :background "#ffb86c"))))
 '(font-lock-builtin-face ((t (:foreground "#8BB2C1"))))
 '(font-lock-comment-delimiter-face ((t (:inherit (Font\ Lock\ Comment)))))
 '(font-lock-comment-face ((t (:foreground "#707078"))))
 '(font-lock-constant-face ((t (:foreground "#ffa0a0"))))
 '(font-lock-doc-face ((t (:foreground "#6272a4"))))
 '(font-lock-function-name-face ((t (:foreground "#738290" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#8BB2C1" :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#8be9fd"))))
 '(font-lock-preprocessor-face ((t (:foreground "#DB5461"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#8be9fd"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#bd93f9"))))
 '(font-lock-string-face ((t (:foreground "#ffa0a0"))))
 '(font-lock-type-face ((t (:foreground "#A04668"))))
 '(font-lock-variable-name-face ((t (:foreground "#f8f8f2"))))
 '(font-lock-warning-face ((t (:foreground "#A04603" :background "#373844"))))
 '(button ((t (:inherit (link)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#8be9fd"))))
 '(link-visited ((t (:foreground "violet" :inherit (link)))))
 '(fringe ((t (:background "#1f1f1f" :foreground "#b6b6b2"))))
 '(header-line ((t (:background "#282a36"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box (:line-width 1 :color "#373844" :style nil) :inverse-video nil :foreground "#f8f8f2" :background "#373844"))))
 '(isearch ((t (:weight bold :inherit (match)))))
 '(isearch-fail ((t (:foreground "#282a36" :background "#ffb86c"))))
 '(lazy-highlight ((t (:foreground "#e2e2dc" :background "#373844"))))
 '(match ((t (:background "gray" :foreground "dim gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(mode-line ((t (:background "#2f2f2f" :foreground "white smoke"))))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1f1f1f" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "outline" :family "Hasklig"))))
 '(ido-subdir ((t (:foreground "#ffa0a0")))))

(provide-theme 'pink-ocean)
