;;; website-publish.el --- Org‑publish setup for my static site -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the code needed to generate my website https://github.com/therealdrdos/simonswebsite comfortable from org files, including rss-feed, dynamic blog list generation and more.
;; Export the site with:
;;   M-x org-publish RET website RET
;;


;;; Code:
(require 'ox-publish)
(require 'ox-rss)
(require 'subr-x) ;; for when-let

(defvar my/site-root (expand-file-name "~/Dokumente/git/simonswebsite/")
  "Root directory of the website project.  Must end with a slash.")

(defun my/site-path (sub)
  "Return absolute path of SUB inside `my/site-root'."
  (expand-file-name sub my/site-root))

;; Global export parameters
(setq org-html-doctype "html5")
(setq org-html-html5-fancy t)
(setq org-html-scripts "")           ; strip all JS snippets that ox-html adds
(setq org-export-with-toc nil)       ; default – override per file via #+OPTIONS

(setq org-html-validation-link nil) ; deactivate org validation link

;; Define the html template (I use an external file)
(defun my/html-template (output backend info)
  "Replace placeholders in template.html and return full HTML page.
OUTPUT  – HTML fragment produced by Org export.
INFO    – plist with export context (titles, descriptions, tags).
BACKEND – What backend is the caller

{{title}}        – #+TITLE of the document
{{description}}  – #+DESCRIPTION (optional)
{{year}}         – current year
{{tags}}         – tag link list (posts only)
{{contents}}     – the exported HTML of the buffer"
  (if (eq backend 'html)
      (let* ((template-file (my/site-path "template.html"))
             (title (org-export-data (plist-get info :title) info))
             (description (or (plist-get info :description) ""))
             (year (format-time-string "%Y"))
             (tags (when-let ((filetags (plist-get info :filetags)))
                     (mapconcat (lambda (tag)
				  (format "<a class=\"tag\" href=\"/blog/blog.html#%s\">#%s</a>" tag tag))
				filetags " "))))
	(with-temp-buffer
	  (insert-file-contents template-file)
	  (goto-char (point-min))
	  (while (re-search-forward "{{\\(title\\|description\\|year\\|tags\\|contents\\)}}" nil t)
            (replace-match
             (pcase (match-string 1)
               ("title" title)
               ("description" description)
               ("year" year)
               ("tags" (or tags ""))
               ("contents" output))))
	  (buffer-string)))
  output))
(setq org-export-filter-final-output-functions '(my/html-template))

;; Sitemap entry formatting for blog index
(defun my/sitemap-entry (entry _style project)
  "Return formatted line for sitemap ENTRY in PROJECT."
  (let* ((base-dir   (or (org-publish-property :base-directory project)
                       default-directory))
         (abs-entry  (expand-file-name entry base-dir))
         (filename   (file-name-nondirectory entry))
         (title      (org-publish-find-title entry project))
         (date       (org-publish-find-date  entry project))
         (description (with-temp-buffer
                        (insert-file-contents abs-entry)
                        (goto-char (point-min))
                        (when (re-search-forward
                               "^#\\+DESCRIPTION:[ \\t]+\\(.*\\)$" nil t)
                          (match-string 1)))))
    (format "%s [[file:%s][%s]] – %s"
            (format-time-string "%Y-%m-%d" date)
            filename title (or description ""))))

;; ANSI color post-processing for txt export
(defun my/add-ansi-colors (filename)
  "Post‑process FILENAME, adding basic ANSI escape codes."
  (with-temp-buffer
    (insert-file-contents filename)
    ;; bold (**text** in ox-ascii output)
    (goto-char (point-min))
    (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
      (replace-match (concat "\e[1m" (match-string 1) "\e[0m") nil t))
    ;; italic (//text//)
    (goto-char (point-min))
    (while (re-search-forward "//\\([^/]+\\)//" nil t)
      (replace-match (concat "\e[3m" (match-string 1) "\e[0m") nil t))
    ;; underline (__text__)
    (goto-char (point-min))
    (while (re-search-forward "__\\([^_]+\\)__" nil t)
      (replace-match (concat "\e[4m" (match-string 1) "\e[0m") nil t))
    (write-region (point-min) (point-max) filename)))

;; Wrapper to include the ansi escape codes
(defun my/org-ascii-publish-with-ansi (plist filename pub-dir)
  "Export to ASCII and inject ANSI codes."
  (let ((outfile (org-ascii-publish-to-ascii plist filename pub-dir)))
    (my/add-ansi-colors outfile)
    outfile))

;; Org-publish project definition
(setq org-publish-project-alist
      `(
        ;; Main static pages (index, aboutme, pgp, blog overview)
        ("pages"
         :base-directory ,(my/site-path "src")
         :base-extension "org"
         :exclude "blog/posts/.*"
         :publishing-directory ,(my/site-path "site")
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author nil)

        ;; Blog posts
        ("posts"
         :base-directory ,(my/site-path "src/blog/posts") ;
         :publishing-directory ,(my/site-path "site/blog/posts")
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author nil
         :section-numbers nil
         :auto-sitemap t
         :sitemap-filename "../blog.org"
         :sitemap-title "Blog"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry my/sitemap-entry)

        ;; RSS feed for posts
        ("rss"
         :base-directory ,(my/site-path "src/blog")
         :publishing-directory ,(my/site-path "site/blog/posts")
         :base-extension "org"
	 :include ("blog.org")
         :publishing-function org-rss-publish-to-rss
         :html-link-home "https://www.dr-dos.org/blog/posts"
         :html-link-use-abs-url t
         :rss-extension "xml"
         :rss-image-url "https://www.dr-dos.org/static/avatar.png"
         :rss-description "Latest blog posts")

        ;; Static assets (CSS, images …)
        ("static"
         :base-directory ,(my/site-path "static")
         :base-extension "css\\|png\\|jpg\\|svg\\|gif\\|webp"
         :publishing-directory ,(my/site-path "site")
         :recursive t
         :publishing-function org-publish-attachment)

        ;; Plain‑text variant for terminal displayability
        ("txt"
         :base-directory ,(my/site-path "src")
         :publishing-directory ,(my/site-path "site/txt")
         :base-extension "org"
         :recursive t
         :publishing-function my/org-ascii-publish-with-ansi
         :body-only t
         :ascii-text-width 80)

        ;; Build everything together
        ("website" :components ("pages" "posts" "rss" "static" "txt"))))


(provide 'website-publish)
;;; website-publish.el ends here
