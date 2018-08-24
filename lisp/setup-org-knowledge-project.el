
(setq org-agenda-files (directory-files-recursively "E:/opt/knowledges/" "\.org$"))

(setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))

(setq org-publish-project-alist
      '(("knowledges-pages"
         :base-directory "E:/opt/knowledges/"
         :publishing-directory "E:/opt/knowledges/output/"
         :publishing-function org-html-publish-to-html
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/htmlize.css\"/>
<link rel=\"stylesheet\" type=\"text/css\" href=\"http://www.pirilampo.org/styles/readtheorg/css/readtheorg.css\"/>
<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js\"></script>
<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>
<script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/lib/js/jquery.stickytableheaders.js\"></script>
<script type=\"text/javascript\" src=\"http://www.pirilampo.org/styles/readtheorg/js/readtheorg.js\"></script>"
         :recursive t
         :auto-sitemap t)
        ("knowledges-images"
         :base-directory "E:/opt/knowledges/generated/"
         :base-extension "png\\|jpg\\|jpeg\\|bmp"
         :publishing-directory "E:/opt/knowledges/output/generated/"
         :publishing-function org-publish-attachment)
        ("knowledges" :components ("knowledges-pages" "knowledges-images"))))
