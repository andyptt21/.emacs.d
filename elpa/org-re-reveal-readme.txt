This package provides Org export functionality to generate HTML
presentations with the presentation framework reveal.js.

Quickstart:
0. Install reveal.js: https://revealjs.com/
1. Activate org-re-reveal.
   (a) Place this directory into your load path or install it from MELPA
       (https://melpa.org/#/getting-started).
   (b) Load package manually ("M-x load-library" followed by
       "org-re-reveal") or place "(require 'org-re-reveal)" into your
       ~/.emacs and restart.
2. Load an Org file and export it to HTML.
   (a) Make sure that reveal.js is available in your current directory
       (e.g., as sub-directory or symbolic link).
   (b) Load "Readme.org" (coming with org-re-reveal).
   (c) Export to HTML: Press "C-c C-e r r" (write HTML file) or
       "C-c C-e r b" (write HTML file and open in browser)
See "Readme.org" for introduction and details.

Note that emacs-reveal offers a project that embeds org-re-reveal,
reveal.js, and various reveal.js plugins:
https://gitlab.com/oer/emacs-reveal
Its howto, generated from Org source file in GitLab CI environment:
https://oer.gitlab.io/emacs-reveal-howto/howto.html

The package org-re-reveal grew out of a forked version of org-reveal
whose development seems to have stalled:
https://github.com/yjwen/org-reveal/issues/342
