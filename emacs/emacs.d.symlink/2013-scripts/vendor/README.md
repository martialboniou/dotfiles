My site lisp
============

Main files
----------

* `update-auto-loads.el`
* *third-party* directory
* *third-party/bookmark-plus* directory

Personal scripts
----------------

In *third-party*:

* `mars-tiling.el`
* `mars-windows-archiver.el`
* `revive+.el`

And all `compile*.sh` files you may need to compile the current directory.

Required packages
-----------------

### Auto-install

Additional packages **are auto-installed in the first directory of `mars/site-lisp-path` according to a simple command line scripting**. This destination should be `$HOME/.emacs.d/vendor` by default. If you experience problems with a package, you may replace it:

1. install the replacement package by hand in `~/.emacs.d/vendor`;
2. add it to your `load-path`;
3. remove the installer line of the previous version of the required package in `mars/site-lisp-packages` in `lisp/packs.el`;
4. update autoloads (it should be made at startup if `loaddefs.el` isn't found in `~/.emacs.d/vendor`);
5. (optional) if the install process is simple and doable on the command line, add a line in `mars/site-lisp-packages` to check/build the new package in another environment.

Otherwise, you may want to simply update the erratic package by hand; for example, by relaunching the versioning tool via `git pull`, `svn up` or another command, and by byte-recompiling with `make`, `rake` or the included program `emacs-compile-directory`. A simple trick consists in removing the package directory and call interactively `mars/renew-site-lisp`. This last command will populate `~/.emacs.d/vendor` by:

1. checking if a required package is present;
2. install the required package if not found;
3. renew `load-path` AND FORCE TO GENERATE additional `autoloads` if newly installed.

In this case, the offending package will be re-installed and post-processed.

A `ELPA` installer process is planned as Emacs 24.1 seems the way to go. `pases` were used to manage the packages `wanderlust` and `org` but it is deprecated as the git/cvs version may be easily installed by the `packs` functions. Anyway, `pases` proposed some hints to manage multiple major versions of **GNU Emacs** or **XEmacs** which will be merged (I hope) in `ELPA`.

### Tagging conventions

Add:
1. `.cedet` in the directory where the files and those ones in its subdirectories need a `cedet-autoload` (including `defclass`) instead of the standard *autoload* (normally, `cedet` and `ecb` directories should have one);
2. `.noauto` in the directory where the files and those ones in its subdirectories should not be seen via the `load-path` and should not have their *autoloads* generated and added to the `.emacs.d/lisp/loaddefs.el` file (here, `ecb`'s *autoloads* are loaded from the Emacs configuration so we don't have to visit the `ecb` directory);
3. `.nosearch` in the directory where the files and those ones in its subdirectories should not be seen via the `load-path`

Those tag-files are automatically added during the packages' auto-install.
