


Installation
============

All users
---------

As packages will be fetched on the Internet then installed and configured, you only need to byte-compile the ~/.emacs.d/vendor directory and the available subdirectories by hand. **First, add the shell script** `emacs-compile-directory` **to your shell path** (and probably also `emacs-compile-file`). This file is used by the script running in `$HOME/.emacs.d/lisp/packs.el` to install missing packages *(for instance)*. To get good performance with byte-compilation, type the following command in any virtual terminal running *Bourne-compatible* shell like `bash` or `zsh`:

    $ cd ~/.emacs.d/vendor/third-party
    $ emacs-compile-directory
    $ emacs-compile-directory bookmark-plus

You don't need to do this before running your emacs for the first time.

Modern UN*X/BSD users
---------------------

Ensure you installed the following packages via `homebrew`/`macports` on OS X or your favorite `deb`/`rpm` package manager:

* `cvs`
* `svn`
* `git`
* `autoconf`
* `curl`
* `python 2.x (should be 2.7+)`
* `pip` or at least `setuptools` (to get python simple egg installer named `easy_install`)
* `mplayer`
* `wget` (used by `newsticker`)
* `gnutls-cli`

On most systems, you should have:

* `sed`
* `awk`
* `make`
* `bash`

In order to install `ruby` I firmly recommend to get it via `rvm`:

    $ bash < <( curl http://rvm.beginrescueend.com/releases/rvm-install-head )
    $ source "$HOME/.rvm/scripts/rvm"
    $ rvm install ree,1.9.2-head,jruby

You should add `[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"` to your `.bashrc` or `.zshrc`. If you use the complete `Dots` packages, the `Dots/.zsh/env/03-rvm.zsh` creates a good path for you for your `zsh` configuration. **ATTENTION** using `rvm` on `zsh` requires `zsh 4.3.10+` (all scripts are tested using a *4.3.11* MacPorts `zsh-devel` install with `mp_completion` and `pcre` options).

To install `haskell`, I don't recommend you to install the `ghc` package and try to build everything by hand: you should install [haskell-plateform](http://sporkcode.wordpress.com/2009/07/11/installing-the-haskell-platform-in-ubuntu/) instead.

Now you can install:

* *Bazaar* and *Mercurial* (`hg` command) via `python` manager:

    $ pip install bzr; pip install mercurial

* `darcs` distributed RCS via `cabal`, the `haskell` package manager:

    $ cabal update; cabal install darcs

* OPTIONAL: `chit`, as small reference card to keep an eye on your favorite cheatsheets, via `rubygem`:

    $ gem install chit

### Mac OS X tip

In order to get perfect colors everywhere, please compile your Emacs 23.2+ with the following `configure` options:

    $ ./configure --without-dbus --with-ns --disable-ns-self-contained

after having replaced all references (methods/variables) from `*Calibrated*` to `*Device*` (especially the method `#colorWithCalibratedRed:green:blue:alpha:`) in the `src/nsterm.m` file.

Windows users
-------------

You will need *MSYS* and/or *MinGW* and the following program:
``touch`, bash`, `tar`, `gzip`, `autoconf`, `make`, `svn`, `git`, `cvs`, `darcs`, `curl`, `wget`, `rake`/`ruby`, `python` (including `pip` or `easy_install`). You should install *Haskell plateform* (useful to get `darcs` via `cabal`, the Haskell package manager), `bzr`, `hg`, `mplayer` (to use `emms`) and/or `mpd`. For an easy installation, please, follow these instructions:

* install [mingw-get-inst](http://sourceforge.net/projects/mingw/files/Automated%20MinGW%20Installer/mingw-get-inst/). By default, the installer provides you with `bash`, `gcc`, `sed`, `awk` and a lot of UN*X tools like `tar`:
* open a *MinGW Shell* terminal and type:

    $ mingw-get update
    $ mingw-get install msys-system-builder msys-unzip msys-cvs msys-gmp msys-wget msys-openssl

* OPTIONAL: you may install `msys-man`, `msys-flex`, `msys-patch` too;
* ensure `bash` is available in *Command Prompt* (*ie* `cmd.exe`);
* install [ruby](http://rubyforge.org/projects/rubyinstaller/) (`rvm` doesn't seem to work on Windows even if you got `bash` installed);
* OPTIONAL: when ruby is installed, you may install a small reference card to keep an eye on your favorite cheatsheets named `chit`:
    $ gem install chit
* install [svn](http://subversion.tigris.org/servlets/ProjectDocumentList?folderID=91&expandFolder=91&folderID=74);
* install [git](http://code.google.com/p/msysgit/downloads/list);
* install [haskell](http://hackage.haskell.org/platform/windows.html).

To install `python`, this is the recommended way:

* install the current production version in 2.X version [python](http://www.python.org/download/releases/);
* create a `%PYTHONPATH%` environment variable and set `C:\Python2X\Lib` and `C:\Python2X\Lib\site-packages`;
* add `C:\Python2X` to your local or global `%PATH%`;
* fetch the [setuptools](http://pypi.python.org/pypi/setuptools#downloads) for `win32` for the **Py Version** matching your `python` version number in 2.X;
* add `C:\Python2X\Lib\site-packages\setuptools` to your local or global `%PATH%`.
* (optional but recommended) `git clone git://github.com/pypa/pip.git`

You can now install `bzr` and `hg` (*mercurial* egg) via `easy_install` (or `pip`; in this case, replace `easy_install` by `pip install` in the following command line):

    $ easy_install bzr
    $ easy_install mercurial

After the `haskell` installation, you should get the `cabal` package manager. You should use it to build the patch-based RCS named `darcs`. You may follow the `darcs` wiki's instructions  <http://wiki.darcs.net/BuildingUnderWindows>, by  installing  [OpenSSL (32-bit)](http://www.slproweb.com/products/Win32OpenSSL.html) (not the *Light* version), executing on the *MinGW Shell* command line:
    $ cd /c/OpenSSL-Win32/lib/MinGW
    $ cp libeay32.a libcrypto.a
    $ cp ssleay32.a libssl.a
and then, fetching the [libcurl](http://curl.haxx.se/latest.cgi?curl=win32-ssl-devel-msvc) source code (notice that a `curl.exe` is installed when you install `git`) and install it via the next command:
    $ ./configure --with-ssl=/c/OpenSSL-Win32; make; make install
I prefer a simpler way by downloading one of the [`libcurl` *MinGW* builds](http://haskell.forkio.com/Home/curl-win32/curl-7.19.4-mingw32.zip?attredirects=0&d=1) after the OpenSSL installation. Say you unzipped `curl-7.19.4` in a `Code\src` subdirectory in  `"My Documents"` directory. Install `darcs` via your *MinGW32* terminal:
    $ cd $HOME/"My Documents"/Code/src/curl-7.19.4
    $ cabal install --extra-lib-dirs="`echo $PWD/lib`" --extra-include-dirs="`echo $PWD/include`"

Finally, you'll need to install [w3m]().
You may:
* Grab [it](http://www.daionet.gr.jp/~knok/software/misc/w3m.exe) and copy it to `/usr/bin` in the *MinGW shell*.
* Fetch the [Garbage collection for C++](http://sourceforge.net/projects/libgc/) library and unzip it in a **new directory** (the archive is not inside a subdirectory);

Note: Some additional libraries/binaries contained in [*GnuWin*](http://gnuwin32.sourceforge.net/) may be useful for some developers.

Your private data
-----------------

By default, the configuration is fine enough for an Emacs to get ready
for work but you'll probably setup your personal Emacs during your
sessions. **Four important places** are:

* `.emacs.d/data/Customize/` directory used to write custom files
  out. The custom file matches the following pattern:
  `<emacs-name>-<emacs-major-version>-<system-type>.el`. For example, on
  Mac OS X, you may find a `gnuemacs-23-darwin.el`. This file is
  managed by `Customize`. **Don't edit it. Use `M-x customize` instead**;
* `.emacs.d/lisp/init/` directory used for special advanced setup for
  `viper-mode` and `wanderlust` (a great IMAP-based MUA). If you want
  to create your own `wanderlust` setting, start by copying the
  `default.wl.el` to `<username>.wl.el` where `<username>` is your
  username on your system then hack it. If no
  `<username>.<some-settings>.el` is found, the
  `default.<some-settings>.el` is loaded instead. You probably don't
  need to create a `<username>.viper.el` as the `default.viper.el` is
  good enough. **IMPORTANT: if you don't like Vi modal edition, you
  may deactivate **`viper-mode`** by setting **`*i-am-a-vim-user*`
  **variable defined in** `.emacs` **to **`nil` in `.emacs` or in your
  `.emacs.d/lisp/vars.el` after the *FIRST TIME* part (you may
  destroy the `.emacs.d/data/.launched` file too);
* `.emacs.d/data/sys/` directory containing the
  `vars-<system-name>-<system-type>.el` files. You may create such a
  file by hand to add variable definitions using `setq` or
  `setq-default` in the case of a specific machine and its operating
  system; those variables not being specific enough to be set in
  `Customize` and bound to a special hardware/OS not to be the same as
  in the `.emacs.d/lisp/init/common-pre-custom.el`.
* `.emacs.d/data/wl/` directory containing the *folder* definition
  used by the current `wanderlust` configuration. The files should
  match `<username>.folders`; you may copy `default.folders` to create
  your own setting and edit it with the `wanderlust` interactive
  commands in the `*Folders*` buffer.

So, to backup your private settings generated by `Customize`, handmade
`wanderlust` or `viper` configuration or `data/sys` specific
variables, attach those directories/files to your favorite backup
script.
