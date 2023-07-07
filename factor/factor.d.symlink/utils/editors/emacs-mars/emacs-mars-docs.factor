USING: help help.syntax help.markup ;
IN: editors.emacs-mars

ARTICLE: "editors.emacs-mars" "Integration with Martialism version of GNU Emacs using a new named server"
"Full Emacs integration with Factor requires the use of two executable files -- " { $snippet "emacs" } " and " { $snippet "emacsclient" } ", which act as a client/server pair. The default server is named \"mars\". To reach another server, use the following snippet:"
{ $code "USE: editors.emacs-mars"
        "\"foo\" emacsserver-name set-global"
}
"On Windows, if you install Emacs to " { $snippet "Program Files" } " or " { $snippet "Program Files(x86)" } ", Factor will automatically detect the path to " { $snippet "emacsclient.exe" } ". On Unix systems, make sure that " { $snippet "emacsclient" } " is in your path. To set the path manually, use the following snippet:"
{ $code "USE: editors.emacs-mars"
        "\"/my/crazy/bin/emacsclient\" emacsclient-path set-global"
}

"If you would like a new window to open when you ask Factor to edit an object, put this in your " { $snippet ".emacs" } " file:"
{ $code "(setq server-window 'switch-to-buffer-other-frame)" }

"To quickly scaffold a " { $snippet ".emacs" } " file, run the following code:"
{ $code "USE: tools.scaffold"
    "scaffold-emacs"
}

{ $see-also "editor" }

;

ABOUT: "editors.emacs-mars"
