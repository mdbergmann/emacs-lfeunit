# emacs-lfeunit

This is a Emacs minor mode implementation to run LFE (ltest) tests using rebar3.

Tests are actually run with `rebar3 as test lfe ltest -s 'the suite'`, so only tests of the current module are run.
This is by design to allow a fast TDD workflow.

There is no package on Elpa or Melpa.
To install it clone this to some local folder and initialize like this in Emacs:

```
(use-package lfeunit
  :load-path "~/.emacs.d/plugins/lfeunit")
```

The default key binding is `C-c C-t`.

To configure a custom key binding do this:

```
(use-package lfeunit
  :load-path "~/.emacs.d/plugins/lfeunit"
  :bind (:map lfeunit-mode-map
              ("C-c t" . lfeunit-run))
  :commands
  (lfeunit-mode))
```

When done you have a minor mode called `lfeunit-mode`.

This mode can be enabled for basically every buffer but only `lfe-mode` buffers are supported.
On other code or project it just saves the buffer.

The key sequence: `C-c C-t` (or a custom defined one) will first save the buffer and then run the tests using `rebar3`.

After the first execution of `lfeunit-run` you can view the "*LFEUnit output*" buffer for test output.
