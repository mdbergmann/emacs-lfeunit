# emacs-lfeunit

This is a Emacs minor mode implementation to run LFE (ltest) tests using rebar3.

There is no package on Elpa or Melpa.
To install it clone this to some local folder and initialize like this in Emacs:

```
(use-package lfeunit
  :load-path "~/.emacs.d/plugins/lfeunit")
```

To configure a custom key binding do this:

```
(use-package lfeunit
  :load-path "~/.emacs.d/plugins/lfeunit"
  :bind (:map lfeunit-mode-map
              ("C-c t" . lfeunit-run-all))
  :commands
  (lfeunit-mode))
```

When done you have a minor mode called `lfeunit-mode`.

This mode can be enabled for basically every buffer but only `lfe-mode` buffers are supported.
On other code or project it just saves the buffer.

There are three general modes to run tests:

- all tests in the module: `C-c C-t`
- just a single test (is determined from the cursor position): `C-c C-s`
- repeat the last test: `C-c C-r`

After the first execution of `lfeunit-run` you can view the "*LFEUnit output*" buffer for test output.

Optionally specify a base custom command using `lfeunit-custom-cmd`, i.e.: `rebar3 eunit`.
