# emacs-asana

## Getting Started

Add this to your `.bash_profile` (or wherever you keep your environment):

```bash
# Get a Personal Access Token from the `apps' tab in your app.asana.com profile settings.
export ASANA_TOKEN="<my-asana-personal-access-token>" 
```

Add this to your `init.el`:
```elisp
(setq asana-mode t)
```

### Configuration

[Optional] The default prefix for `asana-mode` commands is `C-c a`. To change it, add this to your `init.el`:
```elisp
(setq asana-keymap-prefix "C-c C-a") ; Or whatever you'd like to use as your prefix
```

### Available commands

In the `asana-mode` minor mode, the following interactive commands are available:

```elisp
helm-asana [C-c a a]
helm-asana-change-workspace [C-c a A]

asana-create-task-quickly [C-c a c]
asana-create-task [C-c a C]
```
