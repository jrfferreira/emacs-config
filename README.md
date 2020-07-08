# Emacs config

![Emacs config thumbnail](img/thumb.png)

## How to Setup Emacs 26 on Arch linux

``` shell
pacman -Sy emacs
systemctl --user enable --now emacs
```

## App config

Clone the repo:

``` shell
git clone https://github.com/jrfferreira/emacs-config ~/emacs-config
```

Backup old config if needed:

``` shell

mv ~/.emacs.d ~/.emacs.d.bak
mv ~/.emacs ~/.emacs.bak
```

Link new config:

``` shell

ln -s ~/emacs-config ~/.emacs.d
```

### Main packages

* Magit
* Flycheck
* General
* Helm
* Projectile

### Languages

* Javascript + React + Web
* Python
* Rust

---

# Fixing issues

## Catalina OSX

### User files and folders
>Emacs.app actually launches using a ruby script. As a result, MacOS Catalina uses the permissions set for *ruby*, not the permissions for Emacs.app. Open `General Settings -> Security & Privacy -> Privacy`, select `Full Disk Access` in the left pane, then click `+` and add `/usr/bin/ruby` to resolve your issue.
>`/usr` is hidden by default on MacOS but you can toggle visibility in Finder by using `Shift+Command+Period`

[Stack Exchange issue](https://emacs.stackexchange.com/questions/53026/how-to-restore-file-system-access-in-macos-catalina)
