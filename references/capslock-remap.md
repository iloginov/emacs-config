Чтобы замапить Ctrl на CapsLock на Ubuntu мне помогло следующее:

~~~{.bash}
gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
~~~
