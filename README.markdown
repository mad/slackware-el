[![www](http://img11.imageshack.us/img11/4056/slackwareel.th.png)](http://img11.imageshack.us/img11/4056/slackwareel.png)

# Intro

 This package work only (?) local mirrors.

If your have get local mirror slackware repo, run this string:

    rsync -P --delete -avzlhH  mirror.yandex.ru::slackware/slackware-current /pub/mirrors/

where `/pub/mirrors` is yout local dir and add here string to crontab.


# Installing

Add to your init file this string:

    (add-to-list 'load-path "/path/to/slackware-changelog/")
    (require 'slackware-changelog)
    (setq slackware-mirror-root "/path/to/mirror/slckware/")

# Using

- `g` - update list
- `u` - update package
- `i` - install pakcage
- `r` - remove package
- `n` - next package
- `p` - previous package

# BUG

- if package begining xf86-video-* or xorg-server-* (maybe other)
  then first package (witch match xf86-video-VERSION-ARCH-BUILD)
  marked conflict

# TODO

- install/remove/update from emacs
- work with remote repo
- show info about other package (not official repo)
- check broken version (e.g. r994599)
