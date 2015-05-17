yandex-weather.el v.0.3.
==============================================

# Fetch Yandex Weather forecasts

![Screenshot](screenshot.png?raw=true)

## How to install

- Copy project files in your .emacs.d.
- Add this lines in your emacs config:

```
(load-file "~/.emacs.d/yandex-weather.el")
(load-file "~/.emacs.d/org-yandex-weather.el")
```

- Add this line in your agenda's org file.

`%%(org-yandex-weather "27612")`

Where '27612' is ID of your city [from](http://weather.yandex.ru/static/cities.xml).

- You can also use MELPA for the installation.
```
M-x package-install RET yandex-weather
```

The scripts are distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY! See license.
