# edit-color-stamp
_Edit hex color stamps using a QT, or the internal color picker._

![screenshot](https://github.com/sabof/edit-color-stamp/raw/master/screenshot.png)

#### Rainbow mode integration
This mode can edit any color representation `rainbow-mode` can recognize. A limitation being that it will always write in #FFFFFF format.

#### To use the QT color picker:

Go to the qt\_color\_picker directory

$ qmake qt\_color\_picker.pro

$ make

Move the resulting qt\_color\_picker file somewher in you path.
Alternatively change the value of es-color-qt-picker-exec to the location of the executable.
