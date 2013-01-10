#include <QtGui/QApplication>
#include <stdio.h>
#include <iostream>
#include <QColorDialog>

int main(int argc, char *argv[]) {
    QApplication a(argc, argv);
    QStringList arguments = a.arguments();
    QColorDialog w;
    QColor result_color;

    if ( arguments.size() >= 4 ) {
        int colors[3];
        for (int i = 1; i <= 3; i++) {
            QByteArray ba = arguments[i].toLocal8Bit();
            char * bp = ba.data();
            colors[i - 1] = atoi(bp);
        }

        QColor source_color(colors[0], colors[1], colors[2]);
        result_color = w.getColor(source_color);
    } else {
        result_color = w.getColor();
    }
    if ( result_color.isValid() ) {
        printf("NEW_COLOR %d %d %d\n",
               result_color.red(),
               result_color.green(),
               result_color.blue());
    }
    return EXIT_SUCCESS;
}
