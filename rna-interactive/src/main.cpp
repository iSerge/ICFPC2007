#include <QApplication>

#include "mainview.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QApplication::setQuitOnLastWindowClosed(true);

    MainView mainView;

    mainView.show();

    int retCode = QApplication::exec();

    return retCode;
}
