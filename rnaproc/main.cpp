#include <QtCore/QCoreApplication>
//#include <QApplication>
#include "rnaprocessor.h"
#include "imageview.h"

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    //QApplication a(argc, argv);
    RNAprocessor proc(&a);
    //ImageView v(0);

    QObject::connect(&proc, SIGNAL(finish()), &a, SLOT(quit()));
    //QObject::connect(&proc, SIGNAL(finish()), &v, SLOT(close()));
    //QObject::connect(&proc, SIGNAL(updateImage(QImage*)), &v, SLOT(updateImage(QImage*)));


    //v.show();
    proc.start();
    return a.exec();
}
