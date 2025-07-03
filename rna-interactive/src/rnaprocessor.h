#ifndef RNAPROCESSOR_H
#define RNAPROCESSOR_H

#include <QByteArray>
#include <QPoint>
#include <QList>
#include <QImage>
#include <QSharedPointer>

#include "bucket.h"

enum Direction
{
    N,
    E,
    S,
    W
};

using Image = QSharedPointer<QImage>;

Q_DECLARE_METATYPE(Image);

class RNAprocessor : public QObject
{
    Q_OBJECT
public:
    explicit RNAprocessor(QObject *parent = 0);
    void setFileName(const QString &fileName);
    void setPrefix(const QString &preifx);
    void setDelay(unsigned long delay);

private:
    QString dnaProcessor;
    QString dnaFile;

    Direction dir;
    QPoint position, mark;
    QList<Image> bitmaps;
    Bucket bucket;
    Image img0;
    QString fileName;
    QString prefix;
    unsigned long delay;
    QByteArray input;
    QByteArray progress;

    void move();
    void turnCounterClockwise();
    void turnClockwise();
    void line();
    void tryfill();
    void addBitmap();
    void compose();
    void clip();
    QColor currentPixel();

signals:
    void finish();
    void updateImage(Image img);
    void updateStatus(const QString &msg);

public slots:
    void draw();
    void startProcessing();
    void newStatus(const QString &msg);
    void processInput();
};

#endif // RNAPROCESSOR_H
