#ifndef RNAPROCESSOR_H
#define RNAPROCESSOR_H

#include <QThread>
#include <QPoint>
#include <QList>
#include <QImage>
#include "bucket.h"

enum Direction {N, E, S, W};

class RNAprocessor : public QThread
{
Q_OBJECT
public:
    explicit RNAprocessor(QObject *parent = 0);

protected:
    virtual void run();

private:
    Direction dir;
    QPoint position, mark;
    QList<QImage*> bitmaps;
    Bucket bucket;
    QImage *img0;

    void build();
    void move();
    void turnCounterClockwise();
    void turnClockwise();
    void line();
    void tryfill();
    void addBitmap();
    void compose();
    void clip();
    void draw();
    QColor currentPixel();

signals:
    void finish();
    void updateImage(QImage *img);

public slots:
    void startProcessing();
};

#endif // RNAPROCESSOR_H
