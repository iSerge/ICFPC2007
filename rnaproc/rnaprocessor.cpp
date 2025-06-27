#include "rnaprocessor.h"
#include <stdio.h>
#include <string.h>
#include <QFile>
#include <QImageWriter>
#include <QColor>

QImage *transparentImage()
{
    QImage *image = new QImage(600, 600, QImage::Format_ARGB32);
    image->fill(qRgba(0, 0, 0, 0));
    return image;
}

RNAprocessor::RNAprocessor(QObject *parent)
    : QThread(parent)
    , dir(E)
    , position(0, 0)
    , mark(0, 0)
    , bitmaps()
    , bucket(this)
{
    img0 = transparentImage();
    bitmaps.append(img0);
}

void RNAprocessor::run() { startProcessing(); }

void RNAprocessor::build()
{
    size_t bytesRead;
    char buf[8];
    buf[7] = 0;
    while (1)
    {
        bytesRead = fread(buf, 1, 7, stdin);
        if (bytesRead < 7)
            break;

        if (strncmp(buf, "PIPIIIC", 7) == 0)
        {
            bucket.addColor(QColor(0, 0, 0, 0)); // black
        }
        if (strncmp(buf, "PIPIIIP", 7) == 0)
        {
            bucket.addColor(QColor(255, 0, 0, 0)); //red
        }
        if (strncmp(buf, "PIPIICC", 7) == 0)
        {
            bucket.addColor(QColor(0, 255, 0, 0)); //green
        }
        if (strncmp(buf, "PIPIICF", 7) == 0)
        {
            bucket.addColor(QColor(255, 255, 0, 0)); //yellow
        }
        if (strncmp(buf, "PIPIICP", 7) == 0)
        {
            bucket.addColor(QColor(0, 0, 255, 0)); //blue
        }
        if (strncmp(buf, "PIPIIFC", 7) == 0)
        {
            bucket.addColor(QColor(255, 0, 255, 0)); //magenta
        }
        if (strncmp(buf, "PIPIIFF", 7) == 0)
        {
            bucket.addColor(QColor(0, 255, 255, 0)); //cyan
        }
        if (strncmp(buf, "PIPIIPC", 7) == 0)
        {
            bucket.addColor(QColor(255, 255, 255, 0)); // white
        }
        if (strncmp(buf, "PIPIIPF", 7) == 0)
        {
            bucket.addColor(0); //tranparent
        }
        if (strncmp(buf, "PIPIIPP", 7) == 0)
        {
            bucket.addColor(255); //opaque
        }
        if (strncmp(buf, "PIIPICP", 7) == 0)
        {
            bucket.clear();
        }
        if (strncmp(buf, "PIIIIIP", 7) == 0)
        {
            move();
        }
        if (strncmp(buf, "PCCCCCP", 7) == 0)
        {
            turnCounterClockwise();
        }
        if (strncmp(buf, "PFFFFFP", 7) == 0)
        {
            turnClockwise();
        }
        if (strncmp(buf, "PCCIFFP", 7) == 0)
        {
            mark = position;
        }
        if (strncmp(buf, "PFFICCP", 7) == 0)
        {
            line();
        }
        if (strncmp(buf, "PIIPIIP", 7) == 0)
        {
            tryfill();
        }
        if (strncmp(buf, "PCCPFFP", 7) == 0)
        {
            addBitmap();
        }
        if (strncmp(buf, "PFFPCCP", 7) == 0)
        {
            compose();
        }
        if (strncmp(buf, "PFFICCF", 7) == 0)
        {
            clip();
        }
        //anything else # do nothing
        //QThread::msleep(1);
    }
}

void RNAprocessor::startProcessing()
{
    build();
    draw();
    emit finish();
}

void RNAprocessor::draw()
{
    QImage *image = bitmaps.takeFirst();

    for (int y = 0; y < 600; y++)
        for (int x = 0; x < 600; x++)
        {
            image->setPixel(x, y, 0xff000000 | image->pixel(x, y));
        }

    //QFile outFile("test.png");
    QFile outFile;
    outFile.open(stdout, QIODevice::WriteOnly);
    QImageWriter writer(&outFile, "PNG");
    if (writer.canWrite())
    {
        writer.write(*image);
    }
    else
    {
        fprintf(stderr, "FATAL: Can't write image data\n");
    }
    outFile.close();

    delete image;
    while (bitmaps.length() != 0)
        delete bitmaps.takeFirst();
}

void RNAprocessor::move()
{
    switch (dir)
    {
    case N:
        position.ry() = (position.y() - 1);
        if (position.y() < 0)
            position.ry() += 600;
        break;
    case E:
        position.rx() = (position.x() + 1);
        if (position.x() > 599)
            position.rx() %= 600;
        break;
    case S:
        position.ry() = (position.y() + 1);
        if (position.y() > 599)
            position.ry() %= 600;
        break;
    case W:
        position.rx() = (position.x() - 1);
        if (position.x() < 0)
            position.rx() += 600;
        break;
    };
}
void RNAprocessor::turnCounterClockwise()
{
    switch (dir)
    {
    case N:
        dir = W;
        break;
    case E:
        dir = N;
        break;
    case S:
        dir = E;
        break;
    case W:
        dir = S;
        break;
    };
}

void RNAprocessor::turnClockwise()
{
    switch (dir)
    {
    case N:
        dir = E;
        break;
    case E:
        dir = S;
        break;
    case S:
        dir = W;
        break;
    case W:
        dir = N;
        break;
    };
}

void setPixel(QImage *img, int x, int y, QColor c) { img->setPixel(x, y, c.rgba()); }

QColor getPixel(QImage *img, int x, int y) { return img->pixel(x, y); }

void RNAprocessor::line()
{
    int deltax = mark.x() - position.x();
    int deltay = mark.y() - position.y();
    int d = qMax(qAbs(deltax), qAbs(deltay));
    int c;
    if ((deltax * deltay) <= 0)
    {
        c = 1;
    }
    else
    {
        c = 0;
    }
    int x = position.x() * d + (d - c) / 2;
    int y = position.y() * d + (d - c) / 2;
    for (int i = 0; i < d; i++)
    {
        setPixel(img0, x / d, y / d, bucket.getColor());
        x += deltax;
        y += deltay;
    }
    setPixel(img0, mark.x(), mark.y(), bucket.getColor());
    updateImage(img0);
}

void RNAprocessor::tryfill()
{
    uint newColor = bucket.getColor().rgba();
    uint oldColor = img0->pixel(position);

    if (newColor == oldColor)
        return;

    QList<QPoint> pbuf;
    QPoint p = position;
    pbuf.append(position);
    while (!pbuf.isEmpty())
    {
        if (img0->pixel(p) == oldColor)
        {
            img0->setPixel(p, newColor);
            if (p.x() > 0)
                pbuf.prepend(QPoint(p.x() - 1, p.y()));
            if (p.x() < 599)
                pbuf.prepend(QPoint(p.x() + 1, p.y()));
            if (p.y() > 0)
                pbuf.prepend(QPoint(p.x(), p.y() - 1));
            if (p.y() < 599)
                pbuf.prepend(QPoint(p.x(), p.y() + 1));
        }
        p = pbuf.takeFirst();
    }
    updateImage(img0);
}

void RNAprocessor::addBitmap()
{
    if (bitmaps.length() < 10)
    {
        img0 = transparentImage();
        bitmaps.prepend(img0);
    }
    updateImage(img0);
}

void RNAprocessor::compose()
{
    if (bitmaps.length() < 2)
        return;

    QImage *image1 = bitmaps.at(1);
    for (int y = 0; y < 600; y++)
    {
        for (int x = 0; x < 600; x++)
        {
            QRgb c0 = img0->pixel(x, y);
            QRgb c1 = image1->pixel(x, y);
            image1->setPixel(x,
                             y,
                             qRgba(qRed(c0) + qRed(c1) * (255 - qAlpha(c0)) / 255,
                                   qGreen(c0) + qGreen(c1) * (255 - qAlpha(c0)) / 255,
                                   qBlue(c0) + qBlue(c1) * (255 - qAlpha(c0)) / 255,
                                   qAlpha(c0) + qAlpha(c1) * (255 - qAlpha(c0)) / 255));
        }
    }
    delete bitmaps.takeFirst();
    bitmaps.pop_front();
    img0 = bitmaps.first();
    updateImage(img0);
}

void RNAprocessor::clip()
{
    if (bitmaps.length() < 2)
        return;

    QImage *image1 = bitmaps.at(1);
    for (int y = 0; y < 600; y++)
    {
        for (int x = 0; x < 600; x++)
        {
            QRgb c0 = img0->pixel(x, y);
            QRgb c1 = image1->pixel(x, y);
            image1->setPixel(x,
                             y,
                             qRgba(qRed(c1) * qAlpha(c0) / 255,
                                   qGreen(c1) * qAlpha(c0) / 255,
                                   qBlue(c1) * qAlpha(c0) / 255,
                                   qAlpha(c1) * qAlpha(c0) / 255));
        }
    }
    delete bitmaps.takeFirst();
    bitmaps.pop_front();
    img0 = bitmaps.first();
    updateImage(img0);
}

QColor RNAprocessor::currentPixel() { return bucket.getColor(); }
