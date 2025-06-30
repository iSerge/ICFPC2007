#include "rnaprocessor.h"

#include <QBuffer>
#include <QFile>
#include <QImageWriter>
#include <QColor>
#include <QProcess>
#include <QThread>

#include <QDebug>

Image transparentImage()
{
    auto image = Image::create(600, 600, QImage::Format_ARGB32);
    image->fill(qRgba(0, 0, 0, 0));
    return image;
}

RNAprocessor::RNAprocessor(QObject *parent)
    : QObject(parent)
    , dir(E)
    , position(0, 0)
    , mark(0, 0)
    , bitmaps()
    , bucket(this)
    , fileName(QString())
    , prefix(QString())
    , delay(0)
{
    dnaProcessor = QStringLiteral(
        "./DNAProcessor/dist-newstyle/build/x86_64-linux/ghc-9.10.1/DNAProcessor-0.0.1/x/DNAProcessor/build/DNAProcessor/DNAProcessor");

    dnaFile = QStringLiteral("./endo.dna");

    img0 = transparentImage();
    bitmaps.append(img0);
}

void RNAprocessor::setFileName(const QString &fileName) { this->fileName = fileName; }

void RNAprocessor::setPrefix(const QString &prefix) { this->prefix = prefix; }

void RNAprocessor::setDelay(unsigned long delay) { this->delay = delay; }

void RNAprocessor::startProcessing()
{
    QThread *runner = new QThread();

    QProcess *reader = new QProcess();

    if (fileName.isEmpty())
    {
        QStringList args;
        args << dnaFile;
        if (!prefix.isEmpty())
        {
            args << prefix;
        }

        reader->setProgram(dnaProcessor);
        reader->setArguments(args);
    }
    else
    {
        QStringList args;
        args << fileName;

        reader->setProgram(QStringLiteral("cat"));
        reader->setArguments(args);
    }

    reader->moveToThread(runner);

    connect(
        reader,
        &QProcess::readyReadStandardOutput,
        this,
        [this, reader]() {
            this->input.append(reader->readAllStandardOutput());
            this->processInput();
        },
        Qt::QueuedConnection);
    connect(
        reader,
        &QProcess::readyReadStandardError,
        reader,
        [reader, this]() {
            this->progress.append(reader->readAllStandardError());
            while (progress.contains('\n'))
            {
                int i;
                for (i = 0; progress[i] != '\n' && i < progress.length(); ++i)
                {
                }
                QByteArray a = progress.left(i);
                emit updateStatus(QString::fromLocal8Bit(a));
                this->progress.remove(0, i + 1);
            }
        },
        Qt::QueuedConnection);
    connect(runner, &QThread::started, reader, [reader]() { reader->start(); }, Qt::QueuedConnection);
    connect(reader, &QProcess::finished, runner, &QThread::exit, Qt::QueuedConnection);
    connect(
        runner,
        &QThread::finished,
        this,
        [this, runner]() {
            this->processInput();
            this->draw();
            runner->exit(0);
        },
        Qt::QueuedConnection);
    connect(runner, &QThread::finished, runner, &QThread::deleteLater, Qt::QueuedConnection);

    runner->start();
}

void RNAprocessor::draw()
{
    auto image = bitmaps.takeFirst();

    for (int y = 0; y < 600; y++)
    {
        for (int x = 0; x < 600; x++)
        {
            image->setPixel(x, y, 0xff000000 | image->pixel(x, y));
        }
    }

    emit updateImage(image);

    bitmaps.clear();

    emit updateStatus(QStringLiteral("Finished"));
    emit finish();
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

void setPixel(Image img, int x, int y, QColor c) { img->setPixel(x, y, c.rgba()); }

QColor getPixel(Image img, int x, int y) { return img->pixel(x, y); }

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
    emit updateImage(img0);
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
        p = pbuf.takeFirst();
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
    }
    emit updateImage(img0);
}

void RNAprocessor::addBitmap()
{
    if (bitmaps.length() < 10)
    {
        img0 = transparentImage();
        bitmaps.prepend(img0);
    }
    emit updateImage(img0);
}

void RNAprocessor::compose()
{
    if (bitmaps.length() < 2)
        return;

    Image image1 = bitmaps.at(1);
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

    bitmaps.pop_front();
    img0 = bitmaps.front();
    emit updateImage(img0);
}

void RNAprocessor::clip()
{
    if (bitmaps.length() < 2)
        return;

    Image image1 = bitmaps.at(1);
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

    bitmaps.pop_front();
    img0 = bitmaps.first();
    emit updateImage(img0);
}

QColor RNAprocessor::currentPixel() { return bucket.getColor(); }

void RNAprocessor::newStatus(const QString &msg) { emit updateStatus(msg); }

void RNAprocessor::processInput()
{
    while (input.length() >= 7)
    {
        if (input.startsWith("PIPIIIC"))
        {
            bucket.addColor(QColor(0, 0, 0, 0)); // black
        }
        if (input.startsWith("PIPIIIP"))
        {
            bucket.addColor(QColor(255, 0, 0, 0)); //red
        }
        if (input.startsWith("PIPIICC"))
        {
            bucket.addColor(QColor(0, 255, 0, 0)); //green
        }
        if (input.startsWith("PIPIICF"))
        {
            bucket.addColor(QColor(255, 255, 0, 0)); //yellow
        }
        if (input.startsWith("PIPIICP"))
        {
            bucket.addColor(QColor(0, 0, 255, 0)); //blue
        }
        if (input.startsWith("PIPIIFC"))
        {
            bucket.addColor(QColor(255, 0, 255, 0)); //magenta
        }
        if (input.startsWith("PIPIIFF"))
        {
            bucket.addColor(QColor(0, 255, 255, 0)); //cyan
        }
        if (input.startsWith("PIPIIPC"))
        {
            bucket.addColor(QColor(255, 255, 255, 0)); // white
        }
        if (input.startsWith("PIPIIPF"))
        {
            bucket.addColor(0); //tranparent
        }
        if (input.startsWith("PIPIIPP"))
        {
            bucket.addColor(255); //opaque
        }
        if (input.startsWith("PIIPICP"))
        {
            bucket.clear();
        }
        if (input.startsWith("PIIIIIP"))
        {
            move();
        }
        if (input.startsWith("PCCCCCP"))
        {
            turnCounterClockwise();
        }
        if (input.startsWith("PFFFFFP"))
        {
            turnClockwise();
        }
        if (input.startsWith("PCCIFFP"))
        {
            mark = position;
        }
        if (input.startsWith("PFFICCP"))
        {
            line();
        }
        if (input.startsWith("PIIPIIP"))
        {
            tryfill();
        }
        if (input.startsWith("PCCPFFP"))
        {
            addBitmap();
        }
        if (input.startsWith("PFFPCCP"))
        {
            compose();
        }
        if (input.startsWith("PFFICCF"))
        {
            clip();
        }

        input.remove(0, 7);

        //anything else # do nothing
        if (0 != delay)
        {
            QThread::usleep(delay);
        }
    }
}
