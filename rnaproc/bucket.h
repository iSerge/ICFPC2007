#ifndef BUCKET_H
#define BUCKET_H

#include <QObject>
#include <QList>
#include <QColor>

class Bucket : public QObject
{
Q_OBJECT
private:
    QList<uint> transparency;
    QList<QColor> colors;
    QColor color;
    bool colorChanged;

public:
    explicit Bucket(QObject *parent = 0);

    void addColor(uint t);
    void addColor(QColor c);
    QColor getColor();
    void clear();

signals:

public slots:

};

#endif // BUCKET_H
