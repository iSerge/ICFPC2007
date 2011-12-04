#ifndef IMAGEVIEW_H
#define IMAGEVIEW_H

#include <QDialog>
#include <QGraphicsScene>

namespace Ui {
    class ImageView;
}

class ImageView : public QDialog {
    Q_OBJECT
private:
    QGraphicsScene *scene;
public:
    ImageView(QWidget *parent = 0);
    ~ImageView();

protected:
    void changeEvent(QEvent *e);

private:
    Ui::ImageView *ui;

public slots:
    void updateImage(QImage *img);

};

#endif // IMAGEVIEW_H
