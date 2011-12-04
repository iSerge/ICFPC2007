#include "imageview.h"
#include "ui_imageview.h"
#include <QMessageBox>

ImageView::ImageView(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::ImageView)
{
    ui->setupUi(this);
    scene = new QGraphicsScene(0,0,600, 600, this);
    scene->setBackgroundBrush(Qt::SolidPattern);
    ui->graphicsView->setScene(scene);
}

ImageView::~ImageView()
{
    delete ui;
    delete scene;
}

void ImageView::changeEvent(QEvent *e)
{
    QDialog::changeEvent(e);
    switch (e->type()) {
    case QEvent::LanguageChange:
        ui->retranslateUi(this);
        break;
    default:
        break;
    }
}

void ImageView::updateImage(QImage *img){
    scene->clear();
    scene->addPixmap(QPixmap::fromImage(*img));
    ui->graphicsView->update();
    //QMessageBox::warning(this, tr("imgUpdate"), tr("Image being updated"));
}
