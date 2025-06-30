#ifndef MAINVIEW_H
#define MAINVIEW_H

#include <QDialog>

#include "rnaprocessor.h"

namespace Ui
{
class MainView;
}

class MainView : public QDialog
{
    Q_OBJECT

public:
    explicit MainView(QWidget *parent = nullptr);
    ~MainView();

public slots:
    void open();
    void dataFromRna();
    void updateImage(Image img);
    void updateIteration(const QString &msg);

private:
    Ui::MainView *ui;

    bool loadFile(const QString &);
    void startProcessor(RNAprocessor *processor);
    void enableButtons(bool enable);
};

#endif // MAINVIEW_H
