#include "mainview.h"
#include "ui_mainview.h"

#include <QFileDialog>
#include <QImageReader>
#include <QImageWriter>
#include <QMessageBox>
#include <QStandardPaths>
#include <QThread>

#include <QDebug>

#include "rnaprocessor.h"
#include "prefixmodel.h"

MainView::MainView(QWidget *parent)
    : QDialog(parent)
    , ui(new Ui::MainView)
{
    ui->setupUi(this);

    PrefixModel *prefixes = new PrefixModel();

    ui->prefixView->setModel(prefixes);

    ui->renderView->setBackgroundRole(QPalette::Base);

    qRegisterMetaType<Image>();

    connect(ui->renderBtn, &QPushButton::clicked, this, &MainView::open);
    connect(ui->processDnaBtn, &QPushButton::clicked, this, &MainView::dataFromRna);
}

MainView::~MainView() { delete ui; }

static void initializeImageFileDialog(QFileDialog &dialog, QFileDialog::AcceptMode acceptMode)
{
    static bool firstDialog = true;

    if (firstDialog)
    {
        firstDialog = false;
        //const QStringList picturesLocations = QStandardPaths::standardLocations(QStandardPaths::PicturesLocation);
        // dialog.setDirectory(picturesLocations.isEmpty() ? QDir::currentPath() : picturesLocations.last());
        dialog.setDirectory(QDir::currentPath());
    }

    // QStringList mimeTypeFilters;
    // const QByteArrayList supportedMimeTypes =
    //     acceptMode == QFileDialog::AcceptOpen ? QImageReader::supportedMimeTypes() : QImageWriter::supportedMimeTypes();
    // for (const QByteArray &mimeTypeName : supportedMimeTypes)
    //     mimeTypeFilters.append(mimeTypeName);
    // mimeTypeFilters.sort();
    // dialog.setMimeTypeFilters(mimeTypeFilters);
    // dialog.selectMimeTypeFilter(QStringLiteral("image/png"));
    dialog.setAcceptMode(acceptMode);
    if (acceptMode == QFileDialog::AcceptSave)
        dialog.setDefaultSuffix(QStringLiteral("png"));
}

void MainView::open()
{
    QFileDialog dialog(this, tr("Open File"));
    initializeImageFileDialog(dialog, QFileDialog::AcceptOpen);

    while (dialog.exec() == QDialog::Accepted && !loadFile(dialog.selectedFiles().constFirst()))
    {
    }
}

void MainView::dataFromRna()
{
    auto processor = new RNAprocessor();
    processor->setPrefix(QString());

    auto rows = ui->prefixView->selectionModel()->selectedRows();
    if (rows.size() > 0)
    {
        QModelIndex idx = rows.at(0);
        qDebug() << QStringLiteral("Trying to add prefix row: %1, col: %2").arg(idx.row()).arg(idx.column());
        QString prefix = ui->prefixView->model()->data(idx, Qt::UserRole).toString();
        qDebug() << QStringLiteral("  found prefix: %1").arg(prefix);
        if (!prefix.isEmpty())
        {
            processor->setPrefix(prefix);
        }
    }

    startProcessor(processor);
}

void MainView::updateImage(Image img)
{
    auto newImage = QPixmap::fromImage(*img);
    ui->renderView->setPixmap(newImage);
}

void MainView::startProcessor(RNAprocessor *processor)
{
    enableButtons(false);
    if (ui->msecBtn->isChecked())
    {
        processor->setDelay(1000);
    }
    else if (ui->usecBtn->isChecked())
    {
        processor->setDelay(1);
    }

    QThread *runner = new QThread();
    processor->moveToThread(runner);

    connect(processor, &RNAprocessor::updateImage, this, &MainView::updateImage, Qt::QueuedConnection);
    connect(processor, &RNAprocessor::updateStatus, this, &MainView::updateIteration, Qt::QueuedConnection);
    connect(runner, &QThread::started, processor, &RNAprocessor::startProcessing, Qt::QueuedConnection);
    connect(
        processor,
        &RNAprocessor::finish,
        this,
        [this]() {
            qDebug() << "MainView: RNA processor finished";
            this->enableButtons(true);
        },
        Qt::QueuedConnection);
    connect(runner, &QThread::finished, runner, &QThread::deleteLater, Qt::QueuedConnection);

    runner->start();
}

bool MainView::loadFile(const QString &fileName)
{
    auto processor = new RNAprocessor();
    processor->setFileName(fileName);

    startProcessor(processor);

    return true;
}

void MainView::updateIteration(const QString &msg) { ui->iterationLbl->setText(msg); }

void MainView::enableButtons(bool enable)
{
    ui->renderBtn->setEnabled(enable);
    ui->processDnaBtn->setEnabled(enable);
}
