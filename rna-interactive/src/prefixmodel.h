#pragma once

#include <QAbstractItemModel>
#include <QList>

struct Prefix
{
    qsizetype id;
    QString prefix;
    QString description;
    qsizetype parent;
};

class PrefixModel : public QAbstractItemModel
{
    Q_OBJECT
public:
    explicit PrefixModel(QObject *parent = nullptr)
        : QAbstractItemModel(parent)
    {
        loadData();
    }

    ~PrefixModel() { }

    QModelIndex index(int row, int column, const QModelIndex &parent = QModelIndex()) const;
    QModelIndex parent(const QModelIndex &child) const;
    int rowCount(const QModelIndex &parent = QModelIndex()) const;
    int columnCount(const QModelIndex &parent = QModelIndex()) const;
    QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

protected:
    QModelIndex indexById(int id) const;

private:
    QList<Prefix> prefixes;

    void loadData();
    QVector<qsizetype> childIds(qsizetype parent_id) const;
    const Prefix &prefixById(qsizetype id) const;
};
