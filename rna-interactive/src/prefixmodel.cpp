#include "prefixmodel.h"

#include <QFile>
#include <QTextStream>
#include <qnamespace.h>

static Prefix ROOT_PREFIX = {0, "", "Discovered prefixes", -1};

QModelIndex PrefixModel::index(int row, int column, const QModelIndex &parent) const
{
    int parent_id = parent.isValid() ? parent.internalId() : ROOT_PREFIX.id;
    return createIndex(row, column, childIds(parent_id).at(row));
}

QModelIndex PrefixModel::parent(const QModelIndex &child) const
{
    int id = child.internalId();
    int parent_id = prefixById(id).parent;
    return indexById(parent_id);
}

int PrefixModel::rowCount(const QModelIndex &parent) const
{
    qsizetype parent_id = parent.isValid() ? parent.internalId() : ROOT_PREFIX.id;
    return childIds(parent_id).size();
}

int PrefixModel::columnCount(const QModelIndex &parent) const
{
    Q_UNUSED(parent);
    return 2;
}

QVariant PrefixModel::data(const QModelIndex &index, int role) const
{
    if (role == Qt::DisplayRole)
    {
        auto &r = prefixById(index.internalId());
        switch (index.column())
        {
        case 0:
            return r.description;
        case 1:
            return r.prefix;
        default:
            return QVariant();
        }
    }
    else if (role == Qt::UserRole)
    {
        auto &r = prefixById(index.internalId());
        return r.prefix;
    }

    return QVariant();
}

QModelIndex PrefixModel::indexById(int id) const
{
    if (id == 0)
        return QModelIndex();
    qsizetype parent_id = prefixById(id).parent;
    qsizetype row = childIds(parent_id).indexOf(id);
    return index(row, 0, indexById(parent_id));
}

void PrefixModel::loadData()
{
    prefixes.push_back(ROOT_PREFIX);

    QFile prefixData(QStringLiteral("HintPrefix.txt"));

    if (prefixData.open(QIODevice::ReadOnly))
    {
        QTextStream in(&prefixData);
        qsizetype header_id = 0;
        while (!in.atEnd())
        {
            QString line = in.readLine();
            if (line.startsWith(QStringLiteral("--")))
            {
                header_id = prefixes.size();
                Prefix prefix = {header_id, "", line.sliced(3), ROOT_PREFIX.id};
                prefixes.push_back(prefix);
            }
            else
            {
                auto prefixDescr = line.split(QStringLiteral(" -- "));
                Prefix prefix = {prefixes.size(), prefixDescr[0], prefixDescr[1], header_id};
                prefixes.push_back(prefix);
            }
        }
    }

    prefixData.close();
}

QVector<qsizetype> PrefixModel::childIds(qsizetype parent_id) const
{
    QVector<qsizetype> ret;
    for (auto &r : prefixes)
        if (r.parent == parent_id)
            ret.push_back(r.id);
    return ret;
}

const Prefix &PrefixModel::prefixById(qsizetype id) const
{
    if (id < 0)
    {
        return ROOT_PREFIX;
    }

    for (auto &r : prefixes)
        if (r.id == id)
            return r;

    return ROOT_PREFIX;
}
