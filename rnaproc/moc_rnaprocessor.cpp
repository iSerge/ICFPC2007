/****************************************************************************
** Meta object code from reading C++ file 'rnaprocessor.h'
**
** Created: Thu Feb 11 16:15:06 2010
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "rnaprocessor.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'rnaprocessor.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_RNAprocessor[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      14,   13,   13,   13, 0x05,
      27,   23,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
      48,   13,   13,   13, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_RNAprocessor[] = {
    "RNAprocessor\0\0finish()\0img\0"
    "updateImage(QImage*)\0startProcessing()\0"
};

const QMetaObject RNAprocessor::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_RNAprocessor,
      qt_meta_data_RNAprocessor, 0 }
};

const QMetaObject *RNAprocessor::metaObject() const
{
    return &staticMetaObject;
}

void *RNAprocessor::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_RNAprocessor))
        return static_cast<void*>(const_cast< RNAprocessor*>(this));
    return QThread::qt_metacast(_clname);
}

int RNAprocessor::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: finish(); break;
        case 1: updateImage((*reinterpret_cast< QImage*(*)>(_a[1]))); break;
        case 2: startProcessing(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void RNAprocessor::finish()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void RNAprocessor::updateImage(QImage * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
