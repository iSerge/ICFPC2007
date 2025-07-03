#include "bucket.h"

Bucket::Bucket(QObject *parent) :
    QObject(parent),
    color(0, 0, 0, 255),
    colorChanged(false)
{
}

void Bucket::addColor(uint t){
    transparency.prepend(t);
    colorChanged = true;
}

void Bucket::addColor(QColor c){
    colors.prepend(c);
    colorChanged = true;
}

QColor Bucket::getColor(){
    if (colorChanged){
        uint r = 0, g = 0, b = 0, t = 0;
        QList<uint>::const_iterator ti;
        if (!transparency.isEmpty()){
           for(ti = transparency.begin(); ti != transparency.constEnd(); ++ti){
               t += *ti;
           }
           t /= transparency.length();
       } else {
           t = 255;
       }
       QList<QColor>::const_iterator ci;
       if (!colors.isEmpty()){
          for(ci = colors.begin(); ci != colors.constEnd(); ++ci){
              r += (*ci).red();
              g += (*ci).green();
              b += (*ci).blue();
          }
          r /= colors.length();
          g /= colors.length();
          b /= colors.length();
      } else {
          r = 0; g = 0; b = 0;
      }
       color = QColor(r * t / 255, g * t / 255, b * t / 255, t);
       colorChanged = false;
    }
    return color;
}

void Bucket::clear(){
    transparency.clear();
    colors.clear();
    color = QColor(0, 0, 0, 255);
    colorChanged = false;
}
