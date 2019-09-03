# md5-converter

## Description

Convert md5mesh and md5anim formats from version 6 to version 10.

## Build

`sbt clean compile`

## Usage

This is still early in development but jars are on the way. For now:

`sbt run <path to input md5mesh> <path to output md5mesh>`

## Notice

The rotation matrix to quaternion conversion function in [Quaternion.scala](src/main/scala/com/mjsonofharry/md5model/math/Quaternion.scala) is a port of the [NiPy implementation](https://github.com/nipy/nibabel/blob/master/nibabel/quaternions.py).

The entirety of [Eig.scala](src/main/scala/com/mjsonofharry/md5model/math/Eig.scala) is modified from [Breeze](https://github.com/scalanlp/breeze/blob/master/math/src/main/scala/breeze/linalg/functions/eig.scala).

## Reference

[MD5Mesh and MD5Anim files formats](http://tfc.duke.free.fr/coding/md5-specs-en.html)

[Unofficial DoomIII model specs v0.1](https://www.doomworld.com/forum/topic/57897-alpha-shotgun-mod/?page=4&tab=comments#comment-1581404)