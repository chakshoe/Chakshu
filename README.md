
 <!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/chakshoe/chakshu">

  </a>

  <h3 align="center">chakshu</h3>

  <p align="center">
    A Mathematica Bank for VCE Maths Methods.
    <br />
    <br />
    <br />
  </p>
</div>


## About

This is all of the Mathematica commands that I have made and curated over the last 2 years of doing VCE Maths Methods.

## Getting Started

### Loading this module into your local install

Go to the location that $UserBaseDirectory points to. For Windows, this a subfolder of the Roaming directory, for Mac systems it is `~/Library/Mathematica` Navigate to the Applications directory, and then clone the repository into that folder.


You may then load Chakshu by running the following:

```
Needs["Chakshu`"]
```

This will load in the library.

### Alternative

You can also load Chakshu by running the following (also if it crashes)
```
<<Chakshu`
```

### Paclet Compilation

Run `CreatePacletArchive[source,dir]` where `source` is the location of Chakshu, and `dir` is the directory you want the paclet to be made in. 



### Step by Step Guide

To see the step by step guide, open the Paclet Installation.nb file and read through it to get an in detail way of creating and running the Paclet
