# Unicode Chart Generator

This is a chart generator which reads in some text from standard input and outputs a chart to standard output.

Usage is as follows:
```
chart [-h] [-d delimiter]... [FILE]
```
The most important aspect here is the `[-d delimiter]...` section. This specifies what string should be used to
split the input into discrete elements. Any number of delimiters may be used. For example, 
```
chart -d "," -d ";" file.txt
```
assuming file.txt contains this content:
```
MICE,DOUBLOONS,RASCALS,COMPASSION,COLD WAR,OBOE,Y/N,RESULT
2 BLIND,GOLD,LITTLE;ROBOT LOVE,HOT WAR;SAXOPHONE,Y,NOT BAD
MOUSES,PIRATES,POTION SELLERS,DISPASSION,IDK,XYLOPHONE,N,NOT GREAT
DISTRACTION;SECRET,THEATRE;EMBEDDED,TRANSFORMATION,DIGERIDOO;/,?
```
will output a chart like so:
```
┌─────────────┬───────────┬────────────────┬────────────┬────────────────┬───────────┬─────┬───────────┐
│    MICE     │ DOUBLOONS │    RASCALS     │ COMPASSION │    COLD WAR    │   OBOE    │ Y/N │  RESULT   │
├─────────────┼───────────┼────────────────┼────────────┼────────────────┼───────────┼─────┼───────────┤
│   2 BLIND   │   GOLD    │     LITTLE     │ ROBOT LOVE │    HOT WAR     │ SAXOPHONE │  Y  │  NOT BAD  │
├─────────────┼───────────┼────────────────┼────────────┼────────────────┼───────────┼─────┼───────────┤
│   MOUSES    │  PIRATES  │ POTION SELLERS │ DISPASSION │      IDK       │ XYLOPHONE │  N  │ NOT GREAT │
├─────────────┼───────────┼────────────────┼────────────┼────────────────┼───────────┼─────┼───────────┤
│ DISTRACTION │  SECRET   │    THEATRE     │  EMBEDDED  │ TRANSFORMATION │ DIGERIDOO │  /  │     ?     │
└─────────────┴───────────┴────────────────┴────────────┴────────────────┴───────────┴─────┴───────────┘
```
