# pdf2csv

WORK IN PROGRESS

A Clojure application that extracts matrixes from PDF's files and saves those as csv files.

A separate application extracts the word positions from a PDF.

## Usage

FIXME

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

## How it works

The input file contains the positions from the PDF. The format of a single word position:
1. page number
2. line number
3. font name
4. font size
5. space width
6. x1 (x1,y1) - (x2,y2) are the bounding box of the word 
7. x2
8. y1
9. y2
10. word

The font information is currently not used, but will serve to estimate the minimum width / height.

### Spans 

The list of words is sorted by page / line / x1.
For each page/line, the spans are calculated (the white space between the words)
The small spans are removed (minimum width as given as input parameter)

### Stripes

The spans are transformed to stripes. Spans are defined on a single line, in constrast stripes will cover multiple lines. If a span of a line N overlaps a span of the line N+1, the common part of the overlapping spans will form a stripe. A stripe will be stretched as long there is an overlapping span in the next line.
The stripes are vertical separators without crossing a word.

The process of building stripes is repeated for each line.
Line 1 will have stripes that start on that line and descend as far as possible. Line 2 will have stripes that start on line 2 and descend as far as possible,...

The small stripes are removed (not wide or height enought, defined as input parameter)

### Columns

A column is defined as the space between 2 stripes. The shortest stripe defines the height of the column. Like the stripes, each line will have a number of columns that start on that line. 

For each column, a simple statistic 'word-fillage'  is calculated, which reflects the number of words in that column relative to the number of lines covered by that column. (The idea is to prefer full columns)

Again, a number of small columns are removed (almost-empty columns).

### Score

The goal is to give a score to each line. The score indicates how 'good' the columns are. The current implementation uses a simple score: the score is the size of the matrix (columns x height).

Smarter strategies could try to merge several columns, or use the height of the columns per line etc...

### CSV

The content of the best grid is stored as a csv file.



