# BBS-Ada-Widgets
##Introduction
A collection of Gtk widgets written in Ada for use with Gtkada.  Currently it only
contains a Dial widget.  This is probably most useful as an example of how to create
a custom Gtkada widget.  There is quite a comprehensive set of dial widgets already
available, but their complexity may obscure some of the basics for creating your own.

###The Dial Widget
The dial widget has a number of options for setting the type of dial.
* Setting the size, minimum, and maximum values.  This is set at creation and can't be changed.
* Is the dial a full circle or an arc.
* Does the dial use a pointer or a filled arc.
* Number of major and minor tick marks.
* Red and yellow lines and both the lower and upper ends of the dial.
* Does the dial indication jump to a new value or does it slew.
Note that these options were added in an ad hoc fashion based on what I was interested in.

##Usage
This software is licensed with GPL v3.  If you want to use this software under a different
license, contact me and we may be able to work something out.