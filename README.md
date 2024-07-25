# BBS-Ada-Widgets
## Introduction
A collection of Gtk widgets written in Ada for use with Gtkada.  Additional
widgets will be added or current widgets updated as I feel the need.

### The Dial Widget
The primary widget is a Dial widget.  This is probably most useful as an example
of how to create a custom Gtkada widget.  There is quite a comprehensive set of
dial widgets already available, but their complexity may obscure some of the
basics for creating your own.  The dial widget has a number of options for
setting the type of dial.

* Setting the size, minimum, and maximum values.  This is set at creation and can't be changed.
* Is the dial a full circle or an arc.
* Does the dial use a pointer or a filled arc.
* Number of major and minor tick marks.
* Red and yellow lines and both the lower and upper ends of the dial.
* Does the dial indication jump to a new value or does it slew.

Note that these options were added in an ad hoc fashion based on what I was interested in.

### Flight Instrument Widgets
A set of widgets is being developed that emulate the basic flight instruments.  As
these are under development, they should be considered to be experimental and
subject to change.

NOTE: These widgets are suitable for demonstrations, mock-up, simulations, and the
like.  They are not suitable for use as actual aircraft instruments.

The current set is:
* Attitude Indicator
* Altimeter
* Compass
* Turn Coordinator
* Vertical Speed
* Airspeed

## Usage
This software is licensed with GPL v3.  If you want to use this software under a different
license, contact me and we may be able to work something out.
