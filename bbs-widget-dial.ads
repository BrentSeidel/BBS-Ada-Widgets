--
--   author:  Brent Seidel
--   version: V00.01
--   date:    23-Mar-2016
--
-- This file is copyrighted (C) 2016 by Brent Seidel.  It is available under
-- version 3 of the GPL.  See the file LICENSE for more details.
--
-- Please contact the author if you are interested in other licensing arrangements.
--
with Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics;
with Gtkada.Types;
with Gtk.Main;
with Gtkada.Builder;
with Gtk.Widget;
with Gtk.Misc;
with Gtk.Level_Bar;
with Gtk.Progress_Bar;
with Gtk.Drawing_Area;
with Gdk.Window;
with Gdk.Frame_Clock;
with Glib;
use type Glib.Gdouble;
use type Glib.Gint;
with Glib.Object;
with Glib.Properties;
with Cairo;
package bbs.widget.dial is
   --
   -- Define a widget for a dial.  The dial has a number of options and not all
   -- combinations of options make sense.  In some cases unsexpected behaviour
   -- may occur.
   --
   type bbs_dial_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_dial is access all bbs_dial_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_dial);
   procedure initialize(self : not null access bbs_dial_record'class);
   procedure setup(self : not null access bbs_dial_record'class; minimum : float; maximum : float;
                   radius : Float; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the dial to display.  It should be within
   -- the range that the dial can display.
   --
   procedure set_value(self : in out bbs_dial_record'Class; value : Float);
   --
   -- This is called when the data source for the dial has failed or is
   -- unavailable.  It replaces the dial display with a red "X".
   --
   procedure set_failed(self : in out bbs_dial_record'Class; value : Boolean);
   --
   -- This controls the slew option of the dial.  If set to false, the dial
   -- jumps to the new value.  If set to true, the dial moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_dial_record'Class; value : Boolean; rate : Float);
   --
   -- If set_park is called with a True value, the pointer parks at the minimun
   -- and maximum limits of the dial.  If it is False, the pointer can move
   -- beyond the limits.
   --
   procedure set_park(self : in out bbs_dial_record'Class; value : Boolean);
   --
   -- Sets the style of the dial.  Either a complete circle or an arc.  The
   -- start and end of the arc can be specified in radians.  If they're not
   -- specified default values are used.
   --
   procedure set_arc(self : in out bbs_dial_record'Class; value : Boolean);
   procedure set_arc(self : in out bbs_dial_record'Class; value : Boolean; start : Float; finish : Float);
   --
   -- Sets if the value is indicated by a filled arc or by a pointer.
   --
   procedure set_fill(self : in out bbs_dial_record'Class; value : Boolean);
   --
   -- This set of procedures is used to set lower and upper red and yellow lines.
   -- The order is expected to be lower red line, lower yellow line, upper yellow
   -- line, and upper red line.  All of these are optional.
   --
   procedure set_low_red(self : in out bbs_dial_record'Class; valid : Boolean; value : Float);
   procedure set_low_yellow(self : in out bbs_dial_record'Class; valid : Boolean; value : Float);
   procedure set_high_yellow(self : in out bbs_dial_record'Class; valid : Boolean; value : Float);
   procedure set_high_red(self : in out bbs_dial_record'Class; valid : Boolean; value : Float);
   --
   -- This procedure is used to set the number of major and minor ticks.  Use a
   -- value of zero to omit the ticks.
   --
   procedure set_ticks(self : in out bbs_dial_record'Class; major : Integer; minor : Integer);

private
   type bbs_dial_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         major : Integer;
         minor : Integer;
         radius : Float;
         size : Integer;
         arc_start : Float;
         arc_end : Float;
         low_red : Float;
         low_yellow : Float;
         high_yellow : Float;
         high_red : Float;
         min : Float;
         max : Float;
         value : Float;
         slew_rate : Float;
         pointer : Float;
         failed : Boolean;
         park : Boolean;
         slew : Boolean;
         arc : Boolean;
         fill : Boolean;
         low_red_present : Boolean;
         low_yellow_present : Boolean;
         high_yellow_present : Boolean;
         high_red_present : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant := 2.0*Ada.Numerics.Pi;

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_dial(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_dial_arc(self : access bbs_dial_record'Class; context : Cairo.Cairo_Context);
   procedure draw_ticks(self : access bbs_dial_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context);
   procedure draw_pointer(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context);
   procedure draw_filled(Self : access bbs_dial_record'Class; context : Cairo.Cairo_Context);
   function compute_angle(Self : access bbs_dial_record'Class; value : Float) return Float;

end;
