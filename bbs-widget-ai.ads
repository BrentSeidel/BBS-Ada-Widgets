--
--   author Brent Seidel
--   date 2-Feb-2016
--
-- This is a simple attitude indicator widget.  It provides an indication of
-- pitch and roll similar to attitude indicators found in airplanes.
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
package bbs.widget.ai is
   --
   type bbs_ai_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_ai is access all bbs_ai_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_ai);
   procedure initialize(self : not null access bbs_ai_record'class);
   --
   -- Width is the width of the widget in pixels.  The widget is square so that
   -- only the width needs to be specified.  (this may change in the future to
   -- allow for a rectangular widget).
   --
   procedure setup(self : not null access bbs_ai_record'class;
                   width : Integer; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the ai to display.  It should be within
   -- the range that the ai can display.
   --
   procedure set_value(self : in out bbs_ai_record'Class; pitch : Float; roll : Float);
   --
   -- This is called when the data source for the ai has failed or is
   -- unavailable.  It replaces the ai display with a red "X".
   --
   procedure set_failed(self : in out bbs_ai_record'Class; value : Boolean);
   --
   -- This controls the slew option of the ai.  If set to false, the ai
   -- jumps to the new value.  If set to true, the ai moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_ai_record'Class; value : Boolean; rate : Float);

private
   type bbs_ai_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         size : Integer;
         pitch : Float;
         roll : Float;
         slew_rate : Float;
         dsp_pitch : Float;
         dsp_roll : Float;
         failed : Boolean;
         slew : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant := 2.0*Ada.Numerics.Pi;
   sqrt_2 : constant Float := math.Sqrt(2.0);

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_ai(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_horizon(self : access bbs_ai_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context);
   procedure draw_fixed(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context);
   procedure draw_roll(Self : access bbs_ai_record'Class; context : Cairo.Cairo_Context);

end;
