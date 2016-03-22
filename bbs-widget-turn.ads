--
--   author: Brent Seidel
--   date: 22-Mar-2016
--
-- This is a simple turn coordinator similar to the ones found in light aircraft.t
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
package bbs.widget.turn is
   --
   type bbs_turn_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with private;
   type bbs_turn is access all bbs_turn_record'class;
   --
   -- Generics used
   --
   package math is new Ada.Numerics.Generic_Elementary_Functions(Float);
   --
   -- Initialization and setup
   --
   function  Get_Type return Glib.GType;
   procedure gtk_new(self : in out bbs_turn);
   procedure initialize(self : not null access bbs_turn_record'class);
   --
   -- Width is the width of the widget in pixels.  The widget is square so that
   -- only the width needs to be specified.
   --
   procedure setup(self : not null access bbs_turn_record'class;
                   width : Integer; parent : Gdk.Gdk_Window);
   --
   -- Options
   --
   -- This simply sets the value for the turn coordinator to display.  The full-
   -- scale value is a standard rate turn - 3 degrees/second or a complete 360
   -- degree turn in 2 minutes.
   --
   procedure set_value(self : in out bbs_turn_record'Class; value : Float);
   --
   -- This is called when the data source for the turn has failed or is
   -- unavailable.  It replaces the turn display with a red "X".
   --
   procedure set_failed(self : in out bbs_turn_record'Class; value : Boolean);
   --
   -- This controls the slew option of the turn.  If set to false, the turn
   -- jumps to the new value.  If set to true, the turn moves to the new value
   -- at a speed set by the rate parameter.  The larger the rate parameter, the
   -- faster the pointer moves.  The actual speed depends on the rate of the
   -- callbacks.
   --
   procedure set_slew(self : in out bbs_turn_record'Class; value : Boolean; rate : Float);
   --
   -- Set the slide-slip value.  This is just lateral acceleration in Gs.
   --
   procedure set_slip(self : in out bbs_turn_record'Class; value : Float);

private
   type bbs_turn_record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record with
      record
         radius : Float;
         size : Integer;
         value : Float;
         slew_rate : Float;
         pointer : Float;
         slip : Float;
         failed : Boolean;
         slew : Boolean;
         callback_id : Glib.Guint;
      end record;

   two_pi : constant Float := 2.0*Ada.Numerics.Pi;

   klass : aliased Glib.Object.Ada_GObject_Class := Glib.Object.Uninitialized_Class;

   function slew_handler(Self : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                         Frame_Clock : not null access Gdk.Frame_Clock.Gdk_Frame_Clock_Record'Class) return Boolean;
   function draw_turn(Self : access Gtk.Widget.Gtk_Widget_Record'Class; context : Cairo.Cairo_Context) return boolean;
   procedure draw_Background(self : access bbs_turn_record'Class; context : Cairo.Cairo_Context);
   procedure draw_failed(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context);
   procedure draw_pointers(Self : access bbs_turn_record'Class; context : Cairo.Cairo_Context);
end;
